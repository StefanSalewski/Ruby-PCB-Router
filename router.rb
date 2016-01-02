# file: router.rb
# note: source code is indented with tabs, tab-width=2
#
# A rubberband topological router based on the region concept mentioned in
# Tal Dayan's PhD thesis and other papers.
# See http://www.delorie.com/archives/browse.cgi?p=geda-user/2015/10/11/08:56:15
#
# c Stefan Salewsk, mail@ssalewski.de
# License GPL
# Version 0.21 29-DEC-2015
#
# For testing with random points, call
# ruby router.rb
# or ruby router.rb seed
# for reproduceable results
#
# ruby pcbtr.rb
# will route the t.pcb file, currently component side only
#
# Data type of coordinates
# ------------------------
# Generally integer data type should make our life easierer, but then we have to be careful
# when float datatype is mixed in by math operations. Special care is necessary when we use
# coordinates as Hash keys -- float and integer are of course different keys.
# Current default unit in PCB file format is 0.01 mil, generally in combination with integer
# numbers. (Internal unit in PCB program is nm.)
# For now we use 0.01 mil as our general coordinates. (For Ruby performance it may be a good idea
# to use only integers less than 2^30 in magnitude?)
#
# Markers used:
# TODO: needs improving
# UGLY: we should remove or at least carefully check this...
#
# Used termes:
# trace_width: i.e 10mil; we should avoid unprecise PCB term thickness
# trace_clearance: copper free distance to next element -- in PCB file format there is a factor of 2
# via_diameter: outer diameter of copper annulus; avoid unprecise term size
# via_drill_diameter:
# via_clearance: copper free distance to next elemment
# via_mask_diameter: 
#
# we use the term "routing style" to name a set of {trace_width, trace_clearance, via...}
#
require 'set'
require_relative '../RBOOST/Fibonacci_Queue_Hash/rboost_fibonacci_queue'
require_relative '../RCGAL/rcgal_cdt'
require_relative '../RAPOLLONIUS/rcgal_apollonius'

require_relative 'rsupport'
require_relative 'canvas'

$glob = 0 # global variable for debugging and statistics

module RBR # Rubber Band Router

@@one = 1
@@two = 2

# Some global constants -- we try to use integer range less than 2 ** 30
# Note: Our internal unit is 0.01 mil
Maximum_Board_Diagonal = MBD = (2 ** 30 - 2 ** 24) # something like our own Infinity
Average_Via_Diameter = AVD = 50_00 # 50 mil -- we may determine that value from actual PCB board data
Average_Trace_Width = ATW = 10_00 # 10 mil -- we may determine that value from actual PCB board data
Board_Size = 800 # cairo PNG picture

# the following constants are used for tests with random numbers, not for real PCB board
PCB_Size = 140000 # size of our set of random vertices
Points = 64 # terminal count for test
Pin_Radius = 1000
Trace_Width = 1200
Clearance = 800
MinCutSize = 6000

	def self.init_seed
		seed = (ARGV[0] ? ARGV[0].to_i : rand(1000))
		print 'random seed is ', seed, "\n"
		srand(seed)
		seed
	end

	# 	  b
	#    ^
	#   /
	# o/--------> a
	#
	def self.boolean_really_smart_cross_product_2d_with_offset(a, b, o)
		a = a.vertex if a.is_a? RBR::Region
		b = b.vertex if b.is_a? RBR::Region
		o = o.vertex if o.is_a? RBR::Region
		ax = a.x
		ay = a.y
		bx = b.x
		by = b.y
		ox = o.x
		oy = o.y
		fail if ax == bx && ay == by # undefined result, we should raise an exeption!
		ax -= ox
		ay -= oy
		bx -= ox
		by -= oy
		fail if (ax == 0 && ay == 0) || (bx == 0 && by == 0) # zero length should not occur
		if (p = ax * by - ay * bx) != 0
			p > 0
		else
			ax != bx ? ax < bx : ay < by # straight line -- ensure arbitrary but well defined result
		end
	end

	def self.xboolean_really_smart_cross_product_2d_with_offset(a, b, o)
		ax = a.vertex.x + a.ox
		ay = a.vertex.y + a.oy
		bx = b.vertex.x + b.ox
		by = b.vertex.y + b.ox
		ox = o.vertex.x
		oy = o.vertex.y
		fail if ax == bx && ay == by # undefined result, we should raise an exeption!
		ax -= ox
		ay -= oy
		bx -= ox
		by -= oy
		fail if (ax == 0 && ay == 0) || (bx == 0 && by == 0) # zero length should not occur
		if (p = ax * by - ay * bx) != 0
			p > 0
		else
			ax != bx ? ax < bx : ay < by # straight line -- ensure arbitrary but well defined result
		end
	end

	# same as above for plain numbers -- used in qbors()
	def self.boolean_really_smart_cross_product_2d(ax, ay, bx, by)
		fail if ax == bx && ay == by # undefined result, we should raise an exeption!
		fail if (ax == 0 && ay == 0) || (bx == 0 && by == 0) # zero length should not occur
		if (p = ax * by - ay * bx) != 0
			p > 0
		else
			ax != bx ? ax < bx : ay < by # straight line -- ensure arbitrary but well defined result
		end
	end

# Net connecting two terminals (2Net)
class NetDesc
	@@id = 0
	attr_accessor :t1_name, :t2_name # name of terminals
	attr_accessor :style_name
	attr_accessor :trace_width
	attr_accessor :trace_clearance
	attr_accessor :id
	attr_accessor :pri # used for sorting by length -- poor mans net ordering
	attr_accessor :flag # used for sorting of attached nets
	def initialize(t1_name, t2_name, trace_width = Trace_Width, trace_clearance = Clearance)
	  @id = @@id += 1
		@pri = 0
		@t1_name, @t2_name = t1_name, t2_name
		@trace_width, @trace_clearance = trace_width, trace_clearance
	end
end

class NetDescList < Array
end

# Incident or attached net of a terminal
class Step
	attr_accessor :id # id of this net
	attr_accessor :net_desc
	attr_accessor :vertex # the terminal we are attached or incident to
	attr_accessor :prev, :next # terminal
	attr_accessor :pstep, :nstep # previous and next step
	attr_accessor :a, :b, :d, :g, :dir # for sorting, a few may be obsolete
	attr_accessor :radius # radius of arc
	attr_accessor :score # for sorting attached nets of a terminal by angle 
	attr_accessor :index # for sorting
	attr_accessor :ref # also for sorting
	attr_accessor :rgt # tangents of terminal for this step -- left or right side in forward direction
	attr_accessor :outer # do we use the outer lane at this terminal
	attr_accessor :xt # do tangents cross each other -- if so we can collapse this concave step
	attr_accessor :lr_turn # left or right turn

	def initialize(prev, nxt, id)
		@prev, @next, @id = prev, nxt, id
		@radius = 0 # default for incident net
		@outer = false
	end
end

class Tex # plain (temporary) terminal (verTex)
	attr_accessor :x, :y
	def initialize(x, y)
		@x, @y = x, y
	end
end

# Terminal
class Vertex < CGAL::Vertex
	@@id = @@cid = 0
	attr_accessor :id # general unique id number
	attr_accessor :cid # cluster id; -1 for plain pins, nonnegativ values if vertex is part of a pad/cluster
	attr_accessor :vis_flag # for debugging, graphical color mark
	attr_accessor :core # outer copper of pin itself, without attached traces
	attr_accessor :radius # outer copper of outermost attached trace, equal to core when no attached nets exist
	attr_accessor :separation # outer clearance of pin itself or of outermost attached trace
	attr_accessor :neighbors # Vertex/Terminal, the neighbors in the delaunay triangulation
	attr_accessor :incident_nets # Step, nets incident to this terminal
	attr_accessor :attached_nets # Step, nets attached to this terminal
	attr_accessor :name # name of the Terminal, i.e. U7_3 -- pin 3 of IC U7
	attr_accessor :tradius, :trgt # temporary data
	attr_accessor :outer # temporary data for routing,
	attr_accessor :lr_turn # later copied to step
	attr_accessor :via # vertex is a via
	attr_accessor :num_inets # how many incident nets should this vertex get

	def initialize(x = 0, y = 0, r = Pin_Radius, c = Clearance)
		super(x, y)
		@num_inets = 0
		@via = false
		@tradius = 0
		@vis_flag = 0
	  @id = @@id
		@cid = -1
		@@id += 1
		@radius = @core = r
		@separation = c
		@name = ''
		@neighbors = Array.new
		@incident_nets = Array.new
		@attached_nets = Array.new
	end

	def self.reset_class
	@@id = @@cid = 0

	end

	def self.begin_new_cluster # class method!
		@@cid += 1
	end

	def add_to_current_cluster
		@cid = @@cid
	end

	def xy
		return x, y
	end

	# UGLY:
  def reset_initial_size
		@radius, @separation = @core, Clearance
	end

	# UGLY: may be obsolete -- at least it is only an estimation
	def resize
		reset_initial_size
		attached_nets.each{|step|
			net = step.net_desc
			trace_sep = [@separation, net.trace_clearance].max
			@radius += trace_sep + net.trace_width
			step.radius = @radius - net.trace_width * 0.5
			@separation = net.trace_clearance
		}
	end

	# UGLY:
	# assume worst case order --> max radius
	def unfriendly_resize
		cl = attached_nets.map{|step| step.net_desc.trace_clearance}
		@radius = @core + attached_nets.map{|step| step.net_desc.trace_width}.inject(0){|sum, el| sum + el}
		@radius += cl.permutation.map{|el| (el.push(@separation))}.map{|el| s = 0; el.each_cons(2){|a, b| s += [a,b].max}; s}.max
		@separation = cl.push(@separation).max
	end

	# UGLY: may be obsolete -- at least it is only an estimation
  def update(s)
		net = s.net_desc
		trace_sep = [@separation, net.trace_clearance].max
		@radius += trace_sep + net.trace_width
		s.radius = @radius - net.trace_width * 0.5
		@separation = net.trace_clearance
	end

	# UGLY: returns step -- may become obsolete
  def net(id)
		incident_nets.each{|s| return s if s.id == id}
 		attached_nets.each{|s| return s if s.id == id}
		return nil
	end

  # UGLY: delete step -- currently we need this, but we should try to avoid it, at least the expensive resize operation
	def new_delete_net(step)
		incident_nets.delete_if{|s| step == s}
		attached_nets.delete_if{|s| step == s}
		resize
	end

	# UGLY:
	def _full_angle(s)
		return nil unless s.next && s.prev
		v = s.vertex
		d = Math.atan2(s.next.y - v.y, s.next.x - v.x) - Math.atan2(v.y - s.prev.y, v.x - s.prev.x)
		if d < -Math::PI
		 	d += 2 * Math::PI
		elsif d > Math::PI
			d -= 2 * Math::PI
		end
		return d
	end

	# UGLY: check and improve
	def sort_attached_nets # by angle of total trace
		unless attached_nets.length < 2
			attached_nets.each{|n|
				fail unless n.vertex == self
				#n.index = _tangents_full_angle(n) # we may need the tangents angle?
				n.index = _full_angle(n) * (n.rgt ? 1 : -1)
			}
			attached_nets.sort_by!{|n| n.index}
			attached_nets.each_with_index{|n, i| n.index = i}
			shash = Hash.new
			attached_nets.each{|n| # group attached nets with same angle (overlapping)
				l = n.prev
				r = n.next
				n.net_desc.flag = 1
				if shash.has_key?([l, r])
					shash[[l, r]] << n
				elsif shash.has_key?([r, l])
					n.net_desc.flag = -1 # inverted direction
					shash[[r, l]] << n
				else
					shash[[l, r]] = [n]
				end
			}
			shash.each_value{|group| # fine sort each group by following the traces
				if group.length > 1
					group.reverse! # for testing -- initialy reversing the group should give same result!
					group.each{|el| el.ref = el}
					indices = Array.new
					group.each{|el| indices << el.index}
					indices.sort!
					rel = Hash.new
					[-1, 1].each{|direction|
						gr = group.dup
						final = true # for first direction we may get only a preliminary order?
						while gr.length > 1
							gr.map!{|el| (el.net_desc.flag == direction ? el.pstep : el.nstep)} # walk in one direction
							gr.each{|el| el.ref = (el.net_desc.flag == direction ? el.nstep.ref : el.pstep.ref)}
							gr.each{|el| el.score = _full_angle(el)}
							unresolved_combinations = false
							gr.combination(2).each{|el|
								a, b = *el
								relation = rel[[a.ref, b.ref]]
								if !relation || relation.abs < 2
									if !a.score
										c = ((b.rgt == b.ref.rgt) ? 1 : -1)
									elsif !b.score
										c = ((a.rgt == a.ref.rgt) ? -1 : 1)
									else
										if (a.score * a.net_desc.flag - b.score * b.net_desc.flag).abs < 1e-6
										#if ((a.score - b.score).abs < 1e-6) || ((a.score - b.score).abs < 1e-6)
											c = 0
										else
											#c = ((a.score * (a.rgt ? 1 : -1) * ((a.rgt == a.ref.rgt) ? 1 : -1)) <=> (b.score * (b.rgt ? 1 : -1) * ((b.rgt == b.ref.rgt) ? 1 : -1)))
											c = ((a.score * (a.ref.rgt ? 1 : -1)) <=> (b.score * (b.ref.rgt ? 1 : -1))) # same as above
										end
									end
									if c != 0
										if  final # indicate final relation
											c *= 2
										end
											rel[[a.ref, b.ref]] = c
											rel[[b.ref, a.ref]] = -c
									else
										unresolved_combinations = true
									end
								end
							}
							break unless unresolved_combinations
							gr.keep_if{|el| el.next && el.prev}
						end
						fail if unresolved_combinations # we should get at least a preliminary relation
						break if final # indeed always -- we have no preliminary relations
					}
					group.sort!{|a, b| rel[[a, b]]} # do we need rel[[a, b] <=> 0 to ensure -1,0,1 in block? 
					group.each{|el| el.index = indices.shift}
				end
			}
			attached_nets.sort_by!{|el| -el.index}
		end
	end
end

#   \|/       \|/
#    |        /\
#    |        | |
#   _|/      _| |/ 
#    |\       | |\
# when we route a net, we split regions along the path in left and right region.
# so new paths can not cross this path any more.
# problem: first and last terminal.
# Current solution: Use a hash to store forbidden paths for these terminals
# 
class Region
	attr_accessor :vertex, :neighbors, :incident, :outer
	attr_accessor :g, :ox, :oy
	attr_accessor :rx, :ry
	attr_accessor :a # for sorting
	attr_accessor :lr_turn
	attr_accessor :idirs
	attr_accessor :odirs

  def initialize(v)
		@g = 1
		@ox = @oy = 0
		@vertex = v
		@rx = v.x
		@ry = v.y
		@neighbors = Array.new
		@incident = true
		@outer = false
		@idirs = Array.new
		@odirs = Array.new
	end

	def qbors(old)
		if old
			ox = self.vertex.x
			oy = self.vertex.y
			ax = old.rx - ox
			ay = old.ry - oy
			@neighbors.each{|el|
				next if el == old
				bx = el.rx - ox
				by = el.ry - oy
				fail if old.vertex == el.vertex && self.idirs.empty?
				turn = RBR::xboolean_really_smart_cross_product_2d_with_offset(old, el, self)
				bx = el.rx - ox
				by = el.ry - oy
				inner = true 
				outer = self.incident
				unless self.odirs.empty?
					outer = true
					self.odirs.each{|zx, zy|
						if turn
							j = ax * zy >= ay * zx && bx * zy <= by * zx # do we need = here?
						else
							j = ax * zy <= ay * zx && bx * zy >= by * zx
						end
						break unless (outer &&= j)
					}
					inner = !outer
				end
				self.idirs.each{|zx, zy|
					if turn
						j = ax * zy >= ay * zx && bx * zy <= by * zx # do we need = here?
					else
						j = ax * zy <= ay * zx && bx * zy >= by * zx
					end
					if j
						inner = false
					else
						outer = false
					end
					next unless inner || outer
				}
				yield [el, inner, outer]
			}
		else
			@neighbors.each{|el| yield [el, true, true]}
		end
	end

	def distance_to(other)
		Math.hypot(@vertex.x - other.vertex.x, @vertex.y - other.vertex.y)
	end
end

# note: the flow depends on the order of the traces -- trace with less clearance adjanced to trace with much clearance?
class Cut
  attr_accessor :cap # vertex distance
	attr_accessor :free_cap # cap - vertex copper - copper of passing traces
	attr_accessor :cv1, :cv2 # clearance of the two adjanced vertices
	attr_accessor :cl # array of clearances for each trace passing -- order is important for overal space occupied
  def initialize(v1, v2)
		@cap = Math::hypot(v1.x - v2.x, v1.y - v2.y)
		@free_cap = @cap - v1.core - v2.core
		@cv1 = Clearance # UGLY:
		@cv2 = Clearance
		@cl = Array.new
	end

	# return Maximum_Board_Diagonal (MBD) when there is no space available, or
	# a measure for squeeze -- multiple of Average_Via_Size going to zero if there is much space available
	def squeeze_strength(trace_width, trace_clearance)
		if @cl.empty?
			s = ((@cv1 < @cv2 && @cv1 < trace_clearance) ? @cv2 + trace_clearance : (@cv2 < trace_clearance ? @cv1 + trace_clearance : @cv1 + @cv2))
		else
			@cl.push(trace_clearance)
			#s2 = cl.permutation.map{|el| (el.unshift(@cv1).push(@cv2))}.map{|el| s = 0; el.each_cons(2){|a, b| s += [a, b].max}; s}.max
			# ss = cl.permutation.map{|el| el.unshift(@cv1).push(@cv2); s = 0; el.each_cons(2){|a, b| s += [a, b].max}; s}.max # slow
			# faster!
			ll = @cl.length / 2
			hhh = @cl.sort.reverse[0..ll] * 2
			hhh.pop if @cl.length.even?
			hhh.push(@cv1)
			hhh.push(@cv2)
			hhh.sort!
			hhh.shift(2)
			s = hhh.inject(0){|sum, v| sum + v}
			### fail unless s == ss
			@cl.pop
		end
		s = @free_cap - trace_width - s
		s < 0 ? MBD : 10 * AVD * ATW / (ATW + s * 2)
	end

	# we actually route that trace through this cut
	def use(trace_width, trace_clearance)
		@free_cap -= trace_width
		@cl << trace_clearance
	end
end

# we put only one pin in each cell -- for testing
CS = 3 * Pin_Radius

class Router # UGLY:
	attr_accessor :filename
	attr_accessor :netlist
	attr_accessor :pic # debugging
	attr_accessor :rstyles
	attr_accessor :file # pcb_layout_data_file
	attr_accessor :edges_in_cluster # diagonal edge in pad/cluster, forbidden path, we may consider removing it from triagulation
  def initialize(b1x, b1y, b2x, b2y)
	Vertex.reset_class
		@b1x, @b1y, @b2x, @b2y= b1x, b1y, b2x, b2y # corners of the PCB board
		@edges_in_cluster = RouterSupport::Hash_with_ordered_array_index.new
		@name_id = 0
	  @path_ID = 0
		@image = Cairo::ImageSurface.new(Board_Size, Board_Size)
		@pic = Canvas::Pic.new(@image)
		@pic.translate(40, 40)
		@pic.scale(0.9, 0.9)
		x_extent = (b2x - b1x).abs
		y_extent = (b2y - b1y).abs
		max_extent = [x_extent, y_extent].max
		@pic.scale(Board_Size.fdiv(max_extent), Board_Size.fdiv(max_extent))
		@pic.translate(-b1x, -b1y)
		@pic.translate((max_extent - x_extent) * 0.5, (max_extent - y_extent) * 0.5)
		@pic.set_source_rgba(0.8, 0.8, 0.8, 1)
		@pic.paint
		@cdt = CGAL::CDT.new
		@cdt_hash = Hash.new
		@cell = Hash.new
	end

	def next_name
		@name_id += 1
		return @name_id.to_s
	end

	# UGLY:
	# insert some vertices on the boarder of the PCB board, should help
	def insert_pcb_border
		a, b = [@b1x, @b2x].minmax
		d = (b - a).fdiv(25)
		a -= d
		b += d
		dx = (b - a).fdiv(10)
		(a..b).step(dx){|x|
			v = Vertex.new(x, @b1y - d)
			v.name = 'no'
			@cdt.insert(v)
			v = Vertex.new(x, @b2y + d)
			v.name = 'no'
			@cdt.insert(v)
		}
		a, b = [@b1y, @b2y].minmax
		d = (b - a).fdiv(25)
		a -= d
		b += d
		dy = (b - a).fdiv(10)
		a += dy
		b -= dy
		(a..b).step(dy){|y|
			v = Vertex.new(@b1x - d, y)
			v.name = 'no'
			@cdt.insert(v)
			v = Vertex.new(@b2x + d, y)
			v.name = 'no'
			@cdt.insert(v)
		}
	end

	def insert_rescue_vias(l)
		#return
		l.each{|x, y|
			insert_pcb_vertex('rescue_via', x, y, 1000, 1000)
		}
	end

	# UGLY:
	def insert_pcb_vertex(name, x, y, vt, vc)
		return if @cdt_hash.include?([x, y]) # occurs for t.pcb
		v = Vertex.new(x, y, vt, vc)

		v.via = true
		v.name = name
		@cdt_hash[[x, y]] = v
		@cdt.insert(v)
	end

	def insert_cluster(c)
		fail if c.convex_pin_hull.empty?
		Vertex.begin_new_cluster unless (n = c.convex_pin_hull.size) == 1
		last_vertex = first_vertex = nil
		c.convex_pin_hull.each{|cv|
			x = c.mx + cv.rx
			y = c.my + cv.ry
			if @cdt_hash.include?([x, y])
				fail
			else
				v = Vertex.new(x, y, cv.thickness * 0.5, cv.clearance)
				v.name = c.name
				v.add_to_current_cluster unless n == 1
				first_vertex ||= v
				@cdt_hash[[x, y]] = v
				@cdt.insert(v)
				@cdt.insert_constraint(v, last_vertex) if last_vertex
				last_vertex = v
			end
		}
		@cdt.insert_constraint(last_vertex, first_vertex) if n > 2
	end

	# UGLY: rename
	def test_cluster
		@cdt.edges_in_constrained_polygons{|v1, v2|
			@edges_in_cluster[v1, v2] = true
		}
	end

	# UGLY: 
  def generate_test_vertices
		# put a virtual pin on each corner of our board -- we should need this
		[0, PCB_Size].repeated_permutation(2).each{|el|
			v = Vertex.new(*el)
			@cdt.insert(v)
			@cell[[el[0] / CS, el[1] / CS]] = 1
		}
		id = 8
		while id < Points
			r1, r2 = rand(PCB_Size - PCB_Size / 50) + PCB_Size / 100, rand(PCB_Size - PCB_Size / 50) + PCB_Size / 100
			if @cell[[r1 / CS, r2 / CS]] == nil
				@cell[[r1 / CS, r2 / CS]] = 1
				#if true#unless (300..500).include?(r1) or (300..500).include?(r2)
				v = Vertex.new(r1, r2)
				@cdt.insert(v)
				id += 1
			end
		end

	end

	def generate_netlist(l)
		@netlist = NetDescList.new
		l.each{|x1, y1, x2, y2, style|
			v1 = @cdt_hash[[x1, y1]]
			v2 = @cdt_hash[[x2, y2]]
			fail unless v1 && v2
			v1.num_inets += 1
			v2.num_inets += 1
			v1.name ||= next_name
			v2.name ||= next_name
			net_desc = NetDesc.new(v1.name, v2.name)
			net_desc.style_name = style
			net_desc.pri = (x2 - x1) ** 2 + (y2 - y1) ** 2
			@netlist << net_desc
		}
	end

	def sort_netlist
		@netlist.sort_by!{|el| el.pri} 
	end

  def finish_init(rnd_test = false)
		@vertices = Array.new # maybe later we do not need this array, we have @cdt.each{}
		@regions = Array.new
		@cdt.each{|v|
			@vertices << v
			@regions << Region.new(v)
		}
		if rnd_test
			set = @vertices.select{|el| el.id > 3}.each_slice(2).to_a #.shuffle
			set.sort_by!{|el| (el[0].x - el[1].x) ** 2 + (el[0].y - el[1].y) ** 2}
			@netlist = NetDescList.new
			(0..9).each{|i|
				v1, v2 = *set[i * 3]
				if v1 && v2
					v1.name = i.to_s + 's'
					v2.name = i.to_s + 'e'
					net_desc = NetDesc.new(v1.name, v2.name)
					@netlist << net_desc
				end
			}
		end
		@newcuts = RouterSupport::Hash_with_ordered_array_index.new
		@cdt.each{|v|
			@cdt.neighbor_vertices(v).each{|n|
				v.neighbors << n
				if @regions[v.id]
					@regions[v.id].neighbors << @regions[n.id]
				end
				@newcuts[v, n] = Cut.new(v, n)
				fail unless v.core == v.radius
			}
		}
	end

	# UGLY: for debugging only
	def flag_vertices
	@pic.set_font_size(4 * Pin_Radius)
		@pic.set_source_rgba(1, 0, 0, 1)
		@pic.set_line_width(1)
		@vertices.each{|v|
		@pic.move_to(v.x, v.y)
		#@pic.show_text(v.id.to_s)
			if v.vis_flag != 0
				if v.vis_flag == 1
					@pic.set_source_rgba(1, 1, 1, 1)
				else
					@pic.set_source_rgba(0, 1, 0, 1)
				end
				@pic.new_sub_path
				@pic.set_line_width(1)
				@pic.arc(v.x, v.y, 0.3* Pin_Radius, 0, 2*Math::PI)
				@pic.fill
				@pic.stroke
			end
		}
	end

	# UGLY:
	def draw_vertices
		@pic.set_source_rgba(0, 0, 0, 0.3)
		@pic.set_line_width(100)
		@vertices.each{|v|
			@pic.new_sub_path
			if v.cid == -1
			@pic.arc(v.x, v.y, v.core, 0, 2 * Math::PI)
			else
			@pic.arc(v.x, v.y, Pin_Radius, 0, 2 * Math::PI)
			end
			@pic.fill
			@pic.stroke
			v.neighbors.each{|n|
			if @edges_in_cluster.include?(v, n)
		@pic.set_source_rgba(1, 0, 0, 0.3)
				@pic.move_to(v.x, v.y)
				@pic.line_to(n.x, n.y)
				@pic.stroke
			else
		@pic.set_source_rgba(0, 0, 0, 0.3)
				@pic.move_to(v.x, v.y)
				@pic.line_to(n.x, n.y)
				@pic.stroke
			end
			}
		}
		@pic.stroke
		@pic.set_source_rgba(1, 0, 0, 0.7)
		@pic.set_line_width(600)
		@newcuts.each_pair{|k, v|
		#unless v.cid >= 0 && v.cid == k.cid
			if v.cap < MinCutSize
				#@pic.move_to(k[0].x, k[0].y)
				#@pic.line_to(k[1].x, k[1].y)
			end
		#end
		}
		@pic.stroke
	end

	def gen_vias
		@vertices.each{|v|
			if v.via
				@file.write("Via[#{v.x.round} #{v.y.round} #{(2 * v.core).round} #{Clearance.round} #{0} #{1000} \"\" \"\"]\n")
			end
		}
	end

	def gen_line(x0, y0, x1, y1, w)
		@pic.set_line_width(w)
		@pic.move_to(x0, y0)
		@pic.line_to(x1, y1)
		@pic.stroke
		@file.write("Line[#{x0.round} #{y0.round} #{x1.round} #{y1.round} #{w.round} #{Clearance.round} \"\"]\n")
	end

	# from, to should be in the range -PI..PI (from atan2()) or maybe 0..2PI?
  def gen_arc(x, y, r, from, to, width)
		@pic.new_sub_path
		@pic.set_line_width(width)
		@pic.arc(x, y, r, from, to)
		@pic.stroke
		to += 2 * Math::PI if to < from # cairo does this internally, so PCB program need this fix
		pcb_start_angle = ((Math::PI - from) * 180 / Math::PI).round
		pcb_delta_angle = ((from - to) * 180 / Math::PI).round # never positive -- maybe we should flip angles?
		unless pcb_delta_angle == 0
			@file.write("Arc[#{x.round} #{y.round} #{r.round} #{r.round} #{width.round} #{Clearance.round} #{pcb_start_angle} #{pcb_delta_angle} \"\"]\n")
		end
	end

# http://www.faqs.org/faqs/graphics/algorithms-faq/
# http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
# int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
# {
#   int i, j, c = 0;
#   for (i = 0, j = nvert-1; i < nvert; j = i++) {
#     if ( ((verty[i]>testy) != (verty[j]>testy)) &&
# 	 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
#        c = !c;
#   }
#   return c;
# }
#
# input: array of vertices
# result: the vertices inside the polygon (or on the border?)
	def vertices_in_polygon(p_vertices, test_vertices)
		res = Array.new
		nm1 = p_vertices.length - 1
		test_vertices.each{|tp|
			ty = tp.y
			i = 0
			j = nm1
			c = false
			while i <= nm1
        if ((((p_vertices[i].y <= ty) && (ty < p_vertices[j].y)) ||
             ((p_vertices[j].y <= ty) && (ty < p_vertices[i].y))) &&
            (tp.x < (p_vertices[j].x - p_vertices[i].x) * (ty - p_vertices[i].y) / (p_vertices[j].y - p_vertices[i].y) + p_vertices[i].x))
					 c = !c
				end
				j = i
				i += 1
			end
			res << tp if c
		}
		res
	end

	#     (x2,y2)
	#    /
	#   /    (x0,y0)
	#  /
	# (x1,y1)
	# http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
	def distance_line_point(x1, y1, x2, y2, x0, y0)
		x12 = x2 - x1
		y12 = y2 - y1
		(x12 * (y1 - y0) - (x1 - x0) * y12).abs / Math.hypot(x12, y12)
	end

	def distance_line_point_squared(x1, y1, x2, y2, x0, y0)
		x12 = x2 - x1
		y12 = y2 - y1
		(x12 * (y1 - y0) - (x1 - x0) * y12) ** 2 / (x12 ** 2 + y12 ** 2)
	end

	#      (c)
	#     /
	#    /     (p)
	#   /
	# (b)
	# see http://www.geometrictools.com/
	# see also http://paulbourke.net/geometry/pointlineplane/
	#
	def unused_distance_line_segment_point_squared(bx, by, cx, cy, px, py)
		mx = cx - bx
		my = cy - by
		hx = px - bx
		hy = py - by
		t0 = (mx * hx + my * hy).fdiv(mx ** 2 + my ** 2)
		if t0 <= 0
		elsif t0 < 1
			hx -= t0 * mx
			hy -= t0 * my
		else
			hx -= mx
			hy -= my
		end
		return hx ** 2 + hy ** 2
	end

	def normal_distance_line_segment_point_squared(bx, by, cx, cy, px, py)
		mx = cx - bx
		my = cy - by
		hx = px - bx
		hy = py - by
		t0 = (mx * hx + my * hy).fdiv(mx ** 2 + my ** 2)
		if t0 > 0 && t0 < 1
			(hx - t0 * mx) ** 2 + (hy - t0 * my) ** 2
		else
			Maximum_Board_Diagonal 
		end
	end

	# Intersection point of two lines in 2 dimensions
	# http://paulbourke.net/geometry/pointlineplane/
	def line_line_intersection(x1, y1, x2, y2, x3, y3, x4, y4)
		x2x1 = x2 - x1
		y2y1 = y2 - y1
		return nil if (d = (y4 - y3) * x2x1 - (x4 - x3) * y2y1) == 0 # parallel?
		ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / d
		ub = (x2x1 * (y1 - y3) - y2y1 * (x1 - x3)) / d
		[x1 + ua * x2x1, y1 + ua * y2y1, ua, ub]
	end

	P_IN = 1; P_ON = 0; P_OUT = -1; COLLINEAR = -2 
	# see http://en.wikipedia.org/wiki/Barycentric_coordinates_%28mathematics%29
	def unused_point_in_triangle(x1, y1, x2, y2, x3, y3, x, y)
		d  =  (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
		return COLLINEAR if d == 0 # maybe check if x,y is ... -- currently we do not care
		l1 = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / d
		l2 = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / d
		l3 = 1 - l1 - l2
		min, max = [l1, l2, l3].minmax
		if 0 <= min && max <= 1
			0 < min && max < 1 ? P_IN : P_ON
		else
			P_OUT
		end
	end

	def unused_vertices_in_triangle(x1, y1, x2, y2, x3, y3, vertices)
		d  =  (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
		v_in = Array.new
		if d == 0
			puts 'vertices_in_triangle, degenerated'
			return v_in
		end
		y2my3 = (y2 - y3) / d
		x3mx2 = (x3 - x2) / d
		x1mx3 = (x1 - x3) / d
		y3my1 = (y3 - y1) / d
		vertices.each{|v|
			vxmx3 = v.x - x3
			vymy3 = v.y - y3
			l1 = y2my3 * vxmx3 + x3mx2 * vymy3
			l2 = y3my1 * vxmx3 + x1mx3 * vymy3
			min, max = [l1, l2, 1 - l1 - l2].minmax
			if 0 <= min && max <= 1
				v_in << v
			end
		}
		v_in
	end

	# see http://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
	def unused_hcross(o, a, b)
		(a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])
	end
	def unused_convex_hull(points)
		points.sort!.uniq!
		return points if points.length < 3
		lower = Array.new
		points.each{|p|
			while lower.length > 1 and unused_hcross(lower[-2], lower[-1], p) <= 0 do lower.pop end
			lower.push(p)
		}
		upper = Array.new
		points.reverse_each{|p|
			while upper.length > 1 and unused_hcross(upper[-2], upper[-1], p) <= 0 do upper.pop end
			upper.push(p)
		}
		return lower[0...-1] + upper[0...-1]
	end

	def new_convex_vertices(vertices, prev, nxt, hv1, hv2)
		fail if vertices.include?(prev) || vertices.include?(nxt)
		return vertices if vertices.empty?
		x1, y1, x2, y2 = get_tangents(prev.x, prev.y, prev.tradius, prev.trgt, nxt.x, nxt.y, nxt.tradius, nxt.trgt)
		v1 = Vertex.new(x1, y1)
		v2 = Vertex.new(x2, y2)
		vertices << v1 << v2 << hv1 << hv2
		ag = CGAL::Apollonius_graph.new
		vertices.each{|v| ag.insert(v, v.x, v.y, v.tradius)}
		x2 -= x1
		y2 -= y1
		(ag.convex_hull_array - [v1, v2, hv1, hv2]).sort_by{|el| (el.x - x1) * x2 + (el.y - y1) * y2}
	end

	# https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
	# u --> v --> w
	# Dijkstra's shortest path search -- along the edges of the constrained delaunay triangulation
	#
	#        v-----w
	#  \    / \
	#   \  /   \ lcut
	#    u      x
	#
	# Generally we route at the inner lane -- doing so ensures that traces never cross.
	# When the turn at u is opposite to the turn at v then we have to cross
	# the line segment connecting u and v -- we need space for that.
	# When we take the inner lane at v, then the lcut to vertex x is a restriction. 
	# When there starts an incident net in the inner lane this path
	# is blocked, so we can use the outer lane, there can exist no inner
	# crossing lanes. If we take the outer lane at v and inner lane at u,
	# we have not to cross the line segment connection u and v and so on...
	#
	# So for this variant of Dijkstra's shortest path search we have not only to
	# record the previous node, but also the lane (inner/outer) we took, because
	# the next node may be only reachable when we do not have to cross a line
	# segment between current and next node. We also record the parent of the previous
	# node (u) -- because we have to check if there exists a terminal too close to u-v.
	#
	# So the key for the Fibonacci_Queue and parents and distances is not simple
	# a node, but a tripel of node, previous node and lane.
	#
	# Update October 2015: Now we generally use outer lanes also -- for that qbors()
	# function was changed. We still have to see if outer lanes really give benefit. 
	#
	# Default unit of Size is 0.01 mil
	# We will put this into a config file later

	#	TODO: Check backside of first and last vertex of each path -- i.e. when traces
	#	are thicker than pad or pin. Maybe we should leave that for manually fixing.
	#	Ckecking is not really easy, and automatic fixing is some work.
	#
	#	(1.) Note: the cm arroach can fail for this case 
	#	(cm of neightbors of x is v, so distance to v is zero 
	#               
	#    /____    x  /____
	#   /\        /\/   
	#   ||        || v

	def dijkstra(start_node, end_node_name, net_desc, max_detour_factor = 2)
		fail unless start_node.is_a? Region
		fail unless end_node_name.is_a? String
		fail unless net_desc.is_a? NetDesc
		fail if end_node_name.empty?
		fail if start_node.vertex.name == end_node_name
		q = BOOST::Fibonacci_Queue.new(-1, Float::INFINITY) # -1 for minimum queue
		distances = Hash.new
		parents = Hash.new
		outer_lane = Hash.new # record if we used inner or outer trail
		distances[[start_node, nil, true]] = 0 # fake left and right turn before start node
		distances[[start_node, nil, false]] = 0
		x, y = start_node.vertex.xy
		start_cid = start_node.vertex.cid # cluster id, -1 means no pad/cluster but plain pin
		start_node.qbors(nil) do |w, use_inner, use_outer| # initial steps are never blocked, so fill them in
			u = [w, start_node, false] # for rgt == true and rgt == false, so we can continue in all directions
			v = [w, start_node, true]
			q[u] = q[v]	= ((start_cid != -1) && (w.vertex.cid == start_cid) ? 0 : Math.hypot(w.vertex.x - x, w.vertex.y - y))
			parents[u] = parents[v] = [start_node, nil, false] # arbitrary u and rgt for last two array elements
		end
		while true do
			min, old_distance = q.pop
			return nil unless min
			fail unless min.length == 3
			v, uu, prev_rgt = *min
			fail unless v && uu
			hhh = (uu == start_node || (uu.vertex.cid == start_cid && start_cid != -1) ? 0 : uu.vertex.radius + [uu.vertex.separation, net_desc.trace_clearance].max + net_desc.trace_width * 0.5)

			pom = parents[min] # parent of min
			popom = parents[pom]
			popom = popom[0] if popom
			popom = popom.vertex if popom
			if (v.vertex.name == end_node_name) && v.incident # reached destination -- check if we touched a vertex
				hhh	= get_tangents(*uu.vertex.xy, hhh, prev_rgt, *v.vertex.xy, 0, false) # last two arguments are arbitrary
				blocked = false
				(uu.vertex.neighbors & v.vertex.neighbors).each{|el| # only two -- maybe CGAL query is faster?
					if el.cid == -1 || (el.cid != uu.vertex.cid && el.cid != v.vertex.cid)  && el != popom # is this useful here?
						if normal_distance_line_segment_point_squared(*hhh, el.x, el.y) < (el.radius + [el.separation, net_desc.trace_clearance].max + net_desc.trace_width * 0.5) ** 2
							blocked = true
							break
						end
					end
				}
				next if blocked
				# for now avoid long twisted paths which may block others
				if old_distance > 10 * AVD &&
					old_distance > max_detour_factor * Math::hypot(v.vertex.x - start_node.vertex.x, v.vertex.y - start_node.vertex.y)
					puts 'refused'
					return nil
				end
				break
			end
			vcid = v.vertex.cid
			distances[min] = old_distance
			x, y = v.vertex.xy
			pom = parents[min] # parent of min
			popom = parents[pom]
			popom = popom[0] if popom
			popom = popom.vertex if popom
			u = pom[0]
			fail unless u == uu
			path = Set.new # prevent loops
			p = min
			while p
				path << p[0]
				p = parents[p]
			end
			blocking_vertex = nil # NOTE: benefit of this relaxation has not been observed yet!
			# NOTE: this is a bit too restrictive: when we touch a vertex, this path is still valid if next step is exactly this vertex. Rare and difficult.
			uv_blocked = [false, true].map{|b| # does path u-v touch a vertex if we use inner/outer lane at v? [cur_rgt==false, cur_rgt==true]
				blocked = false
				p	= get_tangents(*uu.vertex.xy, hhh, prev_rgt, *v.vertex.xy, v.vertex.radius + [v.vertex.separation, net_desc.trace_clearance].max + net_desc.trace_width * 0.5, b)
				(uu.vertex.neighbors & v.vertex.neighbors).each{|el| # NOTE: There may also exist other vertices touching the path -- very rare case! 
					if (el.cid == -1 || (el.cid != uu.vertex.cid && el.cid != v.vertex.cid)) && el != popom#.vertex
						if normal_distance_line_segment_point_squared(*p, el.x, el.y) < ((el.radius + [el.separation, net_desc.trace_clearance].max + net_desc.trace_width * 0.5)) ** 2
						#puts '@@@@@@@@@@@@@@@@@@@@@'
						fail if normal_distance_line_segment_point_squared(*p, el.x, el.y) != unused_distance_line_segment_point_squared(*p, el.x, el.y)
							blocked = true
							if blocking_vertex # both block, so this path is really blocked, no relaxation possible
								blocking_vertex = nil
							else
							  blocking_vertex = el
							end

							#puts 'blocked'
							break
						end
					end
				}
				blocked
			}
			v.qbors(u) do |w, use_inner, use_outer|
 				only_outer = !use_inner
				next if @edges_in_cluster.include?(v.vertex, w.vertex) # diagonal edge in pad/cluster
				hhh = false
				if false #u == w && u != start_node # direct connection of two adjanced vertices, allow 2 PI loop around
				# indeed this can currently not work: we store lr_turn and outer attributes in region for each step, but do not copy region! 
					v.vertex.incident_nets.map{|el| el.nstep || el.pstep}.each{|el|
						hhh = true if !(el.nstep && el.pstep) && (el.vertex == u.vertex)
					}
				else
					next if path.include?(w)
				end
				lcuts = nil
				if false # we ignore this for now -- complicated and not really necessary...
				#if hhh && pom[1] != v && pom[1] != w && pom[1].vertex != v.vertex && pom[1].vertex != w.vertex
					only_outer = true
					lr_turn = RBR::boolean_really_smart_cross_product_2d_with_offset(pom[1], w, v)
				elsif false#u.vertex == w.vertex # 2 PI turn -- determine left/right turn by center of mass of neightbors
					#fail unless only_outer
					cm = [u.neighbors, w.neighbors].map{|el|
						Tex.new(el.inject(0){|sum, n| sum + n.vertex.x}.fdiv(el.length), el.inject(0){|sum, n| sum + n.vertex.y}.fdiv(el.length))
					}
					next if cm.find{|el| el.x == v.vertex.x && el.y == v.vertex.y} # see note (1.) above
					next if cm[0].x == cm[1].x && cm[0].y == cm[1].y # can this occur?
					lr_turn = RBR::boolean_really_smart_cross_product_2d_with_offset(cm[0], cm[1], v.vertex) # left or right turn?
					lcuts = Array.new # we need an empty lcuts array for outer turn
				else
					lr_turn = RBR::xboolean_really_smart_cross_product_2d_with_offset(u, w, v) # left or right turn?
				end
				cur_rgt = lr_turn
				w_v_rgt = [w, v, cur_rgt]
				w_v_xrgt = [w, v, !cur_rgt]
				# CAUTION: may be wrong -- leave start cluster and come back? But should be no problem, we occupy the cluster early.
				if (start_cid != -1) && (vcid == start_cid) && (w.vertex.cid == start_cid) # free walk around boundary of start cluster
					[w_v_rgt, w_v_xrgt].each{|el|
						unless distances.include?(el)
							if q.inc?(el, old_distance)
								parents[el] = min
							end
						end
					}
					next
				end
				next if only_outer && vcid > -1 # corner of cluster, inner path not allowed, outer also forbidden!
				new_distance = old_distance + Math.hypot(w.vertex.x - x, w.vertex.y - y)
				outer_distance = new_distance # save this value
				### outer_distance = old_distance + 1 * Math.hypot(w.vertex.x - x, w.vertex.y - y) # maybe some "scaling" ?
				if !(can_out = only_outer) && !distances.include?(w_v_rgt)
					not_blocked = catch(:blocked){
						# process test with <can_out = true> clauses first!
						lcuts = new_bor_list(u, w, v) # neighbours in the inner angle/lane
						if vcid >= 0 # at corner of pad/cluster
							if lcuts.find{|el| el.cid == vcid} || (u.vertex.cid == vcid && w.vertex.cid == vcid && lcuts.empty?) # do we need && lcuts.empty? For concave cluster maybe?
								can_out = true
								throw :blocked
							end
						end
						# now test special case when we touch a terminal with incident nets
						v.vertex.incident_nets.map{|el| el.nstep || el.pstep}.each{|el|
							hhh = lcuts.include?(el.vertex)
							can_out = true if hhh && (vcid == -1) # why && (vcid == -1) ?
							throw :blocked if hhh
							throw :blocked if !(el.nstep && el.pstep) && (cur_rgt != prev_rgt) && (el.vertex == u.vertex)
						}
						throw :blocked if uv_blocked[(cur_rgt ? 1 : 0)] && blocking_vertex != w.vertex
						squeeze = lcuts.inject(0){|sum, el| if (h = @newcuts[v.vertex, el].squeeze_strength(net_desc.trace_width, net_desc.trace_clearance)) >= MBD; break h end; sum + h}
						throw :blocked if squeeze >= MBD
						squeeze += @newcuts[v.vertex, u.vertex].squeeze_strength(net_desc.trace_width, net_desc.trace_clearance) if (u != start_node) && (cur_rgt != prev_rgt)
						throw :blocked if squeeze >= MBD
						if hhh = @newcuts[w.vertex, u.vertex] # looking for shorter paths
							nd = (new_distance + distances[pom] + hhh.cap) / 2
							if nd < new_distance
								new_distance = [nd, old_distance].max
							end
						else
							unless lcuts.empty? # can (only) occur for outer vertices of PCB rectangle
								nv = lcuts.min_by{|el| @newcuts[v.vertex, el].cap}
 								if unused_point_in_triangle(*u.vertex.xy, *v.vertex.xy, *w.vertex.xy, *nv.xy) >= 0 # P_IN = 1; P_ON = 0; P_OUT = -1; COLLINEAR = -2
									nd = Math.hypot(u.vertex.x - nv.x, u.vertex.y - nv.y) + Math.hypot(w.vertex.x - nv.x, w.vertex.y - nv.y)
								else
									nd = Math.hypot(u.vertex.x - w.vertex.x, u.vertex.y - w.vertex.y)
								end
								nd = (new_distance + distances[pom] + nd) / 2
								if nd < new_distance
									new_distance = [nd, old_distance].max
								end
							end
						end
						new_distance += AVD if cur_rgt != prev_rgt # wiggly line TODO fix vertex size
						new_distance += squeeze
						if q.inc?(w_v_rgt, new_distance)
							outer_lane[w_v_rgt] = false # we took the inner path
							parents[w_v_rgt] = min # record the path for backtracking
						end
					}
				end
				if use_outer && !distances.include?(w_v_xrgt) # try outer path
					cur_rgt = !cur_rgt
					new_distance = outer_distance
					not_blocked = catch(:blocked){
						lcuts = v.vertex.neighbors - (lcuts || new_bor_list(u, w, v)) - [u.vertex, w.vertex]
						squeeze = lcuts.inject(0){|sum, el| if (h = @newcuts[v.vertex, el].squeeze_strength(net_desc.trace_width, net_desc.trace_clearance)) >= MBD; break h end; sum + h}
						throw :blocked if squeeze >= MBD
						squeeze += @newcuts[v.vertex, u.vertex].squeeze_strength(net_desc.trace_width, net_desc.trace_clearance) if (u != start_node) && (cur_rgt != prev_rgt)
						throw :blocked if squeeze >= MBD
						throw :blocked if uv_blocked[(cur_rgt ? 1 : 0)] && blocking_vertex != w.vertex
						# now test special case when we touch a terminal with incident nets
						v.vertex.incident_nets.map{|el| el.nstep || el.pstep}.each{|el|
							throw :blocked if lcuts.include?(el.vertex)
							throw :blocked if !(el.nstep && el.pstep) && (cur_rgt != prev_rgt) && (el.vertex == u.vertex)
						}
						new_distance += AVD if cur_rgt != prev_rgt # TODO fix vertex size
						new_distance += squeeze
						if q.inc?(w_v_xrgt, new_distance)
							outer_lane[w_v_xrgt] = true
							parents[w_v_xrgt] = min
						end
					}
				end
			end
		end
		path = Array.new
		p = min
		while p
			if n = parents[p]
				fail unless n[0] == p[1]
				n[0].outer = outer_lane[p]
				n[0].lr_turn = p[2] == outer_lane[p]
			end
			path << p[0]
			p = n
		end
		cid = path.last.vertex.cid
		if cid != -1 # ignore steps along edges of start cluster
			while path[-2].vertex.cid == cid
				path.pop
			end
		end
		dijkstra_use_path(path, net_desc)
		return path
	end

	def dijkstra_use_path(path, net_desc)
		path.each_cons(3){|u, v, w| # inverted direction, but it ...
			if u.vertex == w.vertex # the 2 PI turn already seen above
				lcuts = Array.new
			else
				lcuts = new_bor_list(u, w, v) # neighbours in the inner angle/lane
				#fail unless lcuts == new_bor_list(w, u, v) # ... does not matter
			end
			lcuts = v.vertex.neighbors - lcuts - [u.vertex, w.vertex] if v.outer
			lcuts.each{|el| @newcuts[v.vertex, el].use(net_desc.trace_width, net_desc.trace_clearance)}
			if (u != path.first) && ((u.outer == u.lr_turn) != (v.outer == v.lr_turn))
				@newcuts[u.vertex, v.vertex].use(net_desc.trace_width, net_desc.trace_clearance)
			end 
		}
		path.first.outer = path.first.lr_turn = path.last.outer = path.last.lr_turn = nil
	end


	#          /neighbor
	#     a   /
	# <-------\
	#       /  \b
	#      /    \
	#return neighbors of one side of the path
	def split_neighbor_list(a, b, n)
		fail unless a.is_a? Region
		fail unless b.is_a? Region
		nx = n.vertex.x
		ny = n.vertex.y
		v1x = a.vertex.x + a.ox - nx
		v1y = a.vertex.y + a.oy - ny
		v2x = b.vertex.x + b.ox - nx
		v2y = b.vertex.y + b.oy - ny
		n.neighbors.select{|el|
			if el == a || el == b
				false
			else
				ex = el.vertex.x + el.ox - nx
				ey = el.vertex.y + el.oy - ny
				if RBR::xboolean_really_smart_cross_product_2d_with_offset(a, b, n)
					v1x * ey > v1y * ex && v2x * ey < v2y * ex
				else
					v1x * ey > v1y * ex || v2x * ey < v2y * ex
				end
			end
		}
	end


	def full_split_neighbor_list(a, b, n)
		fail unless a.is_a? Region
		fail unless b.is_a? Region
		l = Array.new
		r = Array.new
		nx = n.vertex.x
		ny = n.vertex.y
		v1x = a.rx - nx
		v1y = a.ry - ny
		v2x = b.rx - nx
		v2y = b.ry - ny
		turn = RBR::xboolean_really_smart_cross_product_2d_with_offset(a, b, n)
		n.neighbors.each{|el|
			if el != a && el != b
				ex = el.rx - nx
				ey = el.ry - ny
				if (turn ? v1x * ey > v1y * ex && v2x * ey < v2y * ex : v1x * ey > v1y * ex || v2x * ey < v2y * ex)
					l << el
				else
					r << el
				end
			end
		}
		return r, l
	end


	def atan2_tangents(a, b, id)
		last_step, cur_step = a.net(id), b.net(id)
		t1 = get_tangents(a.x, a.y, last_step.radius, last_step.rgt, b.x, b.y, cur_step.radius, cur_step.rgt)
		Math.atan2(t1[3] - t1[1], t1[2] - t1[0])
	end

#     a
#    /
#   /   select these neighbors of n
#  /    in inner angle < PI
# n_______b
	def new_bor_list(a, b, n)
		aa, bb, nn = a, b, n
		a, b, n = a.vertex, b.vertex, n.vertex
		ax = a.x - n.x
		ay = a.y - n.y
		bx = b.x - n.x
		by = b.y - n.y
		n.neighbors.select{|el|
			if el == a || el == b
				false
			else
				ex = el.x - n.x
				ey = el.y - n.y
				if RBR::xboolean_really_smart_cross_product_2d_with_offset(aa, bb, nn)
					ax * ey > ay * ex && ex * by > ey * bx
				else
					ax * ey < ay * ex && ex * by < ey * bx
				end
			end
		}
	end


# Explanation of the offset ox, oy used below
# -------------------------------------------
# We have a splitted region graph shown below,
# a result of a routed trace from X to Z.
# Region Y was splitted in regions y1 and y2.
#                         /B 
#     /---------------- y1
# O--X------------------y2 \
#                       / \ \
#        				  		 /	 \ \
#                     A     Z
#
# One more trace along X--y1 will introduce one more region split
# along this path -- we split into neighbors to the left and to the right
# of the path. For neighbors A and B this is no problem, one is on the left,
# and one on the right of the path. 
# Problem: The vertex of regions y1 and y2 is the same with identical cooordinates.
# When next trace is from O over X, y1 to B, y2 should be on the right side of
# the path. We add to each fresh split region a small delta offset perpendicular
# to the path, this allows a simple decision which is on the right or left side.
# This may not work when we have a 2 PI full turn -- currently in that case offset
# is zero. We may fix that if we use a more complicated offset calculation, but indeed
# for the full turn we do not need an offset. Indeed we need the offset only for the
# first and last splitted region of a path, not for inner regions. And a full turn
# should not occur for the first or last split.
#
# Splitting the path
# ------------------
# p, c, n -- previous, current, next region; c is splitted into r1 and r2
#
#  p       p        p
#  |      / \      / \
# -c-   -r1 r2-  -r1 r2-
#  |      \ /      |  |
#  n       n     -r1'r2'
#  |       |       \ /   
#                   m
#
	
	def route(net_id, max_detour_factor = 2)
		fail if net_id > @netlist.length - 1 || net_id < 0
		net_desc = @netlist[net_id]
		from, to = net_desc.t1_name, net_desc.t2_name
		#to, from = net_desc.t1_name, net_desc.t2_name
		fail unless from && to # class is string or maybe integer?
		fail if from == to
		fail unless start_node = @regions.find{|r| r.incident && r.vertex.name == from}
		if @rstyles
			net_desc.trace_clearance = @rstyles[net_desc.style_name].trace_clearance
			net_desc.trace_width = @rstyles[net_desc.style_name].trace_width
		else
			net_desc.trace_width = Trace_Width 
			net_desc.trace_clearance = Clearance 
		end
=begin
very basic test for using rescue vias
		unless path = dijkstra(start_node, to, net_desc)
			net_desc = net_desc.dup
			net_desc.t2_name = 'rescue_via'
			path = dijkstra(start_node, 'rescue_via', net_desc)
			if path 
				path[0].vertex.name = 'used_rescue_via'
			end
		end
		unless path #= dijkstra(start_node, to, net_desc)
=end
		if max_detour_factor == 0
return dijkstra(start_node, to, net_desc, 1.5) != nil
		end


		unless path = dijkstra(start_node, to, net_desc, max_detour_factor)
			if max_detour_factor != 2
			puts 'dijkstra failed!'
			x, y = @regions.find{|r| r.incident && r.vertex.name == to}.vertex.xy
			@pic.set_source_rgba(1, 1, 1, 1)
			@pic.set_line_width(ATW)
			@pic.move_to(x, y)
			@pic.line_to(*start_node.vertex.xy)
			@pic.stroke
			end
			return false
		end
		first = path[-1]
		last = path[0]
		fail if first == last
		if path.length > 2 # ignore direct connections without a single split region!
			first.idirs << [path[-2].rx - first.vertex.x, path[-2].ry - first.vertex.y] # same as above
			last.idirs << [path[1].rx - last.vertex.x, path[1].ry - last.vertex.y]
			###first.idirs << [cur.rx - first.rx, cur.ry - first.ry] # this may work when we fix qbors() too
		end
		r1 = r2 = nil
		@pic.arc(first.vertex.x, first.vertex.y, 2 * ATW, 0, 6)
		@pic.stroke
		@pic.move_to(first.vertex.x, first.vertex.y)
		path.reverse.each_cons(3){|prv, cur, nxt|
			fail unless prv && cur && nxt 
			ne, ne_comp = full_split_neighbor_list(prv, nxt, cur)
			fail if ne_comp.include?(prv) || ne_comp.include?(nxt)
			fail if ne.include?(prv) || ne.include?(nxt)
			ne << nxt
			ne_comp << nxt
			if r1
				ne.delete(r2)
				ne_comp.delete(r1)
			else
				ne << prv
				ne_comp << prv
			end
			@regions.delete(cur)
			r1 = Region.new(cur.vertex)
			r2 = Region.new(cur.vertex)
			r1.idirs = cur.idirs.dup
			r2.idirs = cur.idirs.dup
			r1.odirs = cur.odirs.dup
			r2.odirs = cur.odirs.dup
			r1.incident =	r2.incident = cur.incident
			# give r1 and r2 an offset vector perpendicular to the path to allow a distinction
			if false#nxt.vertex == prv.vertex
				puts '+++', nxt.vertex.xy
				@pic.arc(nxt.vertex.x, nxt.vertex.y, 12 * ATW, 0, 4)
		@pic.stroke

			end

			dx1 = dy1 = dx2 = dy2 = 0
			if prv == first
				#puts "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
				dx2 = cur.rx - prv.rx
				dy2 = cur.ry - prv.ry
				h = Math::hypot(dx2, dy2)
				dx2 /= h
				dy2 /= h
			end
			if nxt == last
				#puts "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
				dx1 = nxt.rx - cur.rx
				dy1 = nxt.ry - cur.ry
				h = Math::hypot(dx1, dy1)
				dx1 /= h
				dy1 /= h
			end
			if prv == first || nxt == last
				r1.g = r2.g = cur.g * 0.5
				dy = (dx1 + dx2)
				dx =  -(dy1 + dy2)
				h = Math::hypot(dx, dy) / cur.g # zero for full 2 PI turn
				dx /= h 
				dy /= h
				#fail if dx == 0 || dy == 0
				r1.ox = cur.ox + dx
				r1.oy = cur.oy + dy
				r2.ox = cur.ox - dx
				r2.oy = cur.oy - dy
				r1.rx = r1.vertex.x + r1.ox
				r1.ry = r1.vertex.y + r1.oy
				r2.rx = r2.vertex.x + r2.ox
				r2.ry = r2.vertex.y + r2.oy
			else

				r1.ox = cur.ox
				r1.oy = cur.oy
				r2.ox = cur.ox
				r2.oy = cur.oy
				r1.rx = cur.rx
				r1.ry = cur.ry
				r2.rx = cur.rx
				r2.ry = cur.ry
			end



			if true#nxt.vertex != prv.vertex # h == 0
				#r1.g = r2.g = cur.g * 0.5 # magnitude of additional offset decreases for each split by factor 0.5 
				#dx1 = nxt.vertex.x - cur.vertex.x
				#dy1 = nxt.vertex.y - cur.vertex.y
				dx1 = nxt.rx - cur.rx
				dy1 = nxt.ry - cur.ry
				h = Math::hypot(dx1, dy1)
				dx1 /= h
				dy1 /= h
				#dx2 = cur.vertex.x - prv.vertex.x
				#dy2 = cur.vertex.y - prv.vertex.y
				dx2 = cur.rx - prv.rx
				dy2 = cur.ry - prv.ry
				h = Math::hypot(dx2, dy2)
				dx2 /= h
				dy2 /= h
				dy = (dx1 + dx2)
				dx =  -(dy1 + dy2)
				h = Math::hypot(dx, dy) #/ cur.g # zero for full 2 PI turn
				dx /= h 
				dy /= h
				#r1.ox = cur.ox + dx
				#r1.oy = cur.oy + dy
				#r2.ox = cur.ox - dx
				#r2.oy = cur.oy - dy
				#r1.rx = r1.vertex.x + r1.ox
				#r1.ry = r1.vertex.y + r1.oy
				#r2.rx = r2.vertex.x + r2.ox
				#r2.ry = r2.vertex.y + r2.oy
			end
			@regions << r1 << r2
			cur.neighbors.each{|el| el.neighbors.delete(cur)}
			ne.each{|el|
				el.neighbors << r1
				r1.neighbors << el
			}
			ne_comp.each{|el|
				el.neighbors << r2
				r2.neighbors << el
			}
			if cur.lr_turn != cur.outer
				r1.incident = false
			else
				r2.incident = false
			end
			if cur.outer && dx
				if cur.lr_turn
					r2.odirs << [dx, dy]
				else
					r1.odirs << [-dx, -dy]
				end
			end
			@pic.line_to(cur.vertex.x, cur.vertex.y)
		}

		@pic.line_to(last.vertex.x, last.vertex.y)
		@pic.stroke

		pstep = nil
		path.each_with_index{|cur, i|  
			nxt = (i == path.length - 1 ? nil : path[i + 1])
			prv = (i == 0 ? nil : path[i - 1])
			nv = (nxt ? nxt.vertex : nil)
			pv = (prv ? prv.vertex : nil)
			cv = cur.vertex
			step = Step.new(pv, nv, @path_ID)
			step.outer = cur.outer
			step.lr_turn = !cur.lr_turn
			step.net_desc = net_desc
			step.vertex = cv
			step.pstep = pstep
			pstep = step
			if prv and nxt
				cv.update(step) # TODO: if one vertex includes his neighbor vertex, then skip that one!
				cv.unfriendly_resize
				step.rgt = step.outer != cur.lr_turn
				step.xt = !step.outer
				cv.attached_nets << step
			else
				step.rgt = false
				cv.incident_nets << step
			end
		}
		@path_ID += 1
		while p = pstep.pstep
			p.nstep = pstep
			pstep = p
		end
		return true
	end

	# http://en.wikipedia.org/wiki/Tangent_lines_to_circles
	# http://www.ambrsoft.com/TrigoCalc/Circles2/Circles2Tangent_.htm
	# https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Tangents_between_two_circles
	# UGLY: what when tangent does not exist?
	def get_tangents(x1, y1, r1, l1, x2, y2, r2, l2)
		fail if r1 < 0 || r2 < 0
		#fail if l1.class != boolean || l2.class != Bool
		d = Math.hypot(x1 - x2, y1 - y2)
		vx = (x2 - x1) / d
		vy = (y2 - y1) / d
		r2 *= (l1 == l2  ? 1 : -1)
		c = (r1 - r2) / d
		h = 1 - c ** 2
		if h < 0
		end
		if h >= 0
		h = 0 if h < 0 # wrong -- only for inspection
		h = Math.sqrt(h) * (l1  ? -1 : 1)
		else
h=0
		end
		nx = vx * c - h * vy
		ny = vy * c + h * vx
		[x1 + r1 * nx, y1 + r1 * ny, x2 + r2 * nx,  y2 + r2 * ny]
	end

	def smart_replace(step, list)
		if step.prev == step.next # can occur due to nubly()
			#fail # can this really occur?
			fail unless list.empty? # should be empty?
			step.pstep.nstep = step.nstep.nstep
			step.pstep.next = step.nstep.next
			step.nstep.nstep.pstep = step.pstep
			step.nstep.nstep.prev = step.prev
			step.next.new_delete_net(step.nstep)
		elsif list.empty?
			ps = step.pstep
			ns = step.nstep
 			ps.next = step.next
 			ns.prev = step.prev
			ps.nstep = ns
			ns.pstep = ps
		else
		  pstep = step.pstep
			pv = step.prev
			list.each{|v|
				n = Step.new(pv, nil, step.id)
				n.net_desc = step.net_desc
				n.vertex = v
				n.pstep = pstep
				pstep.nstep = n
				pstep.next = v 
				pstep = n
				pv = v
				n.rgt = !step.rgt
				n.xt = true # TODO: check
				n.outer = true
				v.update(n)
				v.attached_nets << n
			}
			pstep.next = step.next
			pstep.nstep = step.nstep
			pstep.nstep.prev = pv
			pstep.nstep.pstep = pstep
		end
		step.vertex.new_delete_net(step)
	end

	#\   |   /
	# \  |  /
	#  \ | /  3 attached concave nets not overlapping
	#    O ------------
	#     \ -----
	#      \
	#       \
	#        \
	# sort attached nets and calculate its radii.
	# this should work for concave (before attachment operator is applied) and convex final nets
	# regard groups by angle: overlapping angles needs different radii for arcs,
	# non overlapping attached arcs may have the same radius.
	# generally one terminal should have at least two final convex groups -- but a terminal with
	# a very big radius may have more than 2 attached groups.
	# Indeed nets never cross, so overlapping is full including
	# currently we collect all attached nets of a terminal in one single array called
	# attached_nets -- we may consider using a separate array for each group...
	# maybe we should regard minimal gap as overlapping? Introduce an eps?
	# UGLY: Unused
	def currently_unused_smart_prepare_steps
		@vertices.each{|vert|
			next if vert.attached_nets.empty?
			vert.attached_nets.each{|s|
				s.g = -1
				#a = Math.atan2(s.next.y - s.vertex.y, s.next.x - s.vertex.x) # basic angle
				#b = Math.atan2(s.prev.y - s.vertex.y, s.prev.x - s.vertex.x)
				a = atan2_tangents(s.vertex, s.next, s.id) # -PI <= angle <= PI
				b = atan2_tangents(s.prev, s.vertex, s.id) # wrong direction, we have to flip
				b < 0 ? b += Math::PI : b -= Math::PI
				d = (a - b).abs
				dir = (a + b) * 0.5
				if  d > Math::PI # select inner angle < PI -- and flip direction
					d = 2 * Math::PI - d
					dir < 0 ? dir += Math::PI : dir -= Math::PI
				end
				s.d = d * 0.5
				s.dir = dir
			}
			nets = vert.attached_nets.sort_by{|s| s.dir}
			i = nets.length
			last = nets[0]
			last.dir += 2 * Math::PI
			last.g = group = 0
			while (i -= 1) > 0 do # backward while in group 0, start with largest angle
				nxt = nets[i]
				break if nxt.dir + nxt.d < last.dir - last.d
				nxt.g = 0
				last = nxt
			end
			i = 0
			last = nets[0]
			last.dir -= 2 * Math::PI # undo above fix
			while (i += 1) < nets.length do # forward
				nxt = nets[i]
				break if nxt.g == 0
				if nxt.dir - nxt.d > last.dir + last.d
					group += 1
				end
				nxt.g = group
				last = nxt
			end
			group = 0
			loop do
				vert.reset_initial_size
				group_found = false
				vert.attached_nets.each{|step|
					if step.g == group
						group_found = true
						net = step.net_desc
						trace_sep = [vert.separation, net.trace_clearance].max
						vert.radius += trace_sep + net.trace_width
						step.radius = vert.radius - net.trace_width * 0.5
						vert.separation = net.trace_clearance
					end
				}
				break unless group_found
				group += 1
			end
		}
	end

	# sort attached nets and calculate its radii
	def prepare_steps
		@vertices.each{|vert|
			next if vert.attached_nets.empty?
			vert.reset_initial_size
			[true, false].each{|b|
				vert.attached_nets.each{|step|
					next if step.xt == b
					net = step.net_desc
					trace_sep = [vert.separation, net.trace_clearance].max
					vert.radius += trace_sep + net.trace_width
					step.radius = vert.radius - net.trace_width * 0.5
					vert.separation = net.trace_clearance
				}
			}
		}
	end

	def sort_attached_nets
		@vertices.each{|vert| vert.sort_attached_nets}
	end

	def convex_kkk(prev_step, step, nxt_step)
		pv, cv, nv = step.prev, step.vertex, step.next
		x1, y1, x2, y2	= get_tangents(pv.x, pv.y, prev_step.radius, prev_step.rgt, cv.x, cv.y, step.radius, step.rgt)
		x3, y3, x4, y4	= get_tangents(cv.x, cv.y, step.radius, step.rgt, nv.x, nv.y, nxt_step.radius, nxt_step.rgt)
		x2, y2, x3, y3 = line_line_intersection(x1, y1, x2, y2, x3, y3, x4, y4) # get crossing point and ua, ub
				if (x2 != nil) && ((x3 > 0 && x3 < 1) || (y3 > 0 && y3 < 1))
			return x2, y2
		else
			return nil
		end
	end

  def nubly(collapse = false)
		#return
	  replaced = true
		rep_c = 0
		while replaced do
			replaced = false
			rep_c += 1
			@vertices.each{|cv|
				cv.attached_nets.reverse_each{|step|
					prev_step, nxt_step = step.pstep, step.nstep

					pv, nv = step.prev, step.next
					d = Math::hypot(cv.x - pv.x, cv.y - pv.y) - (prev_step.radius - step.radius).abs * 1.02
					if d < 0
						if step.radius < prev_step.radius
							step.radius -= d
							replaced = true
						end
						next
					end
					d = Math::hypot(cv.x - nv.x, cv.y - nv.y) - (nxt_step.radius - step.radius).abs * 1.02
					if d < 0
						if step.radius < nxt_step.radius
							step.radius -= d
							replaced = true
						end
						next
					end

					hx, hy = convex_kkk(prev_step, step, nxt_step)
					step.xt = hx != nil
					if collapse && step.xt
						pv, nv = step.prev, step.next
						hv0 = Vertex.new(hx, hy)

						#fail if pv == nv
						replaced = true
						pvx = pv.x
						pvy = pv.y
						nvx = nv.x
						nvy = nv.y
						if pp = prev_step.pstep
							hx, hy = convex_kkk(pp, prev_step, step)
						end
						if pp && hx
							ppv = Vertex.new(hx, hy)
						else
							ppv = pv
						end
						if nn = nxt_step.nstep
							hx, hy = convex_kkk(step, nxt_step, nn)
						end
						if nn && hx
							nnv = Vertex.new(hx, hy)
						else
							nnv = nv
						end
						hx = nvx - pvx
						hy = nvy - pvy
						if step.rgt
							vec_x, vec_y = hy, -hx
						else
							vec_x, vec_y = -hy, hx
						end
						hv3 = Vertex.new(pvx + hx * 0.5 + vec_x, pvy + hy * 0.5 + vec_y)
						hx *= 2
						hy *= 2
						vec_x *= 2
						vec_y *= 2
						hv4 = Vertex.new(pvx - hx + vec_x, pvy - hy + vec_y)
						hv5 = Vertex.new(nvx + hx + vec_x, nvy + hy + vec_y)
						rep = vertices_in_polygon([ppv, hv0, nnv, hv3], @vertices) - [pv, nv, ppv, cv, nnv, hv3]
						unless rep.empty?
							net = step.net_desc
							rep.each{|v|
								v.trgt = !step.rgt
								v.tradius = v.radius + [net.trace_clearance, v.separation].max + net.trace_width * 0.5
							}
							pv.trgt = step.pstep.rgt
							pv.tradius = step.pstep.radius
							nv.trgt = step.nstep.rgt
							nv.tradius = step.nstep.radius
							rep = new_convex_vertices(rep, pv, nv, hv4, hv5)
						end
						smart_replace(step, rep)
					end
				}
			}
		end
	end

	def draw_routes(layer = 0)
		@file = File.open("layer_#{2 - layer}.pcb", "w")	
 	@file.write("FileVersion[20070407]\n")
 	@file.write("PCB[\"\" 350000 330000]\n")
 	@file.write("Grid[3900.0 1800 100 1]\n")
	if layer == 0
	gen_vias
	
 	@file.write("Layer(1 \"\")\n(\n")
else
 	@file.write("Layer(4 \"\")\n(\n")
end
		set_color(0, 0, 0, 1)
		@vertices.each{|vert|
			vert.incident_nets.each{|n|
				if n.next
					@pic.new_sub_path
					@pic.arc(vert.x, vert.y, 100, 0, 2 * Math::PI)
					@pic.stroke
				end
				thi = n.net_desc.trace_width
				sep = n.net_desc.trace_clearance
				last = vert
				lastx = lasty = nil
				lr = 0
				to = n.next
				to_net = n.nstep
				while to do
					last_net = to_net.pstep
					last = last_net.vertex
					radius = to_net.radius
					if last.x == to.x && last.y == to.y
					last.vis_flag = 1
					else
					t = get_tangents(last.x, last.y, lr, last_net.rgt, to.x, to.y, radius, to_net.rgt)
					gen_line(*t, thi)
					if lr > 0
					  start_angle = Math.atan2(lasty - last.y, lastx - last.x)
						end_angle = Math.atan2(t[1] - last.y, t[0] - last.x)
						start_angle, end_angle = end_angle, start_angle unless last_net.rgt
						gen_arc(last.x, last.y, lr, start_angle, end_angle, thi)
						@pic.set_source_rgba(0, 0, 0, 1) 
					end
					end
					lr = radius
					last = to
					to_net = to_net.nstep
					if to_net
					to = to_net.vertex
					else
						to = nil
					end
					lastx = t[2]
					lasty = t[3]
				end
			}
		}
		@file.write(")\n")
		file.close unless file == nil
	end

	def set_line_width(w)
		@pic.set_line_width(w)
	end
	def set_color(r, g, b, a)
		@pic.set_source_rgba(r, g, b, a)
	end
	def save_picture
		@image.write_to_png(@filename)
	end
end
end
#a = RBR::MBD
#puts RBR.class_variable_get(:@@one)
#RBR.class_variable_set(:@@one,3)
#puts RBR.class_variable_get(:@@one)

#puts RBR.one
#exit

if __FILE__ == $0

rs = RBR::init_seed
#rs = 129
r = RBR::Router.new(0, 0, RBR::PCB_Size, RBR::PCB_Size)

r.generate_test_vertices
r.finish_init(true)
r.filename = 'pic' + rs.to_s.rjust(3, "0") + '.png'
r.draw_vertices

col = ([1, 0, 0].permutation(3).to_a + [1, 1, 0].permutation(3).to_a).uniq - [[1, 0, 0]]
r.set_color(1, 0, 0, 0.7)
r.set_line_width(1200)
#(5..9).each{|i|
###(4..8).each{|i|
#[4].each{|i|
###[0,1,4,5,6,7,8].each{|i|
[0,1,2,3,4,5,6,7,8,9].each{|i|

#[5,6,7].each{|i|
#next if i == 4
#(2..2).each{|i|
r.set_color(*col[i % 5], 0.4)
r.route(i)
}
r.sort_attached_nets

r.prepare_steps
r.nubly
r.prepare_steps

r.sort_attached_nets
r.prepare_steps

r.nubly
r.prepare_steps

r.sort_attached_nets
r.prepare_steps

r.nubly(true)
r.sort_attached_nets
r.prepare_steps

r.draw_routes
r.flag_vertices
				#r.pic.arc(88724, 37103, 2000, 0, 2*Math::PI)
				#r.pic.arc(39826, 57348, 2000, 0, 2*Math::PI)
				#r.pic.fill
				#r.pic.stroke
r.save_picture
puts $glob


end

# The layer assignment algorithm for our topological printed circuit board router
# follows very closely the detailed description in Tal Dayan's PhD thesis.
#
# Currently we do not divide the PCB board in multiple 'Bins', but identify the whole
# board with one large bin, which is routed by our router. For small and medium size boards
# this should work fine, for very large boards we may need a 'global router' which divides
# the whole board into multiple bins, which then our 'local' router can manage.
#
#
# see thesis page 30, section 3.2.1 The Input Domain
#
#require_relative "../REMST/rcgal_emst"
require_relative '../RBOOST/Fibonacci_Queue_Hash/rboost_fibonacci_queue'
#require_relative '../RCGAL/rcgal_cdt'

require_relative 'canvas'
require_relative 'routing_styles'

module RM

	def self.init_seed
		seed = (ARGV[0] ? ARGV[0].to_i : rand(1000))
		print 'random seed is ', seed, "\n"
		srand(seed)
		seed
	end
end

module LA

# pair pair detour costs
class PPDC < Hash
	def initialize
		super(1e6)
	end

	def[](a, b)
		a.object_id < b.object_id ? super([a, b]) : super([b, a])
	end

	def[]=(a, b, v)
		a.object_id < b.object_id ? super([a, b], v) : super([b, a], v)
	end

	def delete(a, b)
		a.object_id < b.object_id ? super([a, b]) : super([b, a])
	end
end

class Segment
	attr_accessor :x1, :y1, :x2, :y2
	attr_accessor :costs
	attr_accessor :active
	attr_accessor :intersecting_segments
	attr_accessor :layer
	attr_accessor :fresh
	def initialize(x1, y1, x2, y2)
		@x1, @y1, @x2, @y2 = x1, y1, x2, y2
		@costs = 0
		@active = false
		@fresh = true
		@intersecting_segments = Array.new
	end
end

class Terminal
	attr_accessor :name
	attr_accessor :net_name
	attr_accessor :style
	attr_accessor :x, :y # location
	attr_accessor :layers # range where it exists
	def initialize(name, net_name, style, x, y, layers)
		@name, @net_name, @x, @y, @layers = name, net_name, x, y, layers
		@style = style
		#puts 'style', @style
	end
end

class V_node
	attr_accessor :x, :y
	attr_accessor :layer
	attr_accessor :next
	attr_accessor :pre
	def initialize(x, y, layer, nxt)
		@x, @y, @layer, @next = x, y, layer, nxt
	end
	def cost_to(n)
		#50
		10000
	end
end

class F_node
	attr_accessor :x, :y
	attr_accessor :layer
	attr_accessor :next
	attr_accessor :pre
	def initialize(x, y, layer, nxt)
		@x, @y, @layer, @next = x, y, layer, nxt
	end
	def cost_to(n)
	fail
	fail unless n

	fail unless n.x && n.y

	fail unless @x && @y
		Math::hypot(n.x - @x, n.y - @y)
	end
end

class S_node
	attr_accessor :x, :y
	attr_accessor :pads # max 0...layer_count
	attr_accessor :next
	def initialize(x, y, pads)
		@x, @y, @pads = x, y, pads
		@next = Array.new
	end
	def cost_to(n)
	#fail
		0
	end

end

class T_node
	attr_accessor :x, :y
	attr_accessor :pads # max 0...layer_count
	attr_accessor :pre
	def initialize(x, y, pads)
		@x, @y, @pads = x, y, pads
	end
end

class Input_2Net
	attr_accessor :t1, :t2 # terminal
	attr_accessor :t1_ext, :t2_ext
	def initialize(t1, t2)
		@t1, @t2 = t1, t2 
	end
end

class Output_2Net
	attr_accessor :via_positions # array of x, y pairs, along a path from start_node to end_node
	attr_accessor :via_thickness, :via_clearance
	attr_accessor :start_node, :end_node
	attr_accessor :m # matrix, first index is the layer, second index the nodes from end_node to start_node
	attr_accessor :segment_list
	attr_accessor :all_intersecting_segments
	attr_accessor :path
	attr_accessor :ppdc
	attr_accessor :i2n
	attr_accessor :active_segments
	attr_accessor :old_cost, :new_cost

	def initialize(start_node, end_node, via_positions, layer_count)
		@start_node, @end_node, @via_positions = start_node, end_node, via_positions
		@layer_count = layer_count
		@segment_list = Array.new
		@old_cost = @new_cost = 0

	end

	def set_ppdc(ppdc)
		@ppdc = ppdc
	end

	def set_active_segments(as)
		@active_segments = as
	end

	def find_segment(x1, y1, x2, y2)
		@segment_list.each{|el|
			return el if el.x1 == x1 && el.y1 == y1 && el.x2 == x2 && el.y2 == y2
		}
		fail
	end

	#layer[i] is a list of segments/branches
	def build_assignment_graph(layer)
		h = @via_positions.length
		fail if (h == 0) || h.odd?
		fail if @start_node.pads.min < 0 || @start_node.pads.max >= @layer_count
		fail if @end_node.pads.min < 0 || @end_node.pads.max >= @layer_count
		via_count = h / 2
		puts via_count
		layers = 0..(@layer_count - 1) 
		colums = 0..(via_count * 2) # F O F O F for via_count == 2 
		#vp = @via_positions.dup # x,y pairs
		#vp.unshift(@start_node.y)
		#vp.unshift(@start_node.x)
		vp = [@start_node.x, @start_node.y] + @via_positions
		m = Array.new(@layer_count){Array.new(via_count * 2 + 1)}
		for i in colums # from T back to S
			if i.even?
				y = vp.pop
				x = vp.pop
			end
			for j in layers
				l = Array.new
				if i.even? # forward
					k = i + 1
					while k > 0
						k -= 2
						if k == -1 # link forward node to T node
							l << @end_node if @end_node.pads.include?(j)
						else
							if (h = m[j][k])
								l << h # link to up/down node 
							end
						end
					end
					unless l.empty?
						m[j][i] = F_node.new(x, y, j, l)
						l.each{|el|
						#unless @segment_list.index{|m| m.x1 ==  && m.y1 == el.y1 & m.x2 == el.x2 && m.y2 == el.y2}
							@segment_list << Segment.new(x, y, el.x, el.y)
						}
					end
				else #up/down
					for k in layers do
						if (k != j) && (h = m[k][i - 1])
							l << h
						end
					end
					unless l.empty?
						m[j][i] = V_node.new(x, y, j, l)
					end
				end
			end
		end
		#puts @segment_list.length
		@segment_list.uniq!{|el| [el.x1, el.y1, el.x2, el.y2]}
		puts @segment_list.length
		@all_intersecting_segments = Array.new
		@segment_list.each{|el|
			@all_intersecting_segments += el.intersecting_segments
		}
		@all_intersecting_segments.uniq!

		for j in layers
			if (h = m[j][-1]) && @start_node.pads.include?(j)
				@start_node.next << h
			end
		end
	end

	def current_path_cost
		cost = 0
		oldseg = nil
		@path.each{|seg|

		if oldseg
			if oldseg.layer != seg.layer
			cost += 50
			end
		end
		oldseg = seg

as = @active_segments[seg.layer]

			cost += seg.intersecting_segments.select{|el| as[el]}.inject(0){|sum, el| sum + @ppdc[seg, el]}
		}
		@old_cost = cost
	end


	#http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
	def dijkstra(gen_seg_list = false)
unless gen_seg_list
		#res = nil
		res = @all_intersecting_segments.find{|si| si.fresh}

		#@segment_list.each{|s|
		#	break if (res = s.intersecting_segments.find{|si| si.fresh})
		#}
		return unless res
end 
		s = @start_node
		t = @end_node
		fail unless s.is_a? S_node
		fail unless t.is_a? T_node
		q = BOOST::Fibonacci_Queue.new
		c = Hash.new(Float::INFINITY)
		q[s] = c[s] = 0
		while (u = q.pop_data)
			break if u == t
			fail if c[u] == Float::INFINITY
			fail if u.next.empty?
			if u.is_a? F_node
				as = @active_segments[u.layer]
				u.next.each{|v|
					seg = find_segment(u.x, u.y, v.x, v.y)
					cost = seg.intersecting_segments.select{|el| as[el]}.inject(0){|sum, el| sum + @ppdc[seg, el]}
					cost += c[u]
					if cost < c[v]
						c[v] = cost
						q[v] = cost
						v.pre = u
					end
				}
			else
				u.next.each{|v|
					cost = u.cost_to(v)
					cost += c[u]
					if cost < c[v]
						c[v] = cost
						q[v] = cost
						v.pre = u
					end
				}
			end
		end
		q.clear
		puts 'ggg', c[t]
		@new_cost = c[t]
		#fail unless u # == t
		#
		fail unless t == u
		if gen_seg_list
			list = Array.new
			n = t
			while n != s
				p = n.pre
				if p.is_a? F_node
					seg = find_segment(p.x, p.y, n.x, n.y)
					seg.layer = p.layer
					list << seg
				end
				n = p
			end
			@path = list
			return list
		end
	end

	def segment_cost(x1, y1, x2, y2, layer)
		s = @segment_list.find{|el| el.x1 == x1 && el.y1 == y1 && el.x2 == x2 && el.y2 == y2}
		fail unless s
		l = s.intersecting_segments.select{|el| @active_segments[layer][s, el]}
		cost = l.inject{|sum, el| sum + @ppdc[s, el]}

	end

end

class Net_List
	attr_accessor :name
	attr_accessor :terminals
end

class Via_Grid < Hash
	attr_accessor :grid
	def [](x, y)
		x = x.fdiv(grid).round
		y = y.fdiv(grid).round
		return self[[x, y]]
	end
end
	Board_Size = 800

# layer assignment
class Assignment
	attr_accessor :b1x, :b1y, :b2x, :b2y # B, routing area in arbitrary units, i.e. um. [bx1, by1] < [bx2, by2]
	attr_accessor :min_via_size, :max_via_size 
	attr_accessor :num_layers
	attr_accessor :rstyles
	attr_accessor :input_nets
	attr_accessor :input_2nets
	attr_accessor :output_nets # Array of Net_list
	attr_accessor :output_2nets
	attr_accessor :i2n, :o2n # input and output 2-Nets
	attr_accessor :conflict_canvas
	attr_accessor :active_segments # array, active_segments[l] is a hash with active segments for that layer

	attr_accessor :all_segment_list
	attr_accessor :ppdc

	def initialize(num_layers, min_via_size, max_via_size, b1x, b1y, b2x, b2y, rt, pcb_board)

		@b1x, @b1y, @b2x, @b2y = b1x, b1y, b2x, b2y
		@rt = rt
		@pcb_board = pcb_board
		@num_layers = num_layers
		fail if num_layers < 2
		@min_via_size, @max_via_size = min_via_size, max_via_size
		@input_nets = Hash.new
		@input_2nets = Array.new
		@output_2nets = Array.new
		@conflict_canvas = Array.new(num_layers){Array.new}
		@active_segments = Array.new(num_layers){Hash.new}
		@ppdc = PPDC.new
		@all_segment_list = Array.new

		@image = Cairo::ImageSurface.new(Board_Size, Board_Size)
		@pic = Canvas::Pic.new(@image)
		max_extent = Board_Size.fdiv([b2x - b1x, b2y - b1y].max)

		@pic.scale(max_extent, max_extent)
		@pic.translate(-b1x, -b1y)



		#@image = Cairo::ImageSurface.new(b2x, b2y)
		#@pic = Canvas::Pic.new(@image)
		@pic.set_source_rgba(0.8, 0.8, 0.8, 1)
		@pic.paint

	end




	
	def save_picture
		@image.write_to_png('LApic.png')
	end

	# 	  a
	#    ^
	#   /
	# o/--------> b
	#
	def self.cross_product(ax, ay, bx, by, ox, oy)
		ax -= ox
		ay -= oy
		bx -= ox
		by -= oy
		#fail if (ax == 0 && ay == 0) || (bx == 0 && by == 0)
		ax * by - ay * bx
	end

	def self.cp(ax, ay, bx, by, ox, oy)
		(ax - ox) * (by - oy) < (ay - oy) * (bx - ox)
	end

#         d
#        /
#  a-------------b
#      /
#     /
#    c
	def segment_segment_intersection(ax, ay, bx, by, cx, cy, dx, dy)
	return false if ((cx == ax) && (cy == ay)) || ((dx == ax) && (dy == ay)) || ((cx == bx) && (cy == by)) || ((dx == bx) && (dy == by))
		#(self.cp(bx, by, cx, cy, ax, ay) != self.cp(bx, by, dx, dy, ax, ay)) && (self.cp(dx, dy, ax, ay, cx, cy) != self.cp(dx, dy, bx, by, cx, cy))
		(self.class.cp(bx, by, cx, cy, ax, ay) != self.class.cp(bx, by, dx, dy, ax, ay)) && (self.class.cp(dx, dy, ax, ay, cx, cy) != self.class.cp(dx, dy, bx, by, cx, cy))
	end

	#      (c)
	#     /
	#    /     (p)
	#   /
	# (b)
	# see http://www.geometrictools.com/
	#
	def distance_line_segment_point_squared(bx, by, cx, cy, px, py)
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

	def generate_via_positions(x1, y1, x2, y2, via_t, via_c)
		via_count = 5
		if Math.hypot(x2 - x1, y2 - y1) < 1e5
		via_count = 2

		end
		via_list = Array.new()#via_count)
		dx = (x2 - x1) / (via_count + 1)
		dy = (y2 - y1) / (via_count + 1)
		x = x1
		y = y1
		vs = @max_via_size * 0.5
		for i in 0...via_count

		@pic.set_source_rgba(1, 1, 1, 1)
		@pic.set_line_width(500)





vp = @pcb_board.get_via_position(x, y, x + dx, y + dy, x + dx + dx, y + dy +dy, via_t, via_c)
vp.each{|el|
puts 'elllll', el[0], el[1]
			@pic.new_sub_path
			@pic.arc(el[0], el[1], 500, 0, Math::PI * 2)
			@pic.stroke
}
			x += dx
			y += dy
			x1 = x - vs
			y1 = y - vs
			x2 = x + vs
			y2 = y + vs
			if !@rt.intersects?(x1, y1, x2, y2).empty?
puts 'isconflict'
else
puts 'noconflict'

			end
			puts @rt.intersects?(x1, y1, x2, y2).empty?
			puts x, y
			puts x1, y1, x2, y2
			#via_list << x << y
			if vp.empty?
			puts 'pech'
#via_list << x << y

			else
			puts 'glueck'
			via_list << vp[0][0] << vp[0][1]


			if !@rt.intersects?(vp[0][0] - vs, vp[0][1] - vs, vp[0][0] + vs, vp[0][1] + vs).empty?
@pic.set_source_rgba(1, 1, 0, 1)

			@pic.new_sub_path
			@pic.arc(vp[0][0], vp[0][1], 2000, 0, Math::PI * 2)
			@pic.stroke
end

			end
		end
		if via_list.empty? #||  !@rt.intersects?(via_list[0] - vs, via_list[1] - vs, via_list[0] + vs, via_list[1] + vs).empty?
		dx = (x2 - x1)
		dy = (y2 - y1)
		x = x1 + dx * 0.5
		y = y1 + dy * 0.5
		dx *= 0.1
		dy *= 0.1
		9.times{
		1e4.to_i.times{
			#xt = x + rand() * dx
			vp = @pcb_board.get_via_position(x1, y1, x + rand() * dx, y + rand() * dy, x2, y2, @min_via_size, @max_via_size)
			if !vp.empty?
		#@pic.set_source_rgba(1, 1, 1, 1)

			@pic.new_sub_path
			@pic.arc(vp[0][0], vp[0][1], 1200, 0, Math::PI * 2)
			@pic.stroke




				return vp[0]
			end
		}
		puts "no space", x, y
		dx *= 2
		dy *= 2
		}
fail



		else


		via_list
		end
	end

	#http://en.wikipedia.org/wiki/Prim's_algorithm
	#http://de.wikipedia.org/wiki/Algorithmus_von_Prim
	def prim(vertices, start_vertex)
		q = BOOST::Fibonacci_Queue.new
		p = Hash.new
		vertices.each{|el| q.push(el, Float::INFINITY)}
		q[start_vertex] = 0
		while (u = q.pop_data)
			q.each{|el|
				v, key = el
				if key > (d = (v.x - u.x) ** 2 + (v.y - u.y) ** 2)
				#if key > (d = Math.hypot((v.x - u.x), (v.y - u.y)))
					q[v] = d
					p[v] = u
				end
			}
		end
		p
	end

	def old_gen_input_2nets
		@input_nets.each_value{|v|
			p = prim(v, v[0])
			v[1..-1].each{|el|
				@input_2nets << Input_2Net.new(el, p[el])
			}
		}
	end


	def gen_input_2nets
		@input_nets.each_value{|v|
			p = prim(v, v[0])
			i2n = Array.new
			v[1..-1].each{|el|
				i2n << Input_2Net.new(el, p[el])
			}
			i2n.each{|n|
				[n.t1, n.t2].each{|t|
					num_terminals = 0
					terminals = Array.new
					#t = tn
					terminals << t
					loop do
						nets = i2n.select{|el| terminals.include?(el.t1) || terminals.include?(el.t2)}
						nets.delete(n)
						nets.each{|el| terminals << el.t1 << el.t2}
						terminals.uniq!
						l = terminals.length
						break if l == num_terminals
						num_terminals = l
					end
					m = terminals.max_by{|el| Math.hypot(el.x - t.x, el.y - t.y)}
					l =  Math.hypot(m.x - t.x, m.y - t.y)
					t == n.t1 ? n.t1_ext = l : n.t2_ext = l
					#puts 	'?????????????????????????', n.t1_ext
				}
			}
			@input_2nets += i2n
		}
	end


	def get_vias
		v = Array.new
		@output_2nets.each{|el|
			el.path.each{|s|
			if s.layer == 0
				v << [s.x2, s.y2]
				v << [s.x1, s.y1]
			end
			}
			#v.pop
		}
		@pic.set_source_rgba(0, 0, 1, 0.5)
		v.each{|el|
			@pic.new_sub_path
			@pic.arc(el[0], el[1], 1000, 0, Math::PI * 2)
			@pic.stroke
		}
	end

	def fix_paths
		@output_2nets.each{|el|
			el.path.each_cons(2){|a, b|
				unless b.x2 == a.x1 &&  b.y2 == a.y1
					puts  a.x1, a.x2 , b.x1, b.x2
					fail
				end
x, y = @pcb_board.get_via_position(b.x1, b.y1, b.x2, b.y2, a.x2, a.y2, @min_via_size, @max_via_size)[0]
 b.x2 = a.x1 = x
 b.y2 = a.y1 = y
#@pcb_board.insert_via(x, y, @min_via_size, @max_via_size)
@pcb_board.insert_via(x, y, el.via_thickness, el.via_clearance)
			}
		}
	end



	def draw_paths
		@pic.set_line_width(1000)

		@output_2nets.each{|el|
			col = ([1, 0, 0].permutation(3).to_a + [1, 1, 0].permutation(3).to_a).uniq - [[1, 0, 0]]
			color_index = rand(5)
			el.path.each{|s|
				if s.layer == 0
					@pic.set_source_rgba(*col[color_index], 0.3)
				else
					@pic.set_source_rgba(*col[color_index], 1)
				end
				@pic.move_to(s.x1, s.y1)

				@pic.line_to(s.x2, s.y2)
				@pic.stroke
				@pic.set_source_rgba(0, 0, 0, 1)

				@pic.new_sub_path
				@pic.arc(s.x1, s.y1, 5, 0, Math::PI * 2)
				@pic.new_sub_path
				@pic.arc(s.x2, s.y2, 5, 0, Math::PI * 2)
				@pic.stroke

			}
			@pic.set_source_rgba(0.5, 0.5, 0.5, 1)

			if el.start_node.pads == (0..0)
				@pic.set_source_rgba(1, 1, 1, 1)
			elsif el.start_node.pads == (1..1)
				@pic.set_source_rgba(0, 0, 0, 1)


			end

			
				@pic.new_sub_path
				@pic.arc(el.start_node.x, el.start_node.y, 5, 0, Math::PI * 2)
				@pic.stroke

			
			@pic.set_source_rgba(0.5, 0.5, 0.5, 1)

			if el.end_node.pads == (0..0)
				@pic.set_source_rgba(1, 1, 1, 1)
			elsif el.end_node.pads == (1..1)
				@pic.set_source_rgba(0, 0, 0, 1)


			end



			#if el.end_node.pads == (0..0)
			#	@pic.set_source_rgba(1, 1, 1, 1)

			
				@pic.new_sub_path
				@pic.arc(el.end_node.x, el.end_node.y, 5, 0, Math::PI * 2)
				@pic.stroke

			




		}


	end


	def draw_input_2nets
		@pic.set_source_rgba(0, 0, 0, 1)
		@pic.set_line_width(2)

		@input_2nets.each{|el|


col = ([1, 0, 0].permutation(3).to_a + [1, 1, 0].permutation(3).to_a).uniq - [[1, 0, 0]]
@pic.set_source_rgba(*col[el.t1.net_name % 5], 1)


			@pic.move_to(el.t1.x, el.t1.y)
			@pic.line_to(el.t2.x, el.t2.y)
			@pic.stroke
			@pic.new_sub_path
			@pic.arc(el.t1.x, el.t1.y, el.t1_ext, 0, Math::PI * 2)
			@pic.stroke
			@pic.new_sub_path
			@pic.arc(el.t2.x, el.t2.y, el.t2_ext, 0, Math::PI * 2)
			@pic.stroke



		}

	end

	def add_input_terminal(terminal)
		n = terminal.net_name
		if (h = @input_nets[n])
			h << terminal
		else
			@input_nets[n] = [terminal]
		end
	end

	def gen_test_input_nets(num)
		num.times{|j|
			i = 2 + rand(4) # create min 2, max 5 terminals for this net
			i.times{
				x = @b1x + rand(@b2x - @b1x)
				y = @b1y + rand(@b2y - @b1y)
				t = Terminal.new(i, j, x, y, [0..0, 0..(@num_layers - 1), (@num_layers - 1)..(@num_layers - 1)][rand(3)])
				add_input_terminal(t)
			}
		}
	end

	def gen_output_2nets
		@input_2nets.each{|el|
			s = S_node.new(el.t1.x, el.t1.y, el.t1.layers)
			t = T_node.new(el.t2.x, el.t2.y, el.t2.layers)


		via_t = @rstyles[el.t1.style].via_diameter #* 0.5
		via_c = @rstyles[el.t1.style].via_clearance #* 0.5
#puts 
			vp = generate_via_positions(el.t1.x, el.t1.y, el.t2.x, el.t2.y, via_t, via_c)
			n =  Output_2Net.new(s, t, vp, @num_layers)
			n.i2n = el
			n.via_clearance = 1 * via_c
			n.via_thickness = 1 * via_t
			n.build_assignment_graph(@conflict_canvas)
			@output_2nets << n
		}
	end

	def gen_all_segment_list
		@output_2nets.each{|el|
			@all_segment_list += el.segment_list
		}
		puts @all_segment_list.length
	end

	def old_gen_PPDC # Pair-Pair_Detour_Conflict
		i = @all_segment_list.length
		while (i -= 1) >= 0 do
			j = i
			while (j -= 1) >= 0 do
				a = @all_segment_list[i]
				b = @all_segment_list[j]
				ax1, ay1, ax2, ay2, bx1, by1, bx2, by2 = a.x1, a.y1, a.x2, a.y2, b.x1, b.y1, b.x2, b.y2
				if segment_segment_intersection(a.x1, a.y1, a.x2, a.y2, b.x1, b.y1, b.x2, b.y2)
					a1_a2 = Math::hypot(ax2 - ax1, ay2 - ay1)
					b1_b2 = Math::hypot(bx2 - bx1, by2 - by1)
					a1_b1 = Math::hypot(bx1 - ax1, by1 - ay1)
					a1_b2 = Math::hypot(bx2 - ax1, by2 - ay1)
					a2_b1 = Math::hypot(bx1 - ax2, by1 - ay2)
					a2_b2 = Math::hypot(bx2 - ax2, by2 - ay2)
					@ppdc[a, b] = [a1_b1 + a2_b1 - a1_a2, a1_b2 + a2_b2 - a1_a2, a1_b1 + a1_b2 - b1_b2, a2_b1 + a2_b2 - b1_b2].min
				end
			end
		end
	end

	def gen_PPDC
		@output_2nets.each{|o2n|
			o2n.segment_list.each{|s|
				ax, ay = s.x1, s.y1
				bx, by = s.x2, s.y2
				s.intersecting_segments.each{|si|
					cx, cy = si.x1, si.y1
					dx, dy = si.x2, si.y2
					c_d = Math::hypot(dx - cx, dy - cy)
					a_c = Math::hypot(cx - ax, cy - ay)
					a_d = Math::hypot(dx - ax, dy - ay)
					b_c = Math::hypot(cx - bx, cy - by)
					b_d = Math::hypot(dx - bx, dy - by)
					d1 = a_c + a_d - c_d
					if ax == o2n.i2n.t1.x && ay == o2n.i2n.t1.y
						d1 += o2n.i2n.t1_ext
					end
					d2 = b_c + b_d - c_d
					if bx == o2n.i2n.t2.x && by == o2n.i2n.t2.y
						d2 += o2n.i2n.t2_ext
					end
					d = @ppdc[s, si]
					@ppdc[s, si] = [d, d1, d2].min
				}
			}
		}
	end

	def find_intersecting_segmensts
		@output_2nets.each{|o2n|
			o2n.segment_list.each{|si|
				@all_segment_list.each{|sj|
					if  segment_segment_intersection(si.x1, si.y1, si.x2, si.y2, sj.x1, sj.y1, sj.x2, sj.y2)
						si.intersecting_segments << sj
					end
				}
			}
		}
	end

	def init_output_2nets
		@output_2nets.each{|o2n|
			o2n.set_ppdc(@ppdc)
			o2n.set_active_segments(@active_segments)
		}
	end

	def route_output_2nets
		@output_2nets.each{|el|
			l = el.dijkstra(true)
			puts '+++'
			l.each{|s| puts s.x1, s.y1, s.x2, s.y2, s.layer}
		}
	end

	def route_output_2nets_first_time
		o2n = @output_2nets.dup

			o2n.each{|el|
				el.dijkstra
			}
@all_segment_list.each{|s| s.fresh = false}
old_list = Array.new

		while !o2n.empty?
			o2n.each{|el|
				el.dijkstra
			}
			n = o2n.min_by{|el| el.new_cost}
			o2n.delete(n)
			seg_list = n.dijkstra(true)
			seg_list.each{|el| @active_segments[el.layer][el] = true}
old_list.each{|el| el.fresh = false}
seg_list.each{|el| el.fresh = true}

			#o2n.each{|el| el.old_cost = el.new_cost}
		end
	end

	def route_output_2nets_optimize
		o2n = @output_2nets
		delta = 1
		while delta > 0
			#o2n.each{|el| el.old_cost = el.new_cost}
			o2n.each{|el| el.current_path_cost}

			o2n.each{|el|
				el.dijkstra
				puts 'xxx'
			}

o2n.each{|el| puts el.old_cost - el.new_cost}



			n = o2n.max_by{|el| el.old_cost - el.new_cost}
			delta = n.old_cost - n.new_cost

		print 'ooo', delta, "\n"
			n.path.each{|s| @active_segments[s.layer].delete(s)}
#@active_segments[0].clear
#@active_segments[1].clear
			seg_list = n.dijkstra(true)
			seg_list.each{|el| @active_segments[el.layer][el] = true}
seg_list.each{|el| el.fresh = true}
		end
	end

end
end

if __FILE__ == $0

rs = RM::init_seed

l = LA::Assignment.new(2, 1000, 1000, 0, 0, 400000, 400000)
l.gen_test_input_nets(32)
l.gen_input_2nets
#l.draw_input_2nets
l.gen_output_2nets
l.gen_all_segment_list
l.find_intersecting_segmensts
l.gen_PPDC
l.init_output_2nets



l.route_output_2nets_first_time
l.route_output_2nets_optimize
l.draw_paths
l.get_vias
#l.draw_input_2nets

l.save_picture

end


=begin
2-Net Assignment Graph, similar to Figure 42
 
  /- |-> F -> O -\    l1
 / - |-> F -> O - \   l2
S -- O   F -> O -- T  l3
 \ - |-> F -> O - /   l4
  \- |-> F -> O -/    l5
	       \--- (to next via node)


Path starts at terminal S. Then Via nodes and Forward nodes alternate.
Each forward node has edges to all via nodes at the right.
Each via node has paths to the next Forward nodes at the right, excloding the
forward node on the same layer.
=end


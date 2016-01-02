require_relative 'canvas' # for graphical verify
require_relative '../RAPOLLONIUS/rcgal_apollonius'
require_relative '../RTREE_2D_RECT/rboost_rtree_2d_rect'

module PCB
	# convert to PCB's internal 0.01mil unit
	Num_With_Unit = /([+-]?\d+(?:\.\d+)?)(\w*)/
	def self.apply_unit(n)
		num, unit = Num_With_Unit.match(n).to_a[1, 2]
		factor = case unit
			when '' then 1
  		when 'mm' then 1e6 / 254.0
  		when 'um' then 1e3 / 254.0
  		when 'nm' then 1e0 / 254.0
			when 'mil' then 100
   		else fail 'invalid unit'
		end
		(Integer(num) rescue num.to_f) * factor
	end
end

module PCB_Element
	# PCB's Element syntax
	# Element [SFlags "Desc" "Name" "Value" MX MY TX TY TDir TScale TSFlags] (
	# Element["" "SMD Cap" "C21" "" 256500 107000 -5000 6100 0 150 ""]
	SFlags = Desc = Name = Value = '(".*")' + '\s+'
	MX = MY = TX = TY = '([+-]?\d+(?:\.\d+)?[[:alpha:]]{,3})' + '\s+'
	TDir = TScale = '(\d+)' + '\s+'
	TSFlags  = '(".*")'
	RegExp_Start = '^\s*Element\s*\[\s*'
	RegExp_End = '\s*\]\s*(\()?\s*$'
	RegExp = RegExp_Start + SFlags + Desc + Name + Value + MX + MY + TX + TY + TDir + TScale + TSFlags + RegExp_End
end

module PCB_Netlist
	# PCB's Netlist syntax
	#	Netlist ( ) (
	RegExp_Start = '^\s*NetList\s*\(\s*'
	RegExp_End = '\s*\)\s*(\()?\s*$'
	RegExp = RegExp_Start + RegExp_End
end

module PCB_Net
	# PCB's Net syntax
	# Net("AGND" "(unknown)") (
	Name = PCB_Element::SFlags
	Style = PCB_Element::TSFlags
	RegExp_Start = '^\s*Net\s*\(\s*'
	RegExp_End = '\s*\)\s*(\()?\s*$'
	RegExp = RegExp_Start + Name + Style + RegExp_End
end

module PCB_Connect
	# PCB's Connect syntax
	# Connect("C9-2")
	Name = PCB_Element::TSFlags
	RegExp_Start = '^\s*Connect\s*\(\s*'
	RegExp_End = '\s*\)\s*(\))?\s*$'
	RegExp = RegExp_Start + Name + RegExp_End
end

module PCB_Pin
	# PCB's Pin syntax
	# Pin [rX rY Thickness Clearance Mask Drill "Name" "Number" SFlags]
	# Pin[50000 0 6000 3000 9000 2800 "RB0" "6" "edge2"]
	RX = RY = Thickness = Clearance = Mask = Drill = PCB_Element::MX
	Name = Number = PCB_Element::SFlags
	SFlags = PCB_Element::TSFlags
	RegExp_Start = '^\s*Pin\s*\[\s*'
	RegExp_End = '\s*\]\s*(\))?\s*$'
	RegExp = RegExp_Start + RX + RY + Thickness + Clearance + Mask + Drill + Name + Number + SFlags + RegExp_End
end

module PCB_Pad
	# PCB's Pad syntax
	# Pad [rX1 rY1 rX2 rY2 Thickness Clearance Mask "Name" "Number" SFlags]
	# Pad[0 0 0 0 6000 3000 9000 "1" "1" "square"]
	RX1 = RY1 = RX2 = RY2 = Thickness = Clearance = Mask = PCB_Element::MX
	Name = Number = PCB_Element::SFlags
	SFlags = PCB_Element::TSFlags
	RegExp_Start = '^\s*Pad\s*\[\s*'
	RegExp_End = PCB_Pin::RegExp_End
	RegExp = RegExp_Start + RX1 + RY1 + RX2 + RY2 + Thickness + Clearance + Mask + Name + Number + SFlags + RegExp_End
end

module PCB


# buffer current line
class PCB_Board_File < File
	def initialize(name, mode)
		@tl = ''
		super
	end

	def this_line
		while @tl && (@tl.empty? || /^\s*$|^\s*#/.match(@tl))
			@tl = self.gets
		end
		@tl
	end

	def this_line=(v)
		@tl = v
	end

	def next_line
		@tl &&= ''
	end
end

class Netlist
  @@regexp = Regexp.new(PCB_Netlist::RegExp)
	@@body_start = /^\s*\(\s*$/
	@@body_end = /^\s*\)\s*$/
  @@net_start = Regexp.new(PCB_Net::RegExp_Start)
	attr_accessor :nets

	def initialize
		@nets = Array.new
	end

	# return == valid_Netlist?
	# raise exception on syntax error
	def read(file)
		if m = @@regexp.match(file.this_line)
			file.next_line
			d = m.to_a
			bra = d.pop
			unless bra
				if bra = @@body_start.match(file.this_line)
					file.next_line
				end
			end
			return false unless bra
			while (line = file.this_line) do
				if @@body_end.match(line)
					file.next_line
					return true
				elsif @@net_start.match(line)
					net = Net.new
					net.read(file)
					@nets << net
				else
					file.next_line
				end
			end
			fail "missing ')' at end of Netlist"
		else
			return false
		end
	end
end

class Net
  @@regexp = Regexp.new(PCB_Net::RegExp)
  @@connect_start = Regexp.new(PCB_Connect::RegExp_Start)
	@@body_start = /^\s*\(\s*$/
	@@body_end = /^\s*\)\s*$/
	attr_accessor :name, :style
	attr_accessor :connects

	def initialize
		@connects = Array.new
	end

	# return == valid_Net?
	# raise exception on syntax error
	def read(file)
		if m = @@regexp.match(file.this_line)
			file.next_line
			d = m.to_a
			bra = d.pop
			d.each{|el| el.gsub!('"', '')}
			@name, @style = d[1, 2]
			unless bra
				if bra = @@body_start.match(file.this_line)
					file.next_line
				end
			end
			return false unless bra
			while (line = file.this_line) do
				if @@body_end.match(line)
					file.next_line
					return true
				elsif @@connect_start.match(line)
					connect = Connect.new
					connect.read(file)
					@connects << connect
				else
					file.next_line
				end
			end
			fail "missing ')' at end of Net"
		else
			return false
		end
	end
end

class Connect
  @@regexp = Regexp.new(PCB_Connect::RegExp)
	attr_accessor :full_name, :element_name, :pin_name

	# raises exception on syntax error
	def read(file)
		if m = @@regexp.match(file.this_line)
			file.next_line
			d = m.to_a
			ket = d.pop
			file.this_line = ')' if ket # push back
			d.each{|el| el.gsub!('"', '')}
			@full_name = d[1]
			puts @full_name
		else
			raise "invalid Connect"
		end
	end
end

class Element
  @@regexp = Regexp.new(PCB_Element::RegExp)
  @@pin_start = Regexp.new(PCB_Pin::RegExp_Start)
  @@pad_start = Regexp.new(PCB_Pad::RegExp_Start)
	@@body_start = /^\s*\(\s*$/
	@@body_end = /^\s*\)\s*$/
	attr_accessor :sflags, :desc, :name, :value, :mx, :my, :tx, :ty, :tdir, :tscale, :tsflags
	attr_accessor :pins
	attr_accessor :pads
	attr_accessor :clusters # overlapping pins/pads with same name

	def initialize
		@pins = Array.new
		@pads = Array.new
		@clusters = Hash.new
	end

	# return == valid_Element?
	# raise exception on syntax error
	def read(file)
		if m = @@regexp.match(file.this_line)
			file.next_line
			d = m.to_a
			bra = d.pop
			d.each{|el| el.gsub!('"', '')}
			@mx, @my, @tx, @ty = d[5, 4].map{|v| PCB::apply_unit(v)}# + rand(10000)}
			@sflags, @desc, @name, @value = d[1, 4]
			@tdir, @tscale = d[9, 2].map{|v| Integer(v)}
			@tsflags = d[-1]
			unless bra
				if bra = @@body_start.match(file.this_line)
					file.next_line
				end
			end
			return false unless bra # no body, so ignore this element
			while (line = file.this_line) do
				if @@body_end.match(line)
					file.next_line
					return true
				elsif @@pin_start.match(line)
					pin = Pin.new
					pin.parent = self
					pin.read(file)
					@pins << pin
				elsif @@pad_start.match(line)
					pad = Pad.new
					pad.parent = self

					pad.read(file)
					@pads << pad
				else
					file.next_line
				end
			end
			fail "missing ')' at end of Element"
		else
			return false # ignore
		end
	end


	def generate_clusters
		@clusters = Hash.new{|h, k| h[k] = Cluster.new}
		@pins.each{|el|
			c = @clusters[@name + '-' + el.number]
			c.on_solder_side = c.on_component_side = true
			c.pins << el
			#@clusters[@name + '_' + el.name].pins << el
		}
		@pads.each{|el|
			c = @clusters[@name + '-' + el.number]
			el.onsolder ? c.on_solder_side = true : c.on_component_side = true
			#c.on_solder_side = c.on_component_side = true
			c.pads << el
			#@clusters[@name + '_' + el.name].pads << el
		}
		ag = CGAL::Apollonius_graph.new

		@clusters.each_value{|c|


puts 'cluster'
puts c.on_solder_side
puts  c.on_component_side

			h = c.pins.dup
			#c.pads.each{|p| h += p.vpins}
			#c.pads.each{|p|  h += p.vpins if c.on_solder_side == p.onsolder || c.on_component_side == !p.onsolder }
			c.pads.each{|p|  h += p.vpins if c.on_component_side == !p.onsolder }



			c.convex_pin_hull = h
			if h.length > 2
			ag.clear
			h.each{|el| ag.insert(el, el.rx, el.ry, el.thickness + el.clearance)}
			c.convex_pin_hull = ag.convex_hull_array
			end
		}
		



		@clusters.each_pair{|k, v|
			v.name = k
			if !v.pins.empty?
				v.x, v.y = v.pins[0].rx, v.pins[0].ry
			elsif !v.pads.empty?
				#v.x, v.y = v.pads[0].rx1, v.pads[0].ry1
				v.x, v.y = v.convex_pin_hull[0].rx, v.convex_pin_hull[0].ry

			else
				fail
			end		
		}
		@clusters.each_value{|v| v.x += @mx; v.y += @my; v.mx = @mx; v.my = @my}

	end





	def old_generate_clusters
		h = Hash.new
		@pins.each{|el|
			if h.include?(el.name)
				h[el.name] << el
			else
				h[el.name] = [el]
			end
		}
		h.each_pair{|k, v|
			c = Cluster.new
			c.name = k
			c.pins = v
		}
		h = Hash.new
		@pads.each{|el|
			if h.include?(el.name)
				h[el.name] << el
			else
				h[el.name] = [el]
			end
		}
		h.each_pair{|k, v|
			c = Cluster.new
			c.name = k
			c.pads = v
		}
	end

end


class Via
	attr_accessor :x, :y, :thickness, :clearance
	def initialize(x, y, thickness, clearance)
		@x, @y, @thickness, @clearance = x, y, thickness, clearance
	end
end


class Pin
  @@regexp = Regexp.new(PCB_Pin::RegExp)
	attr_accessor :rx, :ry, :thickness, :clearance, :mask, :drill, :name, :number, :sflags
	attr_accessor :parent
	attr_accessor :onsolder

def initialize
@rx = @ry = 0
#@onsolder = false
end

	# raises exception on syntax error
	def read(file)
		if m = @@regexp.match(file.this_line)
			file.next_line
			d = m.to_a
			ket = d.pop
			file.this_line = ')' if ket # push back
			d.each{|el| el.gsub!('"', '')}
			@rx, @ry, @thickness, @clearance, @mask, @drill = d[1, 6].map{|v| PCB::apply_unit(v)}
			@name, @number, @sflags = d[7, 3]
		else
			raise "invalid Pin"
		end
	end

	def draw(ct, x, y)
		ct.set_line_width(0)
		ct.new_sub_path
		ct.set_source_rgba(1, 0, 1, 1)
		ct.arc(x + @rx, y + @ry, (@thickness + @clearance).fdiv(2), 0, 2 * Math::PI)
		ct.fill
		ct.new_sub_path
		ct.set_source_rgba(0, 0, 0, 1)
		ct.arc(x + @rx, y + @ry, @thickness.fdiv(2), 0, 2 * Math::PI)
		ct.fill
	end
end

class Pad
  @@regexp = Regexp.new(PCB_Pad::RegExp)
	attr_accessor :rx1, :ry1, :rx2, :ry2, :thickness, :clearance, :mask, :name, :number, :sflags
	attr_accessor :vpins
	attr_accessor :square
	attr_accessor :onsolder
	attr_accessor :parent

	# raises exception on syntax error
	def read(file)
		if m = @@regexp.match(file.this_line)
			file.next_line
			d = m.to_a
			ket = d.pop
			file.this_line = ')' if ket # push back
			d.each{|el| el.gsub!('"', '')}
			@rx1, @ry1, @rx2, @ry2, @thickness, @clearance, @mask = d[1, 7].map{|v| PCB::apply_unit(v)}
			@name, @number, @sflags = d[8, 3]
			@square = /square/.match(@sflags)
			@onsolder = /onsolder/.match(@sflags)
			@vpins = Array.new

			if (@rx1 == @rx2) &&  (@ry1 == @ry2)
#				ox = oy = dx = dy = @thickness * 0.5
#oy = 0.0
#dy = 0.0
@rx1 += 1e-6
end
			#else

			h = @thickness * 0.5 / Math.hypot(@rx1 - @rx2, @ry1 - @ry2)
			puts 'ttt', h
			puts d
			puts @thickness, @rx1, @rx2, @ry1, @ry2
			fail unless h.finite?
			#h=1.0
			ox, oy = (@ry1 - @ry2) * h, (@rx2 - @rx1) * h
			dx = (@rx2 - @rx1) * h
			dy = (@ry2 - @ry1) * h
			#end
			x1, y1 = @rx1 - dx, @ry1 - dy
			x2, y2 = @rx2 + dx, @ry2 + dy
			cx = (@rx1 + @rx2) * 0.5
			cy = (@ry1 + @ry2) * 0.5
			if @square # place virtual pin at all 4 corners and one centered on each side
				[[x1 + ox, y1 + oy], [x1 - ox, y1 - oy], [x2 + ox, y2 + oy], [x2 - ox, y2 - oy], [x1, y1], [x2, y2],
				[cx + ox, cy + oy], [cx - ox, cy - oy]].each{|xy|
					p = Pin.new
					p.onsolder = @onsolder
					p.rx, p.ry = xy
					#p.rx += 1
					#p.ry += 1

					fail if p.rx == Float::NAN
					fail if p.ry == Float::NAN
fail if p.rx.infinite?
fail if p.ry.infinite?

					puts p.rx, p.ry
					p.thickness = 0
					p.clearance = @clearance
					@vpins << p
				}
			else
			if (@rx1 == @rx2) &&  (@ry1 == @ry2)
p = Pin.new
					p.onsolder = @onsolder

p.rx, p.ry = @rx1, @ry1
p.clearance = @clearance
p.thickness = @thickness
@vpins << p

			else


				[[@rx1, @ry1], [@rx2, @ry2], [cx + ox, cy + oy], [cx - ox, cy - oy]].each_with_index{|xy, i|
					p = Pin.new
	p.onsolder = @onsolder


					p.rx, p.ry = xy

					p.rx += 1
					p.ry += 1
					#x, p.ry = 0,0
					fail if p.rx == Float::NAN
					fail if p.ry == Float::NAN

fail if p.rx.infinite?
fail if p.ry.infinite?
					p.thickness = (i < 2 ? @thickness : 0)
					p.clearance = @clearance
					@vpins << p
				}
				end
			end
		else
			raise "invalid Pad"
		end
	end

	def draw(ct, x, y)
		if @square
			ct.set_line_cap(Cairo::LINE_CAP_SQUARE)
		else
			ct.set_line_cap(Cairo::LINE_CAP_ROUND)
		end
		ct.set_source_rgba(1, 1, 1, 1)
		ct.set_line_width(@thickness + @clearance)
		ct.move_to(x + @rx1, y + @ry1)
		ct.line_to(x + @rx2, y + @ry2)
		ct.stroke
		ct.set_source_rgba(0, 0, 0, 1)
		ct.set_line_width(@thickness)
		ct.move_to(x + @rx1, y + @ry1)
		ct.line_to(x + @rx2, y + @ry2)
		ct.stroke
		@vpins.each{|vp| vp.draw(ct, x, y)}
	end
end

# pad with pin at the center builds a cluster -- the 4 vpins at the corners build the convex_pin_hull
# o---------------o
# |       O       |
# o---------------o
#
class Cluster
	attr_accessor :name
	attr_accessor :x, :y
	attr_accessor :mx, :my
	attr_accessor :pins, :pads
	attr_accessor :convex_pin_hull
	attr_accessor :on_solder_side, :on_component_side
	def initialize
		@pins = Array.new
		@pads = Array.new
		@convex_pin_hull = Array.new
		@on_solder_side = @on_component_side = false;
	end
end

class Board
	@@el_start = Regexp.new(PCB_Element::RegExp_Start)
	@@netlist_start = Regexp.new(PCB_Netlist::RegExp_Start)
	attr_accessor :elements
	attr_accessor :netlists
	attr_accessor :clusters
	attr_accessor :inserted_vias
	attr_accessor :rescue_vias
	attr_accessor :rt
	attr_accessor :x1, :y1, :x2, :y2 # board dimension

	def initialize
		@elements = Array.new
		@inserted_vias = Array.new
		@rescue_vias = Array.new
		@netlists = Array.new # should be only one netlist
		@clusters = Hash.new

	end

	def insert_via(x, y, thickness, clearance)
		d = (thickness + clearance) * 0.5
		v = Via.new(x, y, thickness, clearance)
		@rt.insert(v, x - d, y - d, x + d, y + d)
		@inserted_vias << v
	end

	def read_pcb_file(name)
		begin
			PCB_Board_File.open(name, 'r') do |file|
				begin
					while line = file.this_line
						if @@el_start.match(line)
							el = Element.new
							if el.read(file)
								@elements << el
							end
						elsif @@netlist_start.match(line)
							nl = Netlist.new
							if nl.read(file)
								@netlists << nl
							end
						else
							file.next_line
						end
					end
				rescue => e
					print "Error in file #{name} line #{file.lineno}\n"
					puts "==> #{file.this_line}"
					puts e.class, e.message
				end
			end
		rescue Errno::ENOENT => e
			puts e.message
		end
	end
	
	def inspect
		puts 'inspect'
		@elements.each{|el| puts '--', el.mx, el.my}
	end

	Board_Size = 800
	def draw
		x1 = y1 = 1e6
		x2 = y2 = -1e6
		@elements.each{|el|
			x1 = el.mx if x1 > el.mx
			y1 = el.my if y1 > el.my
			x2 = el.mx if x2 < el.mx
			y2 = el.my if y2 < el.my
		}
#puts (x2 - x1), (y2 - y1)

		@image = Cairo::ImageSurface.new(Board_Size, Board_Size)
		@pic = Canvas::Pic.new(@image)
		max_extent = Board_Size.fdiv([@x2 - @x1, @y2 - @y1].max)

		@pic.scale(max_extent, max_extent)
		@pic.translate(-@x1, -@y1)
		@pic.set_source_rgba(0.8, 0.8, 0.8, 1)
		@pic.paint
		@pic.set_source_rgba(0, 0, 0, 1)
		@pic.set_line_width(2000)
		@elements.each{|el|
			@pic.new_sub_path
			@pic.arc(el.mx, el.my, 10, 0, 2 * Math::PI)
			@pic.stroke
		}
		@pic.set_source_rgba(1, 1, 1, 1)

		@pic.set_line_width(0)

		@elements.each{|el|
			el.pins.each{|p|
				p.draw(@pic, el.mx, el.my)
			}
		@pic.set_source_rgba(1, 1, 1, 1)
			el.pads.each{|p|
				p.draw(@pic, el.mx, el.my)
			}
		}
		@image.write_to_png('pic.png')
	end

	def generate_rtree
		@rt = BOOST::R_tree_2d_rect.new
		@elements.each{|el|
			mx = el.mx
			my = el.my
			el.pins.each{|p|
				x = mx + p.rx
				y = my + p.ry
				r = (p.thickness + p.clearance) * 0.5
				x1 = x - r
				y1 = y - r
				x2 = x + r
				y2 = y + r
				@rt.insert(p, x1, y1, x2, y2)
			}
			el.pads.each{|p|
				prx1 = p.rx1 + mx
				pry1 = p.ry1 + my
				prx2 = p.rx2 + mx
				pry2 = p.ry2 + my
				dx = prx2 - prx1
				dy = pry2 - pry1
				l = (p.thickness + p.clearance) / Math.hypot(dx, dy) * 0.5
				dx *= l
				dy *= l
				drx, dry = dy, -dx
				x1 = prx1 - dx - drx
				y1 = pry1 - dy - dry
				x4 = prx1 - dx + drx
				y4 = pry1 - dy + dry
				x2 = prx2 + dx - drx
				y2 = pry2 + dy - dry
				x3 = prx2 + dx + drx
				y3 = pry2 + dy + dry
				fail if Math.hypot(x3 - x1, y3 - y1) - Math.hypot(x2 - x4, y2 - y4) > 1e-3
				#puts 'hach', Math.hypot(x3 - x1, y3 - y1) - Math.hypot(x2 - x4, y2 - y4)
				@rt.insert(p, [x1, x2, x3, x4].min, [y1, y2, y3, y4].min, [x1, x2, x3, x4].max,  [y1, y2, y3, y4].max) 
			}
		}
	end


# We have rounded and rectangular pads arbitrary rotated
# When the via touches the pad, we move it away
#
#         O via
# ------------- 
# |-----------| rectangular pad
# -------------
# 
#         O via
#  -----------
# (-----------) rectangular rounded pad
#  -----------
# 
#         O via
#        O round pad/pin#
#
# For rectangular pads we check the xxx area first, then the y area.
# If via is not in the xxx and the y area, then check nearest corner c
# For rounded pads check xxx area and then y circles
# For circular pad, we use the pin check below.
#   o                     c  xxxxxxxxxxx  c
#  /                        --------------
# /                        y(            )y
# -|-------|-               -------------
# d    l    d             c  xxxxxxxxxxx  c
#
	RTUS = 1 + 1e-6 # move a little bit more to ensure that next (RTree) check will not fail 

	# plain circle-circle overlap check -- returns dx,dy movement for (2) or empty array if no overlap   
	def move_via_pin_check(x1, y1, thickness1, clearance1, x2, y2, thickness2, clearance2)
		c = [clearance1, clearance2].max
		min = (thickness1 + thickness2 + c) * 0.5
		d = Math.hypot(x2 - x1, y2 - y1)
		if d < min
			h = (min - d) / d * RTUS
			return (x2 - x1) * h, (y2 - y1) * h
		else
			return []
		end
	end

	# returns array of dx,dy movement for new via candidate positions, or empty array if no conflict
	def move_via(p, vx, vy, v_thickness, v_clearance)
	unless p.is_a? Via
		vx -= p.parent.mx
		vy -= p.parent.my
	end
		res = Array.new
		round_pad = corner_check = end_check = false
		clearance = [p.clearance, v_clearance].max
		if p.is_a? Pad
			d = (p.thickness + clearance)
			s = 0.5 * d
			x1, y1, x2, y2 = p.rx1, p.ry1, p.rx2, p.ry2
			if x2 == x1 && y2 == y1
				l = d
				ex = 1
				ey = 0
				round_pad = !p.square
			else
				l = Math.hypot(x2 - x1, y2 - y1)
				ex = (x2 - x1) / l
				ey = (y2 - y1) / l
				l += d
			end
			if !round_pad
				corner_check = p.square
				end_check = !corner_check
				for i in 0..1 do
					if i > 0
						break unless p.square
						l = d
						ex, ey = ey, -ex
						x1 = x2 = (x1 + x2) * 0.5
						y1 = y2 = (y1 + y2) * 0.5
					end
					x1 -= ex * s
					y1 -= ey * s
					x2 += ex * s
					y2 += ey * s
					fail if (l * ex - (x2 - x1)).abs > 1e-6
					fail if (l * ey - (y2 - y1)).abs > 1e-6
					if i == 0 && corner_check 
						cx1, cy1, cx2, cy2 = x1, y1, x2, y2
					end
					a = ((x2 - x1) * (vx - x1) + (y2 - y1) * (vy - y1)) / l
					if a > 0 && a < l
						corner_check = end_check = false
						h = (vx - x1) ** 2 + (vy - y1) ** 2 - a ** 2
						mcld = (p.thickness + v_thickness + clearance) * 0.5
						if h < mcld ** 2
							h = Math.sqrt(h)
							(x2 - x1) * (vy - y1) > (y2 - y1) * (vx - x1) ? (dx = -ey; dy = ex) : (dx = ey; dy = -ex) 
							#dx = ((vx - x1) - ex * a) / h # same value if h!=0
							#dy = ((vy - y1) - ey * a) / h
							res << dx * (mcld - h)
							res << dy * (mcld - h)
							res << dx * (-mcld - h)
							res << dy * (-mcld - h)
						end
					end
				end
			end
		end
		if p.is_a?(Pin)
			res += move_via_pin_check(p.rx, p.ry, p.thickness, p.clearance, vx, vy, v_thickness, v_clearance)
		elsif p.is_a?(Via)
			res += move_via_pin_check(p.x, p.y, p.thickness, p.clearance, vx, vy, v_thickness, v_clearance)
		elsif round_pad
			res += move_via_pin_check(p.rx1, p.ry1, p.thickness, p.clearance, vx, vy, v_thickness, v_clearance)
		elsif end_check
			res += move_via_pin_check(p.rx1, p.ry1, p.thickness, p.clearance, vx, vy, v_thickness, v_clearance)
			res += move_via_pin_check(p.rx2, p.ry2, p.thickness, p.clearance, vx, vy, v_thickness, v_clearance)
		elsif corner_check
			ex, ey = ey, -ex
			x1, y1 = [[cx1 - ex, cy1 - ey], [cx1 + ex, cy1 + ey], [cx2 - ex, cy2 - ey], [cx2 + ex, cy2 + ey]].min_by{|x, y| (x - vx) ** 2 + (y - vy) ** 2}
			res += move_via_pin_check(x1, y1, p.thickness, p.clearance, vx, vy, v_thickness, v_clearance)
		end
		res
	end

	#                    (x2,y2)
	#                      /
	#                     /   
	# (x1,y1)----------(x,y)
	#
	# curently there is a straigh line from (1) to (2), but we allow an angle != PI
	#
	def get_via_position(x1, y1, x, y, x2, y2, via_size, via_clearance)
		candidate_pos_b = Array.new
		dir_matrix = [0, 1, 2, 3, 4].product([0, -1, 1, -2, 2])
		vts = via_size + via_clearance
		[x1, y1, x2, y2].each_slice(2){|x12, y12|
			l = Math.hypot(x12 - x, y12 - y)
			dx, dy = [x12 - x,  y12 - y].map{|el| el / l * vts}
			dxr, dyr = dy, -dx
			candidate_pos_b += dir_matrix.map{|a, b| [a * dx + b * dxr, a * dy + b * dyr]}
		}
		#candidate_pos_b.uniq!.sort_by!{|a, b| a ** 2 + b ** 2}
		vts *= 0.5
		for i in 0..2 do
			#candidate_pos_a = candidate_pos_b.map{|a, b| [a + x, b + y]}
			candidate_pos_a = candidate_pos_b.uniq.sort_by{|a, b| a ** 2 + b ** 2}.map{|a, b| [a + x, b + y]}
			candidate_pos_b = Array.new
			candidate_pos_a.each{|x, y|
				intersecting_objects = @rt.intersects?(x - vts, y - vts, x + vts, y + vts)
				col_num = intersecting_objects.length
				if  col_num == 0
					return [[x, y]]
				elsif (col_num < 2) || (col_num < 3 && i == 0)
					for j in 0..(col_num - 1)
						alt_positions = move_via(intersecting_objects[j], x, y, via_size, via_clearance).each_slice(2).to_a
						if alt_positions.empty? && col_num < 2
							return [[x, y]]
						else
							candidate_pos_b += alt_positions
						end
					end
				end
			}
		end
		return []
	end


	SS = 5000
	def gen_rescue_vias_position

		return to_enum(:gen_rescue_vias_position) unless block_given?

		#hhh = Array.new
		dx = (@x2 - @x1).fdiv(25)
		(@x1..@x2).step(dx){|x|
			dy = (@y2 - @y1).fdiv(25)
			(@y1..@y2).step(dy){|y|
				if @rt.intersects?(x - SS, y - SS, x + SS, y + SS).empty?

					#puts x,y
					#h = [4,6]
					yield x, y
					#hhh << [x,y]
				#else
				#	next
				end
			}
		}
		#puts hhh
		#exit
  end


	def prepair
		@elements.each{|el|
			el.generate_clusters
			el.clusters.each_pair{|k, v|
				fail if @clusters.has_key?(k)
				@clusters[k] = v
			}
		}
		@x1, @x2 = @clusters.values.minmax_by{|el| el.x}.map{|v| v.x}
		@y1, @y2 = @clusters.values.minmax_by{|el| el.y}.map{|v| v.y}

		@clusters.values.each{|c|
			x1, x2 = c.convex_pin_hull.minmax_by{|el| el.rx}.map{|v| v.rx}
			x1 += c.mx
			x2 += c.mx
			@x1 = x1 if @x1 > x1
			@x2 = x2 if @x2 < x2
		}
		@clusters.values.each{|c|
			y1, y2 = c.convex_pin_hull.minmax_by{|el| el.ry}.map{|v| v.ry}
			y1 += c.my
			y2 += c.my
			@y1 = y1 if @y1 > y1
			@y2 = y2 if @y2 < y2
		}


		#@x1, @x2 = c1.x, c2.x
		#puts @x1, @x2

		#b.clusters.each_value{|v|
#
#
#		}
	end

end
end

if __FILE__ == $0

b = PCB::Board.new
b.read_pcb_file("tut1.pcb")
b.prepair
b.inspect
b.draw



puts '---------'
b.netlists.each{|nl|
	nl.nets.each{|n|
		puts n.name
		n.connects.each{|c|
			puts c.full_name
			puts b.clusters[c.full_name].x
		}

	}
}

end

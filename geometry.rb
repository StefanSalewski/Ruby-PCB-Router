# file: geometry.rb
# note: source code is indented with tabs, tab-width=2
#
# some basic geometric functions...
#
# c Stefan Salewsk, mail@ssalewski.de
# License GPL
# Version 0.01 04-OCT-2013
#

require_relative 'canvas'

module RM
	def self.init_seed
		seed = (ARGV[0] ? ARGV[0].to_i : rand(1000))
		print 'random seed is ', seed, "\n"
		srand(seed)
		seed
	end
end

module Geometry

	#     (x2,y2)
	#    /
	#   /    (x0,y0)
	#  /
	# (x1,y1)
	# http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
	#
	def self.distance_line_point(x1, y1, x2, y2, x0, y0)
		x12 = x2 - x1
		y12 = y2 - y1
		(x12 * (y1 - y0) - (x1 - x0) * y12).abs.fdiv(Math.hypot(x12, y12))
	end

	def self.distance_line_point_squared(x1, y1, x2, y2, x0, y0)
		x12 = x2 - x1
		y12 = y2 - y1
		((x12 * (y1 - y0) - (x1 - x0) * y12) ** 2).fdiv(x12 ** 2 + y12 ** 2)
	end

	#      (c)
	#     /
	#    /     (p)
	#   /
	# (b)
	# see http://www.geometrictools.com/
	# see also http://paulbourke.net/geometry/pointlineplane/
	#
	def self.distance_line_segment_point_squared(bx, by, cx, cy, px, py)
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

	def cp(ax, ay, bx, by, ox, oy)
		(ax - ox) * (by - oy) < (ay - oy) * (bx - ox)
	end

#         d
#        /
#       / .... a------b
#      /
#     /
#    c
	def extended_segment_intersection?(ax, ay, bx, by, cx, cy, dx, dy)
	return true if ((cx == ax) && (cy == ay)) || ((dx == ax) && (dy == ay)) || ((cx == bx) && (cy == by)) || ((dx == bx) && (dy == by))
		(cp(bx, by, cx, cy, ax, ay) != cp(bx, by, dx, dy, ax, ay)) || (cp(dx, dy, ax, ay, cx, cy) != cp(dx, dy, bx, by, cx, cy))
	end

	def segment_segment_intersection?(ax, ay, bx, by, cx, cy, dx, dy)
	return true if ((cx == ax) && (cy == ay)) || ((dx == ax) && (dy == ay)) || ((cx == bx) && (cy == by)) || ((dx == bx) && (dy == by))
		#(self.cp(bx, by, cx, cy, ax, ay) != self.cp(bx, by, dx, dy, ax, ay)) && (self.cp(dx, dy, ax, ay, cx, cy) != self.cp(dx, dy, bx, by, cx, cy))
		(cp(bx, by, cx, cy, ax, ay) != cp(bx, by, dx, dy, ax, ay)) && (cp(dx, dy, ax, ay, cx, cy) != cp(dx, dy, bx, by, cx, cy))
	end


	def oldsegment_segment_intersection?(p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y)
		s10_x = p1_x - p0_x
		s10_y = p1_y - p0_y
		s32_x = p3_x - p2_x
		s32_y = p3_y - p2_y
		denom = s10_x * s32_y - s32_x * s10_y
		return false if denom == 0 # collinear
		denomPositive = denom > 0
		s02_x = p0_x - p2_x
		s02_y = p0_y - p2_y
		s_numer = s10_x * s02_y - s10_y * s02_x
		return false if (s_numer < 0) == denomPositive #  No collision
		t_numer = s32_x * s02_y - s32_y * s02_x
		return false if (t_numer < 0) == denomPositive #  No collision
		((s_numer > denom) != denomPositive) && ((t_numer > denom) != denomPositive)
	end


	# http://en.wikipedia.org/wiki/Tangent_lines_to_circles
	# https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Tangents_between_two_circles
	def self.get_tangents(x1, y1, r1, l1, x2, y2, r2, l2)
		#d_sq = (x1 - x2) ** 2 + (y1 - y2) ** 2
		#if d_sq <=  (r1 - r2) ** 2
		#	fail 'get_tangents: circles fully overlap!'
		#end
		d = Math.hypot(x1 - x2, y1 - y2)
		vx = (x2 - x1) / d
		vy = (y2 - y1) / d
		r2 *= (l1 == l2  ? 1 : -1)
		c = (r1 - r2) / d
		h = 1 - c ** 2
		if h < 0
puts r1, r2, c, d, h
fail
		end
		#fail if h < 0
		h = 0 if h < 0 # wrong -- only for inspection
		h = Math.sqrt(h) * (l1  ? -1 : 1)
		nx = vx * c - h * vy
		ny = vy * c + h * vx
		[x1 + r1 * nx, y1 + r1 * ny, x2 + r2 * nx,  y2 + r2 * ny]
	end


	class Vertex
		attr_accessor :x, :y
		attr_accessor :r
		def initialize(x, y, r)
			@x, @y, @r = x, y, r
		end
		def xy
			return x, y
		end
		def tradius
			@r
		end
	end


	def new_convex_vertices(vertices)
		fail if vertices.uniq!
		inner = Array.new
		vertices.combination(2){|a, b|
		a, b = b, a if b.r < a.r
#			if a.r < b.r
#				h = a
#				r = a.r
#			else
#				h = b
#				r = b.r
#			end
			if Math.hypot(a.x - b.x, a.y - b.y) + a.r < b.r
				inner << a
			end
		}
		vertices -= inner

		#vertices.sort!{|a, b| [a.x, a.y] <=> [b.x, b.y]}
		vertices.sort!{|a, b| a.x != b.x ? a.x <=> b.x : a.y <=> b.y}
		return vertices if vertices.length < 3
		upper_hull = Array.new
		vertices.each{|v|
			while upper_hull.length > 1 do
				x1, y1 = upper_hull[-2].xy
				x2, y2 = upper_hull[-1].xy
				if (x2 - x1) * (v.y - y2) < (y2 - y1) * (v.x - x2) 
					upper_hull.pop
				else
					break
				end
			end
			upper_hull.push(v)
		}

		lower_hull = Array.new
		vertices.reverse_each{|v|
			while lower_hull.length > 1 do
				x1, y1 = lower_hull[-2].xy
				x2, y2 = lower_hull[-1].xy
				if (x2 - x1) * (v.y - y2) < (y2 - y1) * (v.x - x2) 
					lower_hull.pop
				else
					break
				end
			end
			lower_hull.push(v)
		}

		#return upper_hull[0...-1] + lower_hull[0...-1]
		hull = upper_hull[0...-1] + lower_hull[0...-1]
hull << hull.first
		ttt = Array.new
		hull.each_cons(2){|a, b|
			ttt << get_tangents(a.x, a.y, a.tradius, true, b.x, b.y, b.tradius, true)
		}

hull.pop

		#ttt << ttt.first
		candidates = vertices - hull
		candidates.each{|v|
			ttt.each{|ax, ay, bx, by|
				if unused_distance_line_segment_point_squared(ax, ay, bx, by, v.x, v.y) < v.tradius ** 2
					hull << v
					break
				end
			}
		}
		#return hull
		#
		#hull = vertices
		#
		cx = hull.inject(0){|sum, el| sum + el.x}.fdiv(hull.length)
		cy = hull.inject(0){|sum, el| sum + el.y}.fdiv(hull.length)
		hull.sort!{|a, b| a.x - a.r != b.x - b.r ? a.x - a.r <=> b.x - b.r : a.y - a.r <=> b.y - b.r}
		m = hull.first

	$pic.set_source_rgba(0, 1, 0, 1)
	$pic.set_line_width(5)

		$pic.new_sub_path
		$pic.arc(m.x, m.y, m.r, 0, Math::PI * 2)
		$pic.stroke

		hull.sort_by!{|el| Math::atan2(el.y - cy, el.x - cx)}
		hull.rotate!(hull.index(m))
		hull << hull[0]
		upper = Array.new
		ttt.clear
		hull.each{|v|
			while !upper.empty? do
				h1 = upper[-1]
				tangent = get_tangents(h1.x, h1.y, h1.tradius, true, v.x, v.y, v.tradius, true)
				if upper.length == 1
					ttt << tangent
					break
				end
				if extended_segment_intersection?(*ttt.last, *tangent)
					upper.pop
					ttt.pop
				else
					ttt << 	tangent
					break
				end
			end
			upper.push(v)
		}
		return upper
	end

# http://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
# http://en.wikipedia.org/wiki/Graham_scan
# On Computing the Convex Hull of (Piecewise) Curved Objects by Franz Aurenhammer and Bert Jüttler
# www.igi.tugraz.at/auren/psfiles/aj-cchpco-12.pdf
# input is a set of overlapping circles (x, y, r)
# return value is an array containing only the circles which touch the convex hull
# this is a very simple implementation based on graham scan
# The cited paper of Aurenhammer/Jüttler may describes a much better solution and contains some references
#
	def self.circles_touching_convex_hull(vertices)
		fail if vertices.uniq! # there should be no duplicates
		inner = Array.new # first remove fully overlapping ones
		vertices.combination(2){|a, b|
			if (a.x - b.x) ** 2 + (a.y - b.y) ** 2  < (b.r - a.r) ** 2 # d < R - r
				inner << (a.r < b.r ? a : b)
			end
		}
		vertices -= inner
		vertices.sort!{|a, b| a.x != b.x ? a.x <=> b.x : a.y <=> b.y}
		return vertices if vertices.length < 3
		hull = Array.new # Andrew's monotone chain for hull of centers -- first upper hull
		vertices.each{|v|
			while hull.length > 1 do
				x1, y1 = hull[-2].x, hull[-2].y
				x2, y2 = hull[-1].x, hull[-1].y
				if (x2 - x1) * (v.y - y2) < (y2 - y1) * (v.x - x2) 
					hull.pop
				else
					break
				end
			end
			hull.push(v)
		}
		lower_hull = Array.new
		vertices.reverse_each{|v|
			while lower_hull.length > 1 do
				x1, y1 = lower_hull[-2].x, lower_hull[-2].y
				x2, y2 = lower_hull[-1].x, lower_hull[-1].y
				if (x2 - x1) * (v.y - y2) < (y2 - y1) * (v.x - x2) 
					lower_hull.pop
				else
					break
				end
			end
			lower_hull.push(v)
		}
		hull.pop
		lower_hull.pop
		hull += lower_hull
		candidates = vertices - hull # the ones which may touch the hull
		ttt = Array.new # the really touching ones
		hull << hull.first # append first temporary, because we want to cycle around the hull
		candidates.each{|v|
			hull.each_cons(2){|a, b|
				if (v.r - [a.r, b.r].min) ** 2 > self.distance_line_point_squared(a.x, a.y, b.x, b.y, v.x, v.y) # v.r - d > r
					ttt << v
					break
				end
			}
		}
		hull.pop # remove duplicate of first circle again
		hull += ttt
		cx = hull.inject(0){|sum, el| sum + el.x}.fdiv(hull.length)
		cy = hull.inject(0){|sum, el| sum + el.y}.fdiv(hull.length)
		min = hull.min{|a, b| (h = (a.x - a.r <=> b.x - b.r)) != 0 ? h : a.y - a.r <=> b.y - b.r}
		max= hull.max{|a, b| (h = (a.x + a.r <=> b.x + b.r)) != 0 ? h : a.y + a.r <=> b.y + b.r}
		$pic.set_source_rgba(0, 1, 0, 1)
		$pic.set_line_width(5)
		$pic.new_sub_path
		$pic.arc(min.x, min.y, min.r, 0, Math::PI * 2)
		$pic.stroke
		$pic.set_source_rgba(0, 0, 1, 1)
		$pic.set_line_width(5)
		$pic.new_sub_path
		$pic.arc(max.x, max.y, max.r, 0, Math::PI * 2)
		$pic.stroke
		hull.sort_by!{|el| Math::atan2(el.y - cy, el.x - cx)}
		hull.rotate!(hull.index(min))
		hull << hull[0] # do full cycle
		lower_hull.clear # store final result here
		ttt.clear # the tangents
		hull.each{|v|
			while !lower_hull.empty? do
				h1 = lower_hull[-1]
				tangent = get_tangents(h1.x, h1.y, h1.tradius, true, v.x, v.y, v.tradius, true)
				if lower_hull.length == 1
					ttt << tangent
					break
				end
				x1, y1, x2, y2 = ttt.last
				x3, y3, x4, y4 = tangent
				if (lower_hull.last != max) && ((x2 - x1) * (y4 - y3) < (y2 - y1) * (x4 - x3))
					lower_hull.pop
					ttt.pop
				else
					ttt << 	tangent
					break
				end
			end
			lower_hull.push(v)
		}
		lower_hull.pop # last == first
		return lower_hull
	end
end



=begin
	require 'benchmark'
	v =Array.new
	100000.times{v << Vertex.new(rand(100).to_f, rand(100).to_f, rand(50).to_f)}


		

Benchmark.bmbm do |x|
  x.report("sort1") {v.sort{|a, b| [a.x, a.y] <=> [b.x, b.y]}}
  x.report("sort2") {v.sort{|a, b| a.x != b.x ? a.x <=> b.x : a.y <=> b.y}}
end

	exit
=end

=begin
	require 'benchmark'
	v =Array.new
	100000.times{v << Vertex.new(rand(100).to_f, rand(100).to_f, rand(50).to_f)}


N = 1000000		

Benchmark.bmbm do |x|
	a,b,c,d,e,f = rand(100).to_f, rand(100).to_f, rand(100).to_f, rand(100).to_f, rand(100).to_f, rand(100).to_f,
  x.report("d1") {N.times{distance_line_point_squared(a,b,c,d,e,f)}}
  x.report("d2") {N.times{unused_distance_line_segment_point_squared(a,b,c,d,e,f)}}
end

	exit
=end










	Board_Size = 800
	Circ_Size = 80
	Circles = 9
	RM::init_seed

	$image = Cairo::ImageSurface.new(Board_Size, Board_Size)
	$pic = Canvas::Pic.new($image)

	v = Array.new
	Circles.times{
		v << Geometry::Vertex.new(Circ_Size + rand(Board_Size - 2 * Circ_Size), Circ_Size + rand(Board_Size - 2 * Circ_Size), rand(Circ_Size) + 10)
	}
#v << Geometry::Vertex.new(500, 500, 250)
	$pic.set_source_rgba(0.8, 0.8, 0.8, 1)
	$pic.paint
	$pic.set_source_rgba(0, 0, 0, 1)
	$pic.set_line_width(1)
	v.each{|el|
		$pic.new_sub_path
		$pic.arc(el.x, el.y, el.r, 0, Math::PI * 2)
		$pic.stroke
	}

	v = Geometry::circles_touching_convex_hull(v)
	$pic.set_source_rgba(1, 0, 0, 0.5)
	$pic.set_line_width(3)
	v.each{|el|
		$pic.new_sub_path
		$pic.arc(el.x, el.y, el.r, 0, Math::PI * 2)
		$pic.stroke
	}

	$pic.set_source_rgba(0, 0, 0, 1)
	$pic.set_line_width(1)

	v << v.first
	v.each_cons(2){|a, b|
		x1, y1, x2, y2 = Geometry::get_tangents(a.x, a.y, a.tradius, true, b.x, b.y, b.tradius, true)
		$pic.move_to(x1, y1)
		$pic.line_to(x2, y2)
		$pic.stroke
	}



	$image.write_to_png('CH.png')





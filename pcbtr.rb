require_relative 'pcb'
require_relative 'layer-assignment'
require_relative 'router'
require_relative 'routing_styles'

		
b = PCB::Board.new
b.read_pcb_file("t.pcb")
b.generate_rtree




b.prepair
#b.inspect
#b.draw



#puts '---------'
b.netlists.each{|nl|
	nl.nets.each{|n|
		puts n.name
		n.connects.each{|c|
			puts c.full_name
			puts b.clusters[c.full_name].x
		}

	}
}
puts "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR"
puts b.x1, b.y1, b.x2, b.y2
#exit

b.rescue_vias = b.gen_rescue_vias_position.to_a
#	puts '???w', x,y
#}
#exit
hst = Routing_Styles.new
hst.read_csv('styles.csv')

l = LA::Assignment.new(2, 1000, 1000, b.x1, b.y1, b.x2, b.y2, b.rt, b)
l.rstyles = hst
b.netlists.each{|nl|
	nl.nets.each{|n|
		n.connects.each{|c|
			cluster = b.clusters[c.full_name]
			if cluster.on_solder_side && cluster.on_component_side
				lr = 0..1
			elsif cluster.on_solder_side
				lr = 0..0
			elsif cluster.on_component_side
				lr = 1..1
			else
				fail
			end
			puts c.full_name, n.name, cluster.x, cluster.y, lr
			t = LA::Terminal.new(c.full_name, n.name, n.style, cluster.x, cluster.y, lr)
			#puts 'style', n.style
			l.add_input_terminal(t)
		}
	}
}
#fail
l.gen_input_2nets
#l.draw_input_2nets
l.gen_output_2nets
l.gen_all_segment_list
l.find_intersecting_segmensts
l.gen_PPDC
l.init_output_2nets



l.route_output_2nets_first_time
l.route_output_2nets_optimize
l.fix_paths
l.get_vias
l.draw_paths
#l.draw_input_2nets

l.save_picture



hash = Hash.new


rs = RM::init_seed


(0..1).each{|layer|

r = RBR::Router.new(b.x1, b.y1, b.x2, b.y2)


#r.insert_rescue_vias(b.rescue_vias)

r.rstyles = hst
#r.generate_test_vertices
#begin
=begin
b.clusters.each_value{|c|
	c.pins.each{|p|
	#puts c.mx + p.rx, c.my + p.ry
	r.insert_pcb_vertex(c.name, c.mx + p.rx, c.my + p.ry)
	}

	c.pads.each{|p|
		if !p.onsolder
			p.vpins.each{|vp|
			puts vp
			puts 'izt', vp.rx, vp.ry
			if hash[[c.mx + vp.rx, c.my + vp.ry]] == nil
			r.insert_pcb_vertex(c.name, c.mx + vp.rx, c.my + vp.ry)
			hash[[c.mx + vp.rx, c.my + vp.ry]] = 1
			end
			}
		end
	}
}
=end
b.clusters.each_value{|c|
		if (c.on_solder_side && layer == 0) || (c.on_component_side && layer == 1)

#c.convex_pin_hull.each{|vp|
#			if hash[[c.mx + vp.rx, c.my + vp.ry]] == nil
#			r.insert_pcb_vertex(c.name, c.mx + vp.rx, c.my + vp.ry)
#			hash[[c.mx + vp.rx, c.my + vp.ry]] = 1
#			end
#}
r.insert_cluster(c)



end
}
r.test_cluster




l.output_2nets.each{|el|
	el.path.each{|s|
	if s.layer == layer

		r.insert_pcb_vertex(nil, s.x1, s.y1, el.via_thickness, el.via_clearance)
		r.insert_pcb_vertex(nil, s.x2, s.y2, el.via_thickness, el.via_clearance)
puts   'ert', s.x1, s.y1, s.x2, s.y2
end
	}
}


#r.insert_pcb_border

#end


r.finish_init


hnl = Array.new
l.output_2nets.each{|el|

#puts 'stil', el.i2n.t1.style
fail unless  el.i2n.t1.style == el.i2n.t2.style



	el.path.each{|s|
	if s.layer == layer
	hnl << [s.x1, s.y1, s.x2, s.y2, el.i2n.t1.style]
		#r.generate_netlist(l)
	end
	}
}
r.generate_netlist(hnl)
r.sort_netlist





#r.filename = 'pic' + rs.to_s.rjust(3, "0") + '.png'
r.filename = 'pic' + layer.to_s + '.png'
r.draw_vertices

col = ([1, 0, 0].permutation(3).to_a + [1, 1, 0].permutation(3).to_a).uniq - [[1, 0, 0]]
r.set_color(1, 0, 0, 0.7)
r.set_line_width(1400)
#(5..9).each{|i|
###(4..8).each{|i|
rtry = Array.new
r.netlist.each_index{|i|
r.set_color(*col[(i+1) % 5], 0.4)
if !r.route(i)
	rtry << i
end
}


=begin

rtry.each{|i|

r.netlist[i].pri *= 0.7
}

rtry.clear

r.sort_netlist





r.netlist.each_index{|i|
r.set_color(*col[(i+1) % 5], 0.4)
if !r.route(i, 0)
	rtry << i
end
}

rtry.each{|i|

r.netlist[i].pri *= 0.7
}

rtry.clear

r.sort_netlist




r.netlist.each_index{|i|
#[0,1,4,5,6,7,8].each{|i|
#next if i == 7
#(2..2).each{|i|
r.set_color(*col[(i+1) % 5], 0.4)
if !r.route(i)
	rtry << i
end
}
=end


rtry.each{|i|
r.set_color(*col[(i+1) % 5], 0.4)
if !r.route(i, 3)
end
}



r.sort_attached_nets

r.prepare_steps
r.nubly
r.prepare_steps

r.sort_attached_nets
#r.nobly
r.prepare_steps


r.nubly
r.prepare_steps

r.sort_attached_nets
#r.nobly
r.prepare_steps






r.nubly(true)
#r.nubly(true)
#r.prepare_steps

r.sort_attached_nets
#r.nobly
#r.nubly(true)
r.prepare_steps


r.nubly(true)
#r.nubly(true)
#r.prepare_steps

r.sort_attached_nets
#r.nobly
#r.nubly(true)
r.prepare_steps
#=begin
###r.nubly(true)
#r.nubly(true)
#r.prepare_steps

r.sort_attached_nets
#r.nobly
#r.nubly(true)
r.prepare_steps

#=end




#r.nubly


#r.sort_attached_nets

#r.prepare_steps

#r.nubly


#r.sort_attached_nets

#r.prepare_steps



#r.nobly
#r.sort_attached_nets

#r.prepare_steps
#r.nobly


#r.nubly



r.draw_routes(layer)
r.flag_vertices
r.save_picture


} # (0..1).each

puts $glob






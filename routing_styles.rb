require 'csv'

class Style
	attr_accessor :name
	attr_accessor :trace_width
	attr_accessor :trace_width_unit # keep users desired size unit
	attr_accessor :trace_clearance
	attr_accessor :trace_clearance_unit
	attr_accessor :color
	attr_accessor :via_diameter
	attr_accessor :via_diameter_unit
	attr_accessor :via_clearance
	attr_accessor :via_clearance_unit
	attr_accessor :via_drill_diameter
	attr_accessor :via_drill_diameter_unit
	attr_accessor :via_mask_diameter
	attr_accessor :via_mask_diameter_unit
	def initialize(name)
		@name = name
		@trace_width_unit = ''
		@trace_clearance_unit = ''
		@via_diameter_unit = ''
		@via_clearance_unit = ''
		@via_mask_diameter_unit = ''
		@via_drill_diameter_unit = ''
		@color = 'blue'
	end
end

# we use 0.01 mil (cmil) as internal size units -- that is current default for PCB file format
Inch2nm = 25.4e6
Mil2nm = 25.4e3
Cmil2nm = 25.4e1 

# nm, um, mm to 0.01 mil
Nm2cmil = 1e0 / Cmil2nm
Um2cmil = 1e3 / Cmil2nm
Mm2cmil = 1e6 / Cmil2nm

Cmil2cmil = 1
Mil2cmil = 100

To_cmil_Hash = {'' => Cmil2cmil, 'mil' => Mil2cmil, 'nm' => Nm2cmil, 'um' => Um2cmil, 'mm' => Mm2cmil}
Dim = To_cmil_Hash.keys #- ['']
Num_Dat = %w[trace_width trace_clearance via_diameter via_clearance via_drill_diameter via_mask_diameter]
Str_Dat = %w[color]

class Routing_Styles
	attr_accessor :hash
	def initialize
		@default = Style.new('default')
		@default.trace_width = 1000
		@default.trace_clearance = 1000
		@default.via_diameter = 3000
		@default.via_drill_diameter = 2000
		@default.via_clearance = 1000
		@default.via_mask_diameter = 0

		@hash = Hash.new
		@hash['default'] = @default
	end

	def [](name)
		@hash[name] || @hash['default']
	end

	def read_csv(filename)
		begin # file not found?
			CSV.foreach(filename, {:headers => true, :skip_blanks => true}) do |row|
				unless (name = row['name']).empty? || /^\s*[#$]/.match(name)
					unless s = @hash[name]
						s = @hash['default'].dup
						s.name = name
					end
					Num_Dat.each{|el|
						i = 0
						begin # catch exception from to_float conversion
							([el].product(Dim)).each{|n, unit|
								h = (unit.empty? ? '' : '_')
								if v = row[n + h + unit]
									i += 1
									v, vu = /(.*\d)\s*_?\s*([mnul]{,3})\s*$/.match(v)[1..2] # '8_mil' ==> '8', 'mil'
									vu = unit if vu.empty?
									if h = To_cmil_Hash[vu]
										v = Float(v) * h
									else
										raise 'invalid size unit'
									end
									s.instance_variable_set("@#{n}", v.round)
									s.instance_variable_set("@#{n}_unit", vu)
									if n == 'trace_clearance'
										s.instance_variable_set("@via_clearance", v.round)
										s.instance_variable_set("@via_clearance_unit", vu)
									end
								end
							}
						rescue Exception => e
							print "Line #{$.}, "
							puts e.message
						end
						puts "warning, line #{$.}: multiple definitions of #{n}" if i > 1
					}
					Str_Dat.each{|n|
						if v = row[n]
							s.instance_variable_set("@#{n}", v)
						end
					}
					@hash[name] = s
				end
			end
		rescue Exception => e
			puts e.message
		end
	end
end

if __FILE__ == $0

s = Routing_Styles.new
s.read_csv('styles.csv')

s.hash.each_pair{|k, v|
puts k
puts v.trace_clearance
puts v.trace_clearance_unit
puts v.trace_width
puts v.trace_width_unit
puts v.color
}
puts s['power'].via_diameter
puts s['DGND'].trace_width
end

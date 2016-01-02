require 'csv'

class Style
	attr_accessor :name
	attr_accessor :width
	attr_accessor :clearance
end

# we use 0.01 mil (cmil) as internal size units -- that is current default for PCB file format
Inch2nm = 25.4e6
Mil2nm = 25.4e3
CMil2nm = 25.4e1 

# nm, um, mm to 0.01 mil
Nm2cmil = 1e0 / CMil2nm
Um2cmil = 1e3 / CMil2nm
Mm2cmil = 1e6 / CMil2nm

Cmil2cmil = 1
Mil2cmil = 100

Dim_Hash = {'' => Cmil2cmil, 'mil' => Mil2cmil, 'nm' => Nm2cmil, 'um' => Um2cmil, 'mm' => Mm2cmil}
Dim = Dim_Hash.keys - ['']
#Dim = %w[nm um mm mil]
Num_Dat = %w[width clearance]

class Styles
	attr_accessor :hash
	def initialize
		@default = Style.new
		@default.name = 'default'
		@default.width = 1000
		@default.clearance = 1000
		@hash = Hash.new
		@hash['default'] = @default
	end

	# "width_mil" ==> "with", Dim_Hash("mil")
	def unit(n)
		a, b = n.split('_')
		return a, Dim_Hash[b]
	end

	# 10_mil ==> 10 * Dim_Hash["mil"], 1000 ==> 1000 * scale # scale is default unit for that column
	def apply_unit(v, col_scale)
		a, b = /(.*\d)\s*_?\s*([mnul]{,3})\s*$/.match(v)[1..2]
		#raise "invalid size unit" unless b
		s = Dim_Hash[b] || col_scale
		return Float(a) * s # may raise exception if a is not a valid float
	end

	def read_csv(filename)
		#CSV.foreach(filename, {:headers => true, :header_converters => :symbol}) do |row|
		CSV.foreach(filename, {:headers => true}) do |row|
			unless (name = row['name']).empty?
				unless s = @hash[name]
					s = @hash['default'].dup
					s.name = name
				end
				Num_Dat.each{|el|
					i = 0
					begin # catch exception from to_float conversion
						([el].product(Dim)).map(&:join).each{|n|
							if h = row[n]
								i += 1
								n, scale = unit(n)
								h = apply_unit(h, scale)
							end
							#s.instance_variable_set("@#{n}", h * unit)
							s.instance_variable_set("@x", 1)
						}
					rescue Exception => e
						print "Line #{$.}, "
  					puts e.message
						#puts 'invalid'
					end
					if i == 1
							#s.instance_variable_set("@#{n}", h * unit)
					elsif i > 1

					end

				}
				@hash[name] = s
			end
		end
	end
end

s = Styles.new
s.read_csv('styles.csv')

p s.hash

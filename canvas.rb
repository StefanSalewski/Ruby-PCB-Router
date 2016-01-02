require 'cairo'

module Canvas 
class Pic < Cairo::Context
	def initialize(image)
	  super(image)
		set_source_rgba(1,1,1,1)
		paint
	end
end
end
#image = Cairo::ImageSurface.new(600, 600)
#pic = Canvas::Pic.new(image)
#image.write_to_png('cairo_picture.png')


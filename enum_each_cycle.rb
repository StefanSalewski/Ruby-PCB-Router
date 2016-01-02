
#https://www.ruby-forum.com/topic/106362
module Enumerable
	def each_cycle(window, start = 0)
    return to_enum(__method__, window, start) unless block_given?
		wrap_start = []
		cache = []
		each_with_index do |e,i|
			cache << e
			if i >= start + (window - 1)
				yield cache[start, window]
				cache.shift
			else
				wrap_start << e
			end
		end
		wrap_start.each do |e|
			cache << e
			yield cache[start, window]
			cache.shift
		end
		self
	end
end

#http://stackoverflow.com/questions/7183360/ruby-methods-that-either-yield-or-return-enumerator
class Array
  def double(&block)
    return to_enum(:double) unless block_given?
    each { |x| yield 2*x }
  end
end

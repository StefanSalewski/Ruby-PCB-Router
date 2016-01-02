#!/usr/bin/env ruby
require 'benchmark'
require 'set'

N = 1e6.to_i

class A
  attr_accessor :a, :b, :c
	def d
		@d
	end
	def d=(x)
		@d = x
	end

end



def t00a
  m = A.new
	m.a = 34
	N.times{|er|
		m.b = m.a + 1
		m.c = m.a + m.b
	}
end

def t00b
  m = A.new
	m.a = 34.0
	N.times{|er|
		m.b = m.a + 1.0
		m.c = m.a + m.b
	}
end

def t9
  m = A.new
	N.times{|er|
		m.a = 1
		m.b = m.a
	}
end

def t10
  m = A.new
	N.times{|er|
		m.d = 1
		m.b = m.d
	}
end

def t11
  m = A.new
	b = d = 0
	N.times{|er|
		d = 1
		b = d
	}
end


def t00c
  m = A.new
	m.a = 34
	N.times{|er|
		m.b = m.a + 1
		m.c = Math::atan2(m.a, m.b)
	}
end

def t00d
  m = A.new
	m.a = 34.0
	N.times{|er|
		m.b = m.a + 1.0
		m.c = Math::atan2(m.a, m.b)
	}
end


def ta
  m = (0..99).to_a
	i = 1234
	N.times{|er|
		b = m.include?(i)
	}
end

def t7
  a = true
	b = false
	N.times{|er|
		c = (a != b)
	}
end

def t8
  a = true
	b = false
	N.times{|er|
		c = (a ^ b)
	}
end




def ts
  m = (0..99).to_a.to_set
	i = 7
	N.times{|er|
		b = m.include?(i)
	}
end


def t1
	m = A.new
	m.a = 34
	N.times{|er|
		m.b = m.a + 1
		m.c = m.a + m.b
	}
end

def t2
	m = A.new
	x = 34
	y = 0
	z = 0
	N.times{|er|
		y = x + 1
		z = x + y
	}
end

def t3
	m = A.new
	x = 34
	#y = 0
	#z = 0
	N.times{|er|
		y = x + 1
		z = x + y
	}
end

def t4
	m = A.new
	x = 1.3
	y = 0
	z = 0
	N.times{|er|
		y = Math::sin(x)
		z = Math::sqrt(y)
	}
end

def t5
	a = (0..99).to_a.shuffle!
	a[50] = 50
	y = nil
	N.times{|er|
		y = a.index(50)
	}
end

def t12
	a = 1.2
	b = 12.7
	return a, b
end

def t13
	a = 1.2
	b = 12.7
	[a, b]
end

def t12a
	a = b = 0
	N.times{
		a, b = t12 
	}
end

def t13a
	a = b = 0
	N.times{
		a, b = t13 
	}
end


def xxx1
	a = b = c = d = 2.0
	o = 1.0
	N.times{
		a, b, c, d = [a, b, c, d].map{|x| x + o}
	}
end

def xxx2
	a = b = c = d = 2.0
	o = 1.0
	N.times{
		a, b, c, d = a + o, b + o, c + o, d + o
	}
end


def incl1
	a = [0, 1, 2, 3, 4]
	b = false
	N.times{
		b = a.include?(2)
	}
end

def incl2
	a = {0 => true, 1 => true, 2 => true, 3 => true, 4 => true}
	b = false
	N.times{
		b = a.include?(2)
	}
end

def incl3
	a = [0, 1, 2, 3, 4]
	b = false
	N.times{
		b = 1 == 2
	}
end

def incl4
	a = Set.new [0, 1, 2, 3, 4]

	b = false
	N.times{
		b = a.include?(2)
	}
end

def xt0
	a = 1
	b = 2
	c = 0
	N.times{
		c = a + b 
	}
end

def xt1
	a = 1
	b = 2
	#c = 0
	N.times{
		c = a + b 
	}
end

def xt2
	a = 1.1
	b = 2.2
	c = 0
	N.times{
		c = a + b 
	}
end

def xt3
	a = [1,2,3,4,5]
	b = 0
	N.times{
		c = a.sort 
	}
end

def xt4
	a = [1,2,3,4,5]
	b = 0
	N.times{
		c = a.include?(3)
	}
end

def xt5
	a = [1,2,3,4,5]
	b = [3,4,5,6,7]
	c = 0
	N.times{
		c = a & b
	}
end

class AAA
XXX = 3
attr_accessor :a
end

def aaa
	b = AAA.new
	c = AAA::XXX
	N.times{
		c = b.a
	}
end

def bbb
	b = 5
	c = 0
	N.times{
		c = b * 24
	}
end

def ccc
	b = 5
	c = 0
	N.times{
		c = b * (2 * 3 * 4)
	}
end

def ddd
	b = 5
	c = 0
	N.times{
		c = b * 2 * 3 * 4
	}
end




def xt6
	a = [1,2]
	b = 0
	w = v = 2
	N.times{
		b = a.permutation.map{|el| (el.unshift(w).push(v))}.map{|el| s = 0; el.each_cons(2){|a, b| s += [a,b].max}; s}.max
		#b = a.permutation.each{|el| el.each_cons(2).map{|a, b| (a - b).abs}.inject(0){|sum, el| sum + el}}.max
	}
end

l = ->(i, j){ i > j ? i : j}


Benchmark.bmbm do |x|
  x.report("bbb") {bbb}
  x.report("ccc") {ccc}
  x.report("ddd") {ddd}
#  x.report("array") {N.times{k = [2,3].max}}
#  x.report("lambda") {N.times{k = l.call(2,3)}}
#  x.report("xt1") {xt1}
#  x.report("xt2") {xt2}
#  x.report("xt3") {xt3}
#  x.report("xt4") {xt4}
#  x.report("xt5") {xt5}
#  x.report("xt6") {xt6}
#  x.report("t00c") {t00c}
#  x.report("t00c") {t00d}
#  x.report("t5") {t5}
#  x.report("t4") {t4}
end


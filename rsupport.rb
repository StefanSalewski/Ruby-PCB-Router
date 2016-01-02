module RouterSupport
	class Hash_with_ordered_array_index < Hash
		def [](a, b)
			a.object_id < b.object_id ? super([a, b]) : super([b, a])
		end
		def []=(a, b, c)
			a.object_id < b.object_id ? super([a, b], c) : super([b, a], c)
		end
		def include?(a, b)
			a.object_id < b.object_id ? super([a, b]) : super([b, a])
		end
	end

=begin
http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
int get_line_intersection(float p0_x, float p0_y, float p1_x, float p1_y, 
    float p2_x, float p2_y, float p3_x, float p3_y, float *i_x, float *i_y)
{
    float s02_x, s02_y, s10_x, s10_y, s32_x, s32_y, s_numer, t_numer, denom, t;
    s10_x = p1_x - p0_x;
    s10_y = p1_y - p0_y;
    s32_x = p3_x - p2_x;
    s32_y = p3_y - p2_y;

    denom = s10_x * s32_y - s32_x * s10_y;
    if (denom == 0)
        return 0; // Collinear
    bool denomPositive = denom > 0;

    s02_x = p0_x - p2_x;
    s02_y = p0_y - p2_y;
    s_numer = s10_x * s02_y - s10_y * s02_x;
    if ((s_numer < 0) == denomPositive)
        return 0; // No collision

    t_numer = s32_x * s02_y - s32_y * s02_x;
    if ((t_numer < 0) == denomPositive)
        return 0; // No collision

    if (((s_numer > denom) == denomPositive) || ((t_numer > denom) == denomPositive))
        return 0; // No collision
    // Collision detected
    t = t_numer / denom;
    if (i_x != NULL)
        *i_x = p0_x + (t * s10_x);
    if (i_y != NULL)
        *i_y = p0_y + (t * s10_y);

    return 1;
}
=end

=begin
	def segment_segment_intersection(p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y)
		s10_x = p1_x - p0_x
		s10_y = p1_y - p0_y
		s32_x = p3_x - p2_x
		s32_y = p3_y - p2_y
		denom = s10_x * s32_y - s32_x * s10_y
		return nil if denom == 0 # collinear
		denomPositive = denom > 0
		s02_x = p0_x - p2_x
		s02_y = p0_y - p2_y
		s_numer = s10_x * s02_y - s10_y * s02_x
		return nil if (s_numer < 0) == denomPositive #  No collision
		t_numer = s32_x * s02_y - s32_y * s02_x
		return nil if (t_numer < 0) == denomPositive #  No collision
		return nil if ((s_numer > denom) == denomPositive) || ((t_numer > denom) == denomPositive) #  No collision
		# Collision detected
		t = t_numer / denom
		return p0_x + t * s10_x, p0_y + t * s10_y
	end

	# boolean result
	def segment_segment_intersection?(p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y)
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
=end

end


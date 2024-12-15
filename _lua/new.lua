local sum = function(p1, p2, p3)
	return p1 + p2 + p3
end

local t = {
	a = "Hello",
	[4] = 4,
	["wooahhh"] = function(xyz, abc)
		sum(xyz, abc, 7)
	end,
	"!",
}

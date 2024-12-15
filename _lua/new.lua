local sum = function(p1, p2, p3)
	return p1 + p2 + p3
end

local t = {
	a = "Hello",
	b = {
		c = 40,
	},
	d = function(self, n, m)
		return {
			self.b.c,
			n,
			m,
			prod = n * m,
		}
	end,
	[4] = 4,
	["wooahhh"] = function(xyz, abc)
		sum(xyz, abc, 7)
	end,
	"!",
}

local b = t.b.c
local h = t[4]
local g = t:d(3, 2).prod
local y = t:d(5, 9)[1]

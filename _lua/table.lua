local sum = function(p1, p2, p3)
	return p1 + p2 + p3
end
local t = {
	a = "Hello",
	[4] = "World",
	c = {
		f = -45.732,
		thing = function(g, h)
			sum(g, h, 17)
		end,
	},
	["str_idx"] = function()
		local z = 5
		local lem = 10 + 4 * z - -6
		return lem
	end,
	"Done",
}

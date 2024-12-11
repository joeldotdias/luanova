local t = {
	a = "Hello",
	[4] = "World",
	c = {
		f = -45.732,
		thing = function(g, h)
			print(g, h)
		end,
	},
	["str_idx"] = function()
		local z = 5
		local lem = 10 + 4 * z - -6
		print("Oh what torture")
		return lem
	end,
}

local t = {
	a = "Hello",
	[4] = "World",
	c = {
		f = 45.732,
		thing = function(g, h)
			print(g, h)
		end,
	},
	["str_idx"] = function()
		local lem = 4
		print("Oh what torture")
		return lem
	end,
}

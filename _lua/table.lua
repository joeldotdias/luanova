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
		print("Oh what torture")
	end,
}

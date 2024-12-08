local str, btr = "Luuaaa", "Buaaa"
print(str)

local function greet(abc, xyz)
	local greeting = "Hello, World!"
	local function team()
		local ben = "ten"
		print(greeting, ben, btr)
	end
	print(greeting, str)
end

greet()

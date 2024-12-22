local a = (19 + (9 - 6) * 2) ^ 2
local b, c = "abc", "Cad"
a, b = 7.4, c
c = "Ben"
print(a + 90)
print(c)
local res = print("Hello, World!")
print("Hello,", "World!")
print(res)

local fn = function(p1)
	print(p1(c))
	return c
end
print(fn)
local passed = fn(function(va)
	return "Function as value"
end)
print(passed)

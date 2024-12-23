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
	local inside = function(p1, p2)
		print(p1 + p2)
	end
	inside(1, 3)
	return c
end
print(fn)
local passed = fn(function(va)
	return "Function as value"
end)
print(passed)
local function no_params()
	print("Local func")
end
no_params()

for i = 1.1, 10, 3.3 do
	print("for loop var", i)
end

local wv = 6
while wv >= 3 do
	print("while loop var", wv)
	wv = wv - 1.5
end

if wv < 0 then
	print("If execed")
elseif wv > 100 then
	print("wv is greater than 100???")
elseif wv then
	print("Else if execed")
else
	print("Else execed")
end

local a, b, c = 7, function(p1, p2)
	local t = {
		a = 19,
	}
	return "Greet"
end, "Hello, World!"

if a < 5 and a ~= 2 then
	local n = c
elseif a == 3 then
	a = 40
elseif a == 1 then
	a = 10
else
	a = 17
end

c, a = "Bye", 19 + (9 - 4 * (3 - 1)) * c

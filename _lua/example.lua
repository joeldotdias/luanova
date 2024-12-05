local a = 10
local b = 5

function add(x, y)
	return x + y
end

function check_number(num)
	if num > 0 then
		print(num .. " is positive")
	elseif num < 0 then
		print(num .. " is negative")
	else
		print(num .. " is zero")
	end
end

print("Sum of a and b is: " .. add(a, b))
check_number(a)
check_number(b)
check_number(-3)

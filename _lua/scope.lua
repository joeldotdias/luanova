local str = "Hello"
print(str)

function GlobalFunc()
	print("This global")
end

local function localFunc(greeting)
	local recv = "world"
	print(greeting .. recv)
end

local M = {
	mb = function()
		return "hello"
	end,
}

localFunc(str)

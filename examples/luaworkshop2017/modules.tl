local mymath = require "mymath"
--[[
mymath:{
"deg":(number) -> (number),
"rad":(number) -> (number),
"pow":(number, number) -> (number)
}
]]

print(mymath.deg(1))
print(mymath.rad(1))
print(mymath.pow(2, "foo"))

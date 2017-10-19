require("circle")

local c1 = class(circle.Circle).new(10, 20, 5)
c1:move(50, 50)
print(c1:getPosition())

local c2 = class(circle.Circle).new(100, 200, 10)
print(c2.radius)
c2.radius = 5
print(c2.radius)


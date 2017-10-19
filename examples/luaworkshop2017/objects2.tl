require("circle")

local gray = class(circle.Color).new(128, 128, 128)
local cb = class(circle.ColoredCircle).new(10, 20, 5, gray)

local c = cb:getColor()
print(cb.color.r, cb.color.g, cb.color.b)
c.r = 255
print(cb.color.r, cb.color.g, cb.color.b)

function cb:move (x:number, y:number)
  print("exit")
  os.exit(1)
end

cb:move(50, 50)
os.exit(0)

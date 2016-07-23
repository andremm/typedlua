local Shape = { x = 0.0, y = 0.0 }

const function Shape:new (x:number, y:number):self
  local s = setmetatable({}, { __index = self })
  s.x = x
  s.y = y
  return s
end

const function Shape:move (dx:number, dy:number):()
  self.x = self.x + dx
  self.y = self.y + dy
end

local s = Shape:new(3,2)
s:move(20,25)
print(s + 10) -- to supress generation of .lua

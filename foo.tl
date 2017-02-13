local o = { x = 1 }
const function o:set_x (x:number) self.x = x end
local x = o.set_x
x(5, 1) -- first parameter has 'any' type
x(5)    -- force error


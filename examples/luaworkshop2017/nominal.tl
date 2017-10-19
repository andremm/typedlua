class Nominal1
  x: boolean
  constructor new (x:boolean) self.x = x end
end

typedef Structural1 = { "x":boolean }

class Nominal2
  x: boolean
  constructor new (x:boolean) self.x = x end
end

typedef Structural2 = { "x":boolean }

local function get_x_n (n:Nominal2):boolean
  return n.x
end

local function get_x_s (s:Structural2):boolean
  return s.x
end

print(get_x_n(Nominal1.new(false)))
print(get_x_s({ x = true }))

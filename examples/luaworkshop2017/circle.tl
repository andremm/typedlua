class Circle
  x: number
  y: number
  radius: number

  constructor new (x:number, y:number, radius:number)
    self.x = x
	self.y = y
    self.radius = radius
  end

  method move (x:number, y:number)
    self.x = self.x + x
    self.y = self.y + y
  end

  method getPosition ():(number, number)
    return self.x, self.y
  end
end

class Color
  r: number
  g: number
  b: number

  constructor new (r: number, g: number, b:number)
    self.r = r
    self.g = g
    self.b = b
  end
end

class ColoredCircle extends Circle
  color: Color

  constructor new (x: number, y: number, radius: number, color: Color)
    super.new(x, y, radius)
    self.color = color
  end

  method getColor ():(Color)
    return self.color
  end
end

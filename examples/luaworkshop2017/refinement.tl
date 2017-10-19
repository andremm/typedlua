local typealias Color =
  { "r": number, "g": number, "b": number }

local typealias Circle =
  { "x": number, "y": number, "radius": number, "color": Color }

local gray:Color = { r = 128, g = 128, b = 128, a = 255 }

local circle = {}
circle.x = 10
circle.y = 20
circle.radius = 5
circle.color = gray

-- circle has type Circle


local love = require "love"

typealias Color = {"r":number, "g":number, "b":number}
typealias Circle = {"x":number, "y":number, "radius":number, "color":Color}

love.window.setTitle("Gray Moon")
local width, height = love.window.getMode()
local gray:Color = { r = 128, g = 128, b = 128 }
local circle:Circle = { x = width / 2, y = height / 2,
                        radius = 10, color = gray, }
function love.update (dt:number)
  if love.keyboard.isDown("escape") then
    love.event.quit()
  end
end
function love.draw ()
  love.graphics.setColor(circle.color.r, circle.color.g, circle.color.b)
  love.graphics.circle("fill", circle.x, circle.y, circle.radius)
end

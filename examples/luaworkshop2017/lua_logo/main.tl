local love = require "love"
require "lua_logo.circle"

love.window.setTitle("Lua logo")
local width, height = love.window.getMode()

local blue = class(lua_logo.circle.Color).new(0, 0, 128)
local white = class(lua_logo.circle.Color).new(255, 255, 255)
local earth = class(lua_logo.circle.ColoredCircle).new(width / 2, height / 2, 200, blue)
local hole = class(lua_logo.circle.ColoredCircle).new(width / 2 + 90, height / 2 - 90, 60, white)
local moon = class(lua_logo.circle.ColoredCircle).new(width - 200, 90, 60, blue)

function love.update (dt:number)
  if love.keyboard.isDown("escape") then
    love.event.quit()
  end
end

function love.draw ()
  earth:draw()
  hole:draw()
  moon:draw()
end

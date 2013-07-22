--[[
This file implements the available types in Typed Lua
]]

local types = {}

function types.Any ()
  return "any"
end

function types.Boolean ()
  return "boolean"
end

function types.Nil ()
  return "nil"
end

function types.Number ()
  return "number"
end

function types.String ()
  return "string"
end

function types.isAny (t)
  if t == "any" then return true end
  return false
end

function types.isNumber (t)
  if t == "number" or types.isAny(t) then return true end
  return false
end

function types.isString (t)
  if t == "string" or types.isAny(t) then return true end
  return false
end

function types.tostring (t)
  return tostring(t)
end

return types

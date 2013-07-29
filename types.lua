--[[
This file implements the available types in Typed Lua

Constant = nil | false | true | <numeral> | <literal>

Base = "any" | "boolean" | "number" | "string"

Type = TypeConstant Constant
     | TypeBase Base
     | TypeFunction Type Type
     | TypeTuple Type Type
     | TypeStar Type
     | TypeVoid
]]

local types = {}

function types.Any ()
  return { tag = "TypeBase", [1] = "any" }
end

function types.Boolean ()
  return { tag = "TypeBase", [1] = "boolean" }
end

function types.False ()
  return { tag = "TypeConstant", [1] = false }
end

function types.Function (arg, ret)
  return { tag = "TypeFunction", [1] = arg, [2] = ret }
end

function types.Literal (l)
  return { tag = "TypeConstant", [1] = l }
end

function types.Nil ()
  return { tag = "TypeConstant" --[[, [1] = nil]] }
end

function types.Numeral (n)
  return { tag = "TypeConstant", [1] = n }
end

function types.Number ()
  return { tag = "TypeBase", [1] = "number" }
end

function types.Star (t)
  return { tag = "TypeStar", [1] = t }
end

function types.String ()
  return { tag = "TypeBase", [1] = "string" }
end

function types.True ()
  return { tag = "TypeConstant", [1] = true }
end

function types.Tuple (t1, t2)
  return { tag = "TypeTuple", [1] = t1, [2] = t2 }
end

function types.Void ()
  return { tag = "TypeVoid" }
end

function types.isAny (t)
  if t.tag == "TypeBase" and t[1] == "any" then
    return true
  end
  return false
end

function types.isBaseBoolean (t)
  if t.tag == "TypeBase" and t[1] == "boolean" or
     types.isAny(t) then
    return true
  end
  return false
end

function types.isBoolean (t)
  if t.tag == "TypeBase" and t[1] == "boolean" or
     types.isFalse(t) or types.isTrue(t) or
     types.isAny(t) then
    return true
  end
  return false
end

function types.isFalse (t)
  if t.tag == "TypeConstant" and t[1] == false then
    return true
  end
  return false
end

function types.isInteger (t)
  if t.tag == "TypeConstant" then
    local x = t[1]
    if type(x) == "number" and
       math.floor(x) == x then
      return true
    end
  end
  return false
end

function types.isLiteral (t)
  if t.tag == "TypeConstant" and type(t[1]) == "string" then
    return true
  end
  return false
end

function types.isNil (t)
  if t.tag == "TypeConstant" and t[1] == nil then
    return true
  end
  return false
end

function types.isNumber (t)
  if t.tag == "TypeBase" and t[1] == "number" or
     t.tag == "TypeConstant" and type(t[1]) == "number" or
     types.isInteger(t) or
     types.isAny(t) then
    return true
  end
  return false
end

function types.isString (t)
  if t.tag == "TypeBase" and t[1] == "string" or
     types.isLiteral(t) or
     types.isAny(t) then
    return true
  end
  return false
end

function types.isTrue (t)
  if t.tag == "TypeConstant" and t[1] == true then
    return true
  end
  return false
end

local function type2str (t)
  local tag = t.tag
  if tag == "TypeConstant" then
    return type(t[1])
  elseif tag == "TypeBase" then
    return t[1]
  else
    error("expecting a type, but got a " .. tag)
  end
end

function types.str2type (s)
  if s == "any" then
    return types.Any()
  elseif s == "boolean" then
    return types.Boolean()
  elseif s == "nil" then
    return types.Nil()
  elseif s == "number" then
    return types.Number()
  elseif s == "string" then
    return types.String()
  end
  return nil
end

function types.tostring (t)
  return type2str(t)
end

return types

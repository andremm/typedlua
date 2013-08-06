--[[
This file implements the available types in Typed Lua

Constant = nil | false | true | <integer> | <double> | <word>

Base = "any" | "boolean" | "number" | "string"

Type = TypeConstant Constant
     | TypeBase Base
     | TypeUnion Type Type
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

function types.Union (t1, t2)
  return { tag = "TypeUnion", [1] = t1, [2] = t2 }
end

function types.Void ()
  return { tag = "TypeVoid" }
end

function types.Word (w)
  return { tag = "TypeConstant", [1] = w }
end

function types.isAny (t)
  if t.tag == "TypeBase" and t[1] == "any" then
    return true
  end
  return false
end

function types.isBoolean (t)
  if t.tag == "TypeBase" and t[1] == "boolean" or
     types.isFalse(t) or types.isTrue(t) then
    return true
  end
  return false
end

function types.isDouble (t)
  if t.tag == "TypeConstant" then
    local x = t[1]
    if type(x) == "number" and
       math.floor(x) ~= x then
      return true
    end
  end
  return false
end

function types.isFalse (t)
  if t.tag == "TypeConstant" and t[1] == false then
    return true
  end
  return false
end

function types.isFunction (t)
  if t.tag == "TypeFunction" then
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

function types.isNil (t)
  if t.tag == "TypeConstant" and t[1] == nil then
    return true
  end
  return false
end

function types.isNumber (t)
  if t.tag == "TypeBase" and t[1] == "number" or
     types.isInteger(t) or types.isDouble(t) then
    return true
  end
  return false
end

function types.isString (t)
  if t.tag == "TypeBase" and t[1] == "string" or
     types.isWord(t) then
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

function types.isVoid (t)
  if t.tag == "TypeVoid" then
    return true
  end
  return false
end

function types.isWord (t)
  if t.tag == "TypeConstant" and type(t[1]) == "string" then
    return true
  end
  return false
end

function types.Equal (t1, t2)
  if types.isNil(t1) then
    return types.isNil(t2)
  elseif types.isBoolean(t1) then
    return types.isBoolean(t2)
  elseif types.isNumber(t1) then
    return types.isNumber(t2)
  elseif types.isString(t1) then
    return types.isString(t2)
  elseif types.isVoid(t1) then
    return types.isVoid(t2)
  elseif types.isAny(t1) then
    return types.isAny(t2)
  elseif types.isFunction(t1) and types.isFunction(t2) then
    return types.Equal(t1[1], t2[1]) and types.Equal(t1[2], t2[2])
  end
  return false
end

local function type2str (t)
  local tag = t.tag
  if tag == "TypeConstant" then
    return type(t[1])
  elseif tag == "TypeBase" then
    return t[1]
  elseif tag == "TypeFunction" then
    return "(" .. type2str(t[1]) .. " -> " .. type2str(t[2]) .. ")"
  elseif tag == "TypeTuple" then
    return "(" .. type2str(t[1]) .. " x " .. type2str(t[2]) .. ")"
  elseif tag == "TypeStar" then
    return type2str(t[1]) .. "*"
  elseif tag == "TypeUnion" then
    return "(" .. type2str(t[1]) .. " U " .. type2str(t[2]) .. ")"
  elseif tag == "TypeVoid" then
    return "void"
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

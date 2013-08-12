--[[
This file implements the available types in Typed Lua

Constant = nil | false | true | <integer> | <double> | <word>

Basic = "boolean" | "number" | "string"

Type = TypeConstant Constant
     | TypeBasic Basic
     | TypeObject
     | TypeAny
]]

local types = {}

-- functions that create constant types

function types.Constant (c)
  return { tag = "TypeConstant", [1] = c }
end

function types.Nil ()
  return types.Constant(nil)
end

function types.False ()
  return types.Constant(false)
end

function types.True ()
  return types.Constant(true)
end

function types.ConstantNumber (n)
  assert(type(n) == "number")
  return types.Constant(n)
end

function types.ConstantString (s)
  assert(type(s) == "string")
  return types.Constant(s)
end

-- functions that create basic types

function types.Basic (t)
  return { tag = "TypeBasic", [1] = t }
end

function types.Boolean ()
  return types.Basic("boolean")
end

function types.Number ()
  return types.Basic("number")
end

function types.String ()
  return types.Basic("string")
end

-- function that create object type

function types.Object ()
  return { tag = "TypeObject" }
end

-- function that create any type

function types.Any ()
  return { tag = "TypeAny" }
end

-- functions that check constant types

function types.isConstant (t)
  if t.tag == "TypeConstant" then
    return true
  end
  return false
end

function types.isNil (t)
  if types.isConstant(t) and t[1] == nil then
    return true
  end
  return false
end

function types.isFalse (t)
  if types.isConstant(t) and t[1] == false then
    return true
  end
  return false
end

function types.isTrue (t)
  if types.isConstant(t) and t[1] == true then
    return true
  end
  return false
end

function types.isConstantNumber (t)
  if types.isConstant(t) and type(t[1]) == "number" then
    return true
  end
  return false
end

function types.isDouble (t)
  if types.isConstantNumber(t) and math.floor(t[1]) ~= t[1] then
    return true
  end
  return false
end

function types.isInteger (t)
  if types.isConstantNumber(t) and math.floor(t[1]) == t[1] then
    return true
  end
  return false
end

function types.isConstantString (t)
  if types.isConstant(t) and type(t[1]) == "string" then
    return true
  end
  return false
end

-- functions that check basic types

function types.isBasic (t)
  if t.tag == "TypeBasic" then
    return true
  end
  return false
end

function types.isBoolean (t)
  if types.isBasic(t) and t[1] == "boolean" then
    return true
  end
  return false
end

function types.isNumber (t)
  if types.isBasic(t) and t[1] == "number" then
    return true
  end
  return false
end

function types.isString (t)
  if types.isBasic(t) and t[1] == "string" then
    return true
  end
  return false
end

-- function that check object type

function types.isObject (t)
  if t.tag == "TypeObject" then
    return true
  end
  return false
end

-- function that check any type

function types.isAny (t)
  if t.tag == "TypeAny" then
    return true
  end
  return false
end

-- subtyping

function types.subtype (t1, t2)
  if types.isObject(t2) then -- S-OBJECT
    return true
  elseif types.isAny(t1) and types.isAny(t2) then -- S-ANY
    return true
  elseif types.isConstant(t1) and types.isConstant(t2) then -- S-CONSTANT
    if types.isNil(t1) and types.isNil(t2) then
      return true
    elseif types.isFalse(t1) and types.isFalse(t2) then
      return true
    elseif types.isTrue(t1) and types.isTrue(t2) then
      return true
    elseif types.isDouble(t1) and types.isDouble(t2) then
      return true
    elseif types.isInteger(t1) and types.isInteger(t2) then
      return true
    elseif types.isConstantString(t1) and types.isConstantString(t2) then
      return true
    end
  elseif types.isConstant(t1) and types.isBasic(t2) then
    if types.isFalse(t1) and types.isBoolean(t2) then -- S-FALSE
      return true
    elseif types.isTrue(t1) and types.isBoolean(t2) then -- S-TRUE
      return true
    elseif types.isDouble(t1) and types.isNumber(t2) then -- S-DOUBLE
      return true
    elseif types.isInteger(t1) and types.isNumber(t2) then -- S-INTEGER
      return true
    elseif types.isConstantString(t1) and types.isString(t2) then -- S-STRING
      return true
    end
  elseif types.isBasic(t1) and types.isBasic(t2) then -- S-BASIC
    if types.isBoolean(t1) and types.isBoolean(t2) then
      return true
    elseif types.isNumber(t1) and types.isNumber(t2) then
      return true
    elseif types.isString(t1) and types.isString(t2) then
      return true
    end
  end
  return false
end

return types

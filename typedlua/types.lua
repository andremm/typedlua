--[[
This file implements the available types in Typed Lua

type:
  `Literal{ literal }
  | `Base{ base }
  | `Any

literal: nil | false | true | <number> | <string>

base: 'boolean' | 'number' | 'string'
]]

local types = {}

-- literal types

function types.Literal (l)
  return { tag = "Literal", [1] = l }
end

types.Nil = types.Literal(nil)

types.False = types.Literal(false)

types.True = types.Literal(true)

function types.isLiteral (t)
  return t.tag == "Literal"
end

function types.isNil (t)
  return types.isLiteral(t) and t[1] == nil
end

function types.isFalse (t)
  return types.isLiteral(t) and t[1] == false
end

function types.isTrue (t)
  return types.isLiteral(t) and t[1] == true
end

function types.isLiteralNumber (t)
  return types.isLiteral(t) and type(t[1]) == "number"
end

function types.isLiteralString (t)
  return types.isLiteral(t) and type(t[1]) == "string"
end

-- base types

types.Boolean = { tag = "Base", [1] = "boolean" }

types.Number = { tag = "Base", [1] = "number" }

types.String = { tag = "Base", [1] = "string" }

function types.isBase (t)
  return t.tag == "Base"
end

function types.isBoolean (t)
  return types.isBase(t) and t[1] == "boolean"
end

function types.isNumber (t)
  return types.isBase(t) and t[1] == "number"
end

function types.isString (t)
  return types.isBase(t) and t[1] == "string"
end

-- dynamic type

types.Any = { tag = "Any" }

function types.isAny (t)
  return t.tag == "Any"
end

-- subtyping

local function subtype_literal (t1, t2)
  if types.isLiteral(t1) and types.isLiteral(t2) then
    return t1[1] == t2[1]
  elseif types.isLiteral(t1) and types.isBase(t2) then
    if types.isBoolean(t2) then
      return types.isFalse(t1) or types.isTrue(t1)
    elseif types.isNumber(t2) then
      return types.isLiteralNumber(t1)
    elseif types.isString(t2) then
      return types.isLiteralString(t1)
    else
      return false
    end
  else
    return false
  end
end

local function subtype_base (t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function subtype_any (t1, t2)
  return types.isAny(t1) and types.isAny(t2)
end

function types.subtype (t1, t2)
  return subtype_any(t1, t2) or
         subtype_literal(t1, t2) or
         subtype_base(t1, t2)
end

-- consistent-subtyping

local function consistent_subtype_literal (t1, t2)
  if types.isLiteral(t1) and types.isLiteral(t2) then
    return t1[1] == t2[1]
  elseif types.isLiteral(t1) and types.isBase(t2) then
    if types.isBoolean(t2) then
      return types.isFalse(t1) or types.isTrue(t1)
    elseif types.isNumber(t2) then
      return types.isLiteralNumber(t1)
    elseif types.isString(t2) then
      return types.isLiteralString(t1)
    else
      return false
    end
  else
    return false
  end
end

local function consistent_subtype_base (t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function consistent_subtype_any (t1, t2)
  return types.isAny(t1) or types.isAny(t2)
end

function types.consistent_subtype (t1, t2)
  return consistent_subtype_any(t1, t2) or
         consistent_subtype_literal(t1, t2) or
         consistent_subtype_base(t1, t2)
end

-- tostring

local function type2str (t)
  if types.isLiteral(t) then
    return tostring(t[1])
  elseif types.isBase(t) then
    return t[1]
  elseif types.isAny(t) then
    return "any"
  else
    error("expecting type but got " .. t.tag)
  end
end

function types.tostring (t)
  return type2str(t)
end

return types

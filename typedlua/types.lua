--[[
This file implements the available types in Typed Lua

Constant = nil | false | true | <integer> | <double> | <word>

Base = "boolean" | "number" | "string"

Type = TypeConstant Constant
     | TypeBase Base
     | TypeObject
     | TypeAny
     | TypeName String
     | TypeUndefined
     | TypeFunction Type Type
     | TypeTuple [Type]
     | TypeUnion Type Type
     | TypeVarArg Type
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

function types.Base (t)
  return { tag = "TypeBase", [1] = t }
end

function types.Boolean ()
  return types.Base("boolean")
end

function types.Number ()
  return types.Base("number")
end

function types.String ()
  return types.Base("string")
end

-- function that create object type

function types.Object ()
  return { tag = "TypeObject" }
end

-- function that create any type

function types.Any ()
  return { tag = "TypeAny" }
end

-- function that create type name

function types.Name (name)
  return { tag = "TypeName", [1] = name }
end

-- function that create type undefined

function types.Undefined ()
  return { tag = "TypeUndefined" }
end

-- function that create function type

function types.Function (args, ret)
  return { tag = "TypeFunction", [1] = args, [2] = ret }
end

-- function that create tuple type

function types.Tuple (list)
  return { tag = "TypeTuple", [1] = list }
end

-- function that create union type

function types.Union (t1, t2)
  return { tag = "TypeUnion", [1] = t1, [2] = t2 }
end

-- function that create vararg type

function types.VarArg (t)
  return { tag = "TypeVarArg", [1] = t }
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

function types.isBase (t)
  if t.tag == "TypeBase" then
    return true
  end
  return false
end

function types.isBoolean (t)
  if types.isBase(t) and t[1] == "boolean" then
    return true
  end
  return false
end

function types.isNumber (t)
  if types.isBase(t) and t[1] == "number" then
    return true
  end
  return false
end

function types.isString (t)
  if types.isBase(t) and t[1] == "string" then
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

-- function that check name type

function types.isName (t)
  if t.tag == "TypeName" then
    return true
  end
  return false
end

-- function that check undefined type

function types.isUndefined (t)
  if t.tag == "TypeUndefined" then
    return true
  end
  return false
end

-- function that check function type

function types.isFunction (t)
  if t.tag == "TypeFunction" then
    return true
  end
  return false
end

-- function that check tuple type

function types.isTuple (t)
  if t.tag == "TypeTuple" then
    return true
  end
  return false
end

-- function that check union type

function types.isUnion (t)
  if t.tag == "TypeUnion" then
    return true
  end
  return false
end

-- function that check vararg type

function types.isVarArg (t)
  if t.tag == "TypeVarArg" then
    return true
  end
  return false
end

function types.typeofVarArg (t)
  if t.tag == "TypeVarArg" then
    return t[1]
  end
  return nil
end

-- subtyping

function types.subtype (t1, t2)
  if types.isObject(t2) then -- S-OBJECT
    if types.isTuple(t1) or types.isVarArg(t1) then
      return false
    end
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
    elseif types.isInteger(t1) and types.isDouble(t2) then -- S-INTEGER1
      return true
    elseif types.isConstantString(t1) and types.isConstantString(t2) then
      return true
    end
  elseif types.isConstant(t1) and types.isBase(t2) then
    if types.isFalse(t1) and types.isBoolean(t2) then -- S-FALSE
      return true
    elseif types.isTrue(t1) and types.isBoolean(t2) then -- S-TRUE
      return true
    elseif types.isDouble(t1) and types.isNumber(t2) then -- S-DOUBLE
      return true
    elseif types.isInteger(t1) and types.isNumber(t2) then -- S-INTEGER2
      return true
    elseif types.isConstantString(t1) and types.isString(t2) then -- S-STRING
      return true
    end
  elseif types.isBase(t1) and types.isBase(t2) then -- S-BASIC
    if types.isBoolean(t1) and types.isBoolean(t2) then
      return true
    elseif types.isNumber(t1) and types.isNumber(t2) then
      return true
    elseif types.isString(t1) and types.isString(t2) then
      return true
    end
  elseif not types.isUnion(t1) and types.isUnion(t2) then -- S-UNION1 and S-UNION2
    return types.subtype(t1, t2[1]) or types.subtype(t1, t2[2])
  elseif types.isUnion(t1) then -- S-UNION3
    return types.subtype(t1[1], t2) and types.subtype(t1[2], t2)
  elseif types.isFunction(t1) and types.isFunction(t2) then
    return types.subtype(t2[1], t1[1]) and types.subtype(t1[2], t2[2])
  elseif types.isTuple(t1) and types.isTuple(t2) then
    local s1, s2 = t1[1], t2[1]
    if #s1 == #s2 then
      for k, v in ipairs(s1) do
        if not types.subtype(s1[k], s2[k]) then
          return false
        end
      end
      return true
    elseif #s1 < #s2 then
      if types.isVarArg(s1[#s1]) then
        local i = 1
        while i < #s1 do
          if not types.subtype(s1[i], s2[i]) then
            return false
          end
          i = i + 1
        end
        local j = i
        while j <= #s2 do
          if not types.subtype(s1[i], s2[j]) then
            return false
          end
          j = j + 1
        end
        return true
      end
      return false
    elseif #s1 > #s2 then
      if types.isVarArg(s2[#s2]) then
        local i = 1
        while i < #s2 do
          if not types.subtype(s1[i], s2[i]) then
            return false
          end
          i = i + 1
        end
        local j = i
        while j <= #s1 do
          if not types.subtype(s1[j], s2[i]) then
            return false
          end
          j = j + 1
        end
        return true
      end
      return false
    end
  elseif types.isVarArg(t1) and types.isVarArg(t2) then
    if types.isNil(t1[1]) then
      return true
    end
    return types.subtype(t1[1], t2[1])
  elseif types.isVarArg(t1) then
    if types.isTuple(t2) then
      for k, v in ipairs(t2[1]) do
        if not types.subtype(t1, v) then
          return false
        end
      end
      return true
    end
    return types.subtype(t1[1], t2)
  elseif types.isVarArg(t2) then
    if types.isTuple(t1) then
      for k, v in ipairs(t1[1]) do
        if not types.subtype(v, t2) then
          return false
        end
      end
      return true
    end
    return types.subtype(t1, t2[1])
  end
  return false
end

-- consistent-subtyping

function types.csubtype (t1, t2)
  if types.isObject(t2) then
    if types.isTuple(t1) or types.isVarArg(t1) then
      return false
    end
    return true
  elseif types.isAny(t1) then
    if types.isVarArg(t2) then
      return types.csubtype(t1, t2[1])
    elseif types.isTuple(t2) then
      return false
    end
    return true
  elseif types.isAny(t2) then
    if types.isVarArg(t1) then
      return types.csubtype(t1[1], t2)
    elseif types.isTuple(t1) then
      return false
    end
    return true
  elseif types.isConstant(t1) and types.isConstant(t2) then
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
    elseif types.isInteger(t1) and types.isDouble(t2) then
      return true
    elseif types.isConstantString(t1) and types.isConstantString(t2) then
      return true
    end
  elseif types.isConstant(t1) and types.isBase(t2) then
    if types.isFalse(t1) and types.isBoolean(t2) then
      return true
    elseif types.isTrue(t1) and types.isBoolean(t2) then
      return true
    elseif types.isDouble(t1) and types.isNumber(t2) then
      return true
    elseif types.isInteger(t1) and types.isNumber(t2) then
      return true
    elseif types.isConstantString(t1) and types.isString(t2) then
      return true
    end
  elseif types.isBase(t1) and types.isBase(t2) then
    if types.isBoolean(t1) and types.isBoolean(t2) then
      return true
    elseif types.isNumber(t1) and types.isNumber(t2) then
      return true
    elseif types.isString(t1) and types.isString(t2) then
      return true
    end
  elseif not types.isUnion(t1) and types.isUnion(t2) then
    return types.csubtype(t1, t2[1]) or types.csubtype(t1, t2[2])
  elseif types.isUnion(t1) then
    return types.csubtype(t1[1], t2) and types.csubtype(t1[2], t2)
  elseif types.isFunction(t1) and types.isFunction(t2) then
    return types.csubtype(t2[1], t1[1]) and types.csubtype(t1[2], t2[2])
  elseif types.isTuple(t1) and types.isTuple(t2) then
    local s1, s2 = t1[1], t2[1]
    if #s1 == #s2 then
      for k, v in ipairs(s1) do
        if not types.csubtype(s1[k], s2[k]) then
          return false
        end
      end
      return true
    elseif #s1 < #s2 then
      if types.isVarArg(s1[#s1]) then
        local i = 1
        while i < #s1 do
          if not types.csubtype(s1[i], s2[i]) then
            return false
          end
          i = i + 1
        end
        local j = i
        while j <= #s2 do
          if not types.csubtype(s1[i], s2[j]) then
            return false
          end
          j = j + 1
        end
        return true
      end
      return false
    elseif #s1 > #s2 then
      if types.isVarArg(s2[#s2]) then
        local i = 1
        while i < #s2 do
          if not types.csubtype(s1[i], s2[i]) then
            return false
          end
          i = i + 1
        end
        local j = i
        while j <= #s1 do
          if not types.csubtype(s1[j], s2[i]) then
            return false
          end
          j = j + 1
        end
        return true
      end
      return false
    end
  elseif types.isVarArg(t1) and types.isVarArg(t2) then
    if types.isNil(t1[1]) then
      return true
    end
    return types.csubtype(t1[1], t2[1])
  elseif types.isVarArg(t1) then
    if types.isTuple(t2) then
      for k, v in ipairs(t2[1]) do
        if not types.csubtype(t1, v) then
          return false
        end
      end
      return true
    end
    return types.csubtype(t1[1], t2)
  elseif types.isVarArg(t2) then
    if types.isTuple(t1) then
      for k, v in ipairs(t1[1]) do
        if not types.csubtype(v, t2) then
          return false
        end
      end
      return true
    end
    return types.csubtype(t1, t2[1])
  end
  return false
end

function types.supertypeof (t)
  if types.isFalse(t) or types.isTrue(t) then
    return types.Boolean()
  elseif types.isDouble(t) or types.isInteger(t) then
    return types.Number()
  elseif types.isConstantString(t) then
    return types.String()
  end
  return t
end

local function type2str (t)
  if types.isConstant(t) then
    return type(t[1])
  elseif types.isBase(t) or types.isName(t) then
    return t[1]
  elseif types.isObject(t) then
    return "object"
  elseif types.isAny(t) then
    return "any"
  elseif types.isUndefined(t) then
    return "?"
  elseif types.isFunction(t) then
    return type2str(t[1]) .. " -> " .. type2str(t[2])
  elseif types.isTuple(t) then
    local l = {}
    for k,v in ipairs(t[1]) do
      l[k] = type2str(v)
    end
    local v = table.concat(l, ", ")
    if #l > 1 then
      return "(" .. v .. ")"
    else
      return v
    end
  elseif types.isUnion(t) then
    return "(" .. type2str(t[1]) .. " | " .. type2str(t[2]) .. ")"
  elseif types.isVarArg(t) then
    return type2str(t[1]) .. "*"
  else
    error("expecting type but got " .. t.tag)
  end
end

function types.tostring (t)
  return type2str(t)
end

return types

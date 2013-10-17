--[[
This file implements the available types in Typed Lua

Constant = nil | false | true | <integer> | <double> | <word>

Base = "boolean" | "number" | "string"

Type = TypeValue
     | TypeConstant Constant
     | TypeBase Base
     | TypeAny
     | TypeName String
     | TypeUndefined
     | TypeUnion Type Type
     | TypeIntersection Type Type
     | TypeFunction TypeList TypeList
     | TypeRecord [(Type,Type)]
     | TypeVarArg Type

TypeList = TypeTuple [Type]
]]

local types = {}

-- value type

types.Value = { tag = "TypeValue" }

function types.isValue (t)
  if t.tag == "TypeValue" then
    return true
  end
  return false
end

-- constant types

function types.Constant (c)
  return { tag = "TypeConstant", [1] = c }
end

types.Nil = { tag = "TypeConstant", --[[ [1] = nil ]] }

types.False = { tag = "TypeConstant", [1] = false }

types.True = { tag = "TypeConstant", [1] = true }

function types.ConstantNumber (n)
  assert(type(n) == "number")
  return types.Constant(n)
end

function types.ConstantString (s)
  assert(type(s) == "string")
  return types.Constant(s)
end

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

function types.isConstantString (t)
  if types.isConstant(t) and type(t[1]) == "string" then
    return true
  end
  return false
end

-- base types

types.Boolean = { tag = "TypeBase", [1] = "boolean" }

types.Number = { tag = "TypeBase", [1] = "number" }

types.String = { tag = "TypeBase", [1] = "string" }

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

-- any type

types.Any = { tag = "TypeAny" }

function types.isAny (t)
  if t.tag == "TypeAny" then
    return true
  end
  return false
end

-- name type

function types.Name (name)
  return { tag = "TypeName", [1] = name }
end

function types.isName (t)
  if t.tag == "TypeName" then
    return true
  end
  return false
end

-- undefined type

function types.Undefined ()
  return { tag = "TypeUndefined" }
end

function types.isUndefined (t)
  if t.tag == "TypeUndefined" then
    return true
  end
  return false
end

-- union type

function types.Union (t1, t2)
  return { tag = "TypeUnion", [1] = t1, [2] = t2 }
end

function types.isUnion (t)
  if t.tag == "TypeUnion" then
    return true
  end
  return false
end

-- union type

function types.Intersection (t1, t2)
  return { tag = "TypeIntersection", [1] = t1, [2] = t2 }
end

function types.isIntersection (t)
  if t.tag == "TypeIntersection" then
    return true
  end
  return false
end

-- function type

function types.Function (t1, t2)
  return { tag = "TypeFunction", [1] = t1, [2] = t2 }
end

function types.isFunction (t)
  if t.tag == "TypeFunction" then
    return true
  end
  return false
end

-- vararg type

function types.VarArg (t)
  return { tag = "TypeVarArg", [1] = t }
end

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

-- tuple type

function types.Tuple (list)
  list.tag = "TypeList"
  return { tag = "TypeTuple", [1] = list }
end

function types.isTuple (t)
  if t.tag == "TypeTuple" then
    return true
  end
  return false
end

-- void type

types.Void = { tag = "TypeTuple", [1] = { tag = "TypeList" } }

function types.isVoid (t)
  if t.tag == "TypeTuple" and #t[1] == 0 then
    return true
  end
  return false
end

-- record type

function types.Record (tuple_list)
  return { tag = "TypeRecord", [1] = tuple_list }
end

function types.isRecord (t)
  if t.tag == "TypeRecord" then
    return true
  end
  return false
end

-- subtyping

local function subtype_value (t1, t2)
  return types.isValue(t2)
end

local function subtype_any (t1, t2)
  if types.isAny(t1) and types.isAny(t2) then
    return true
  end
  return false
end

local function subtype_constant_reflexive (t1, t2)
  return t1[1] == t2[1]
end

local function subtype_constant_base (t1, t2)
  if types.isFalse(t1) and types.isBoolean(t2) then
    return true
  elseif types.isTrue(t1) and types.isBoolean(t2) then
    return true
  elseif types.isConstantNumber(t1) and types.isNumber(t2) then
    return true
  elseif types.isConstantString(t1) and types.isString(t2) then
    return true
  end
  return false
end

local function subtype_constant (t1, t2)
  if types.isConstant(t1) and types.isConstant(t2) then
    return subtype_constant_reflexive(t1, t2)
  elseif types.isConstant(t1) and types.isBase(t2) then
    return subtype_constant_base(t1, t2)
  end
  return false
end

local function subtype_base (t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
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

local function subtype_union (t1, t2)
  if not types.isUnion(t1) and types.isUnion(t2) then
    return types.subtype(t1, t2[1]) or types.subtype(t1, t2[2])
  elseif types.isUnion(t1) then
    return types.subtype(t1[1], t2) and types.subtype(t1[2], t2)
  end
  return false
end

local function subtype_intersection (t1, t2)
  if types.isIntersection(t1) then
    return types.subtype(t1[1], t2) or
           types.subtype(t1[2], t2)
  elseif types.isIntersection(t2) then
    return types.subtype(t1, t2[1]) and types.subtype(t1, t2[2])
  end
  return false
end

local function subtype_function (t1, t2)
  if types.isFunction(t1) and types.isFunction(t2) then
    return types.subtype2(t2[1], t1[1]) and types.subtype2(t1[2], t2[2])
  end
  return false
end

local function subtype_record_field (l, f, t)
  for k, v in ipairs(t[1]) do
    if types.subtype(l, v[1]) and types.subtype(f, v[2]) then
      return true
    end
  end
  return false
end

local function subtype_record (t1, t2)
  if types.isRecord(t1) and types.isRecord(t2) then
    for k, v in ipairs(t1[1]) do
      if not subtype_record_field(v[1], v[2], t2) then
        return false
      end
    end
    return true
  end
  return false
end

local function subtype_vararg (t1, t2)
  if types.isVoid(t1) and types.isVarArg(t2) then
    return true
  elseif types.isVarArg(t1) and types.isVarArg(t2) then
    return types.subtype(types.typeofVarArg(t1), types.typeofVarArg(t2))
  elseif types.isVarArg(t1) and not types.isVarArg(t2) then
    local t = types.typeofVarArg(t1)
    return types.subtype(types.Union(t, types.Nil), t2)
  elseif not types.isVarArg(t1) and types.isVarArg(t2) then
    local t = types.typeofVarArg(t2)
    return types.subtype(t1, t)
  end
  return false
end

function types.subtype2 (t1, t2)
  if types.isTuple(t1) and types.isTuple(t2) then
    local s1, s2 = t1[1], t2[1]
    if types.isVoid(t1) and types.isVoid(t2) then
      return true
    elseif types.isVoid(t1) and not types.isVoid(t2) then
      if #s2 == 1 and types.isVarArg(s2[1]) then
        return true
      end
      return false
    elseif not types.isVoid(t1) and types.isVoid(t2) then
      return false
    elseif #s1 == #s2 then
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
  end
  return false
end

local function csubtype_any (t1, t2)
  if types.isAny(t1) and types.isAny(t2) then
    return true
  elseif types.isAny(t1) then
    return not types.isVarArg(t2) and not types.isTuple(t2)
  elseif types.isAny(t2) then
    return not types.isVarArg(t1) and not types.isTuple(t2)
  end
  return false
end

local function csubtype_constant_reflexive (t1, t2)
  return t1[1] == t2[1]
end

local function csubtype_constant_base (t1, t2)
  if types.isFalse(t1) and types.isBoolean(t2) then
    return true
  elseif types.isTrue(t1) and types.isBoolean(t2) then
    return true
  elseif types.isConstantNumber(t1) and types.isNumber(t2) then
    return true
  elseif types.isConstantString(t1) and types.isString(t2) then
    return true
  end
  return false
end

local function csubtype_constant (t1, t2)
  if types.isConstant(t1) and types.isConstant(t2) then
    return csubtype_constant_reflexive(t1, t2)
  elseif types.isConstant(t1) and types.isBase(t2) then
    return csubtype_constant_base(t1, t2)
  end
  return false
end

local function csubtype_base (t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
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

local function csubtype_union (t1, t2)
  if not types.isUnion(t1) and types.isUnion(t2) then
    return types.csubtype(t1, t2[1]) or types.csubtype(t1, t2[2])
  elseif types.isUnion(t1) then
    return types.csubtype(t1[1], t2) and types.csubtype(t1[2], t2)
  end
  return false
end

local function csubtype_intersection (t1, t2)
  if types.isIntersection(t1) then
    return types.csubtype(t1[1], t2) or
           types.csubtype(t1[2], t2)
  elseif types.isIntersection(t2) then
    return types.csubtype(t1, t2[1]) and types.csubtype(t1, t2[2])
  end
  return false
end

local function csubtype_function (t1, t2)
  if types.isFunction(t1) and types.isFunction(t2) then
    return types.csubtype2(t2[1], t1[1]) and types.csubtype2(t1[2], t2[2])
  end
  return false
end

local function csubtype_record_field (l, f, t)
  for k, v in ipairs(t[1]) do
    if types.csubtype(l, v[1]) and types.csubtype(f, v[2]) then
      return true
    end
  end
  return false
end

local function csubtype_record (t1, t2)
  if types.isRecord(t1) and types.isRecord(t2) then
    for k, v in ipairs(t1[1]) do
      if not csubtype_record_field(v[1], v[2], t2) then
        return false
      end
    end
    return true
  end
  return false
end

local function csubtype_vararg (t1, t2)
  if types.isVoid(t1) and types.isVarArg(t2) then
    return true
  elseif types.isVarArg(t1) and types.isVarArg(t2) then
    return types.csubtype(types.typeofVarArg(t1), types.typeofVarArg(t2))
  elseif types.isVarArg(t1) and not types.isVarArg(t2) then
    local t = types.typeofVarArg(t1)
    return types.csubtype(types.Union(t, types.Nil), t2)
  elseif not types.isVarArg(t1) and types.isVarArg(t2) then
    local t = types.typeofVarArg(t2)
    return types.csubtype(t1, t)
  end
  return false
end

function types.csubtype2 (t1, t2)
  if types.isTuple(t1) and types.isTuple(t2) then
    local s1, s2 = t1[1], t2[1]
    if types.isVoid(t1) and types.isVoid(t2) then
      return true
    elseif types.isVoid(t1) and not types.isVoid(t2) then
      if #s2 == 1 and types.isVarArg(s2[1]) then
        return true
      end
      return false
    elseif not types.isVoid(t1) and types.isVoid(t2) then
      return false
    elseif #s1 == #s2 then
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
  end
  return false
end

function types.subtype (t1, t2)
  return subtype_value(t1, t2) or
         subtype_any(t1, t2) or
         subtype_constant(t1, t2) or
         subtype_base(t1, t2) or
         subtype_union(t1, t2) or
         subtype_intersection(t1, t2) or
         subtype_function(t1, t2) or
         subtype_record(t1, t2) or
         subtype_vararg(t1, t2) or
         types.subtype2(t1, t2)
end

function types.csubtype (t1, t2)
  return csubtype_any(t1, t2) or
         csubtype_constant(t1, t2) or
         csubtype_base(t1, t2) or
         csubtype_union(t1, t2) or
         csubtype_intersection(t1, t2) or
         csubtype_function(t1, t2) or
         csubtype_record(t1, t2) or
         csubtype_vararg(t1, t2) or
         types.csubtype2(t1, t2)
end

function types.supertypeof (t)
  if types.isFalse(t) or types.isTrue(t) then
    return types.Boolean
  elseif types.isConstantNumber(t) then
    return types.Number
  elseif types.isConstantString(t) then
    return types.String
  end
  return t
end

local function type2str (t)
  if types.isConstant(t) then
    return tostring(t[1])
  elseif types.isBase(t) or types.isName(t) then
    return t[1]
  elseif types.isAny(t) then
    return "any"
  elseif types.isUndefined(t) then
    return "?"
  elseif types.isFunction(t) then
    return type2str(t[1]) .. " -> " .. type2str(t[2])
  elseif types.isUnion(t) then
    return "(" .. type2str(t[1]) .. " | " .. type2str(t[2]) .. ")"
  elseif types.isIntersection(t) then
    return "(" .. type2str(t[1]) .. " ^ " .. type2str(t[2]) .. ")"
  elseif types.isRecord(t) then
    local l = {}
    for k, v in ipairs(t[1]) do
      l[k] = type2str(v[1]) .. ":" .. type2str(v[2])
    end
    return "{" .. table.concat(l, ", ") .. "}"
  elseif types.isVarArg(t) then
    return type2str(t[1]) .. "*"
  elseif types.isTuple(t) then
    local l = {}
    for k,v in ipairs(t[1]) do
      l[k] = type2str(v)
    end
    local v = table.concat(l, ", ")
    if #l == 0 then
      return "()"
    elseif #l == 1 then
      return v
    else
      return "(" .. v .. ")"
    end
  else
    error("expecting type but got " .. t.tag)
  end
end

function types.tostring (t)
  return type2str(t)
end

return types

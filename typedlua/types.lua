--[[
This file implements the available types in Typed Lua

type:
  `Literal{ literal }
  | `Base{ base }
  | `Value
  | `Any
  | `Union{ type type }
  | `Function{ typelist typelist }

literal: false | true | <number> | <string>

base: 'nil' | 'boolean' | 'number' | 'string'

typelist: `Tuple{ type* `Vararg{ type } }
]]

local types = {}

-- literal types

function types.Literal (l)
  return { tag = "Literal", [1] = l }
end

types.False = types.Literal(false)

types.True = types.Literal(true)

function types.isLiteral (t)
  return t.tag == "Literal"
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

types.Nil = { tag = "Base", [1] = "nil" }

types.Boolean = { tag = "Base", [1] = "boolean" }

types.Number = { tag = "Base", [1] = "number" }

types.String = { tag = "Base", [1] = "string" }

function types.isBase (t)
  return t.tag == "Base"
end

function types.isNil (t)
  return types.isBase(t) and t[1] == "nil"
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

-- top type

types.Value = { tag = "Value" }

function types.isValue (t)
  return t.tag == "Value"
end

-- dynamic type

types.Any = { tag = "Any" }

function types.isAny (t)
  return t.tag == "Any"
end

-- union types

local function union_length (t)
  if types.isUnion(t) then
    return union_length(t[1]) + union_length(t[2])
  else
    return 1
  end
end

local function union_member (t1, t2)
  if types.isUnion(t1) and types.isUnion(t2) then
    return union_member(t1[1], t2) and union_member(t1[2], t2)
  elseif not types.isUnion(t1) and types.isUnion(t2) then
    return union_member(t1, t2[1]) or union_member(t1, t2[2])
  else
    return types.equal(t1, t2)
  end
end

function types.Union (t1, t2)
  if union_member(t1, t2) then
    return t2
  elseif union_member(t2, t1) then
    return t1
  elseif types.isUnion(t1) and types.isUnion(t2) then
    return types.Union(t1[2], types.Union(t1[1], t2))
  else
    return { tag = "Union", [1] = t1, [2] = t2 }
  end
end

function types.isUnion (t)
  return t.tag == "Union"
end

function types.isUnionNil (t)
  if types.isUnion(t) then
    return types.isUnionNil(t[1]) or types.isUnionNil(t[2])
  else
    return types.isNil(t)
  end
end

function types.UnionNoNil (t)
  if types.isUnionNil(t) then
    if types.isNil(t[1]) then
      return t[2]
    elseif types.isNil(t[2]) then
      return t[1]
    elseif types.isUnionNil(t[1]) then
      t[1] = types.UnionNoNil(t[1])
      return t
    elseif types.isUnionNil(t[2]) then
      t[2] = types.UnionNoNil(t[2])
      return t
    else
      return t
    end
  else
    return t
  end
end

-- function types

function types.Function (t1, t2)
  return { tag = "Function", [1] = t1, [2] = t2 }
end

function types.isFunction (t)
  return t.tag == "Function"
end

-- equality

local function equal_literal (t1, t2)
  if types.isLiteral(t1) and types.isLiteral(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function equal_base (t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function equal_top (t1, t2)
  return types.isValue(t1) and types.isValue(t2)
end

local function equal_any (t1, t2)
  return types.isAny(t1) and types.isAny(t2)
end

local function equal_union (t1, t2)
  if types.isUnion(t1) and types.isUnion(t2) then
    if union_length(t1) == union_length(t2) then
      return union_member(t1[1], t2) and union_member(t1[2], t2)
    else
      return false
    end
  else
    return false
  end
end

function types.equal (t1, t2)
  return equal_literal(t1, t2) or
         equal_base(t1, t2) or
         equal_top(t1, t2) or
         equal_any(t1, t2) or
         equal_union(t1, t2)
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

local function subtype_top (t1, t2)
  return types.isValue(t2)
end

local function subtype_any (t1, t2)
  return types.isAny(t1) and types.isAny(t2)
end

local function subtype_union (t1, t2)
  if types.isUnion(t1) then
    return types.subtype(t1[1], t2) and types.subtype(t1[2], t2)
  elseif types.isUnion(t2) then
    return types.subtype(t1, t2[1]) or types.subtype(t1, t2[2])
  else
    return false
  end
end

function types.subtype (t1, t2)
  return subtype_literal(t1, t2) or
         subtype_base(t1, t2) or
         subtype_top(t1, t2) or
         subtype_any(t1, t2) or
         subtype_union(t1, t2)
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

local function consistent_subtype_top (t1, t2)
  return types.isValue(t2)
end

local function consistent_subtype_any (t1, t2)
  return types.isAny(t1) or types.isAny(t2)
end

local function consistent_subtype_union (t1, t2)
  if types.isUnion(t1) then
    return types.consistent_subtype(t1[1], t2) and types.consistent_subtype(t1[2], t2)
  elseif types.isUnion(t2) then
    return types.consistent_subtype(t1, t2[1]) or types.consistent_subtype(t1, t2[2])
  else
    return false
  end
end

function types.consistent_subtype (t1, t2)
  return consistent_subtype_literal(t1, t2) or
         consistent_subtype_base(t1, t2) or
         consistent_subtype_top(t1, t2) or
         consistent_subtype_any(t1, t2) or
         consistent_subtype_union(t1, t2)
end

-- supertypeof

function types.supertypeof (t)
  if types.isFalse(t) or types.isTrue(t) then
    return types.Boolean
  elseif types.isLiteralNumber(t) then
    return types.Number
  elseif types.isLiteralString(t) then
    return types.String
  elseif types.isUnion(t) then
    t[1] = types.supertypeof(t[1])
    t[2] = types.supertypeof(t[2])
    return t
  else
    return t
  end
end

-- tostring

local function typelist2str (typelist)
  local l = {}
  local len = #typelist
  for i = 1, len - 1 do
    l[i] = types.tostring(typelist[i])
  end
  l[len] = types.tostring(typelist[len][1]) .. "*"
  return table.concat(l, ", ")
end

local function type2str (t)
  if types.isLiteral(t) then
    return tostring(t[1])
  elseif types.isBase(t) then
    return t[1]
  elseif types.isValue(t) then
    return "value"
  elseif types.isAny(t) then
    return "any"
  elseif types.isUnion(t) then
    return "(" .. type2str(t[1]) .. " | " .. type2str(t[2]) .. ")"
  elseif types.isFunction(t) then
    return "(" .. typelist2str(t[1]) .. ") -> (" .. typelist2str(t[2]) .. ")"
  else
    error("expecting type but got " .. t.tag)
  end
end

function types.tostring (t)
  return type2str(t)
end

return types

--[[
This file implements the available types in Typed Lua

type:
  `Literal{ literal }
  | `Base{ base }
  | `Nil
  | `Value
  | `Any
  | `Union{ type type type* }
  | `Function{ type type }
  | `Table{ { type type }* }
  | `Variable{ <string> }
  | `Unionlist{ type type type* }
  | `Tuple{ type* }
  | `Vararg{ type }

literal: false | true | <number> | <string>

base: 'boolean' | 'number' | 'string'
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

-- nil type

types.Nil = { tag = "Nil" }

function types.isNil (t)
  return t.tag == "Nil"
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

function types.Union (t1, t2)
  if types.isUnion(t1) then
    if not types.isUnion(t2) then
      t1[#t1 + 1] = t2
      return t1
    else
      for k, v in ipairs(t2) do
        t1[#t1 + 1] = v
      end
      return t1
    end
  elseif not types.isUnion(t1) then
    if types.isUnion(t2) then
      t2[#t2 + 1] = t1
      return t2
    else
      return { tag = "Union", [1] = t1, [2] = t2 }
    end
  end
end

function types.isUnion (t)
  return t.tag == "Union"
end

function types.isUnionNil (t)
  if types.isUnion(t) then
    for k, v in ipairs(t) do
      if types.isNil(v) then
        return true
      end
    end
    return false
  else
    return false
  end
end

function types.UnionNoNil (t)
  if types.isUnionNil(t) then
    local n = { tag = "Union" }
    for k, v in ipairs(t) do
      if not types.isNil(v) then
        n[#n + 1] = v
      end
    end
    if #n == 1 then
      return n[1]
    else
      return n
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

-- table types

function types.Table (...)
  return { tag = "Table", ... }
end

function types.isTable (t)
  return t.tag == "Table"
end

-- type variables

function types.Variable (t)
  return { tag = "Variable", [1] = t }
end

function types.isVariable (t)
  return t.tag == "Variable"
end

-- union list

function types.isUnionlist (t)
  return t.tag == "Unionlist"
end

-- tuple types

function types.Tuple (...)
  return { tag = "Tuple", ... }
end

function types.isTuple (t)
  return t.tag == "Tuple"
end

-- vararg types

function types.Vararg (t)
  return { tag = "Vararg", [1] = t }
end

function types.isVararg (t)
  return t.tag == "Vararg"
end

-- subtyping

local function subtype_literal (env, t1, t2)
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

local function subtype_base (env, t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function subtype_nil (env, t1, t2)
  return types.isNil(t1) and types.isNil(t2)
end

local function subtype_top (env, t1, t2)
  return types.isValue(t2)
end

local function subtype_any (env, t1, t2)
  return types.isAny(t1) and types.isAny(t2)
end

local function subtype_union (env, t1, t2)
  if types.isUnion(t1) then
    for k, v in ipairs(t1) do
      if not types.subtype(env, v, t2) then
        return false
      end
    end
    return true
  elseif types.isUnion(t2) then
    for k, v in ipairs(t2) do
      if types.subtype(env, t1, v) then
        return true
      end
    end
    return false
  else
    return false
  end
end

local function subtype_function (env, t1, t2)
  if types.isFunction(t1) and types.isFunction(t2) then
    return types.subtype(env, t2[1], t1[1]) and types.subtype(env, t1[2], t2[2])
  else
    return false
  end
end

local function subtype_table (env, t1, t2)
  if types.isTable(t1) and types.isTable(t2) then
  local m, n = #t1, #t2
  for i = 1, n do
    local subtype = false
    for j = 1, m do
      if types.subtype(env, t2[i][1], t1[j][1]) and
         types.subtype(env, t1[j][2], t2[i][2]) and
         types.subtype(env, t2[i][2], t1[j][2]) then
        subtype = true
        break
      end
    end
    if not subtype then
      return false
    end
  end
  return true
  else
    return false
  end
end

local function get_type (env, t)
  return env[t] or types.Nil
end

local function subtype_variable (env, t1, t2)
  if types.isVariable(t1) and types.isVariable(t2) then
    return types.subtype(env, get_type(env, t1[1]), get_type(env, t2[1]))
  elseif types.isVariable(t1) and not types.isVariable(t2) then
    return types.subtype(env, get_type(env, t1[1]), t2)
  elseif not types.isVariable(t1) and types.isVariable(t2) then
    return types.subtype(env, t1, get_type(env, t2[1]))
  else
    return false
  end
end

local function subtype_tuple (env, t1, t2)
  if types.isTuple(t1) and types.isTuple(t2) then
    local len1, len2 = #t1, #t2
    if not types.isVararg(t1[len1]) or
       not types.isVararg(t2[len2]) then
      return false
    end
    if len1 < len2 then
      local i = 1
      while i < len1 do
        if not types.subtype(env, t1[i], t2[i]) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len2 do
        if not types.subtype(env, t1[i], t2[j]) then
          return false
        end
        j = j + 1
      end
      return true
    elseif len1 > len2 then
      local i = 1
      while i < len2 do
        if not types.subtype(env, t1[i], t2[i]) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len1 do
        if not types.subtype(env, t1[j], t2[i]) then
          return false
        end
        j = j + 1
      end
      return true
    else
      for k, v in ipairs(t1) do
        if not types.subtype(env, t1[k], t2[k]) then
          return false
        end
      end
      return true
    end
  else
    return false
  end
end

function types.subtype (env, t1, t2)
  if types.isUnionlist(t1) then
    for k, v in ipairs(t1) do
      if not types.subtype(env, v, t2) then
        return false
      end
    end
    return true
  elseif types.isUnionlist(t2) then
    for k, v in ipairs(t2) do
      if types.subtype(env, t1, v) then
        return true
      end
    end
    return false
  elseif types.isTuple(t1) and types.isTuple(t2) then
    return subtype_tuple(env, t1, t2)
  elseif types.isTuple(t1) and not types.isTuple(t2) then
    return false
  elseif not types.isTuple(t1) and types.isTuple(t2) then
    return false
  elseif types.isVararg(t1) and types.isVararg(t2) then
    local t1_nil = types.Union(t1[1], types.Nil)
    local t2_nil = types.Union(t2[1], types.Nil)
    return types.subtype(env, t1_nil, t2_nil)
  elseif types.isVararg(t1) and not types.isVararg(t2) then
    local t1_nil = types.Union(t1[1], types.Nil)
    return types.subtype(env, t1_nil, t2)
  elseif not types.isVararg(t1) and types.isVararg(t2) then
    local t2_nil = types.Union(t2[1], types.Nil)
    return types.subtype(env, t1, t2_nil)
  else
    return subtype_literal(env, t1, t2) or
           subtype_base(env, t1, t2) or
           subtype_nil(env, t1, t2) or
           subtype_top(env, t1, t2) or
           subtype_any(env, t1, t2) or
           subtype_union(env, t1, t2) or
           subtype_function(env, t1, t2) or
           subtype_table(env, t1, t2) or
           subtype_variable(env, t1, t2)
  end
end

-- consistent-subtyping

local function consistent_subtype_literal (env, t1, t2)
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

local function consistent_subtype_base (env, t1, t2)
  if types.isBase(t1) and types.isBase(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function consistent_subtype_nil (env, t1, t2)
  return types.isNil(t1) and types.isNil(t2)
end

local function consistent_subtype_top (env, t1, t2)
  return types.isValue(t2)
end

local function consistent_subtype_any (env, t1, t2)
  return types.isAny(t1) or types.isAny(t2)
end

local function consistent_subtype_union (env, t1, t2)
  if types.isUnion(t1) then
    for k, v in ipairs(t1) do
      if not types.consistent_subtype(env, v, t2) then
        return false
      end
    end
    return true
  elseif types.isUnion(t2) then
    for k, v in ipairs(t2) do
      if types.consistent_subtype(env, t1, v) then
        return true
      end
    end
    return false
  else
    return false
  end
end

local function consistent_subtype_function (env, t1, t2)
  if types.isFunction(t1) and types.isFunction(t2) then
    return types.consistent_subtype(env, t2[1], t1[1]) and types.consistent_subtype(env, t1[2], t2[2])
  else
    return false
  end
end

local function consistent_subtype_table (env, t1, t2)
  if types.isTable(t1) and types.isTable(t2) then
  local m, n = #t1, #t2
  for i = 1, n do
    local subtype = false
    for j = 1, m do
      if types.consistent_subtype(env, t2[i][1], t1[j][1]) and
         types.consistent_subtype(env, t1[j][2], t2[i][2]) and
         types.consistent_subtype(env, t2[i][2], t1[j][2]) then
        subtype = true
        break
      end
    end
    if not subtype then
      return false
    end
  end
  return true
  else
    return false
  end
end

local function consistent_subtype_variable (env, t1, t2)
  if types.isVariable(t1) and types.isVariable(t2) then
    return types.consistent_subtype(env, get_type(env, t1[1]), get_type(env, t2[1]))
  elseif types.isVariable(t1) and not types.isVariable(t2) then
    return types.consistent_subtype(env, get_type(env, t1[1]), t2)
  elseif not types.isVariable(t1) and types.isVariable(t2) then
    return types.consistent_subtype(env, t1, get_type(env, t2[1]))
  else
    return false
  end
end

local function consistent_subtype_tuple (env, t1, t2)
  if types.isTuple(t1) and types.isTuple(t2) then
    local len1, len2 = #t1, #t2
    if not types.isVararg(t1[len1]) or
       not types.isVararg(t2[len2]) then
      return false
    end
    if len1 < len2 then
      local i = 1
      while i < len1 do
        if not types.consistent_subtype(env, t1[i], t2[i]) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len2 do
        if not types.consistent_subtype(env, t1[i], t2[j]) then
          return false
        end
        j = j + 1
      end
      return true
    elseif len1 > len2 then
      local i = 1
      while i < len2 do
        if not types.consistent_subtype(env, t1[i], t2[i]) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len1 do
        if not types.consistent_subtype(env, t1[j], t2[i]) then
          return false
        end
        j = j + 1
      end
      return true
    else
      for k, v in ipairs(t1) do
        if not types.consistent_subtype(env, t1[k], t2[k]) then
          return false
        end
      end
      return true
    end
  else
    return false
  end
end

function types.consistent_subtype (env, t1, t2)
  if types.isUnionlist(t1) then
    for k, v in ipairs(t1) do
      if not types.consistent_subtype(env, v, t2) then
        return false
      end
    end
    return true
  elseif types.isUnionlist(t2) then
    for k, v in ipairs(t2) do
      if types.consistent_subtype(env, t1, v) then
        return true
      end
    end
    return false
  elseif types.isTuple(t1) and types.isTuple(t2) then
    return consistent_subtype_tuple(env, t1, t2)
  elseif types.isTuple(t1) and not types.isTuple(t2) then
    return false
  elseif not types.isTuple(t1) and types.isTuple(t2) then
    return false
  elseif types.isVararg(t1) and types.isVararg(t2) then
    local t1_nil = types.Union(t1[1], types.Nil)
    local t2_nil = types.Union(t2[1], types.Nil)
    return types.consistent_subtype(env, t1_nil, t2_nil)
  elseif types.isVararg(t1) and not types.isVararg(t2) then
    local t1_nil = types.Union(t1[1], types.Nil)
    return types.consistent_subtype(env, t1_nil, t2)
  elseif not types.isVararg(t1) and types.isVararg(t2) then
    local t2_nil = types.Union(t2[1], types.Nil)
    return types.consistent_subtype(env, t1, t2_nil)
  else
    return consistent_subtype_literal(env, t1, t2) or
           consistent_subtype_base(env, t1, t2) or
           consistent_subtype_nil(env, t1, t2) or
           consistent_subtype_top(env, t1, t2) or
           consistent_subtype_any(env, t1, t2) or
           consistent_subtype_union(env, t1, t2) or
           consistent_subtype_function(env, t1, t2) or
           consistent_subtype_table(env, t1, t2) or
           consistent_subtype_variable(env, t1, t2)
  end
end

-- supertypeof

local function typelist_supertypeof (typelist)
  local l = types.Tuple()
  local len = #typelist
  for i = 1, len - 1 do
    l[i] = types.supertypeof(typelist[i])
  end
  l[len] = types.Vararg(types.supertypeof(typelist[len][1]))
  return l
end

function types.supertypeof (t)
  if types.isFalse(t) or types.isTrue(t) then
    return types.Boolean
  elseif types.isLiteralNumber(t) then
    return types.Number
  elseif types.isLiteralString(t) then
    return types.String
  elseif types.isUnion(t) then
    local t1 = types.supertypeof(t[1])
    local t2 = types.supertypeof(t[2])
    local n = types.Union(t1, t2)
    for i = 3, #t do
      n[i] = types.supertypeof(t[i])
    end
    return n
  elseif types.isFunction(t) then
    local t1 = types.supertypeof(t[1])
    local t2 = types.supertypeof(t[2])
    return types.Function(t1,t2)
  elseif types.isTable(t) then
    local n = { tag = "Table" }
    for k, v in ipairs(t) do
      n[k] = { tag = "Field" }
      n[k][1] = v[1]
      n[k][2] = types.supertypeof(v[2])
    end
    return n
  elseif types.isUnionlist(t) then
    local t1 = types.supertypeof(t[1])
    local t2 = types.supertypeof(t[2])
    local n = { tag = "Unionlist", [1] = t1, [2] = t2 }
    for i = 3, #t do
      n[i] = types.supertypeof(t[i])
    end
  elseif types.isTuple(t) then
    local n = types.Tuple()
    for k, v in ipairs(t) do
      n[k] = types.supertypeof(v)
    end
    return n
  elseif types.isVararg(t) then
    local n = types.supertypeof(t[1])
    return types.Vararg(n)
  else
    return t
  end
end

-- tostring

local function type2str (t)
  if types.isLiteral(t) then
    return tostring(t[1])
  elseif types.isBase(t) then
    return t[1]
  elseif types.isNil(t) then
    return "nil"
  elseif types.isValue(t) then
    return "value"
  elseif types.isAny(t) then
    return "any"
  elseif types.isUnion(t) or
         types.isUnionlist(t) then
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    return "(" .. table.concat(l, " | ") .. ")"
  elseif types.isFunction(t) then
    return type2str(t[1]) .. " -> " .. type2str(t[2])
  elseif types.isTable(t) then
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v[1]) .. ":" .. type2str(v[2])
    end
    return "{" .. table.concat(l, ", ") .. "}"
  elseif types.isVariable(t) then
    return t[1]
  elseif types.isTuple(t) then
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    return "(" .. table.concat(l, ", ") .. ")"
  elseif types.isVararg(t) then
    return type2str(t[1]) .. "*"
  else
    error("expecting type but got " .. t.tag)
  end
end

function types.tostring (t)
  return type2str(t)
end

return types

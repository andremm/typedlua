--[[
This file implements the available types in Typed Lua

type:
  `Literal{ literal }
  | `Base{ base }
  | `Nil
  | `Value
  | `Any
  | `Self
  | `Union{ type type type* }
  | `Function{ type type }
  | `Table{ { type type }* }
  | `Variable{ <string> }
  | `Recursive{ <string> type }
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

-- self type

types.Self = { tag = "Self" }

function types.isSelf (t)
  return t.tag == "Self"
end

-- union types

function types.Union (...)
  -- remove unions of unions
  local l1 = {}
  for k, v in ipairs({...}) do
    if types.isUnion(v) then
      for i = 1, #v do
        l1[#l1 + 1] = v[i]
      end
    else
      l1[#l1 + 1] = v
    end
  end
  -- remove duplicated types
  local l2 = {}
  for i = 1, #l1 do
    local enter = true
    for j = i + 1, #l1 do
      if types.subtype({}, l1[i], l1[j]) and types.subtype({}, l1[j], l1[i]) then
        enter = false
        break
      end
    end
    if enter then
      l2[#l2 + 1] = l1[i]
    end
  end
  -- simplify union
  local t = { tag = "Union" }
  for i = 1, #l2 do
    local enter = true
    for j = 1, #l2 do
      if i ~= j and types.subtype({}, l2[i], l2[j]) then
        enter = false
        break
      end
    end
    if enter then
      t[#t + 1] = l2[i]
    end
  end
  if #t == 1 then
    return t[1]
  else
    return t
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
  end
  return false
end

function types.filterUnion (u, t)
  if types.isUnion(u) then
    local l = {}
    for i = 1, #u do
      if not (types.subtype({}, u[i], t) and types.subtype({}, t, u[i])) then
        l[#l + 1] = u[i]
      end
    end
    return types.Union(table.unpack(l))
  else
    return u
  end
end

function types.UnionNoNil (t)
  return types.filterUnion(t, types.Nil)
end

-- function types

function types.Function (t1, t2)
  return { tag = "Function", [1] = t1, [2] = t2 }
end

function types.isFunction (t)
  return t.tag == "Function"
end

-- table types

function types.Field (t1, t2)
  if not t2 then
    return { tag = "Field", [1] = types.Number, [2] = t1 }
  else
    return { tag = "Field", [1] = t1, [2] = t2 }
  end
end

function types.ConstField (t1, t2)
  if not t2 then
    return { tag = "Const", [1] = types.Number, [2] = t1 }
  else
    return { tag = "Const", [1] = t1, [2] = t2 }
  end
end

function types.isField (f)
  return f.tag == "Field"
end

function types.isConstField (f)
  return f.tag == "Const"
end

function types.Table (...)
  return { tag = "Table", ... }
end

function types.isTable (t)
  return t.tag == "Table"
end

function types.isValidTable (env, t)
  if types.isTable(t) then
    for i = 1, #t do
      for j = 1, #t do
        if i ~= j and types.subtype(env, t[i][1], t[j][1]) then
          return false
        end
      end
    end
    return true
  else
    return false
  end
end

-- type variables

function types.Variable (t)
  return { tag = "Variable", [1] = t }
end

function types.isVariable (t)
  return t.tag == "Variable"
end

-- recursive types

function types.Recursive (x, t)
  return { tag = "Recursive", [1] = x, [2] = t }
end

function types.isRecursive (t)
  return t.tag == "Recursive"
end

-- union list

function types.Unionlist (...)
  local t = types.Union(...)
  if types.isUnion(t) then
    t.tag = "Unionlist"
  end
  return t
end

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

function types.ArgTuple (t)
  if not types.isVararg(t[#t]) then
    t[#t + 1] = types.ValueStar
  end
  return t
end

function types.RetTuple (t)
  if not types.isVararg(t[#t]) then
    t[#t + 1] = types.NilStar
  end
  return t
end

function types.Type2RetTuple (t)
  local n = {}
  if types.isUnion(t) then
    n.tag = "Unionlist"
    for k, v in ipairs(t) do
      local a = { tag = "Tuple", [1] = v }
      n[#n + 1] = types.RetTuple(a)
    end
  else
    local a = { tag = "Tuple", [1] = t }
    n = types.RetTuple(a)
  end
  return n
end

-- vararg types

function types.Vararg (t)
  return { tag = "Vararg", [1] = t }
end

function types.isVararg (t)
  return t.tag == "Vararg"
end

types.ValueStar = { tag = "Vararg", [1] = types.Value }

types.NilStar = { tag = "Vararg", [1] = types.Nil }

types.ErrorRetTuple = { tag = "Tuple", [1] = types.Nil, [2] = types.String, [3] = types.NilStar }

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

local function subtype_self (env, t1, t2)
  return types.isSelf(t1) and types.isSelf(t2)
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

local function subtype_field (env, f1, f2)
  if types.isField(f1) and types.isField(f2) then
    return types.subtype(env, f2[1], f1[1]) and
           types.subtype(env, f1[2], f2[2]) and
           types.subtype(env, f2[2], f1[2])
  elseif types.isField(f1) and types.isConstField(f2) then
    return types.subtype(env, f2[1], f1[1]) and
           types.subtype(env, f1[2], f2[2])
  elseif types.isConstField(f1) and types.isConstField(f2) then
    return types.subtype(env, f2[1], f1[1]) and
           types.subtype(env, f1[2], f2[2])
  else
    return false
  end
end

local function subtype_table (env, t1, t2)
  if types.isTable(t1) and types.isTable(t2) then
    if not t1.open then
      local m, n = #t1, #t2
      for i = 1, n do
        local subtype = false
        for j = 1, m do
          if subtype_field(env, t1[j], t2[i]) then
            subtype = true
            break
          end
        end
        if not subtype then return false end
      end
      return true
    else
      local m, n = #t2, #t1
      for i = 1, m do
        local subtype_key, subtype_value = false, false
        for j = 1, n do
          if types.subtype(env, t1[j][1], t2[i][1]) then
            subtype_key = true
            if subtype_field(env, t2[i], t1[j]) then
              subtype_value = true
              break
            end
          end
        end
        if subtype_key then
          if not subtype_value then return false end
        else
          if not types.subtype(env, types.Nil, t2[i][2]) then return false end
        end
      end
      return true
    end
  else
    return false
  end
end

local function subtype_variable (env, t1, t2)
  if types.isVariable(t1) and types.isVariable(t2) then
    return env[t1[1] .. t2[1]]
  else
    return false
  end
end

local function subtype_recursive (env, t1, t2)
  if types.isRecursive(t1) and types.isRecursive(t2) then
    env[t1[1] .. t2[1]] = true
    return types.subtype(env, t1[2], t2[2])
  elseif types.isRecursive(t1) and not types.isRecursive(t2) then
    if not env[t1[1] .. t1[1]] then
      env[t1[1] .. t1[1]] = true
      return types.subtype(env, t1[2], t2)
    else
      return types.subtype(env, types.Variable(t1[1]), t2)
    end
  elseif not types.isRecursive(t1) and types.isRecursive(t2) then
    if not env[t2[1] .. t2[1]] then
      env[t2[1] .. t2[1]] = true
      return types.subtype(env, t1, t2[2])
    else
      return types.subtype(env, t1, types.Variable(t2[1]))
    end
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
           subtype_self(env, t1, t2) or
           subtype_union(env, t1, t2) or
           subtype_function(env, t1, t2) or
           subtype_table(env, t1, t2) or
           subtype_variable(env, t1, t2) or
           subtype_recursive(env, t1, t2)
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

local function consistent_subtype_self (env, t1, t2)
  return types.isSelf(t1) and types.isSelf(t2)
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

local function consistent_subtype_field (env, f1, f2)
  if types.isField(f1) and types.isField(f2) then
    return types.consistent_subtype(env, f2[1], f1[1]) and
           types.consistent_subtype(env, f1[2], f2[2]) and
           types.consistent_subtype(env, f2[2], f1[2])
  elseif types.isField(f1) and types.isConstField(f2) then
    return types.consistent_subtype(env, f2[1], f1[1]) and
           types.consistent_subtype(env, f1[2], f2[2])
  elseif types.isConstField(f1) and types.isConstField(f2) then
    return types.consistent_subtype(env, f2[1], f1[1]) and
           types.consistent_subtype(env, f1[2], f2[2])
  else
    return false
  end
end

local function consistent_subtype_table (env, t1, t2)
  if types.isTable(t1) and types.isTable(t2) then
    if not t1.open then
      local m, n = #t1, #t2
      for i = 1, n do
        local subtype = false
        for j = 1, m do
          if consistent_subtype_field(env, t1[j], t2[i]) then
            subtype = true
            break
          end
        end
        if not subtype then return false end
      end
      return true
    else
      local m, n = #t2, #t1
      for i = 1, m do
        local subtype_key, subtype_value = false, false
        for j = 1, n do
          if types.consistent_subtype(env, t1[j][1], t2[i][1]) then
            subtype_key = true
            if consistent_subtype_field(env, t2[i], t1[j]) then
              subtype_value = true
              break
            end
          end
        end
        if subtype_key then
          if not subtype_value then return false end
        else
          if not types.consistent_subtype(env, types.Nil, t2[i][2]) then return false end
        end
      end
      return true
    end
  else
    return false
  end
end

local function consistent_subtype_variable (env, t1, t2)
  if types.isVariable(t1) and types.isVariable(t2) then
    return env[t1[1] .. t2[1]]
  else
    return false
  end
end

local function consistent_subtype_recursive (env, t1, t2)
  if types.isRecursive(t1) and types.isRecursive(t2) then
    env[t1[1] .. t2[1]] = true
    return types.consistent_subtype(env, t1[2], t2[2])
  elseif types.isRecursive(t1) and not types.isRecursive(t2) then
    if not env[t1[1] .. t1[1]] then
      env[t1[1] .. t1[1]] = true
      return types.consistent_subtype(env, t1[2], t2)
    else
      return types.consistent_subtype(env, types.Variable(t1[1]), t2)
    end
  elseif not types.isRecursive(t1) and types.isRecursive(t2) then
    if not env[t2[1] .. t2[1]] then
      env[t2[1] .. t2[1]] = true
      return types.consistent_subtype(env, t1, t2[2])
    else
      return types.consistent_subtype(env, t1, types.Variable(t2[1]))
    end
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
           consistent_subtype_self(env, t1, t2) or
           consistent_subtype_union(env, t1, t2) or
           consistent_subtype_function(env, t1, t2) or
           consistent_subtype_table(env, t1, t2) or
           consistent_subtype_variable(env, t1, t2) or
           consistent_subtype_recursive(env, t1, t2)
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
    local n = { tag = "Table", open = t.open }
    for k, v in ipairs(t) do
      n[k] = { tag = v.tag }
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
    return n
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

-- first-class

local function resize_tuple (t, n)
  local tuple = { tag = "Tuple" }
  local vararg = t[#t][1]
  for i = 1, #t - 1 do
    tuple[i] = t[i]
  end
  for i = #t, n - 1 do
    if types.isNil(vararg) then
      tuple[i] = vararg
    else
      tuple[i] = types.Union(vararg, Nil)
    end
  end
  tuple[n] = types.Vararg(vararg)
  return tuple
end

function types.unionlist2tuple (t)
  local max = 1
  for i = 1, #t do
    if #t[i] > max then max = #t[i] end
  end
  local u = {}
  for i = 1, #t do
    if #t[i] < max then
      u[i] = resize_tuple(t[i], max)
    else
      u[i] = t[i]
    end
  end
  local l = {}
  for i = 1, #u do
    for j = 1, #u[i] do
      if not l[j] then l[j] = {} end
      table.insert(l[j], u[i][j])
    end
  end
  local n = { tag = "Tuple" }
  for i = 1, #l - 1 do
    n[i] = types.Union(table.unpack(l[i]))
  end
  local vs = {}
  for k, v in ipairs(l[#l]) do
    table.insert(vs, v[1])
  end
  n[#l] = types.Vararg(types.Union(table.unpack(vs)))
  return n
end

function types.unionlist2union (t, i)
  local l = {}
  for k, v in ipairs(t) do
    l[#l + 1] = v[i]
  end
  return types.Union(table.unpack(l))
end

function types.first_class (t)
  if types.isUnionlist(t) then
    return types.first_class(types.unionlist2tuple(t))
  elseif types.isTuple(t) then
    return types.first_class(t[1])
  elseif types.isVararg(t) then
    if types.isNil(t[1]) then
      return t[1]
    else
      return types.Union(t[1], types.Nil)
    end
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
  elseif types.isSelf(t) then
    return "self"
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
      if types.isConstField(v) then
        l[k] = "const " .. l[k]
      end
    end
    return "{" .. table.concat(l, ", ") .. "}"
  elseif types.isVariable(t) then
    return t[1]
  elseif types.isRecursive(t) then
    return t[1] .. "." .. type2str(t[2])
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

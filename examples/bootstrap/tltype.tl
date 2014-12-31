--[[
This module implements Typed Lua tltype.
]]

local tltype = {}

typealias Type = {
  "tag":string
}

typealias LiteralType = {
  "tag":string,
  1:boolean|number|string
}

typealias BaseType = {
  "tag":string,
  1:string
}

typealias UnionType = {
  "tag":string,
  number:Type
}

typealias FieldType = {
  "tag":string,
  "const":boolean?,
  1:Type,
  2:Type
}

function tltype.subtype (t1:Type, t2:Type):(boolean) return false end
function tltype.consistent_subtype (t1:Type, t2:Type):(boolean) return false end
function tltype.general (t:Type):(Type) return { tag = "TNil" } end
function tltype.first ():(any) end

-- literal types

function tltype.Literal (l:boolean|number|string):(LiteralType)
  return { tag = "TLiteral", [1] = l }
end

function tltype.False ():(LiteralType)
  return tltype.Literal(false)
end

function tltype.True ():(LiteralType)
  return tltype.Literal(true)
end

function tltype.Num (n:number):(LiteralType)
  return tltype.Literal(n)
end

function tltype.Str (s:string):(LiteralType)
  return tltype.Literal(s)
end

function tltype.isLiteral (t:Type):(boolean)
  return t.tag == "TLiteral"
end

function tltype.isFalse (t:LiteralType):(boolean)
  return tltype.isLiteral(t) and t[1] == false
end

function tltype.isTrue (t:LiteralType):(boolean)
  return tltype.isLiteral(t) and t[1] == true
end

function tltype.isNum (t:LiteralType):(boolean)
  return tltype.isLiteral(t) and type(t[1]) == "number"
end

function tltype.isStr (t:LiteralType):(boolean)
  return tltype.isLiteral(t) and type(t[1]) == "string"
end

-- base types

function tltype.Base (s:string):(Type)
  return { tag = "TBase", [1] = s }
end

function tltype.Boolean ():(Type)
  return tltype.Base("boolean")
end

function tltype.Number ():(Type)
  return tltype.Base("number")
end

function tltype.String ():(Type)
  return tltype.Base("string")
end

function tltype.isBase (t:Type):(boolean)
  return t.tag == "TBase"
end

function tltype.isBoolean (t:BaseType):(boolean)
  return tltype.isBase(t) and t[1] == "boolean"
end

function tltype.isNumber (t:BaseType):(boolean)
  return tltype.isBase(t) and t[1] == "number"
end

function tltype.isString (t:BaseType):(boolean)
  return tltype.isBase(t) and t[1] == "string"
end

-- nil type

function tltype.Nil ():(Type)
  return { tag = "TNil" }
end

function tltype.isNil (t:Type):(boolean)
  return t.tag == "TNil"
end

-- value type

function tltype.Value ():(Type)
  return { tag = "TValue" }
end

function tltype.isValue (t:Type):(boolean)
  return t.tag == "TValue"
end

-- dynamic type

function tltype.Any ():(Type)
  return { tag = "TAny" }
end

function tltype.isAny (t:Type):(boolean)
  return t.tag == "TAny"
end

-- self type

function tltype.Self ():(Type)
  return { tag = "TSelf" }
end

function tltype.isSelf (t:Type):(boolean)
  return t.tag == "TSelf"
end

-- void type

function tltype.Void ():(Type)
  return { tag = "TVoid" }
end

function tltype.isVoid (t:Type):(boolean)
  return t.tag == "TVoid"
end

-- union types

function tltype.isUnion (t1:Type, t2:Type?):(boolean)
  if not t2 then
    return t1.tag == "TUnion"
  else
    if t1.tag == "TUnion" then
      local t1:any = t1
      for k, v in ipairs(t1) do
        if tltype.subtype(t2, v) and tltype.subtype(v, t2) then
          return true
        end
      end
      return false
    else
      return false
    end
  end
end

function tltype.Union (...:Type):(Type)
  local l1:any = {...}
  -- remove unions of unions
  local l2:any = {}
  for i = 1, #l1 do
    if tltype.isUnion(l1[i]) then
      for j = 1, #l1[i] do
        table.insert(l2, l1[i][j])
      end
    else
      table.insert(l2, l1[i])
    end
  end
  -- remove duplicated types
  local l3:any = {}
  for i = 1, #l2 do
    local enter = true
    for j = i + 1, #l2 do
      if tltype.subtype(l2[i], l2[j]) and tltype.subtype(l2[j], l2[i]) then
        enter = false
        break
      end
    end
    if enter then table.insert(l3, l2[i]) end
  end
  -- simplify union
  local t:any = { tag = "TUnion" }
  for i = 1, #l3 do
    local enter = true
    for j = 1, #l3 do
      if i ~= j and tltype.consistent_subtype(l3[i], l3[j]) then
        enter = false
        break
      end
    end
    if enter then table.insert(t, l3[i]) end
  end
  if #t == 0 then
    return tltype.Any()
  elseif #t == 1 then
    return t[1]
  else
    return t
  end
end

function tltype.filterUnion (u:Type, t:Type):(Type)
  if tltype.isUnion(u) then
    local l:any = {}
    local u:any = u
    for k, v in ipairs(u) do
      if not (tltype.subtype(t, v) and tltype.subtype(v, t)) then
        table.insert(l, v)
      end
    end
    return tltype.Union(table.unpack(l))
  else
    return u
  end
end

function tltype.UnionNil (t:Type, is_union_nil:boolean?):(Type)
  if is_union_nil then
    return tltype.Union(t, tltype.Nil())
  else
    return t
  end
end

-- vararg types

function tltype.Vararg (t:Type):(Type)
  return { tag = "TVararg", [1] = t }
end

function tltype.isVararg (t:Type):(boolean)
  return t.tag == "TVararg"
end

-- tuple types

-- Tuple : ({number:type}, true?) -> (type)
function tltype.Tuple (l:{Type}, is_vararg:boolean?):(Type)
  if is_vararg then
    local t = l[#l]
    if t then l[#l] = tltype.Vararg(t) end
  end
  return { tag = "TTuple", table.unpack(l) }
end

function tltype.inputTuple (t:Type?, strict:boolean):(Type)
  if not strict then
    if not t then
      return tltype.Tuple({ tltype.Value() }, true)
    else
      local t:any = t
      if not tltype.isVararg(t[#t]) then
        table.insert(t, #t + 1, tltype.Vararg(tltype.Value()))
      end
      return t
    end
  else
    if not t then
      return tltype.Void()
    else
      return t
    end
  end
end

function tltype.outputTuple (t:Type?, strict:boolean):(Type)
  if not strict then
    if not t then
      return tltype.Tuple({ tltype.Nil() }, true)
    else
      local t:any = t
      if not tltype.isVararg(t[#t]) then
        table.insert(t, #t + 1, tltype.Vararg(tltype.Nil()))
      end
      return t
    end
  else
    if not t then
      return tltype.Void()
    else
      return t
    end
  end
end

function tltype.retType (t:Type, strict:boolean):(Type)
  return tltype.outputTuple(tltype.Tuple({ t }), strict)
end

function tltype.isTuple (t:Type):(boolean)
  return t.tag == "TTuple"
end

-- union of tuple types

function tltype.Unionlist (...:Type):(Type)
  local t = tltype.Union(...)
  if tltype.isUnion(t) then t.tag = "TUnionlist" end
  return t
end

function tltype.isUnionlist (t:Type):(boolean)
  return t.tag == "TUnionlist"
end

function tltype.UnionlistNil (t:Type, is_union_nil:boolean?):(Type)
  if type(is_union_nil) == "boolean" then
    local u = tltype.Tuple({ tltype.Nil(), tltype.String() })
    return tltype.Unionlist(t, tltype.outputTuple(u, is_union_nil))
  else
    return t
  end
end

-- function types

function tltype.Function (t1:Type, t2:Type, is_method:boolean?):(Type)
  if is_method then
    if tltype.isVoid(t1) then
      t1 = tltype.Tuple({ tltype.Self() })
    else
      local t1:any = t1
      table.insert(t1, 1, tltype.Self())
    end
  end
  return { tag = "TFunction", [1] = t1, [2] = t2 }
end

function tltype.isFunction (t:Type):(boolean)
  return t.tag == "TFunction"
end

function tltype.isMethod (t:Type):(boolean)
  if tltype.isFunction(t) then
    local t:any = t
    for k, v in ipairs(t[1]) do
      if tltype.isSelf(v) then return true end
    end
    return false
  else
    return false
  end
end

-- table types

function tltype.Field (is_const:boolean, t1:Type, t2:Type):(Type)
  return { tag = "TField", const = is_const, [1] = t1, [2] = t2 }
end

function tltype.isField (f:FieldType):(boolean)
  return f.tag == "TField" and not f.const
end

function tltype.isConstField (f:FieldType):(boolean?)
  return f.tag == "TField" and f.const
end

function tltype.Table (...:FieldType):(Type)
  return { tag = "TTable", ... }
end

function tltype.isTable (t:Type):(boolean)
  return t.tag == "TTable"
end

function tltype.getField (f:Type, t:Type):(Type)
  if tltype.isTable(t) then
    local t:any = t
    for k, v in ipairs(t) do
      if tltype.consistent_subtype(f, v[1]) then
        return v[2]
      end
    end
    return tltype.Nil()
  else
    return tltype.Nil()
  end
end

function tltype.fieldlist (idlist:any, t:Type):(FieldType*)
  local l:any = {}
  for k, v in ipairs(idlist) do
    table.insert(l, #l + 1, tltype.Field(v.const, tltype.Literal(v[1]), t))
  end
  return table.unpack(l)
end

function tltype.checkTypeDec (n:string, t:Type):(boolean, nil)?
  local predef_type:{string:boolean} = {
    ["boolean"] = true,
    ["number"] = true,
    ["string"] = true,
    ["value"] = true,
    ["any"] = true,
    ["self"] = true,
    ["const"] = true,
  }
  if not predef_type[n] then
    if tltype.isTable(t) then
      local namelist:any = {}
      local t:any = t
      for k, v in ipairs(t) do
        local f1, f2 = v[1], v[2]
        if tltype.isStr(f1) then
          local name = f1[1]
          if not namelist[name] then
            namelist[name] = true
          else
            local msg = "attempt to redeclare field '%s'"
            return nil, string.format(msg, name)
          end
        end
      end
      return true
    else
      return nil, "attempt to name a type that is not a table"
    end
  else
    local msg = "attempt to redeclare type '%s'"
    return nil, string.format(msg, n)
  end
end

-- type variables

function tltype.Variable (name:string):(Type)
  return { tag = "TVariable", [1] = name }
end

function tltype.isVariable (t:Type):(boolean)
  return t.tag == "TVariable"
end

-- recursive types

function tltype.Recursive (x:string, t:Type):(Type)
  return { tag = "TRecursive", [1] = x, [2] = t }
end

function tltype.isRecursive (t:Type):(boolean)
  return t.tag == "TRecursive"
end

local function check_recursive (t:Type, name:string):(boolean)
  if tltype.isLiteral(t) or
     tltype.isBase(t) or
     tltype.isNil(t) or
     tltype.isValue(t) or
     tltype.isAny(t) or
     tltype.isSelf(t) or
     tltype.isVoid(t) then
    return false
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
    local t:any = t
    for k, v in ipairs(t) do
      if check_recursive(v, name) then
        return true
      end
    end
    return false
  elseif tltype.isFunction(t) then
    local t:any = t
    return check_recursive(t[1], name) or check_recursive(t[2], name)
  elseif tltype.isTable(t) then
    local t:any = t
    for k, v in ipairs(t) do
      if check_recursive(v[2], name) then
        return true
      end
    end
    return false
  elseif tltype.isVariable(t) then
    local t:any = t
    return t[1] == name
  elseif tltype.isRecursive(t) then
    local t:any = t
    return check_recursive(t[2], name)
  elseif tltype.isVararg(t) then
    local t:any = t
    return check_recursive(t[1], name)
  else
    return false
  end
end

function tltype.checkRecursive (t:Type, name:string):(boolean)
  if tltype.isTable(t) then
    local t:any = t
    for k, v in ipairs(t) do
      if check_recursive(v[2], name) then return true end
    end
    return false
  else
    return false
  end
end

-- subtyping and consistent-subtyping

local subtype

local typealias SEnv = {string:boolean}

local function subtype_literal (env:SEnv, t1:Type, t2:Type):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isLiteral(t1) and tltype.isLiteral(t2) then
    return t1[1] == t2[1]
  elseif tltype.isLiteral(t1) and tltype.isBase(t2) then
    if tltype.isBoolean(t2) then
      return tltype.isFalse(t1) or tltype.isTrue(t1)
    elseif tltype.isNumber(t2) then
      return tltype.isNum(t1)
    elseif tltype.isString(t2) then
      return tltype.isStr(t1)
    else
      return false
    end
  else
    return false
  end
end

local function subtype_base (env:SEnv, t1:Type, t2:Type):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isBase(t1) and tltype.isBase(t2) then
    return t1[1] == t2[1]
  else
    return false
  end
end

local function subtype_nil (env:SEnv, t1:Type, t2:Type):(boolean)
  return tltype.isNil(t1) and tltype.isNil(t2)
end

local function subtype_top (env:SEnv, t1:Type, t2:Type):(boolean)
  return tltype.isValue(t2)
end

local function subtype_any (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  if relation == "<:" then
    return tltype.isAny(t1) and tltype.isAny(t2)
  else
    return tltype.isAny(t1) or tltype.isAny(t2)
  end
end

local function subtype_self (env:SEnv, t1:Type, t2:Type):(boolean)
  return tltype.isSelf(t1) and tltype.isSelf(t2)
end

local function subtype_union (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isUnion(t1) then
    for k, v in ipairs(t1) do
      if not subtype(env, v, t2, relation) then
        return false
      end
    end
    return true
  elseif tltype.isUnion(t2) then
    for k, v in ipairs(t2) do
      if subtype(env, t1, v, relation) then
        return true
      end
    end
    return false
  else
    return false
  end
end

local function subtype_function (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isFunction(t1) and tltype.isFunction(t2) then
    return subtype(env, t2[1], t1[1], relation) and subtype(env, t1[2], t2[2], relation)
  else
    return false
  end
end

local function subtype_field (env:SEnv, f1:FieldType, f2:FieldType, relation:string):(boolean)
  if tltype.isField(f1) and tltype.isField(f2) then
    return subtype(env, f2[1], f1[1], relation) and
           subtype(env, f1[2], f2[2], relation) and
           subtype(env, f2[2], f1[2], relation)
  elseif tltype.isField(f1) and tltype.isConstField(f2) then
    return subtype(env, f2[1], f1[1], relation) and
           subtype(env, f1[2], f2[2], relation)
  elseif tltype.isConstField(f1) and tltype.isConstField(f2) then
    return subtype(env, f2[1], f1[1], relation) and
           subtype(env, f1[2], f2[2], relation)
  else
    return false
  end
end

local function subtype_table (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isTable(t1) and tltype.isTable(t2) then
    if t1.unique then
      local m, n = #t1, #t2
      local k, l:any = 0, {}
      for i = 1, m do
        for j = 1, n do
          if subtype(env, t1[i][1], t2[j][1], relation) then
            if subtype(env, t1[i][2], t2[j][2], relation) then
              if not l[j] then
                k = k + 1
                l[j] = true
              end
            else
              return false
            end
          end
        end
      end
      if k == n then
        return true
      else
        for j = 1, n do
          if not l[j] then
            if not subtype(env, tltype.Nil(), t2[j][2], relation) then
              return false
            end
          end
        end
      end
      return true
    elseif t1.open then
      local m, n = #t1, #t2
      local k, l:any = 0, {}
      for i = 1, m do
        for j = 1, n do
          if subtype(env, t1[i][1], t2[j][1], relation) then
            if subtype_field(env, t2[j], t1[i], relation) then
              if not l[j] then
                k = k + 1
                l[j] = true
              end
            else
              return false
            end
          end
        end
      end
      if k == n then
        return true
      else
        for j = 1, n do
          if not l[j] then
            if not subtype(env, tltype.Nil(), t2[j][2], relation) then
              return false
            end
          end
        end
      end
      return true
    else
      local m, n = #t1, #t2
      for i = 1, n do
        local subtype = false
        for j = 1, m do
          if subtype_field(env, t1[j], t2[i], relation) then
            subtype = true
            break
          end
        end
        if not subtype then return false end
      end
      return true
    end
  else
    return false
  end
end

local function subtype_variable (env:SEnv, t1:Type, t2:Type):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isVariable(t1) and tltype.isVariable(t2) then
    if env[t1[1] .. t2[1]] then return true else return false end
  else
    return false
  end
end

local function subtype_recursive (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isRecursive(t1) and tltype.isRecursive(t2) then
    env[t1[1] .. t2[1]] = true
    return subtype(env, t1[2], t2[2], relation)
  elseif tltype.isRecursive(t1) and not tltype.isRecursive(t2) then
    if not env[t1[1] .. t1[1]] then
      env[t1[1] .. t1[1]] = true
      return subtype(env, t1[2], t2, relation)
    else
      return subtype(env, tltype.Variable(t1[1]), t2, relation)
    end
  elseif not tltype.isRecursive(t1) and tltype.isRecursive(t2) then
    if not env[t2[1] .. t2[1]] then
      env[t2[1] .. t2[1]] = true
      return subtype(env, t1, t2[2], relation)
    else
      return subtype(env, t1, tltype.Variable(t2[1]), relation)
    end
  else
    return false
  end
end

local function subtype_tuple (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isTuple(t1) and tltype.isTuple(t2) then
    local len1, len2 = #t1, #t2
    if len1 < len2 then
      if not tltype.isVararg(t1[len1]) then return false end
      local i = 1
      while i < len1 do
        if not subtype(env, t1[i], t2[i], relation) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len2 do
        if not subtype(env, t1[i], t2[j], relation) then
          return false
        end
        j = j + 1
      end
      return true
    elseif len1 > len2 then
      if not tltype.isVararg(t2[len2]) then return false end
      local i = 1
      while i < len2 do
        if not subtype(env, t1[i], t2[i], relation) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len1 do
        if not subtype(env, t1[j], t2[i], relation) then
          return false
        end
        j = j + 1
      end
      return true
    else
      for k, v in ipairs(t1) do
        if not subtype(env, t1[k], t2[k], relation) then
          return false
        end
      end
      return true
    end
  else
    return false
  end
end

function subtype (env:SEnv, t1:Type, t2:Type, relation:string):(boolean)
  local t1:any, t2:any = t1, t2
  if tltype.isVoid(t1) and tltype.isVoid(t2) then
    return true
  elseif tltype.isUnionlist(t1) then
    for k, v in ipairs(t1) do
      if not subtype(env, v, t2, relation) then
        return false
      end
    end
    return true
  elseif tltype.isUnionlist(t2) then
    for k, v in ipairs(t2) do
      if subtype(env, t1, v, relation) then
        return true
      end
    end
    return false
  elseif tltype.isTuple(t1) and tltype.isTuple(t2) then
    return subtype_tuple(env, t1, t2, relation)
  elseif tltype.isTuple(t1) and not tltype.isTuple(t2) then
    return false
  elseif not tltype.isTuple(t1) and tltype.isTuple(t2) then
    return false
  elseif tltype.isVararg(t1) and tltype.isVararg(t2) then
    local t1_nil = tltype.Union(t1[1], tltype.Nil())
    local t2_nil = tltype.Union(t2[1], tltype.Nil())
    return subtype(env, t1_nil, t2_nil, relation)
  elseif tltype.isVararg(t1) and not tltype.isVararg(t2) then
    local t1_nil = tltype.Union(t1[1], tltype.Nil())
    return subtype(env, t1_nil, t2, relation)
  elseif not tltype.isVararg(t1) and tltype.isVararg(t2) then
    local t2_nil = tltype.Union(t2[1], tltype.Nil())
    return subtype(env, t1, t2_nil, relation)
  else
    return subtype_literal(env, t1, t2) or
           subtype_base(env, t1, t2) or
           subtype_nil(env, t1, t2) or
           subtype_top(env, t1, t2) or
           subtype_any(env, t1, t2, relation) or
           subtype_self(env, t1, t2) or
           subtype_union(env, t1, t2, relation) or
           subtype_function(env, t1, t2, relation) or
           subtype_table(env, t1, t2, relation) or
           subtype_variable(env, t1, t2) or
           subtype_recursive(env, t1, t2, relation)
  end
end

function tltype.subtype (t1:Type, t2:Type):(boolean)
  return subtype({}, t1, t2, "<:")
end

function tltype.consistent_subtype (t1:Type, t2:Type):(boolean)
  return subtype({}, t1, t2, "<~")
end

-- most general type

function tltype.general (t:Type):(Type)
  local t:any = t
  if tltype.isFalse(t) or tltype.isTrue(t) then
    return tltype.Boolean()
  elseif tltype.isNum(t) then
    return tltype.Number()
  elseif tltype.isStr(t) then
    return tltype.String()
  elseif tltype.isUnion(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      table.insert(l, #l + 1, tltype.general(v))
    end
    return tltype.Union(table.unpack(l))
  elseif tltype.isFunction(t) then
    return tltype.Function(tltype.general(t[1]), tltype.general(t[2]))
  elseif tltype.isTable(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      table.insert(l, #l + 1, tltype.Field(v.const, v[1], tltype.general(v[2])))
    end
    local n:any = tltype.Table(table.unpack(l))
    n.unique = t.unique
    n.open = t.open
    return n
  elseif tltype.isTuple(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      table.insert(l, #l + 1, tltype.general(v))
    end
    return tltype.Tuple(l)
  elseif tltype.isUnionlist(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      table.insert(l, #l + 1, tltype.general(v))
    end
    return tltype.Unionlist(table.unpack(l))
  elseif tltype.isVararg(t) then
    return tltype.Vararg(tltype.general(t[1]))
  else
    return t
  end
end

-- first level type

local function resize_tuple (t:Type, n:number):(Type)
  local t:any = t
  local tuple:any = { tag = "TTuple" }
  local vararg = t[#t][1]
  for i = 1, #t - 1 do
    tuple[i] = t[i]
  end
  for i = #t, n - 1 do
    if tltype.isNil(vararg) then
      tuple[i] = vararg
    else
      tuple[i] = tltype.Union(vararg, tltype.Nil())
    end
  end
  tuple[n] = tltype.Vararg(vararg)
  return tuple
end

function tltype.unionlist2tuple (t:any):(Type)
  local max = 1
  for i = 1, #t do
    if #t[i] > max then max = #t[i] end
  end
  local u:any = {}
  for i = 1, #t do
    if #t[i] < max then
      u[i] = resize_tuple(t[i], max)
    else
      u[i] = t[i]
    end
  end
  local l:any = {}
  for i = 1, #u do
    for j = 1, #u[i] do
      if not l[j] then l[j] = {} end
      table.insert(l[j], u[i][j])
    end
  end
  local n:any = { tag = "TTuple" }
  for i = 1, #l - 1 do
    n[i] = tltype.Union(table.unpack(l[i]))
  end
  local vs:any = {}
  for k, v in ipairs(l[#l]) do
    table.insert(vs, v[1])
  end
  n[#l] = tltype.Vararg(tltype.Union(table.unpack(vs)))
  return n
end

function tltype.unionlist2union (t:any, i:number):(Type)
  local l:any = {}
  for k, v in ipairs(t) do
    l[#l + 1] = v[i]
  end
  return tltype.Union(table.unpack(l))
end

function tltype.first (t:any):(Type)
  if tltype.isTuple(t) then
    return tltype.first(t[1])
  elseif tltype.isUnionlist(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      table.insert(l, #l + 1, tltype.first(v))
    end
    return tltype.Union(table.unpack(l))
  elseif tltype.isVararg(t) then
    return tltype.Union(t[1], tltype.Nil())
  else
    return t
  end
end

-- tostring

local function type2str (t:Type):(string)
  local t:any = t
  local str = ""
  if tltype.isLiteral(t) then
    str = tostring(t[1])
  elseif tltype.isBase(t) then
    str = t[1]
  elseif tltype.isNil(t) then
    str = "nil"
  elseif tltype.isValue(t) then
    str = "value"
  elseif tltype.isAny(t) then
    str = "any"
  elseif tltype.isSelf(t) then
    str = "self"
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    str = "(" .. table.concat(l, " | ") .. ")"
  elseif tltype.isFunction(t) then
    str = type2str(t[1]) .. " -> " .. type2str(t[2])
  elseif tltype.isTable(t) then
    --if t.interface then return t.interface end
    local l:any = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v[1]) .. ":" .. type2str(v[2])
      if tltype.isConstField(v) then
        l[k] = "const " .. l[k]
      end
    end
    str = "{" .. table.concat(l, ", ") .. "}"
  elseif tltype.isVariable(t) then
    str = t[1]
  elseif tltype.isRecursive(t) then
    str = t[1] .. "." .. type2str(t[2])
  elseif tltype.isVoid(t) then
    str = "(void)"
  elseif tltype.isTuple(t) then
    local l:any = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    str = "(" .. table.concat(l, ", ") .. ")"
  elseif tltype.isVararg(t) then
    str = type2str(t[1]) .. "*"
  else
    error("trying to convert type to string but got " .. t.tag)
  end
  return str
end

function tltype.tostring (t:Type):(string)
  return type2str(t)
end

return tltype

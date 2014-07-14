--[[
This module implements Typed Lua tltype.
]]

local tltype = {}

-- literal types

-- Literal : (boolean|number|string) -> (type) 
function tltype.Literal (l)
  return { tag = "Literal", [1] = l }
end

-- False : () -> (type)
function tltype.False ()
  return tltype.Literal(false)
end

-- True : () -> (type)
function tltype.True ()
  return tltype.Literal(true)
end

-- Num : (number) -> (type)
function tltype.Num (n)
  return tltype.Literal(n)
end

-- Str : (string) -> (type)
function tltype.Str (s)
  return tltype.Literal(s)
end

-- isLiteral : (type) -> (boolean)
function tltype.isLiteral (t)
  return t.tag == "Literal"
end

-- isFalse : (type) -> (boolean)
function tltype.isFalse (t)
  return tltype.isLiteral(t) and t[1] == false
end

-- isTrue : (type) -> (boolean)
function tltype.isTrue (t)
  return tltype.isLiteral(t) and t[1] == true
end

-- isNum : (type) -> (boolean)
function tltype.isNum (t)
  return tltype.isLiteral(t) and type(t[1]) == "number"
end

-- isStr : (type) -> (boolean)
function tltype.isStr (t)
  return tltype.isLiteral(t) and type(t[1]) == "string"
end

-- base types

-- Base : ("boolean"|"number"|"string") -> (type)
function tltype.Base (s)
  return { tag = "Base", [1] = s }
end

-- Boolean : () -> (type)
function tltype.Boolean ()
  return tltype.Base("boolean")
end

-- Number : () -> (type)
function tltype.Number ()
  return tltype.Base("number")
end

-- String : () -> (type)
function tltype.String ()
  return tltype.Base("string")
end

-- isBase : (type) -> (boolean)
function tltype.isBase (t)
  return t.tag == "Base"
end

-- nil type

-- Nil : () -> (type)
function tltype.Nil ()
  return { tag = "Nil" }
end

-- isNil : (type) -> (boolean)
function tltype.isNil (t)
  return t.tag == "Nil"
end

-- value type

-- Value : () -> (type)
function tltype.Value (t)
  return { tag = "Value" }
end

-- isValue : (type) -> (boolean)
function tltype.isValue (t)
  return t.tag == "Value"
end

-- dynamic type

-- Any : () -> (type)
function tltype.Any ()
  return { tag = "Any" }
end

-- isAny : (type) -> (boolean)
function tltype.isAny (t)
  return t.tag == "Any"
end

-- self type

-- Self : () -> (type)
function tltype.Self ()
  return { tag = "Self" }
end

-- isSelf : (type) -> (boolean)
function tltype.isSelf (t)
  return t.tag == "Self"
end

-- void type

-- Void : () -> (type)
function tltype.Void ()
  return { tag = "Void" }
end

-- isVoid : (type) -> (boolean)
function tltype.isVoid (t)
  return t.tag == "Void"
end

-- union types

-- Union : (type*) -> (type)
function tltype.Union (...)
  local l = {...}
  local t = { tag = "Union" }
  -- remove unions of unions
  for i = 1, #l do
    if tltype.isUnion(l[i]) then
      for j = 1, #l[i] do
        table.insert(t, l[i][j])
      end
    else
      table.insert(t, l[i])
    end
  end
  if #t == 1 then
    return t[1]
  else
    return t
  end
end

-- isUnion : (type) -> (boolean)
function tltype.isUnion (t)
  return t.tag == "Union"
end

-- UnionNil : (type, true?) -> (type)
function tltype.UnionNil (t, is_union_nil)
  if is_union_nil then
    return tltype.Union(t, tltype.Nil())
  else
    return t
  end
end

-- vararg types

-- Vararg : (type) -> (type)
function tltype.Vararg (t)
  return { tag = "Vararg", [1] = t }
end

-- isVararg : (type) -> (boolean)
function tltype.isVararg (t)
  return t.tag == "Vararg"
end

-- tuple types

-- Tuple : ({number:type}, true?) -> (type)
function tltype.Tuple (l, is_vararg)
  if is_vararg then
    l[#l] = tltype.Vararg(l[#l])
  end
  return { tag = "Tuple", table.unpack(l) }
end

-- inputTuple : (type?, boolean) -> (type)
function tltype.inputTuple (t, strict)
  if not strict then
    if not t then
      return tltype.Tuple({ tltype.Value() }, true)
    else
      if not tltype.isVararg(t[#t]) then
        table.insert(t, tltype.Vararg(tltype.Value()))
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

-- outputTuple : (type?, boolean) -> (type)
function tltype.outputTuple (t, strict)
  if not strict then
    if not t then
      return tltype.Tuple({ tltype.Nil() }, true)
    else
      if not tltype.isVararg(t[#t]) then
        table.insert(t, tltype.Vararg(tltype.Nil()))
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

-- retType : (type, boolean) -> (type)
function tltype.retType (t, strict)
  return tltype.outputTuple(tltype.Tuple({ t }), strict)
end

-- isTuple : (type) -> (boolean)
function tltype.isTuple (t)
  return t.tag == "Tuple"
end

-- union of tuple types

-- Unionlist : (type*) -> (type)
function tltype.Unionlist (...)
  local l = {...}
  local t = { tag = "Unionlist" }
  -- remove unions of unions
  for i = 1, #l do
    if tltype.isUnionlist(l[i]) then
      for j = 1, #l[i] do
        table.insert(t, l[i][j])
      end
    else
      table.insert(t, l[i])
    end
  end
  if #t == 1 then
    return t[1]
  else
    return t
  end
end

-- isUnionlist : (type) -> (boolean)
function tltype.isUnionlist (t)
  return t.tag == "Unionlist"
end

-- UnionlistNil : (type, boolean?) -> (type)
function tltype.UnionlistNil (t, is_union_nil)
  if type(is_union_nil) == "boolean" then
    local u = tltype.Tuple({ tltype.Nil(), tltype.String() })
    return tltype.Unionlist(t, tltype.outputTuple(u, is_union_nil))
  else
    return t
  end
end

-- function types

-- Function : (type, type, true?) -> (type)
function tltype.Function (t1, t2, is_method)
  if is_method then
    if tltype.isVoid(t1) then
      t1 = tltype.Tuple({ tltype.Self() })
    else
      table.insert(t1, 1, tltype.Self())
    end
  end
  return { tag = "Function", [1] = t1, [2] = t2 }
end

function tltype.isFunction (t)
  return t.tag == "Function"
end

-- table types

-- Field : (boolean, type, type) -> (field)
function tltype.Field (is_const, t1, t2)
  if is_const then
    return { tag = "Const", [1] = t1, [2] = t2 }
  else
    return { tag = "Field", [1] = t1, [2] = t2 }
  end
end

-- isField : (field) -> (boolean)
function tltype.isField (f)
  return f.tag == "Field"
end

-- isConstField : (field) -> (boolean)
function tltype.isConstField (f)
  return f.tag == "Const"
end

-- Table : (field*) -> (type)
function tltype.Table (...)
  return { tag = "Table", ... }
end

-- isTable : (type) -> (boolean)
function tltype.isTable (t)
  return t.tag == "Table"
end

-- type variables

-- Variable : (string) -> (type)
function tltype.Variable (name)
  return { tag = "Variable", [1] = name }
end

-- isVariable : (type) -> (boolean)
function tltype.isVariable (t)
  return t.tag == "Variable"
end

-- tostring

-- type2str (type) -> (string)
local function type2str (t)
  print(t.tag)
  if tltype.isLiteral(t) then
    return tostring(t[1])
  elseif tltype.isBase(t) then
    return t[1]
  elseif tltype.isNil(t) then
    return "nil"
  elseif tltype.isValue(t) then
    return "value"
  elseif tltype.isAny(t) then
    return "any"
  elseif tltype.isSelf(t) then
    return "self"
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) then
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    return "(" .. table.concat(l, " | ") .. ")"
  elseif tltype.isFunction(t) then
    return type2str(t[1]) .. " -> " .. type2str(t[2])
  elseif tltype.isTable(t) then
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v[1]) .. ":" .. type2str(v[2])
      if tltype.isConstField(v) then
        l[k] = "const " .. l[k]
      end
    end
    return "{" .. table.concat(l, ", ") .. "}"
  elseif tltype.isTuple(t) then
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    return "(" .. table.concat(l, ", ") .. ")"
  elseif tltype.isVararg(t) then
    return type2str(t[1]) .. "*"
  else
    error("trying to convert type to string but got " .. t.tag)
  end
end

-- tostring : (type) -> (string)
function tltype.tostring (t)
  return type2str(t)
end

return tltype

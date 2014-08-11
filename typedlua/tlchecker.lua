--[[
This file implements Typed Lua type checker
]]

if not table.unpack then table.unpack = unpack end

local tlchecker = {}

local tlast = require "typedlua.tlast"
local tlst = require "typedlua.tlst"
local tltype = require "typedlua.tltype"
local tldparser = require "typedlua.tldparser"

local Value = tltype.Value()
local Any = tltype.Any()
local Nil = tltype.Nil()
local Self = tltype.Self()
local False = tltype.False()
local True = tltype.True()
local Boolean = tltype.Boolean()
local Number = tltype.Number()
local String = tltype.String()

local check_block, check_stm, check_exp, check_var

local function lineno (s, i)
  if i == 1 then return 1, 1 end
  local l, lastline = 0, ""
  s = s:sub(1, i) .. "\n"
  for line in s:gmatch("[^\n]*[\n]") do
    l = l + 1
    lastline = line
  end
  local c = lastline:len() - 1
  return l, c ~= 0 and c or 1
end

function typeerror (env, msg, pos)
  local l, c = lineno(env.subject, pos)
  local error_msg = "%s:%d:%d: type error, %s"
  error_msg = string.format(error_msg, env.filename, l, c, msg)
  table.insert(env.messages, error_msg)
end

local function set_type (node, t)
  node["type"] = t
end

local function get_type (node)
  return node["type"] or Nil
end

local function get_interface (env, name, pos)
  local t = tlst.get_interface(env, name)
  if not t then
    local msg = "type alias '%s' is not defined"
    msg = string.format(msg, name)
    typeerror(env, msg, pos)
    return Nil
  else
    return t
  end
end

local function replace_names (env, t, pos)
  if tltype.isLiteral(t) or
     tltype.isBase(t) or
     tltype.isNil(t) or
     tltype.isValue(t) or
     tltype.isAny(t) or
     tltype.isSelf(t) or
     tltype.isVoid(t) or
     tltype.isRecursive(t) then
    return t
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
    local r = { tag = t.tag }
    for k, v in ipairs(t) do
      r[k] = replace_names(env, t[k], pos)
    end
    return r
  elseif tltype.isFunction(t) then
    t[1] = replace_names(env, t[1], pos)
    t[2] = replace_names(env, t[2], pos)
    return t
  elseif tltype.isTable(t) then
    for k, v in ipairs(t) do
      t[k][2] = replace_names(env, t[k][2], pos)
    end
    return t
  elseif tltype.isVariable(t) then
    return get_interface(env, t[1], pos)
  elseif tltype.isVararg(t) then
    t[1] = replace_names(env, t[1], pos)
    return t
  else
    return t
  end
end

local function check_arith (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local msg = "attempt to perform arithmetic on a '%s'"
  if tltype.subtype(t1, Number) and tltype.subtype(t2, Number) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typeerror(env, msg, exp1.pos)
    end
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t2))
      typeerror(env, msg, exp2.pos)
    end
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = tltype.general(t1), exp1.pos
    if tltype.subtype(t1, Number) or tltype.isAny(t1) then
      wrong_type, wrong_pos = tltype.general(t2), exp2.pos
    end
    msg = string.format(msg, tltype.tostring(wrong_type))
    typeerror(env, msg, wrong_pos)
  end
end

local function check_concat (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local msg = "attempt to concatenate a '%s'"
  if tltype.subtype(t1, String) and tltype.subtype(t2, String) then
    set_type(exp, String)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typeerror(env, msg, exp1.pos)
    end
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t2))
      typeerror(env, msg, exp2.pos)
    end
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = tltype.general(t1), exp1.pos
    if tltype.subtype(t1, String) or tltype.isAny(t1) then
      wrong_type, wrong_pos = tltype.general(t2), exp2.pos
    end
    msg = string.format(msg, tltype.tostring(wrong_type))
    typeerror(env, msg, wrong_pos)
  end
end

local function check_equal (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  set_type(exp, Boolean)
end

local function check_order (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local msg = "attempt to compare '%s' with '%s'"
  if tltype.subtype(t1, Number) and tltype.subtype(t2, Number) then
    set_type(exp, Boolean)
  elseif tltype.subtype(t1, String) and tltype.subtype(t2, String) then
    set_type(exp, Boolean)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, msg, exp1.pos)
    end
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, msg, exp2.pos)
    end
  else
    set_type(exp, Any)
    t1, t2 = tltype.general(t1), tltype.general(t2)
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, msg, exp.pos)
  end
end

local function check_and (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  if tltype.isNil(t1) or tltype.isFalse(t1) then
    set_type(exp, t1)
  elseif tltype.isUnion(t1, Nil) then
    set_type(exp, tltype.Union(t2, Nil))
  elseif tltype.isUnion(t1, False) then
    set_type(exp, tltype.Union(t2, False))
  else
    set_type(exp, tltype.Union(t1, t2))
  end
end

local function check_or (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  if tltype.isNil(t1) or tltype.isFalse(t1) then
    set_type(exp, t2)
  elseif tltype.isUnion(t1, Nil) then
    set_type(exp, tltype.Union(tltype.filterUnion(t1, Nil), t2))
  elseif tltype.isUnion(t1, False) then
    set_type(exp, tltype.Union(tltype.filterUnion(t1, False), t2))
  else
    set_type(exp, tltype.Union(t1, t2))
  end
end

local function check_binary_op (env, exp)
  local op = exp[1]
  if op == "add" or op == "sub" or
     op == "mul" or op == "div" or op == "mod" or
     op == "pow" then
    check_arith(env, exp)
  elseif op == "concat" then
    check_concat(env, exp)
  elseif op == "eq" then
    check_equal(env, exp)
  elseif op == "lt" or op == "le" then
    check_order(env, exp)
  elseif op == "and" then
    check_and(env, exp)
  elseif op == "or" then
    check_or(env, exp)
  else
    error("cannot type check binary operator " .. op)
  end
end

local function check_not (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  set_type(exp, Boolean)
end

local function check_minus (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  local t1 = tltype.first(get_type(exp1))
  local msg = "attempt to perform arithmetic on a '%s'"
  if tltype.subtype(t1, Number) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typeerror(env, msg, exp1.pos)
    end
  else
    set_type(exp, Any)
    t1 = tltype.general(t1)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, msg, exp1.pos)
  end
end

local function check_len (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  local t1 = tltype.first(get_type(exp1))
  local msg = "attempt to get length of a '%s'"
  if tltype.subtype(t1, String) or
     tltype.subtype(t1, tltype.Table(tltype.Field(false, Number, Value))) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typeerror(env, msg, exp1.pos)
    end
  else
    set_type(exp, Any)
    t1 = tltype.general(t1)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, msg, exp1.pos)
  end
end

local function check_unary_op (env, exp)
  local op = exp[1]
  if op == "not" then
    check_not(env, exp)
  elseif op == "unm" then
    check_minus(env, exp)
  elseif op == "len" then
    check_len(env, exp)
  else
    error("cannot type check unary operator " .. op)
  end
end

local function check_op (env, exp)
  if exp[3] then
    check_binary_op(env, exp)
  else
    check_unary_op(env, exp)
  end
end

local function check_paren (env, exp)
  local exp1 = exp[1]
  check_exp(env, exp1)
  local t1 = get_type(exp1)
  set_type(exp, tltype.first(t1))
end

local function check_parameters (env, parlist)
  local len = #parlist
  if len == 0 then
    if env.strict then
      return tltype.Void()
    else
      return tltype.Tuple({ Value }, true)
    end
  else
    local l = {}
    for i = 1, len do
      if not parlist[i][2] then parlist[i][2] = Any end
      l[i] = parlist[i][2]
    end
    if parlist[len].tag == "Dots" then
      tlst.set_vararg(env, parlist[len][2])
      return tltype.Tuple(l, true)
    else
      if env.strict then
        return tltype.Tuple(l)
      else
        l[len + 1] = Value
        return tltype.Tuple(l, true)
      end
    end
  end
end

local function check_explist (env, explist)
  for k, v in ipairs(explist) do
    check_exp(env, v)
  end
end

local function infer_return_type (env)
  local l = tlst.get_return_type(env)
  if #l == 0 then
    if env.strict then
      return tltype.Void()
    else
      return tltype.Tuple({ Nil }, true)
    end
  else
    return tltype.Unionlist(table.unpack(l))
  end
end

local function check_function (env, exp)
  local idlist, ret_type, block = exp[1], exp[2], exp[3]
  if not block then
    block = ret_type
    ret_type = tltype.Tuple({ Any }, true)
  end
  tlst.begin_function(env)
  tlst.begin_scope(env)
  local input_type = check_parameters(env, idlist)
  local t = tltype.Function(input_type, ret_type)
  for k, v in ipairs(idlist) do
    set_type(v, v[2])
    tlst.set_local(env, v)
  end
  check_block(env, block)
  tlst.end_scope(env)
  ret_type = infer_return_type(env)
  t = tltype.Function(input_type, ret_type)
  tlst.end_function(env)
  set_type(exp, t)
end

local function check_table (env, exp)
  local l = {}
  local i = 1
  for k, v in ipairs(exp) do
    local tag = v.tag
    local t1, t2
    if tag == "Pair" then
      local exp1, exp2 = v[1], v[2]
      check_exp(env, exp1)
      check_exp(env, exp2)
      t1, t2 = get_type(exp1), tltype.general(get_type(exp2))
    else
      local exp1 = v
      check_exp(env, exp1)
      t1, t2 = tltype.Literal(i), tltype.general(get_type(exp1))
      i = i + 1
    end
    l[k] = tltype.Field(v.const, t1, t2)
  end
  local t = tltype.Table(table.unpack(l))
  t.open = true
  set_type(exp, t)
end

local function var2name (var)
  local tag = var.tag
  if tag == "Id" then
    return string.format("local '%s'", var[1])
  elseif tag == "Index" then
    if var[1].tag == "Id" and var[1][1] == "_ENV" and var[2].tag == "String" then
      return string.format("global '%s'", var[2][1])
    else
      return string.format("field '%s'", var[2][1])
    end
  end
end

local function explist2typegen (explist)
  local len = #explist
  return function (i)
    if i <= len then
      local t = get_type(explist[i])
      return tltype.first(t)
    else
      local t
      if len == 0 then t = Nil else t = get_type(explist[len]) end
      if tltype.isVararg(t) then
        return tltype.first(t)
      else
        return Nil
      end
    end
  end
end

local function explist2type (explist)
  local len = #explist
  if len == 0 then
    return tltype.Tuple({ Nil }, true)
  else
    local l = {}
    for i = 1, len do
      l[i] = tltype.first(get_type(explist[i]))
    end
    if not tltype.isVararg(explist[len]) then
      l[len + 1] = Nil
    end
    return tltype.Tuple(l, true)
  end
end

local function check_arguments (env, func_name, dec_type, infer_type, pos)
  local msg = "attempt to pass '%s' to %s of input type '%s'"
  infer_type = replace_names(env, infer_type, pos)
  if tltype.subtype(infer_type, dec_type) then
  elseif tltype.consistent_subtype(infer_type, dec_type) then
    if env.warnings then
      msg = string.format(msg, tltype.tostring(infer_type), func_name, tltype.tostring(dec_type))
      typeerror(env, msg, pos)
    end
  else
    msg = string.format(msg, tltype.tostring(infer_type), func_name, tltype.tostring(dec_type))
    typeerror(env, msg, pos)
  end
end

local function check_call (env, exp)
  local exp1 = exp[1]
  local explist = {}
  for i = 2, #exp do
    explist[i - 1] = exp[i]
  end
  check_exp(env, exp1)
  check_explist(env, explist)
  local t = get_type(exp1)
  local inferred_type = explist2type(explist)
  local msg = "attempt to call %s of type '%s'"
  if tltype.isFunction(t) then
    check_arguments(env, var2name(exp1), t[1], inferred_type, exp.pos)
    set_type(exp, t[2])
  elseif tltype.isAny(t) then
    if env.warnings then
      msg = string.format(msg, var2name(exp1), tltype.tostring(t))
      typeerror(env, msg, exp.pos)
    end
    set_type(exp, Any)
  else
    msg = string.format(msg, var2name(exp1), tltype.tostring(t))
    typeerror(env, msg, exp.pos)
    set_type(exp, Nil)
  end
end

local function check_invoke (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  local explist = {}
  for i = 3, #exp do
    explist[i - 2] = exp[i]
  end
  check_exp(env, exp1)
  check_exp(env, exp2)
  check_explist(env, explist)
  local t = get_type(exp1)
  local msg = "attempt to call method '%s' of type '%s'"
  if tltype.isFunction(t) then
    set_type(exp, t[2])
  elseif tltype.isAny(t) then
    if env.warnings then
      msg = string.format(msg, exp2[1], tltype.tostring(t))
      typeerror(env, msg, exp.pos)
    end
    set_type(exp, Any)
  else
    msg = string.format(msg, exp2[1], tltype.tostring(t))
    typeerror(env, msg, exp.pos)
    set_type(exp, Nil)
  end
end

local function check_local_var (env, id, inferred_type, close_local)
  local local_name, local_type, pos = id[1], id[2], id.pos
  inferred_type = replace_names(env, inferred_type, pos)
  if not local_type then
    if tltype.isNil(inferred_type) then
      local_type = Any
    else
      local_type = tltype.general(inferred_type)
      if close_local then local_type.open = nil end
    end
  else
    local_type = replace_names(env, local_type, pos)
    local msg = "attempt to assign '%s' to '%s'"
    msg = string.format(msg, tltype.tostring(inferred_type), tltype.tostring(local_type))
    if tltype.subtype(inferred_type, local_type) then
    elseif tltype.consistent_subtype(inferred_type, local_type) then
      if env.warnings then
       typeerror(env, msg, pos)
      end
    else
      typeerror(env, msg, pos)
    end
  end
  set_type(id, local_type)
  tlst.set_local(env, id)
end

local function check_local (env, idlist, explist)
  check_explist(env, explist)
  local tuple = explist2typegen(explist)
  for k, v in ipairs(idlist) do
    local t = tuple(k)
    local close_local = explist[k] and explist[k].tag == "Id" and tltype.isTable(t)
    check_local_var(env, v, t, close_local)
  end
end

local function check_localrec (env, id, exp)
  local idlist, ret_type, block = exp[1], exp[2], exp[3]
  if not block then
    block = ret_type
    ret_type = tltype.Tuple({ Any }, true)
  end
  tlst.begin_function(env)
  local input_type = check_parameters(env, idlist)
  local t = tltype.Function(input_type, ret_type)
  t = replace_names(env, t, exp.pos)
  id[2] = t
  set_type(id, t)
  tlst.set_local(env, id)
  tlst.begin_scope(env)
  for k, v in ipairs(idlist) do
    v[2] = replace_names(env, v[2], exp.pos)
    set_type(v, v[2])
    tlst.set_local(env, v)
  end
  check_block(env, block)
  tlst.end_scope(env)
  ret_type = infer_return_type(env)
  t = tltype.Function(input_type, ret_type)
  t = replace_names(env, t, exp.pos)
  id[2] = t
  set_type(id, t)
  tlst.set_local(env, id)
  set_type(exp, t)
  tlst.end_function(env)
end

local function explist2typelist (explist)
  local len = #explist
  if len == 0 then
    return tltype.Tuple({ Nil }, true)
  else
    local l = {}
    for i = 1, len - 1 do
      table.insert(l, tltype.first(get_type(explist[i])))
    end
    local last_type = get_type(explist[len])
    if tltype.isTuple(last_type) then
      for k, v in ipairs(last_type) do
        table.insert(l, v)
      end
    else
      table.insert(l, last_type)
    end
    if not tltype.isVararg(last_type) then
      table.insert(l, tltype.Vararg(Nil))
    end
    return tltype.Tuple(l)
  end
end

local function check_return (env, stm)
  check_explist(env, stm)
  local t = explist2typelist(stm)
  tlst.set_return_type(env, tltype.general(t))
end

local function check_assignment (env, varlist, explist)
  check_explist(env, explist)
  --check_explist(env, varlist)
  for k, v in ipairs(varlist) do
    check_var(env, v, explist[k])
  end
  local var_type, exp_type = explist2typelist(varlist), explist2typelist(explist)
  local msg = "attempt to assign '%s' to '%s'"
  if tltype.subtype(exp_type, var_type) then
  elseif tltype.consistent_subtype(exp_type, var_type) then
    if env.warnings then
      msg = string.format(msg, tltype.tostring(exp_type), tltype.tostring(var_type))
      typeerror(env, msg, varlist[1].pos)
    end
  else
    msg = string.format(msg, tltype.tostring(exp_type), tltype.tostring(var_type))
    typeerror(env, msg, varlist[1].pos)
  end
  for k, v in ipairs(varlist) do
    local tag = v.tag
    if tag == "Id" then
      local name = v[1]
      local l = tlst.get_local(env, name)
      local exp = explist[k]
      if exp and exp.tag == "Op" and exp[1] == "or" and
         exp[2].tag == "Id" and exp[2][1] == name and not l.assigned then
        local t1, t2 = get_type(exp), get_type(l)
        if tltype.subtype(t1, t2) then
          l.bkp = t2
          set_type(l, t1)
        end
      end
      l.assigned = true
    elseif tag == "Index" then
    end
  end
end

local function check_while (env, stm)
  local exp1, stm1 = stm[1], stm[2]
  check_exp(env, exp1)
  check_block(env, stm1)
end

local function check_repeat (env, stm)
  local stm1, exp1 = stm[1], stm[2]
  check_block(env, stm1)
  check_exp(env, exp1)
end

local function tag2type (t)
  if tltype.isLiteral(t) then
    local tag = t[1]
    if tag == "nil" then
      return Nil
    elseif tag == "boolean" then
      return Boolean
    elseif tag == "number" then
      return Number
    elseif tag == "string" then
      return String
    else
      return t
    end
  else
    return t
  end
end

local function check_if (env, stm)
  local l = {}
  for i = 1, #stm, 2 do
    local exp, block = stm[i], stm[i + 1]
    if block then
      check_exp(env, exp)
      if exp.tag == "Id" then
        local name = exp[1]
        l[name] = tlst.get_local(env, name)
        if not l[name].bkp then l[name].bkp = get_type(l[name]) end
        l[name].filter = Nil
        set_type(l[name], tltype.filterUnion(get_type(l[name]), Nil))
      elseif exp.tag == "Op" and exp[1] == "not" and exp[2].tag == "Id" then
        local name = exp[2][1]
        l[name] = tlst.get_local(env, name)
        if not l[name].bkp then l[name].bkp = get_type(l[name]) end
        if not l[name].filter then
          l[name].filter = tltype.filterUnion(get_type(l[name]), Nil)
        else
          l[name].filter = tltype.filterUnion(l[name].filter, Nil)
        end
        set_type(l[name], Nil)
      elseif exp.tag == "Op" and exp[1] == "eq" and
             exp[2].tag == "Call" and exp[2][1].tag == "Index" and
             exp[2][1][1].tag == "Id" and exp[2][1][1][1] == "_ENV" and
             exp[2][1][2].tag == "String" and exp[2][1][2][1] == "type" and
             exp[2][2].tag == "Id" then
        local name = exp[2][2][1]
        l[name] = tlst.get_local(env, name)
        local t = tag2type(get_type(exp[3]))
        if not l[name].bkp then l[name].bkp = get_type(l[name]) end
        if not l[name].filter then
          l[name].filter = tltype.filterUnion(get_type(l[name]), t)
        else
          l[name].filter = tltype.filterUnion(l[name].filter, t)
        end
        set_type(l[name], t)
      elseif exp.tag == "Op" and exp[1] == "not" and
             exp[2].tag == "Op" and exp[2][1] == "eq" and
             exp[2][2].tag == "Call" and exp[2][2][1].tag == "Index" and
             exp[2][2][1][1].tag == "Id" and exp[2][2][1][1][1] == "_ENV" and
             exp[2][2][1][2].tag == "String" and exp[2][2][1][2][1] == "type" and
             exp[2][2][2].tag == "Id" then
        local name = exp[2][2][2][1]
        l[name] = tlst.get_local(env, name)
        local t = tag2type(get_type(exp[2][3]))
        if not l[name].bkp then l[name].bkp = get_type(l[name]) end
        l[name].filter = t
        set_type(l[name], tltype.filterUnion(get_type(l[name]), t))
      end
    else
      block = exp
    end
    check_block(env, block)
    for k, v in pairs(l) do
      set_type(v, v.filter)
    end
  end
  for k, v in pairs(l) do
    set_type(v, v.bkp)
  end
end

local function check_fornum (env, stm)
  local id, exp1, exp2, exp3, block = stm[1], stm[2], stm[3], stm[4], stm[5]
  id[2] = Number
  set_type(id, Number)
  tlst.begin_scope(env)
  tlst.set_local(env, id)
  check_exp(env, exp1)
  local t = get_type(exp1)
  local msg = "'for' initial value must be a number"
  if tltype.subtype(t, Number) then
  elseif tltype.consistent_subtype(t, Number) then
    if env.warnings then typeerror(env, msg, exp1.pos) end
  else
    typeerror(env, msg, exp1.pos)
  end
  check_exp(env, exp2)
  t = get_type(exp2)
  msg = "'for' limit must be a number"
  if tltype.subtype(t, Number) then
  elseif tltype.consistent_subtype(t, Number) then
    if env.warnings then typeerror(env, msg, exp2.pos) end
  else
    typeerror(env, msg, exp2.pos)
  end
  if block then
    check_exp(env, exp3)
    t = get_type(exp3)
    msg = "'for' step must be a number"
    if tltype.subtype(t, Number) then
    elseif tltype.consistent_subtype(t, Number) then
      if env.warnings then typeerror(env, msg, exp3.pos) end
    else
      typeerror(env, msg, exp3.pos)
    end
  else
    block = exp3
  end
  check_block(env, block)
  tlst.end_scope(env)
end

local function check_forin (env, idlist, explist, block)
  tlst.begin_scope(env)
  check_local(env, idlist, explist)
  check_block(env, block)
  tlst.end_scope(env)
end

local function check_id (env, exp)
  local name = exp[1]
  local l = tlst.get_local(env, name)
  set_type(exp, get_type(l))
end

local function check_index (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = get_type(exp1), get_type(exp2)
  local msg = "attempt to index '%s' with '%s'"
  if tltype.isTable(t1) then
    local field_type = tltype.getField(t2, t1)
    if not tltype.isNil(field_type) then
      set_type(exp, field_type)
    else
      if exp1.tag == "Id" and exp1[1] == "_ENV" and exp2.tag == "String" then
        msg = "attempt to access undeclared global '%s'"
        msg = string.format(msg, exp2[1])
      else
        msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      end
      typeerror(env, msg, exp.pos)
      set_type(exp, Nil)
    end
  elseif tltype.isAny(t1) then
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, msg, exp.pos)
    end
    set_type(exp, Nil)
  else
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, msg, exp.pos)
    set_type(exp, Nil)
  end
end

local function check_interface (env, stm)
  local name, t, is_local = stm[1], stm[2], stm.is_local
  if tlst.get_interface(env, name) then
    local msg = "attempt to redeclare interface '%s'"
    msg = string.format(msg, name)
    typeerror(env, msg, stm.pos)
  else
    tlst.set_interface(env, name, t, is_local)
  end
end

function check_var1 (env, var)
  local tag = var.tag
  if tag == "Id" then
    local name = var[1]
    local l = tlst.get_local(env, name)
    set_type(var, get_type(l))
  elseif tag == "Index" then
    local exp1, exp2 = var[1], var[2]
    check_exp(env, exp1)
    check_exp(env, exp2)
    local t1, t2 = get_type(exp1), get_type(exp2)
    local msg = "attempt to index '%s' with '%s'"
    if tltype.isTable(t1) then
      local field_type = tltype.getField(t2, t1)
      if not tltype.isNil(field_type) then
        set_type(var, field_type)
      else
        if exp1.tag == "Id" and exp1[1] == "_ENV" and exp2.tag == "String" then
          msg = "attempt to access undeclared global '%s'"
          msg = string.format(msg, exp2[1])
        else
          msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
        end
        typeerror(env, msg, var.pos)
        set_type(var, Nil)
      end
    elseif tltype.isAny(t1) then
      if env.warnings then
        msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
        typeerror(env, msg, var.pos)
      end
      set_type(var, Any)
    else
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, msg, var.pos)
      set_type(var, Nil)
    end
  end
end

function check_var (env, var, exp)
  local tag = var.tag
  if tag == "Id" then
    local name = var[1]
    local l = tlst.get_local(env, name)
    set_type(var, get_type(l))
  elseif tag == "Index" then
    local exp1, exp2 = var[1], var[2]
    check_exp(env, exp1)
    check_exp(env, exp2)
    local t1, t2 = get_type(exp1), get_type(exp2)
    local msg = "attmept to index '%s' with '%s'"
    if tltype.isTable(t1) then
      local field_type = tltype.getField(t2, t1)
      if not tltype.isNil(field_type) then
        set_type(var, field_type)
      else
        if t1.open then
          if exp then
            local t3 = tltype.general(get_type(exp))
            local t = tltype.general(t1)
            table.insert(t, tltype.Field(var.const, t2, t3))
            if tltype.subtype(t, t1) then
              table.insert(t1, tltype.Field(var.const, t2, t3))
            end
            set_type(var, t3)
          else
            set_type(var, Nil)
          end
        else
          if exp1.tag == "Id" and exp1[1] == "_ENV" and exp2.tag == "String" then
            msg = "attempt to access undeclared global '%s'"
            msg = string.format(msg, exp2[1])
          else
            msg = "attempt to use '%s' to index closed table"
            msg = string.format(msg, tltype.tostring(t2))
          end
          typeerror(env, msg, var.pos)
          set_type(var, Nil)
        end
      end
    elseif tltype.isAny(t1) then
      if env.warnings then
        msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
        typeerror(env, msg, var.pos)
      end
      set_type(var, Any)
    else
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, msg, var.pos)
      set_type(var, Nil)
    end
  else
    error("cannot type check variable " .. tag)
  end
end

function check_exp (env, exp)
  local tag = exp.tag
  if tag == "Nil" then
    set_type(exp, Nil)
  elseif tag == "Dots" then
    set_type(exp, tltype.Vararg(tlst.get_vararg(env)))
  elseif tag == "True" then
    set_type(exp, True)
  elseif tag == "False" then
    set_type(exp, False)
  elseif tag == "Number" then
    set_type(exp, tltype.Literal(exp[1]))
  elseif tag == "String" then
    set_type(exp, tltype.Literal(exp[1]))
  elseif tag == "Function" then
    check_function(env, exp)
  elseif tag == "Table" then
    check_table(env, exp)
  elseif tag == "Op" then
    check_op(env, exp)
  elseif tag == "Paren" then
    check_paren(env, exp)
  elseif tag == "Call" then
    check_call(env, exp)
  elseif tag == "Invoke" then
    check_invoke(env, exp)
  elseif tag == "Id" then
    check_id(env, exp)
  elseif tag == "Index" then
    check_index(env, exp)
  else
    error("cannot type check expression " .. tag)
  end
end

function check_stm (env, stm)
  local tag = stm.tag
  if tag == "Do" then
    check_block(env, stm)
  elseif tag == "Set" then
    check_assignment(env, stm[1], stm[2])
  elseif tag == "While" then
    check_while(env, stm)
  elseif tag == "Repeat" then
    check_repeat(env, stm)
  elseif tag == "If" then
    check_if(env, stm)
  elseif tag == "Fornum" then
    check_fornum(env, stm)
  elseif tag == "Forin" then
    check_forin(env, stm[1], stm[2], stm[3])
  elseif tag == "Local" then
    check_local(env, stm[1], stm[2])
  elseif tag == "Localrec" then
    check_localrec(env, stm[1][1], stm[2][1])
  elseif tag == "Goto" then
  elseif tag == "Label" then
  elseif tag == "Return" then
    check_return(env, stm)
  elseif tag == "Break" then
  elseif tag == "Call" then
    check_call(env, stm)
  elseif tag == "Invoke" then
    check_invoke(env, stm)
  elseif tag == "Interface" then
    check_interface(env, stm)
  else
    error("cannot type check statement " .. tag)
  end
end

function check_block (env, block)
  tlst.begin_scope(env)
  for k, v in ipairs(block) do
    check_stm(env, v, strict, warnings)
  end
  tlst.end_scope(env)
end

local function searchpath (name, path)
  if package.searchpath then
    return package.searchpath("typedlua/lsl", path)
  else
    local error_msg = ""
    for tldpath in string.gmatch(path, "([^;]*);") do
      tldpath = string.gsub(tldpath, "?", name)
      local f = io.open(tldpath, "r")
      if f then
        f:close()
        return tldpath
      else
        error_msg = error_msg .. string.format("no file '%s'\n", tldpath)
      end
    end
    return nil, error_msg
  end
end

function tlchecker.typecheck (ast, subject, filename, strict, warnings)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  assert(type(strict) == "boolean")
  assert(type(warnings) == "boolean")
  local env = tlst.new_env(subject, filename, strict, warnings)
  tlst.begin_function(env)
  tlst.begin_scope(env)
  tlst.set_vararg(env, String)
  local tldpath = string.gsub(package.path, "[.]lua", ".tld")
  local lslpath = assert(searchpath("typedlua/lsl", tldpath))
  local _env = tlast.ident(0, "_ENV", tldparser.parse(lslpath, strict))
  _env[2].open = true
  set_type(_env, _env[2])
  tlst.set_local(env, _env)
  for k, v in ipairs(ast) do
    check_stm(env, v)
  end
  tlst.end_scope(env)
  tlst.end_function(env)
  if #env.messages > 0 then
    return ast, table.concat(env.messages, "\n")
  else
    return ast
  end
end

return tlchecker

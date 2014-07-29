--[[
This file implements Typed Lua type checker
]]

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

local check_block, check_stm, check_exp

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

local function check_arith (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = get_type(exp1), get_type(exp2)
  local msg = "attempt to perform arithmetic on a '%s'"
  if tltype.subtype(t1, Number) and tltype.subtype(t2, Number) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typerror(env, msg, exp1.pos)
    end
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t2))
      typerror(env, msg, exp2.pos)
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
  local t1, t2 = get_type(exp1), get_type(exp2)
  local msg = "attempt to concatenate a '%s'"
  if tltype.subtype(t1, String) and tltype.subtype(t2, String) then
    set_type(exp, String)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typerror(env, msg, exp1.pos)
    end
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t2))
      typerror(env, msg, exp2.pos)
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
  local t1, t2 = get_type(exp1), get_type(exp2)
  local msg = "attempt to compare '%s' with '%s'"
  if tltype.subtype(t1, Number) and tltype.subtype(t2, Number) then
    set_type(exp, Boolean)
  elseif tltype.subtype(t1, String) and tltype.subtype(t2, String) then
    set_type(exp, Boolean)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typerror(env, msg, exp1.pos)
    end
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typerror(env, msg, exp2.pos)
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
  local t1, t2 = get_type(exp1), get_type(exp2)
  set_type(exp, tltype.Union(t1, t2))
end

local function check_or (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = get_type(exp1), get_type(exp2)
  set_type(exp, tltype.Union(t1, t2))
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
  local t1 = get_type(exp1)
  local msg = "attempt to perform arithmetic on a '%s'"
  if tltype.subtype(t1, Number) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typerror(env, msg, exp1.pos)
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
  local t1 = general(exp1)
  local msg = "attempt to get length of a '%s'"
  if tltype.subtype(t1, String) or
     tltype.subtype(t1, tltype.Table(tltype.Field(false, Number, Value))) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    if env.warnings then
      msg = string.format(msg, tltype.tostring(t1))
      typerror(env, msg, exp1.pos)
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
  set_type(env, t1)
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
  elseif tag == "Table" then
  elseif tag == "Op" then
    check_op(env, exp)
  elseif tag == "Paren" then
    check_paren(env, exp)
  elseif tag == "Call" then
  elseif tag == "Invoke" then
  elseif tag == "Id" then
  elseif tag == "Index" then
  else
    error("cannot type check expression " .. tag)
  end
end

local function check_explist (env, explist)
  for k, v in ipairs(explist) do
    check_exp(env, v)
  end
end

local function explist2typelist (explist)
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

local function check_local_var (env, id, inferred_type)
  local local_name, local_type, pos = id[1], id[2], id.pos
  if not local_type then
    if tltype.isNil(inferred_type) then
      local_type = Any
    else
      local_type = tltype.general(inferred_type)
    end
  else
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
  local tuple = explist2typelist(explist)
  for k, v in ipairs(idlist) do
    local t = tuple(k)
    check_local_var(env, v, t)
  end
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

local function check_localrec (env, id, exp)
  local idlist, ret_type, block = exp[1], exp[2], exp[3]
  if not block then
    block = ret_type
    ret_type = tltype.Tuple({ Any }, true)
  end
  tlst.begin_function(env)
  local input_type = check_parameters(env, idlist)
  local t = tltype.Function(input_type, ret_type)
  id[2] = t
  set_type(id, t)
  tlst.set_local(env, id)
  tlst.begin_scope(env)
  for k, v in ipairs(idlist) do
    tlst.set_local(env, v)
  end
  check_block(env, block)
  tlst.end_scope(env)
  tlst.end_function(env)
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

local function check_return (env, stm)
  check_explist(env, stm)
  local t = explist2type(stm)
  tlst.set_return_type(env, t)
end

local function check_var (env, var)
  local tag = var.tag
  if tag == "Id" then
    local name = var[1]
    local l = tlst.get_local(env, name)
    set_type(var, get_type(l))
  elseif tag == "Index" then
    local exp1, exp2 = var[1], var[2]
    check_exp(env, exp1)
    check_exp(env, exp2)
  end
end

local function check_assignment (env, varlist, explist)
  check_explist(env, explist)
  local tuple = explist2typelist(explist)
  for k, v in ipairs(varlist) do
    check_var(env, v)
    local var_type, exp_type = get_type(v), tuple(k)
    local msg = "attempt to assign '%s' to '%s'"
    msg = string.format(msg, tltype.tostring(exp_type), tltype.tostring(var_type))
    if tltype.subtype(exp_type, var_type) then
    elseif tltype.consistent_subtype(exp_type, var_type) then
      if env.warnings then
       typeerror(env, msg, v.pos)
      end
    else
      typeerror(env, msg, v.pos)
    end
  end
end

function check_stm (env, stm)
  local tag = stm.tag
  if tag == "Do" then
    check_block(env, stm)
  elseif tag == "Set" then
    check_assignment(env, stm[1], stm[2])
  elseif tag == "While" then
  elseif tag == "Repeat" then
  elseif tag == "If" then
  elseif tag == "Fornum" then
  elseif tag == "Forin" then
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
  elseif tag == "Invoke" then
  elseif tag == "Interface" then
  elseif tag == "LocalInterface" then
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
  local _env = tlast.ident(0, "_ENV", assert(tldparser.parse("typedlua/lsl.tld", strict)))
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

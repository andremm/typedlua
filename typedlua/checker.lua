--[[
This file implements the type checker for Typed Lua
]]

local scope = require "typedlua.scope"
local types = require "typedlua.types"

local lineno = scope.lineno
local begin_scope, end_scope = scope.begin_scope, scope.end_scope
local begin_function, end_function = scope.begin_function, scope.end_function

local Any = types.Any
local Nil = types.Nil
local False = types.False
local True = types.True
local Boolean = types.Boolean
local Number = types.Number
local String = types.String

local checker = {}

local check_block, check_stm, check_exp

local function errormsg (env, pos)
  local l,c = lineno(env.subject, pos)
  return string.format("%s:%d:%d:", env.filename, l, c)
end

local function typeerror (env, msg, pos)
  local error_msg = "%s type error, %s"
  error_msg = string.format(error_msg, errormsg(env, pos), msg)
  table.insert(env.messages, error_msg)
end

local function warning (env, msg, pos)
  local error_msg = "%s warning, %s"
  error_msg = string.format(error_msg, errormsg(env, pos), msg)
  table.insert(env.messages, error_msg)
end

local function type2str (t)
  return types.tostring(t)
end

local function get_local (env, name)
  local scope = env.scope
  for s = scope, 0, -1 do
    if env[s]["local"][name] then
      return true
    end
  end
  return nil
end

local function set_local (env, id, inferred_type, scope)
  local local_name, local_type, pos = id[1], id[2], id.pos
  if not local_type then
    if not inferred_type or types.isNil(inferred_type) then
      local_type = Any
    else
      local_type = types.supertypeof(inferred_type)
    end
  end
  if types.subtype(inferred_type, local_type) then
  elseif types.consistent_subtype(inferred_type, local_type) then
    local msg = "attempt to assign '%s' to '%s'"
    msg = string.format(msg, type2str(inferred_type), type2str(local_type))
    warning(env, msg, pos)
  else
    local msg = "attempt to assign '%s' to '%s'"
    msg = string.format(msg, type2str(inferred_type), type2str(local_type))
    typeerror(env, msg, pos)
  end
  env[scope]["local"][local_name] = local_type
end

local function set_type (node, t)
  node["type"] = t
end

local function check_explist (env, explist)
  for k, v in ipairs(explist) do
    check_exp(env, v)
  end
end

local function check_local (env, idlist, explist)
  check_explist(env, explist)
  for k, v in ipairs(idlist) do
    local t = Nil
    if explist[k] then
      t = explist[k]["type"]
    end
    set_local(env, v, t, env.scope)
  end
end

local function check_arith (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  local msg = "attempt to perform arithmetic on a '%s'"
  if types.subtype(t1, Number) and types.subtype(t2, Number) then
    set_type(exp, Number)
  elseif types.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t1))
    warning(env, msg, exp1.pos)
  elseif types.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t2))
    warning(env, msg, exp2.pos)
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = types.supertypeof(t1), exp1.pos
    if types.subtype(t1, Number) or types.isAny(t1) then
      wrong_type = types.supertypeof(t2)
      wrong_pos = exp2.pos
    end
    msg = string.format(msg, type2str(wrong_type))
    typeerror(env, msg, wrong_pos)
  end
end

local function check_concat (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  local msg = "attempt to concatenate a '%s'"
  if types.subtype(t1, String) and types.subtype(t2, String) then
    set_type(exp, String)
  elseif types.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t1))
    warning(env, msg, exp1.pos)
  elseif types.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t2))
    warning(env, msg, exp2.pos)
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = types.supertypeof(t1), exp1.pos
    if types.subtype(t1, String) or types.isAny(t1) then
      wrong_type = types.supertypeof(t2)
      wrong_pos = exp2.pos
    end
    msg = string.format(msg, type2str(wrong_type))
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
  local t1, t2 = exp1["type"], exp2["type"]
  local msg = "attempt to compare '%s' with '%s'"
  if types.subtype(t1, Number) and types.subtype(t2, Number) then
    set_type(exp, Boolean)
  elseif types.subtype(t1, String) and types.subtype(t2, String) then
    set_type(exp, Boolean)
  elseif types.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t1), types.supertypeof(t2))
    warning(env, msg, exp1.pos)
  elseif types.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(types.supertypeof(t1)), type2str(t2))
    warning(env, msg, exp2.pos)
  else
    set_type(exp, Any)
    t1, t2 = types.supertypeof(t1), types.supertypeof(t2)
    msg = string.format(msg, type2str(t1), type2str(t2))
    typeerror(env, msg, exp.pos)
  end
end

local function check_and (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  if types.isNil(t1) or types.isFalse(t1) then
    set_type(exp, t1)
  else
    set_type(exp, types.Union(t1, t2))
  end
end

local function check_or (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  if types.isNil(t1) or types.isFalse(t1) then
    set_type(exp, t2)
  else
    set_type(exp, types.Union(t1, t2))
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
  local t1 = exp1["type"]
  local msg = "attempt to perform arithmetic on a '%s'"
  if types.subtype(t1, Number) then
    set_type(exp, Number)
  elseif types.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t1))
    warning(env, msg, exp1.pos)
  else
    set_type(exp, Any)
    msg = string.format(msg, type2str(types.supertypeof(t1)))
    typeerror(env, msg, exp1.pos)
  end
end

local function check_len (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  local t1 = exp1["type"]
  local msg = "attempt to get length of a '%s' value"
  if types.subtype(t1, String) then
    set_type(exp, Number)
  elseif types.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, type2str(t1))
    warning(env, msg, exp1.pos)
  else
    set_type(exp, Any)
    msg = string.format(msg, type2str(types.supertypeof(t1)))
    typeerror(env, msg, exp1.pos)
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
    error("expecting binary operator, but got " .. op)
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
    error("expecting unary operator, but got " .. op)
  end
end

local function check_paren (env, exp)
  check_exp(env, exp[1])
  set_type(exp, exp[1]["type"])
end

function check_exp (env, exp)
  local tag = exp.tag
  if tag == "Nil" then
    set_type(exp, Nil)
  elseif tag == "Dots" then
    set_type(exp, Any)
  elseif tag == "True" then
    set_type(exp, True)
  elseif tag == "False" then
    set_type(exp, False)
  elseif tag == "Number" then -- `Number{ <number> }
    set_type(exp, types.Literal(exp[1]))
  elseif tag == "String" then -- `String{ <string> }
    set_type(exp, types.Literal(exp[1]))
  elseif tag == "Function" then -- `Function{ { ident* { `Dots type? }? } type? block }
    set_type(exp, Any)
  elseif tag == "Table" then -- `Table{ ( `Pair{ expr expr } | expr )* }
    set_type(exp, Any)
  elseif tag == "Op" then -- `Op{ opid expr expr? }
    if exp[3] then
      check_binary_op(env, exp)
    else
      check_unary_op(env, exp)
    end
  elseif tag == "Paren" then -- `Paren{ expr }
    check_paren(env, exp)
  elseif tag == "Call" then -- `Call{ expr expr* }
    set_type(exp, Any)
  elseif tag == "Invoke" then -- `Invoke{ expr `String{ <string> expr* }
    set_type(exp, Any)
  elseif tag == "Id" then -- `Id{ <string> }
    set_type(exp, Any)
  elseif tag == "Index" then -- `Index{ expr expr }
    set_type(exp, Any)
  else
    error("cannot type check expression " .. tag)
  end
end

local function check_while (env, stm)
  check_exp(env, stm[1])
  check_block(env, stm[2])
end

local function check_repeat (env, stm)
  check_block(env, stm[1])
  check_exp(env, stm[2])
end

local function check_if (env, stm)
  for i = 1, #stm, 2 do
    local exp, block = stm[i], stm[i + 1]
    if block then
      check_exp(env, exp)
      check_block(env, block)
    else
      block = exp
      check_block(env, block)
    end
  end
end

local function check_fornum (env, stm)
  local id, exp1, exp2, exp3, block = stm[1], stm[2], stm[3], stm[4], stm[5]
  id[2] = Number
  begin_scope(env)
  set_local(env, id, Number, env.scope)
  check_exp(env, exp1)
  local t1 = exp1["type"]
  local msg = "'for' initial value must be a number"
  if types.subtype(t1, Number) then
  elseif types.consistent_subtype(t1, Number) then
    warning(env, msg, exp1.pos)
  else
    typeerror(env, msg, exp1.pos)
  end
  check_exp(env, exp2)
  local t2 = exp2["type"]
  local msg = "'for' limit must be a number"
  if types.subtype(t2, Number) then
  elseif types.consistent_subtype(t2, Number) then
    warning(env, msg, exp2.pos)
  else
    typeerror(env, msg, exp2.pos)
  end
  if block then
    check_exp(env, exp3)
    local t3 = exp3["type"]
    local msg = "'for' step must be a number"
    if types.subtype(t3, Number) then
    elseif types.consistent_subtype(t3, Number) then
      warning(env, msg, exp3.pos)
    else
      typeerror(env, msg, exp3.pos)
    end
    check_exp(env, exp3)
  else
    block = exp3
  end
  check_block(env, block)
  end_scope(env)
end

function check_stm (env, stm)
  local tag = stm.tag
  if tag == "Do" then -- `Do{ stat* }
    check_block(env, stm)
  elseif tag == "Set" then -- `Set{ {lhs+} {expr+} }
  elseif tag == "While" then -- `While{ expr block }
    check_while(env, stm)
  elseif tag == "Repeat" then -- `Repeat{ block expr }
    check_repeat(env, stm)
  elseif tag == "If" then -- `If{ (expr block)+ block? }
    check_if(env, stm)
  elseif tag == "Fornum" then -- `Fornum{ ident expr expr expr? block }
    check_fornum(env, stm)
  elseif tag == "Forin" then -- `Forin{ {ident+} {expr+} block }
  elseif tag == "Local" then -- `Local{ {ident+} {expr+}? }
    check_local(env, stm[1], stm[2])
  elseif tag == "Localrec" then -- `Localrec{ ident expr }
  elseif tag == "Goto" then -- `Goto{ <string> }
  elseif tag == "Label" then -- `Label{ <string> }
  elseif tag == "Return" then -- `Return{ <expr>* }
  elseif tag == "Break" then
  elseif tag == "Call" then -- `Call{ expr expr* }
  elseif tag == "Invoke" then -- `Invoke{ expr `String{ <string> } expr* }
  else
    error("cannot type check statement " .. tag)
  end
end

function check_block (env, block)
  begin_scope(env)
  for k, v in ipairs(block) do
    check_stm(env, v)
  end
  end_scope(env)
end

local function new_env (subject, filename)
  local env = {}
  env.subject = subject -- stores the subject for error messages
  env.filename = filename -- stores the filename for error messages
  env["function"] = {} -- stores function attributes
  env.messages = {} -- stores errors and warnings
  return env
end

function checker.typecheck (ast, subject, filename)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  local env = new_env(subject, filename)
  begin_function(env)
  check_block(env, ast)
  end_function(env)
  if #env.messages > 0 then
    return ast, table.concat(env.messages, "\n")
  else
    return ast
  end
end

return checker

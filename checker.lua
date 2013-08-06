--[[
This file implements the type checker for Typed Lua
]]

local parser = require "parser"
local types = require "types"

local checker = {}

local st = {} -- symbol table

local function lineno (pos)
  return parser.lineno(st.subject, pos)
end

local function errormsg (pos)
  local l,c = lineno(pos)
  return string.format("%s:%d:%d:", st.filename, l, c)
end

local function semerror (msg, pos)
  local error_msg = "%s semantic error, %s"
  error_msg = string.format(error_msg, errormsg(pos), msg)
  return nil,error_msg
end

local function typeerror (msg, pos)
  local error_msg = "%s type error, %s"
  error_msg = string.format(error_msg, errormsg(pos), msg)
  return nil,error_msg
end

local function set_pos (node, node_pos)
  node.pos = node_pos
  return true
end

local function set_type (node, node_type)
  node.type = node_type
  return true
end

local function get_node_type (node)
  if node then
    return node.type
  else
    return types.Nil()
  end
end

local function str2type (str, pos)
  local t = types.str2type(str)
  if not t then
    msg = "type '%s' is not defined"
    msg = string.format(msg, str)
    return typeerror(msg, pos)
  end
  return t
end

-- functions that handle the symbol table

local function new_scope ()
  if not st.scope then
    st.scope = 0
  else
    st.scope = st.scope + 1
  end
  return st.scope
end

local function begin_scope ()
  local scope = new_scope()
  st["maxscope"] = scope
  st[scope] = {} -- new hash for new scope
  st[scope]["label"] = {} -- stores label definitions of a scope
  st[scope]["goto"] = {} -- stores goto definitions of a scope 
  st[scope]["local"] = {} -- stores local variables of a scope
end

local function end_scope ()
  st.scope = st.scope - 1
end

local function insideloop ()
  if not st.loop then
    st.loop = 1
  else
    st.loop = st.loop + 1
  end
end

local function outsideloop ()
  st.loop = st.loop - 1
end

local function isinsideloop ()
  if st.loop and st.loop > 0 then
    return true
  end
  return false
end

local function set_pending_goto (stm)
  local scope = st.scope
  table.insert(st[scope]["goto"], stm)
end

local function set_label (stm)
  local scope = st.scope
  local label = stm[1]
  if st[scope]["label"][label] then
    local msg = "label '%s' already defined on line %d"
    local line,col = lineno(st[scope]["label"][label].pos)
    msg = string.format(msg, label, line)
    return semerror(msg, stm.pos)
  end
  st[scope]["label"][label] = stm
  return true
end

local function lookup_label (stm, scope)
  local label = stm[1]
  for i=scope,0,-1 do
    if st[i]["label"][label] then
      return true
    end
  end
  local msg = "no visible label '%s' for <goto> at line %d"
  local line,col = lineno(stm.pos)
  msg = string.format(msg, label, line)
  return semerror(msg, stm.pos)
end

local function check_pending_gotos ()
  for s=st.maxscope,0,-1 do
    for k,v in ipairs(st[s]["goto"]) do
      local status,msg = lookup_label(v,s)
      if not status then return status,msg end
    end
  end
  return true
end

local function new_id (id_name, id_pos, id_type)
  local id = {}
  id["name"] = id_name
  id["pos"] = id_pos
  id["type"] = id_type
  return id
end

local function isglobal (name)
  if st["global"][name] then
    return true
  end
  return false
end

local function set_global (name, pos, dec_type, exp_type)
  local status,msg
  if types.Equal(dec_type, exp_type) or
     types.isAny(dec_type) then
    st["global"][name] = new_id(name, pos, exp_type)
    return true
  end
  local msg = "attempt to assign '%s' to '%s'"
  msg = string.format(msg, types.tostring(exp_type), types.tostring(dec_type))
  return typeerror(msg, pos)
end

local function get_global (name)
  return st["global"][name]
end

local function get_global_type (name)
  return st["global"][name]["type"]
end

local function islocal (name)
  for i=st.scope,0,-1 do
    if st[i]["local"][name] then
      return true
    end
  end
  return false
end

local function set_local (name, pos, dec_type, exp_type)
  local status,msg
  local scope = st.scope
  if types.Equal(dec_type, exp_type) or
     types.isAny(dec_type) then
    st[scope]["local"][name] = new_id(name, pos, exp_type)
    return true
  end
  local msg = "attempt to assign '%s' to '%s'"
  msg = string.format(msg, types.tostring(exp_type), types.tostring(dec_type))
  return typeerror(msg, pos)
end

local function get_local (name)
  local scope = st.scope
  for i=scope,0,-1 do
    if st[i]["local"][name] then
      return st[i]["local"][name]
    end
  end
  return nil
end

local function get_local_type (name, scope)
  return st[scope]["local"][name]["type"]
end

local function get_local_pos (name, scope)
  return st[scope]["local"][name]["pos"]
end

local function update_local (name, pos, scope, exp_type)
  local var_type = get_local_type(name, scope)
  if types.Equal(var_type, exp_type) then
    return true
  end
  local msg = "attempt to assign '%s' to '%s'"
  msg = string.format(msg, types.tostring(exp_type), types.tostring(var_type))
  return typeerror(msg, pos)
end

local function get_local_scope (name)
  local scope = st.scope
  for i=scope,0,-1 do
    if st[i]["local"][name] then
      return i
    end
  end
  return nil
end

local function get_func_name (fn)
  local tag = fn.tag
  if tag == "Function" then
    if #fn == 1 then
      return fn[1]
    else
      error("Declaring functions inside tables is not implemented yet")
    end
  elseif tag == "Method" then
    error("Method not implemented yet")
  end
end

local function get_var_name (var)
  local tag = var.tag
  if tag == "VarID" then
    return var[1]
  elseif tag == "VarIndex" then
    error("Indexing of variables not implemented yet")
  else
    error("Unknown variable type " .. tag)
  end
end

local function get_var_pos (var)
  return var.pos
end

local function get_var_type (var)
  local tag = var.tag
  if tag == "VarID" then
    return var.type
  elseif tag == "VarIndex" then
    error("Indexing of variables not implemented yet")
  else
    error("Unknown variable type " .. tag)
  end
end

local function infer_arguments (list)
  local t,msg
  local len = #list
  if len == 0 then
    if list.is_vararg then
      t = types.Star(types.Any())
    else
      t = types.Void()
    end
  else
    if list.is_vararg then
      t = types.Star(types.Any())
    else
      t,msg = str2type(list[len][2])
      if not t then return t,msg end
      len = len - 1
    end
    for i=len,1,-1 do
      local u,msg = str2type(list[i][2])
      if not u then return u,msg end
      t = types.Tuple(u, t)
    end
  end
  return t
end

local function infer_explist (list)
  local t
  local len = #list
  if len == 0 then
    t = types.Void()
  else
    t = list[len].type
    for i=len-1,1,-1 do
      t = types.Tuple(list[i].type, t)
    end
  end
  return t
end

local check_block, check_stm, check_exp, check_var
local check_explist

-- expressions

local function check_and (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_type(exp, types.Union(exp[1].type, exp[2].type)) -- T-AND
  set_pos(exp, exp[1].pos)
  return true
end

local function check_anonymous_function (exp)
  local status,msg

  local t1 = infer_arguments(exp[1])

  local t2,msg = str2type(exp[2], exp[2].pos)
  if not t2 then return t2,msg end

  set_type(exp, types.Function(t1, t2))

  status,msg = check_stm(exp[3])
  if not status then return status,msg end

  return true
end

local function check_arith (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  if types.isNumber(exp[1].type) and
     types.isNumber(exp[2].type) then -- T-ARITH1
    set_type(exp, types.Number())
    return true
  elseif types.isAny(exp[1].type) or -- T-ARITH2
         types.isAny(exp[2].type) then -- T-ARITH3
    set_type(exp, types.Any())
    return true
  else
    if not types.isNumber(exp[2].type) and
       not types.isAny(exp[2].type) then
      exp[1] = exp[2]
    end
  end
  msg = "attempt to perform arithmetic on a %s"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1].pos)
end

local function check_concat (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  if types.isString(exp[1].type) and
     types.isString(exp[2].type) then -- T-CONCAT1
    set_type(exp, types.String())
    return true
  elseif types.isAny(exp[1].type) or -- T-CONCAT2
         types.isAny(exp[2].type) then -- T-CONCAT3
    set_type(exp, types.Any())
    return true
  else
    if not types.isString(exp[2].type) and
       not types.isAny(exp[2].type) then
      exp[1] = exp[2]
    end
  end
  msg = "attempt to concatenate a %s"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1].pos)
end

local function check_equal (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_type(exp, types.Boolean()) -- T-EQUAL
  set_pos(exp, exp[1].pos)
  return true
end

local function check_expvar (exp)
  local status,msg

  status,msg = check_var(exp[1]) ; if not status then return status,msg end

  set_type(exp, exp[1].type)
  return true
end

local function check_function_call (exp)
  local status,msg

  status,msg = check_explist(exp[2])
  if not status then return status,msg end

  local call_type = infer_explist(exp[2])

  local var = exp[1][1]
  local var_name = get_var_name(var)
  local var_type
  local local_or_global

  if islocal(var_name) then
    local_or_global = "local"
    local var_scope = get_local_scope(var_name)
    var_type = get_local_type(var_name, var_scope)
  elseif isglobal(var_name) then
    local_or_global = "global"
    var_type = get_global_type(var_name)
  else
    msg = "attempt to call undeclared function '%s'"
    msg = string.format(msg, var_name)
    return typeerror(msg, exp.pos)
  end

  if not types.isFunction(var_type) then
    msg = "attempt to call %s '%s' a %s"
    msg = string.format(msg, local_or_global, var_name, types.tostring(var_type))
    return typeerror(msg, exp.pos)
  end

  return true
end

local function check_len (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end

  if types.isString(exp[1].type) then -- T-LEN1
    set_type(exp, types.Number())
    return true
  elseif types.isAny(exp[1].type) then -- T-LEN2
    set_type(exp, types.Any())
    return true
  end
  msg = "attempt to get length of a %s value"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1].pos)
end

local function check_method_call (exp)
  local status,msg = check_explist(exp[3])
  if not status then return status,msg end
  local t = infer_explist(exp[3])
  return set_type(exp, types.Any())
end

local function check_minus (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end

  if types.isNumber(exp[1].type) then -- T-MINUS1
    set_type(exp, types.Number())
    return true
  elseif types.isAny(exp[1].type) then -- T-MINUS2
    set_type(exp, types.Any())
    return true
  end
  msg = "attempt to perform arithmetic on a %s"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1].pos)
end

local function check_not (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end

  set_type(exp, types.Boolean()) -- T-NOT
  set_pos(exp, exp[1].pos)
  return true
end

local function check_or (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_type(exp, types.Union(exp[1].type, exp[2].type)) -- T-OR
  set_pos(exp, exp[1].pos)
  return true
end

local function check_order (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_type(exp, types.Boolean())
  if types.isNumber(exp[1].type) and
     types.isNumber(exp[2].type) then -- T-ORDER1
    return true
  elseif types.isString(exp[1].type) and
         types.isString(exp[2].type) then -- T-ORDER2
    return true
  elseif types.isAny(exp[1].type) or -- T-ORDER3
         types.isAny(exp[2].type) then -- T-ORDER4
    return true
  end
  msg = "attempt to compare %s with %s"
  msg = string.format(msg, types.tostring(exp[1].type), types.tostring(exp[2].type))
  return typeerror(msg, exp[1].pos)
end

local function check_table (exp)
  return set_type(exp, types.Any())
end

-- variables

local function check_varid (var)
  local t,msg = str2type(var[2], var.pos)
  if not t then return t,msg end
  return set_type(var, t)
end

local function check_varindex (var)
  local status,msg

  status,msg = check_exp(var[1]) ; if not status then return status,msg end
  status,msg = check_exp(var[2]) ; if not status then return status,msg end

  set_type(var, types.Any())
  return true
end

-- statements

local function check_break (stm)
  if not isinsideloop() then
    local msg = "<break> at line %d not inside a loop"
    local line,col = lineno(stm.pos)
    msg = string.format(msg, line)
    return semerror(msg, stm.pos)
  end
  set_type(stm, types.Void())
  return true
end

local function check_call (stm)
  local status,msg

  status,msg = check_exp(stm[1]) ; if not status then return status,msg end

  set_type(stm, stm[1].type)
  return true
end

local function check_if_else (stm)
  local status,msg

  status,msg = check_exp(stm[1]) ; if not status then return status,msg end
  status,msg = check_stm(stm[2]) ; if not status then return status,msg end
  status,msg = check_stm(stm[3]) ; if not status then return status,msg end

  return true
end

local function check_goto (stm)
  set_pending_goto(stm)
  set_type(stm, types.Void())
  return true
end

local function check_label (stm)
  local status,msg

  status,msg = set_label(stm) ; if not status then return status,msg end

  set_type(stm, types.Void())
  return true
end

local function check_generic_for (stm)
  local status,msg

  insideloop()
  status,msg = check_explist(stm[2]) ; if not status then return status,msg end
  status,msg = check_stm(stm[3]) ; if not status then return status,msg end
  outsideloop()

  return true
end

local function check_numeric_for (stm)
  local t,status,msg

  t,msg = str2type(stm[1][2], stm.pos)
  if not t then return t,msg end

  if not types.isNumber(t) and not types.isAny(t) then
    msg = "'for' control variable must be a number"
    return typeerror(msg, stm[1].pos)
  end

  status,msg = check_exp(stm[2]) ; if not status then return status,msg end
  status,msg = check_exp(stm[3]) ; if not status then return status,msg end
  status,msg = check_exp(stm[4]) ; if not status then return status,msg end

  if not types.isNumber(stm[2].type) then
    msg = "'for' initial value must be a number"
    return typeerror(msg, stm[2].pos)
  elseif not types.isNumber(stm[3].type) then
    msg = "'for' limit must be a number"
    return typeerror(msg, stm[3].pos)
  elseif not types.isNumber(stm[4].type) then
    msg = "'for' step must be a number"
    return typeerror(msg, stm[4].pos)
  end

  insideloop()
  status,msg = check_stm(stm[5]) ; if not status then return status,msg end
  outsideloop()

  return true
end

local function check_repeat (stm)
  local status,msg

  insideloop()
  status,msg = check_stm(stm[1]) ; if not status then return status,msg end
  status,msg = check_exp(stm[2]) ; if not status then return status,msg end
  outsideloop()

  return true
end

local function check_while (stm)
  local status,msg

  insideloop()
  status,msg = check_exp(stm[1]) ; if not status then return status,msg end
  status,msg = check_stm(stm[2]) ; if not status then return status,msg end
  outsideloop()

  return true
end

local function check_assignment (stm)
  local status,msg

  status,msg = check_explist(stm[2]) ; if not status then return status,msg end

  for k,v in ipairs(stm[1]) do
    status,msg = check_var(v) ; if not status then return status,msg end
    local var_name = get_var_name(v)
    local var_pos = get_var_pos(v)
    local var_type = get_var_type(v)
    local var_scope = get_local_scope(var_name)
    local exp_type = get_node_type(stm[2][k])
    if var_scope then -- local
      status,msg = update_local(var_name, var_pos, var_scope, exp_type)
      if not status then return status,msg end
    else -- global
      status,msg = set_global(var_name, var_pos, var_type, exp_type)
      if not status then return status,msg end
    end
  end

  return true
end

local function check_local_var (stm)
  local status,msg

  status,msg = check_explist(stm[2])
  if not status then return status,msg end

  for k,v in ipairs(stm[1]) do
    local var_name = v[1]
    local var_pos = v["pos"]
    local var_type,msg = str2type(v[2])
    if not var_type then return var_type,msg end
    local exp_type = get_node_type(stm[2][k])
    status,msg = set_local(var_name, var_pos, var_type, exp_type)
    if not status then return status,msg end
  end

  return true
end

local function check_global_function (stm)
  local status,msg
  local t1 = infer_arguments(stm[2])
  local t2,msg = str2type(stm[3], stm[3].pos)
  if not t2 then return t2,msg end

  status,msg = check_stm(stm[4])
  if not status then return status,msg end

  local var_name = get_func_name(stm[1])
  local var_pos = stm["pos"]
  local var_type = types.Function(t1, t2)
  local var_scope = get_local_scope(var_name)
  local exp_type = types.Function(t1, t2)
  if var_scope then -- local
    status,msg = update_local(var_name, var_pos, var_scope, exp_type)
    if not status then return status,msg end
  else -- global
    status,msg = set_global(var_name, var_pos, var_type, exp_type)
    if not status then return status,msg end
  end

  return true
end

local function check_local_function (stm)
  local status,msg
  local t1 = infer_arguments(stm[2])
  local t2,msg = str2type(stm[3], stm[3].pos)
  if not t2 then return t2,msg end

  status,msg = check_stm(stm[4])
  if not status then return status,msg end

  local var_name = stm[1]
  local var_pos = stm["pos"]
  local var_type = t2
  local exp_type = types.Function(t1, t2)

  status,msg = set_local(var_name, var_pos, var_type, exp_type)
  if not status then return status,msg end

  return true
end

function check_explist (explist)
  local t,m
  for k,v in ipairs(explist) do
    t,m = check_exp(v)
    if not t then return t,m end
  end
  return true
end

function check_var (var)
  local tag = var.tag
  if tag == "VarID" then -- VarID ID
    return check_varid(var)
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
    return check_varindex(var)
  else
    error("cannot type check a variable " .. tag)
  end
end

function check_exp (exp)
  local tag = exp.tag
  if tag == "ExpNil" then
    return set_type(exp, types.Nil()) -- T-NIL
  elseif tag == "ExpFalse" then
    return set_type(exp, types.False()) -- T-FALSE
  elseif tag == "ExpTrue" then
    return set_type(exp, types.True()) -- T-TRUE
  elseif tag == "ExpDots" then
    return set_type(exp, types.Star(types.Any())) -- T-VARARG
  elseif tag == "ExpNum" then -- ExpNum Double
    return set_type(exp, types.Numeral(exp[1])) -- T-NUMBER
  elseif tag == "ExpStr" then -- ExpStr String
    return set_type(exp, types.Word(exp[1])) -- T-STRING
  elseif tag == "ExpVar" then -- ExpVar Var
    return check_expvar(exp)
  elseif tag == "ExpFunction" then -- ExpFunction ParList Type Stm
    return check_anonymous_function(exp)
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    return check_table(exp)
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    return check_method_call(exp)
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    return check_function_call(exp)
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" then -- ExpPow Exp Exp
    return check_arith(exp)
  elseif tag == "ExpConcat" then -- ExpConcat Exp Exp
    return check_concat(exp)
  elseif tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" then -- ExpEQ Exp Exp
    return check_equal(exp)
  elseif tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" then -- ExpGE Exp Exp
    return check_order(exp)
  elseif tag == "ExpAnd" then -- ExpAnd Exp Exp
    return check_and(exp)
  elseif tag == "ExpOr" then -- ExpOr Exp Exp
    return check_or(exp)
  elseif tag == "ExpNot" then -- ExpNot Exp
    return check_not(exp)
  elseif tag == "ExpMinus" then -- ExpMinus Exp
    return check_minus(exp)
  elseif tag == "ExpLen" then -- ExpLen Exp
    return check_len(exp)
  else
    error("cannot type check expression " .. tag)
  end
  return true
end

function check_stm (stm)
  local tag = stm.tag
  local status,msg
  if tag == "StmBlock" then -- StmBlock [Stm]
    return check_block(stm)
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    return check_if_else(stm)
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
    return check_while(stm)
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    return check_numeric_for(stm)
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    return check_generic_for(stm)
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
    return check_repeat(stm)
  elseif tag == "StmFunction" then -- StmFunction FuncName ParList Type Stm
    return check_global_function(stm)
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name ParList Type Stm
    return check_local_function(stm)
  elseif tag == "StmLabel" then -- StmLabel Name
    return check_label(stm)
  elseif tag == "StmGoTo" then -- StmGoTo Name
    return check_goto(stm)
  elseif tag == "StmBreak" then -- StmBreak
    return check_break(stm)
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    return check_assignment(stm)
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    return check_local_var(stm)
  elseif tag == "StmRet" then -- StmRet [Exp]
    status,msg = check_explist(stm[1])
    if not status then return status,msg end
  elseif tag == "StmCall" then -- StmCall Exp
    return check_call(stm)
  else
    error("cannot type check statement " .. tag)
  end
  return true
end

function check_block (block)
  local tag = block.tag
  local status,msg
  if tag ~= "StmBlock" then
    error("cannot type block " .. tag)
  end
  begin_scope()
  for k,v in ipairs(block) do
    status,msg = check_stm(v)
    if not status then return status,msg end
  end
  end_scope()
  return true
end

local function init_symbol_table (subject, filename)
  st = {} -- reseting the symbol table
  st.subject = subject -- store subject for error messages
  st.filename = filename -- store filename for error messages
  st["global"] = {} -- store global names
  for k,v in pairs(_ENV) do
    local t = type(v)
    local any = types.Any()
    local any_star = types.Star(any)
    if t == "string" then
      st["global"][k] = new_id(k, 0, types.Word(v))
    elseif t == "function" then
      st["global"][k] = new_id(k, 0, types.Function(any_star,any))
    else
      st["global"][k] = new_id(k, 0, any)
    end
  end
end

function checker.typecheck (ast, subject, filename)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  init_symbol_table(subject, filename)
  local status,msg
  status,msg = check_block(ast) ; if not status then return status,msg end
  status,msg = check_pending_gotos() ; if not status then return status,msg end
  return ast
end

return checker

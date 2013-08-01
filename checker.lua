--[[
This file implements the type checker for Typed Lua
]]

local parser = require "parser"
local types = require "types"

local checker = {}

local st = {} -- symbol table

local function semerror (msg, node)
  local l,c = parser.lineno(checker.subject, node.pos)
  local error_msg = "%s:%d:%d: semantic error, %s"
  error_msg = string.format(error_msg, checker.filename, l, c, msg)
  return nil,error_msg
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
end

local function end_scope ()
  st.scope = st.scope - 1
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
    local line,col = parser.lineno(checker.subject, st[scope]["label"][label].pos)
    msg = string.format(msg, label, line)
    return semerror(msg, stm)
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
  local line,col = parser.lineno(checker.subject, stm.pos)
  msg = string.format(msg, label, line)
  return semerror(msg, stm)
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

local function set_pos (node, node_pos)
  node.pos = node_pos
  return true
end

local function set_type (node, node_type)
  node.type = node_type
  return true
end

local function typeerror (msg, node)
  local l,c = parser.lineno(checker.subject, node.pos)
  local error_msg = "%s:%d:%d: type error, %s"
  error_msg = string.format(error_msg, checker.filename, l, c, msg)
  return nil,error_msg
end

local function infer_arguments (list)
  local t
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
      t = types.str2type(list[len][2])
      len = len - 1
    end
    for i=len,1,-1 do
      t = types.Tuple(types.str2type(list[i][2]), t)
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

  set_pos(exp, exp[1].pos)

  if types.isNil(exp[1].type) or types.isFalse(exp[1].type) then
    set_type(exp, exp[1].type)
  elseif types.isBaseBoolean(exp[1].type) then
    set_type(exp, types.Any())
  else
    set_type(exp, exp[2].type)
  end

  return true
end

local function check_arith (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  if types.isNumber(exp[1].type) then
    if types.isNumber(exp[2].type) then
      set_type(exp, types.Number())
      return true
    end
    exp[1] = exp[2]
  end
  msg = "attempt to perform arithmetic on a %s"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1])
end

local function check_concat (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  if types.isString(exp[1].type) then
    if types.isString(exp[2].type) then
      set_type(exp, types.String())
      return true
    end
    exp[1] = exp[2]
  end
  msg = "attempt to concatenate a %s"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1])
end

local function check_equal (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_pos(exp, exp[1].pos)
  set_type(exp, types.Boolean())
  return true
end

local function check_expvar (exp)
  local status,msg

  status,msg = check_var(exp[1]) ; if not status then return status,msg end

  set_type(exp, exp[1].type)
  return true
end

local function check_len (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end

  if types.isString(exp[1].type) then
    set_type(exp, types.Number())
    return true
  end
  msg = "attempt to get length of a %s value"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1])
end

local function check_minus (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end

  if types.isNumber(exp[1].type) then
    set_type(exp, types.Number())
    return true
  end
  msg = "attempt to perform arithmetic on a %s"
  msg = string.format(msg, types.tostring(exp[1].type))
  return typeerror(msg, exp[1])
end

local function check_not (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end

  set_pos(exp, exp[1].pos)
  set_type(exp, types.Boolean())
  return true
end

local function check_or (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_pos(exp, exp[1].pos)

  if types.isNil(exp[1].type) or types.isFalse(exp[1].type) then
    set_type(exp, exp[2].type)
  elseif types.isBaseBoolean(exp[1].type) then
    set_type(exp, types.Any())
  else
    set_type(exp, exp[1].type)
  end

  return true
end

local function check_order (exp)
  local status,msg

  status,msg = check_exp(exp[1]) ; if not status then return status,msg end
  status,msg = check_exp(exp[2]) ; if not status then return status,msg end

  set_type(exp, types.Boolean())
  if types.isNumber(exp[1].type) and types.isNumber(exp[2].type) then
    return true
  elseif types.isString(exp[1].type) and types.isString(exp[2].type) then
    return true
  end
  msg = "attempt to compare %s with %s"
  msg = string.format(msg, types.tostring(exp[1].type), types.tostring(exp[2].type))
  return typeerror(msg, exp[1])
end

-- variables

local function check_varid (var)
  set_type(var, types.str2type(var[2]))
  return true
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

  status,msg = check_explist(stm[2]) ; if not status then return status,msg end

  return check_stm(stm[3])
end

local function check_numeric_for (stm)
  local status,msg
  local t = types.str2type(stm[1][2])
  if not t then
    msg = "type '%s' is not defined"
    msg = string.format(msg, stm[1][2])
    return typeerror(msg, stm)
  end

  if not types.isNumber(t) then
    msg = "'for' control variable must be a number"
    return typeerror(msg, stm[1])
  end

  status,msg = check_exp(stm[2]) ; if not status then return status,msg end
  status,msg = check_exp(stm[3]) ; if not status then return status,msg end
  status,msg = check_exp(stm[4]) ; if not status then return status,msg end

  if not types.isNumber(stm[2].type) then
    msg = "'for' initial value must be a number"
    return typeerror(msg, stm[2])
  elseif not types.isNumber(stm[3].type) then
    msg = "'for' limit must be a number"
    return typeerror(msg, stm[3])
  elseif not types.isNumber(stm[4].type) then
    msg = "'for' step must be a number"
    return typeerror(msg, stm[4])
  end

  return check_stm(stm[5])
end

local function check_repeat (stm)
  local status,msg

  status,msg = check_exp(stm[2]) ; if not status then return status,msg end

  return check_stm(stm[1])
end

local function check_while (stm)
  local status,msg

  status,msg = check_exp(stm[1]) ; if not status then return status,msg end

  return check_stm(stm[2])
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
    return set_type(exp, types.Nil())
  elseif tag == "ExpFalse" then
    return set_type(exp, types.False())
  elseif tag == "ExpTrue" then
    return set_type(exp, types.True())
  elseif tag == "ExpDots" then
    return set_type(exp, types.Any())
  elseif tag == "ExpNum" then -- ExpNum Double
    return set_type(exp, types.Numeral(exp[1]))
  elseif tag == "ExpStr" then -- ExpStr String
    return set_type(exp, types.Literal(exp[1]))
  elseif tag == "ExpVar" then -- ExpVar Var
    return check_expvar(exp)
  elseif tag == "ExpFunction" then -- ExpFunction ParList Type Stm
    local t1 = infer_arguments(exp[1])
    local t2 = types.str2type(exp[2])
    if not t2 then
      local msg = "type '%s' is not defined"
      msg = string.format(msg, exp[2])
      return typeerror(msg, exp)
    end
    return set_type(exp, types.Function(t1, t2))
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    return set_type(exp, types.Any())
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    local status,msg = check_explist(exp[3])
    if not status then return status,msg end
    local t = infer_explist(exp[3])
    return set_type(exp, types.Any())
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    local status,msg = check_explist(exp[2])
    if not status then return status,msg end
    local t = infer_explist(exp[2])
    return set_type(exp, types.Any())
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
    local t1 = infer_arguments(stm[2])
    local t2 = types.str2type(stm[3])
    if not t2 then
      local msg = "type '%s' is not defined"
      msg = string.format(msg, stm[2])
      return typeerror(msg, exp)
    end
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name ParList Type Stm
    local t1 = infer_arguments(stm[2])
    local t2 = types.str2type(stm[3])
    if not t2 then
      local msg = "type '%s' is not defined"
      msg = string.format(msg, stm[2])
      return typeerror(msg, exp)
    end
  elseif tag == "StmLabel" then -- StmLabel Name
    return check_label(stm)
  elseif tag == "StmGoTo" then -- StmGoTo Name
    return check_goto(stm)
  elseif tag == "StmBreak" then -- StmBreak
    return check_break(stm)
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    status,msg = check_explist(stm[2])
    if not status then return status,msg end
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    status,msg = check_explist(stm[2])
    if not status then return status,msg end
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

function checker.typecheck (ast, subject, filename)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  checker.subject = subject
  checker.filename = filename
  st = {} -- reseting the symbol table
  local status,msg
  status,msg = check_block (ast) ; if not status then return status,msg end
  status,msg = check_pending_gotos() ; if not status then return status,msg end
  return ast
end

return checker

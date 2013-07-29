--[[
This file implements the type checker for Typed Lua
]]

local parser = require "parser"
local types = require "types"

local checker = {}

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

local check_block, check_stm, check_exp, check_var
local check_explist

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

local function check_varid (var)
  set_type(var, var[2])
  return true
end

local function check_varindex (var)
  local status,msg

  status,msg = check_exp(var[1]) ; if not status then return status,msg end
  status,msg = check_exp(var[2]) ; if not status then return status,msg end

  set_type(var, types.Any())
  return true
end

check_explist = function (explist)
  local t,m
  for k,v in ipairs(explist) do
    t,m = check_exp(v)
    if not t then return t,m end
  end
  return true
end

check_var = function (var)
  local tag = var.tag
  if tag == "VarID" then -- VarID ID
    return check_varid(var)
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
    return check_varindex(var)
  else
    error("cannot type check a variable " .. tag)
  end
end

check_exp = function (exp)
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
    local is_vararg = exp[1].is_vararg
    return set_type(exp, types.Any())
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    return set_type(exp, types.Any())
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    return set_type(exp, types.Any())
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
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

check_stm = function (stm)
  local tag = stm.tag
  local t,m
  if tag == "StmBlock" then -- StmBlock [Stm]
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
  elseif tag == "StmFunction" then -- StmFunction FuncName ParList Type Stm
    local is_vararg = stm[2].is_vararg
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name ParList Type Stm
    local is_vararg = stm[2].is_vararg
  elseif tag == "StmLabel" or -- StmLabel Name
         tag == "StmGoTo" then -- StmGoTo Name
  elseif tag == "StmBreak" then -- StmBreak
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    t,m = check_explist(stm[2])
    if not t then return t,m end
  elseif tag == "StmRet" then -- StmRet [Exp]
  elseif tag == "StmCall" then -- StmCall Exp
  else
    error("cannot type check statement " .. tag)
  end
  return true
end

check_block = function (block)
  local tag = block.tag
  local t,m
  if tag ~= "StmBlock" then
    error("cannot type block " .. tag)
  end
  for k,v in ipairs(block) do
    t,m = check_stm(v)
    if not t then return t,m end
  end
  return true
end

function checker.typecheck (ast, subject, filename)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  checker.subject = subject
  checker.filename = filename
  local t,m = check_block (ast)
  if not t then return t,m end
  return ast
end

return checker

--[[
This file implements the type checker for Typed Lua
]]

local parser = require "parser"

local checker = {}

local function typeerror (msg, node)
  local l,c = parser.lineno(checker.subject, node.pos)
  local error_msg = ("%s:%d:%d: type error, %s"):format(checker.filename, l, c, msg)
  return nil,error_msg
end

local check_block, check_stm, check_exp, check_var
local check_explist

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
    var.type = var[2]
    return true
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
  else
    error("cannot type check a variable " .. tag)
  end
end

check_exp = function (exp)
  local tag = exp.tag
  local t,m,msg
  if tag == "ExpNil" then
  elseif tag == "ExpFalse" then
    exp.type = "boolean"
  elseif tag == "ExpTrue" then
    exp.type = "boolean"
  elseif tag == "ExpDots" then
    exp.type = "any"
  elseif tag == "ExpNum" then -- ExpNum Double
    exp.type = "number"
  elseif tag == "ExpStr" then -- ExpStr String
    exp.type = "string"
  elseif tag == "ExpVar" then -- ExpVar Var
    t,m = check_var(exp[1])
    if not t then return t,m end
    exp.type = exp[1].type
    return true
  elseif tag == "ExpFunction" then -- ExpFunction ParList Type Stm
    local is_vararg = exp[1].is_vararg
    exp.type = "any"
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    exp.type = "any"
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    exp.type = "any"
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    exp.type = "any"
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" then -- ExpPow Exp Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    t,m = check_exp(exp[2])
    if not t then return t,m end
    if exp[1].type == "number" then
      if exp[2].type == "number" then
        exp.type = "number"
        return true
      end
      exp[1] = exp[2]
    elseif exp[1].type == "any" then
      if exp[2].type == "any" then
        exp.type = "any"
        return true
      end
      exp[1] = exp[2]
    end
    msg = ("attempt to perform arithmetic on a %s"):format(exp[1].type)
    return typeerror(msg, exp[1])
  elseif tag == "ExpConcat" then -- ExpConcat Exp Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    t,m = check_exp(exp[2])
    if not t then return t,m end
    if exp[1].type == "string" then
      if exp[2].type == "string" then
        exp.type = "string"
        return true
      end
      exp[1] = exp[2]
    elseif exp[1].type == "any" then
      if exp[2].type == "any" then
        exp.type = "any"
        return true
      end
      exp[1] = exp[2]
    end
    msg = ("attempt to concatenate a %s"):format(exp[1].type)
    return typeerror(msg, exp[1])
  elseif tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" then -- ExpEQ Exp Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    t,m = check_exp(exp[2])
    if not t then return t,m end
    exp.type = "boolean"
    return true
  elseif tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" then -- ExpGE Exp Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    t,m = check_exp(exp[2])
    if not t then return t,m end
    exp.type = "boolean"
    if exp[1].type == "number" and exp[2].type == "number" then
      return true
    end
    if exp[1].type == "any" and exp[2].type == "any" then
      return true
    end
    if exp[1].type == "string" and exp[2].type == "string" then
      return true
    end
    return nil,"relational type error"
  elseif tag == "ExpAnd" then -- ExpAnd Exp Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    t,m = check_exp(exp[2])
    if not t then return t,m end
    exp.type = "boolean"
    return true
  elseif tag == "ExpOr" then -- ExpOr Exp Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    t,m = check_exp(exp[2])
    if not t then return t,m end
    exp.type = "boolean"
    return true
  elseif tag == "ExpNot" then -- ExpNot Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    exp.type = "boolean"
    return true
  elseif tag == "ExpMinus" then -- ExpMinus Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    if exp[1].type == "number" then
      exp.type = "number"
      return true
    end
    if exp[1].type == "any" then
      exp.type = "any"
      return true
    end
    return nil,"arithmetic type error"
  elseif tag == "ExpLen" then -- ExpLen Exp
    t,m = check_exp(exp[1])
    if not t then return t,m end
    if exp[1].type == "string" then
      exp[1].type = "number"
      return true
    end
    return nil,"length operator type error"
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

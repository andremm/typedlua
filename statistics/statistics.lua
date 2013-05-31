
local parser = require "parser"
local pp = require "pp"

sizep = 0
anonymousf = 0
globalf = 0
localf = 0

local check_block, check_stm
local check_exp, check_var
local check_explist
local check_fieldlist

check_fieldlist = function (fieldlist)
  --for k,v in pairs(fieldlist) do print (k,v) end
  for k,v in ipairs(fieldlist[1]) do
    check_exp(v[1])
  end
  for k,v in ipairs(fieldlist[2]) do
    check_exp(v[1])
    check_exp(v[2])
  end
end

check_explist = function (explist)
  for k,v in ipairs(explist) do
    check_exp(v)
  end
end

check_var = function (var)
  -- VarID ID
  if var.tag == "VarID" then
    --if var[1] == "type" then print(var[1]) end
  -- VarIndex Exp Exp
  elseif var.tag == "VarIndex" then
  end
end

check_exp = function (exp)
  if exp.tag == "ExpNil" or
     exp.tag == "ExpFalse" or
     exp.tag == "ExpTrue" or
     exp.tag == "ExpDots" then
  -- ExpNum Double
  elseif exp.tag == "ExpNum" then
  -- ExpStr String
  elseif exp.tag == "ExpStr" then
  -- ExpVar Var
  elseif exp.tag == "ExpVar" then
    check_var(exp[1])
  -- ExpFunction ([ID],IsVarArg) Stm 
  elseif exp.tag == "ExpFunction" then
    sizep = sizep + 1
    anonymousf = anonymousf + 1
    check_stm(exp[2])
  -- ExpTableConstructor [Exp] {[Exp] [Exp]}
  elseif exp.tag == "ExpTableConstructor" then
    check_fieldlist(exp[1])
  -- ExpMethodCall Exp ID [Exp]
  elseif exp.tag == "ExpMethodCall" then
    check_exp(exp[1])
    check_explist(exp[3])
  -- ExpFunctionCall Exp [Exp]
  elseif exp.tag == "ExpFunctionCall" then
    check_exp(exp[1])
    check_explist(exp[2])
  -- ExpBin Exp Exp
  elseif exp.tag == "ExpAnd" or
         exp.tag == "ExpOr" or
         exp.tag == "ExpAdd" or
         exp.tag == "ExpSub" or
         exp.tag == "ExpMul" or
         exp.tag == "ExpDiv" or
         exp.tag == "ExpMod" or
         exp.tag == "ExpPow" or
         exp.tag == "ExpConcat" or
         exp.tag == "ExpNE" or
         exp.tag == "ExpEQ" or
         exp.tag == "ExpLT" or
         exp.tag == "ExpLE" or
         exp.tag == "ExpGT" or
         exp.tag == "ExpGE" then
    check_exp(exp[1])
    check_exp(exp[2])
  -- ExpUn Exp
  elseif exp.tag == "ExpNot" or
         exp.tag == "ExpMinus" or
         exp.tag == "ExpLen" then
    check_exp(exp[1])
  else
    print(exp.tag)
    error("expecting an expression, but got a " .. exp.tag)
  end
end

check_stm = function (stm)
  -- StmBlock [Stm]
  if stm.tag == "StmBlock" then
    check_block(stm)
  -- StmIfElse Exp Stm Stm
  elseif stm.tag == "StmIfElse" then
    check_exp(stm[1])
    check_stm(stm[2])
    check_stm(stm[3])
  -- StmWhile Exp Stm
  elseif stm.tag == "StmWhile" then
    check_exp(stm[1])
    check_stm(stm[2])
  -- StmRepeat Stm Exp
  elseif stm.tag == "StmRepeat" then
    check_stm(stm[1])
    check_exp(stm[2])
  -- StmForNum ID Exp Exp Exp Stm
  elseif stm.tag == "StmForNum" then
    check_exp(stm[2])
    check_exp(stm[3])
    check_exp(stm[4])
    check_stm(stm[5])
  -- StmForGen [ID] [Exp] Stm
  elseif stm.tag == "StmForGen" then
    check_stm(stm[3])
  -- StmLocalFunction ID ([ID],IsVarArg) Stm
  elseif stm.tag == "StmLocalFunction" then
    sizep = sizep + 1
    localf = localf + 1
    check_stm(stm[3])
  -- StmFunction FuncName ([ID],IsVarArg) Stm
  elseif stm.tag == "StmFunction" then
    sizep = sizep + 1
    globalf = globalf + 1
    check_stm(stm[3])
  -- StmLabel ID
  elseif stm.tag == "StmLabel" then
  -- StmGoTo ID
  elseif stm.tag == "StmGoTo" then
  -- StmBreak
  elseif stm.tag == "StmBreak" then
  -- StmAssign [Var] [Exp]
  elseif stm.tag == "StmAssign" then
    check_explist(stm[2])
  -- StmLocalVar [ID] [Exp]
  elseif stm.tag == "StmLocalVar" then
    check_explist(stm[2])
  -- StmRet [Exp]
  elseif stm.tag == "StmRet" then
    check_explist(stm[1])
  -- StmCall Exp
  elseif stm.tag == "StmCall" then
    check_exp(stm[1])
  else
    error("expecting a statement, but got a " .. stm.tag)
  end
end

check_block = function (block)
  if block.tag ~= "StmBlock" then
    error("expecting a block, but got a " .. block.tag)
  end
  for k,v in ipairs(block) do
    check_stm(v)
  end
end

local function check (ast)
  sizep = 0
  anonymousf = 0
  globalf = 0
  localf = 0
  check_block(ast)
  print("sizep", sizep)
  print("anonymous", anonymousf)
  print("global", globalf)
  print("local", localf)
end

local function generate (filename)
  assert(type(filename) == "string")
  local ast,errormsg = parser.parse(filename)
  if not ast then
    error(errormsg)
  end
  check(ast)
  return ast
end

return {
  generate = generate,
}

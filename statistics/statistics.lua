
local parser = require "parser"
local pp = require "pp"

sizep = 0
anonymousf = 0
globalf = 0
localf = 0

local count_block, count_stm
local count_exp, count_var
local count_explist
local count_fieldlist

count_fieldlist = function (fieldlist)
  for k,v in ipairs(fieldlist[1]) do
    count_exp(v[1])
  end
  for k,v in ipairs(fieldlist[2]) do
    count_exp(v[1])
    count_exp(v[2])
  end
end

count_explist = function (explist)
  for k,v in ipairs(explist) do
    count_exp(v)
  end
end

count_var = function (var)
  -- VarID ID
  if var.tag == "VarID" then
    --if var[1] == "type" then print(var[1]) end
  -- VarIndex Exp Exp
  elseif var.tag == "VarIndex" then
  end
end

count_exp = function (exp)
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
    count_var(exp[1])
  -- ExpFunction ([ID],IsVarArg) Stm 
  elseif exp.tag == "ExpFunction" then
    sizep = sizep + 1
    anonymousf = anonymousf + 1
    count_stm(exp[2])
  -- ExpTableConstructor [Exp] {[Exp] [Exp]}
  elseif exp.tag == "ExpTableConstructor" then
    count_fieldlist(exp[1])
  -- ExpMethodCall Exp ID [Exp]
  elseif exp.tag == "ExpMethodCall" then
    count_exp(exp[1])
    count_explist(exp[3])
  -- ExpFunctionCall Exp [Exp]
  elseif exp.tag == "ExpFunctionCall" then
    count_exp(exp[1])
    count_explist(exp[2])
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
    count_exp(exp[1])
    count_exp(exp[2])
  -- ExpUn Exp
  elseif exp.tag == "ExpNot" or
         exp.tag == "ExpMinus" or
         exp.tag == "ExpLen" then
    count_exp(exp[1])
  else
    error("expecting an expression, but got a " .. exp.tag)
  end
end

count_stm = function (stm)
  -- StmBlock [Stm]
  if stm.tag == "StmBlock" then
    count_block(stm)
  -- StmIfElse Exp Stm Stm
  elseif stm.tag == "StmIfElse" then
    count_exp(stm[1])
    count_stm(stm[2])
    count_stm(stm[3])
  -- StmWhile Exp Stm
  elseif stm.tag == "StmWhile" then
    count_exp(stm[1])
    count_stm(stm[2])
  -- StmRepeat Stm Exp
  elseif stm.tag == "StmRepeat" then
    count_stm(stm[1])
    count_exp(stm[2])
  -- StmForNum ID Exp Exp Exp Stm
  elseif stm.tag == "StmForNum" then
    count_exp(stm[2])
    count_exp(stm[3])
    count_exp(stm[4])
    count_stm(stm[5])
  -- StmForGen [ID] [Exp] Stm
  elseif stm.tag == "StmForGen" then
    count_stm(stm[3])
  -- StmLocalFunction ID ([ID],IsVarArg) Stm
  elseif stm.tag == "StmLocalFunction" then
    sizep = sizep + 1
    localf = localf + 1
    count_stm(stm[3])
  -- StmFunction FuncName ([ID],IsVarArg) Stm
  elseif stm.tag == "StmFunction" then
    sizep = sizep + 1
    globalf = globalf + 1
    count_stm(stm[3])
  -- StmLabel ID
  elseif stm.tag == "StmLabel" then
  -- StmGoTo ID
  elseif stm.tag == "StmGoTo" then
  -- StmBreak
  elseif stm.tag == "StmBreak" then
  -- StmAssign [Var] [Exp]
  elseif stm.tag == "StmAssign" then
    count_explist(stm[2])
  -- StmLocalVar [ID] [Exp]
  elseif stm.tag == "StmLocalVar" then
    count_explist(stm[2])
  -- StmRet [Exp]
  elseif stm.tag == "StmRet" then
    count_explist(stm[1])
  -- StmCall Exp
  elseif stm.tag == "StmCall" then
    count_exp(stm[1])
  else
    error("expecting a statement, but got a " .. stm.tag)
  end
end

count_block = function (block)
  if block.tag ~= "StmBlock" then
    error("expecting a block, but got a " .. block.tag)
  end
  for k,v in ipairs(block) do
    count_stm(v)
  end
end

local function count (ast)
  sizep = 0
  anonymousf = 0
  globalf = 0
  localf = 0
  count_block(ast)
  print("sizep", sizep)
  print("anonymous", anonymousf)
  print("global", globalf)
  print("local", localf)
  if sizep ~= anonymousf + globalf + localf then
    error("number of functions does not match")
  end
end

local function generate (filename)
  assert(type(filename) == "string")
  local ast,errormsg = parser.parse(filename)
  if not ast then
    error(errormsg)
  end
  count(ast)
  return ast
end

return {
  generate = generate,
}

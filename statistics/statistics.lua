
local parser = require "parser"
local pp = require "pp"

sizep = 0
anonymousf = 0
globalf = 0
localf = 0
result = {}

local count_block, count_stm
local count_exp, count_var
local count_explist
local count_fieldlist

local check_block, check_stm
local check_exp, check_var
local check_explist
local check_fieldlist

count_fieldlist = function (fieldlist)
  for k,v in ipairs(fieldlist[1]) do
    count_exp(v[1])
  end
  for k,v in ipairs(fieldlist[2]) do
    count_exp(v[1])
    count_exp(v[2])
  end
end

check_fieldlist = function (fieldlist, func_name, func_id)
  for k,v in ipairs(fieldlist[1]) do
    check_exp(v[1], func_name, func_id)
  end
  for k,v in ipairs(fieldlist[2]) do
    check_exp(v[1], func_name, func_id)
    check_exp(v[2], func_name, func_id)
  end
end

count_explist = function (explist)
  for k,v in ipairs(explist) do
    count_exp(v)
  end
end

check_explist = function (explist, func_name, func_id)
  for k,v in ipairs(explist) do
    check_exp(v, func_name, func_id)
  end
end

count_var = function (var)
  -- VarID ID
  if var.tag == "VarID" then
    --if var[1] == "type" then print(var[1]) end
  -- VarIndex Exp Exp
  elseif var.tag == "VarIndex" then
    count_exp(var[1])
    count_exp(var[2])
  end
end

check_var = function (var)
  if var.tag == "VarID" then -- VarID ID
  elseif var.tag == "VarIndex" then -- VarIndex Exp Exp
    check_exp(var[1], func_name, func_id)
    check_exp(var[2], func_name, func_id)
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

check_exp = function (exp, func_name, func_id)
  if exp.tag == "ExpNil" or
     exp.tag == "ExpFalse" or
     exp.tag == "ExpTrue" or
     exp.tag == "ExpDots" or
     exp.tag == "ExpNum" or -- ExpNum Double
     exp.tag == "ExpStr" then -- ExpStr String
  elseif exp.tag == "ExpVar" then -- ExpVar Var
    check_var(exp[1], func_name, func_id)
  elseif exp.tag == "ExpFunction" then -- ExpFunction ParList Stm
    result.sizep = result.sizep + 1
    result.anonymousf = result.anonymousf + 1
    check_stm(exp[2], func_name, func_id)
  elseif exp.tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    check_fieldlist(exp[1], func_name, func_id)
  elseif exp.tag == "ExpMethodCall" then -- ExpMethodCall Exp ID [Exp]
    check_exp(exp[1], func_name, func_id)
    check_explist(exp[3], func_name, func_id)
  elseif exp.tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    check_exp(exp[1], func_name, func_id)
    check_explist(exp[2], func_name, func_id)
  elseif exp.tag == "ExpAdd" or -- ExpAdd Exp Exp 
         exp.tag == "ExpSub" or -- ExpSub Exp Exp
         exp.tag == "ExpMul" or -- ExpMul Exp Exp
         exp.tag == "ExpDiv" or -- ExpDiv Exp Exp
         exp.tag == "ExpMod" or -- ExpMod Exp Exp
         exp.tag == "ExpPow" or -- ExpPow Exp Exp
         exp.tag == "ExpConcat" or -- ExpConcat Exp Exp
         exp.tag == "ExpNE" or -- ExpNE Exp Exp
         exp.tag == "ExpEQ" or -- ExpEQ Exp Exp
         exp.tag == "ExpLT" or -- ExpLT Exp Exp
         exp.tag == "ExpLE" or -- ExpLE Exp Exp
         exp.tag == "ExpGT" or -- ExpGT Exp Exp
         exp.tag == "ExpGE" or -- ExpGE Exp Exp
         exp.tag == "ExpAnd" or -- ExpGE Exp Exp
         exp.tag == "ExpOr" then -- ExpOr Exp Exp
    check_exp(exp[1], func_name, func_id)
    check_exp(exp[2], func_name, func_id)
  elseif exp.tag == "ExpNot" or -- ExpNot Exp
         exp.tag == "ExpMinus" or -- ExpMinus Exp
         exp.tag == "ExpLen" then -- ExpLen Exp
    check_exp(exp[1], func_name, func_id)
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
    count_explist(stm[2])
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

check_stm = function (stm, func_name, func_id)
  if stm.tag == "StmBlock" then -- StmBlock [Stm]
    check_block(stm)
  elseif stm.tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmWhile" then -- StmWhile Exp Stm
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
  elseif stm.tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    check_exp(stm[2], func_name, func_id)
    check_exp(stm[3], func_name, func_id)
    check_exp(stm[4], func_name, func_id)
    check_stm(stm[5], func_name, func_id)
  elseif stm.tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    check_explist(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmRepeat" then -- StmRepeat Stm Exp
    check_stm(stm[1], func_name, func_id)
    check_exp(stm[2], func_name, func_id)
  elseif stm.tag == "StmFunction" then -- StmFunction FuncName ParList Stm
    result.sizep = result.sizep + 1
    result.globalf = result.globalf + 1
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmLocalFunction" then -- StmLocalFunction ID ParList Stm
    result.sizep = result.sizep + 1
    result.localf = result.localf + 1
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmLabel" or -- StmLabel ID
         stm.tag == "StmGoTo" or -- StmGoTo ID
         stm.tag == "StmBreak" then
  elseif stm.tag == "StmAssign" then -- StmAssign [Var] [Exp]
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmRet" then -- StmRet [Exp]
    check_explist(stm[1], func_name, func_id)
  elseif stm.tag == "StmCall" then -- StmCall Exp
    check_exp(stm[1], func_name, func_id)
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

check_block = function (block, func_name, func_id)
  if block.tag ~= "StmBlock" then
    error("expecting a block, but got a " .. block.tag)
  end
  for k,v in ipairs(block) do
    check_stm(v, func_name, func_id)
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

local function init_result ()
  result = {}
  result.sizep = 0
  result.anonymousf = 0
  result.globalf = 0
  result.localf = 0
end

local function print_result ()
  print("sizep", result.sizep)
  print("anonymous", result.anonymousf)
  print("global", result.globalf)
  print("local", result.localf)
end

local function count_recon ()
  local sum = result.anonymousf + result.globalf + result.localf
  if result.sizep ~= sum then
    error("number of functions does not match")
  end
end

local function check (ast)
  init_result()
  check_block(ast, "main", 0)
  print_result()
  count_recon()
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


local parser = require "parser"
local pp = require "pp"

local check_block, check_stm
local check_exp
local check_explist

local function print_func_name (t)

end

check_explist = function (explist)
  for k,v in ipairs(explist) do
    check_exp(v)
  end
end

check_exp = function (exp)
  -- ExpNum Double
  if exp.tag == "ExpNum" then
  -- ExpStr String
  elseif exp.tag == "ExpStr" then
  -- ExpVar Var
  elseif exp.tag == "ExpVar" then
  -- ExpFunction ([ID],IsVarArg) Stm 
  elseif exp.tag == "ExpFunction" then
    check_stm(exp[3])
  -- ExpTableConstructor [Exp] {[Exp] [Exp]}
  elseif exp.tag == "ExpTableConstructor" then
  -- ExpMethodCall Exp ID [Exp]
  elseif exp.tag == "ExpMethodCall" then
  -- ExpFunctionCall Exp [Exp]
  elseif exp.tag == "ExpFunctionCall" then
  -- ExpBin Exp Exp
  elseif exp.tag == "ExpAdd" or
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
  -- ExpUn Exp
  elseif exp.tag == "ExpNot" or
         exp.tag == "ExpMinus" or
         exp.tag == "ExpLen" then
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
    print(stm[1])
    check_stm(stm[3])
  -- StmFunction FuncName ([ID],IsVarArg) Stm
  elseif stm.tag == "StmFunction" then
    print_func_name(stm[1])
  -- StmLabel ID
  elseif stm.tag == "StmLabel" then
  -- StmGoTo ID
  elseif stm.tag == "StmGoTo" then
  -- StmAssign [Var] [Exp]
  elseif stm.tag == "StmAssign" then
  -- StmLocalVar [ID] [Exp]
  elseif stm.tag == "StmLocalVar" then
    check_explist(stm[2])
  -- StmRet [Exp]
  elseif stm.tag == "StmRet" then
  -- StmCall Exp
    check_exp(stm[1])
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
  check_block(ast)
end

local function generate (filename)
  assert(type(filename) == "string")
  local ast,errormsg = parser.parse(filename)
  if not ast then
    error(errormsg)
  end
  --check(ast)
  pp.prinths(ast)
  return ast
end

return {
  generate = generate,
}

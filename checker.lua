--[[
This file implements the type checker for Typed Lua
]]

local checker = {}

local check_block, check_stm, check_exp, check_var

check_var = function (var)
  local tag = var.tag
  if tag == "VarID" then -- VarID ID
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
  else
    error("cannot type check a variable " .. tag)
  end
end

check_exp = function (exp)
  local tag = exp.tag
  if tag == "ExpNil" then
  elseif tag == "ExpFalse" then
  elseif tag == "ExpTrue" then
  elseif tag == "ExpDots" then
  elseif tag == "ExpNum" then -- ExpNum Double
  elseif tag == "ExpStr" then -- ExpStr String
  elseif tag == "ExpVar" then -- ExpVar Var
  elseif tag == "ExpFunction" then -- ExpFunction ParList Type Stm
    local is_vararg = exp[1].is_vararg
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" then -- ExpPow Exp Exp
  elseif tag == "ExpConcat" then -- ExpConcat Exp Exp
  elseif tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" then -- ExpEQ Exp Exp
  elseif tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" then -- ExpGE Exp Exp
  elseif tag == "ExpAnd" then -- ExpGE Exp Exp
  elseif tag == "ExpOr" then -- ExpOr Exp Exp
  elseif tag == "ExpNot" then -- ExpNot Exp
  elseif tag == "ExpMinus" then -- ExpMinus Exp
  elseif tag == "ExpLen" then -- ExpLen Exp
  else
    error("cannot type check expression " .. tag)
  end
end

check_stm = function (stm)
  local tag = stm.tag
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
  elseif tag == "StmRet" then -- StmRet [Exp]
  elseif tag == "StmCall" then -- StmCall Exp
  else
    error("cannot type check statement " .. tag)
  end
end

check_block = function (block)
  local tag = block.tag
  if tag ~= "StmBlock" then
    for k,v in ipairs(block) do
      check_stm(v)
    end
  end
end

function checker.typecheck (ast)
  assert(type(ast) == "table")
  check_block (ast)
end

return checker

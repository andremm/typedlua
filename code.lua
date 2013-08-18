--[[
This file implements the code generator for Typed Lua
]]

local code = {}

local SPACES = 2

local code_block, code_stm, code_exp, code_var
local code_explist, code_varlist, code_fieldlist

local function spaces (n)
  return string.rep(" ", SPACES * n)
end

local function ident (s, n)
  return spaces(n) .. s
end

function code_fieldlist (fieldlist, i)
  local l = {{},{}}
  for k,v in ipairs(fieldlist[1]) do
    l[1][k] = code_exp(v[1], i) .. ","
  end
  for k,v in ipairs(fieldlist[2]) do
    l[2][k] = "[ " .. code_exp(v[1], i) .. " ] = " .. code_exp(v[2], i) .. ","
  end
  return table.concat(l[1], "") .. table.concat(l[2], "")
end

function code_varlist (varlist, i)
  local l = {}
  for k,v in ipairs(varlist) do
    l[k] = code_var(v, i)
  end
  return table.concat(l, ", ")
end

function code_explist (explist, i)
  local l = {}
  for k,v in ipairs(explist) do
    l[k] = code_exp(v, i)
  end
  return table.concat(l, ", ")
end

function code_var (var, i)
  local tag = var.tag
  if tag == "VarID" then -- VarID ID
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
  else
    error("expecting a variable, but got a " .. tag)
  end
end

function code_exp (exp, i)
  local tag = exp.tag
  if tag == "ExpNil" then
  elseif tag == "ExpFalse" then
  elseif tag == "ExpTrue" then
  elseif tag == "ExpDots" then
  elseif tag == "ExpNum" then -- ExpNum Double
  elseif tag == "ExpStr" then -- ExpStr String
  elseif tag == "ExpVar" then -- ExpVar Var
  elseif tag == "ExpFunction" then -- ExpFunction [ID] Type Stm
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" or -- ExpPow Exp Exp
         tag == "ExpConcat" or -- ExpConcat Exp Exp
         tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" or -- ExpEQ Exp Exp
         tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" or -- ExpGE Exp Exp
         tag == "ExpAnd" or -- ExpAnd Exp Exp
         tag == "ExpOr" then -- ExpOr Exp Exp
  elseif tag == "ExpNot" or -- ExpNot Exp
         tag == "ExpMinus" or -- ExpMinus Exp
         tag == "ExpLen" then -- ExpLen Exp
  else
    error("expecting an expression, but got a " .. tag)
  end
end

function code_stm (stm, i)
  local tag = stm.tag
  local str
  if tag == "StmBlock" then -- StmBlock [Stm]
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
  elseif tag == "StmFunction" then -- StmFunction FuncName [ID] Type Stm
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name [ID] Type Stm
  elseif tag == "StmLabel" then -- StmLabel Name
  elseif tag == "StmGoTo" then -- StmGoTo Name
  elseif tag == "StmBreak" then -- StmBreak
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
  elseif tag == "StmRet" then -- StmRet [Exp]
  elseif tag == "StmCall" then -- StmCall Exp
  else
    error("expecting a statement, but got a " .. tag)
  end
end

function code_block (block, i)
end

function code.generate (ast)
  assert(type(ast) == "table")
  return code_block(ast, 0)
end

return code

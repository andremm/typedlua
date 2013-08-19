--[[
This file implements the code generator for Typed Lua
]]

local code = {}

local SPACES = 2

local code_block, code_stm, code_exp, code_var
local code_explist, code_varlist, code_fieldlist, code_idlist

local function spaces (n)
  return string.rep(" ", SPACES * n)
end

local function ident (s, n)
  return spaces(n) .. s
end

local function binop (tag)
  if tag == "ExpAdd" then
    return " + "
  elseif tag == "ExpSub" then
    return " - "
  elseif tag == "ExpMul" then
    return " * "
  elseif tag == "ExpDiv" then
    return " / "
  elseif tag == "ExpMod" then
    return " % "
  elseif tag == "ExpPow" then
    return " ^ "
  elseif tag == "ExpConcat" then
    return " .. "
  elseif tag == "ExpNE" then
    return " ~= "
  elseif tag == "ExpEQ" then
    return " == "
  elseif tag == "ExpLT" then
    return " < "
  elseif tag == "ExpLE" then
    return " <= "
  elseif tag == "ExpGT" then
    return " > "
  elseif tag == "ExpGE" then
    return " >= "
  elseif tag == "ExpAnd" then
    return " and "
  elseif tag == "ExpOr" then
    return " or "
  else
    error("unknown binary operator: " .. tag)
  end
end

local function unop (tag)
  if tag == "ExpNot" then
    return "not "
  elseif tag == "ExpMinus" then
    return "-"
  elseif tag == "ExpLen" then
    return "#"
  else
    error("unknown unary operator: " .. tag)
  end
end

function code_explist (explist, i)
  local l = {}
  for k,v in ipairs(explist) do
    l[k] = code_exp(v, i)
  end
  return table.concat(l, ", ")
end

function code_varlist (varlist, i)
  local l = {}
  for k,v in ipairs(varlist) do
    l[k] = code_var(v, i)
  end
  return table.concat(l, ", ")
end

function code_fieldlist (fieldlist, i)
  local l = {{},{}}
  for k,v in ipairs(fieldlist[1]) do
    l[1][k] = code_exp(v[1], i) .. ","
  end
  for k,v in ipairs(fieldlist[2]) do
    l[2][k] = "[ " .. code_exp(v[1], i) .. " ] = " .. code_exp(v[2], i) .. ","
  end
  return table.concat(l[1], " ") .. table.concat(l[2], " ")
end

function code_idlist (idlist, i)
  local l = {}
  for k,v in ipairs(idlist) do
    l[k] = v[1]
  end
  return table.concat(l, ", ")
end

function code_var (var, i)
  local tag = var.tag
  if tag == "VarID" then -- VarID ID
    return var[1]
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
    return code_exp(var[1], i) .. "[" .. code_exp(var[2], i) .. "]"
  else
    error("expecting a variable, but got a " .. tag)
  end
end

function code_exp (exp, i)
  local tag = exp.tag
  local str
  if tag == "ExpNil" then
    return "nil"
  elseif tag == "ExpFalse" then
    return "false"
  elseif tag == "ExpTrue" then
    return "true"
  elseif tag == "ExpDots" then
    return "..."
  elseif tag == "ExpNum" then -- ExpNum Double
    return tostring(exp[1])
  elseif tag == "ExpStr" then -- ExpStr String
    return '"' .. exp[1] .. '"'
  elseif tag == "ExpVar" then -- ExpVar Var
    return code_var(exp[1], i)
  elseif tag == "ExpFunction" then -- ExpFunction [ID] Type Stm
    str = "function ("
    str = str .. code_idlist(exp[1], i) .. ")"
    str = str .. code_block(exp[3], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    str = "{ " .. code_fieldlist(exp[1], i) .. " }"
    return str
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    str = code_exp(exp[1], i) .. ":" .. exp[2]
    str = str .. "(" .. code_explist(exp[3], i) .. ")"
    return str
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    str = code_exp(exp[1], i)
    str = str .. "(" .. code_explist(exp[2], i) .. ")"
    return str
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
    str = "(" .. code_exp(exp[1], i)
    str = str .. binop(tag)
    str = str .. code_exp(exp[2], i) .. ")"
    return str
  elseif tag == "ExpNot" or -- ExpNot Exp
         tag == "ExpMinus" or -- ExpMinus Exp
         tag == "ExpLen" then -- ExpLen Exp
    str = "(" .. unop(tag)
    str = str .. code_exp(exp[1], i)
    str = str .. ")"
    return str
  else
    error("expecting an expression, but got a " .. tag)
  end
end

local function code_else (stm, i)
  local tag = stm.tag
  local str
  if tag == "StmIfElse" then
    str = ident("elseif ", i) .. code_exp(stm[1], 0) .. " then\n"
    str = str .. code_block(stm[2], i)
    str = str .. code_else(stm[3], i)
    return str
  elseif tag == "StmBlock" then
    if #stm == 0 then
      return ""
    end
    str = ident("else\n", i) .. code_block(stm, i)
    return str
  else
    error("expecting StmIfElse or StmBlock, but got a " .. tag)
  end
end

local function code_funcname (funcname, i)
  local l = {}
  local len = #funcname
  local tag = funcname.tag
  local str
  if tag == "Method" then
    len = len - 1
  end
  for k=1,len do
    l[k] = funcname[k]
  end
  str = table.concat(l, ".")
  if tag == "Method" then
    str = str .. ":" .. funcname[len + 1]
  end
  return str
end

function code_stm (stm, i)
  local tag = stm.tag
  local str
  if tag == "StmBlock" then -- StmBlock [Stm]
    str = ident("do\n", i)
    str = str .. code_block(stm, i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    str = ident("if ", i) .. code_exp(stm[1], 0) .. " then\n"
    str = str .. code_block(stm[2], i)
    str = str .. code_else(stm[3], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
    str = ident("while ", i) .. code_exp(stm[1], 0) .. " do\n"
    str = str .. code_block(stm[2], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    str = ident("for ", i)
    str = str .. stm[1][1] .. "=" .. code_exp(stm[2], i)
    str = str .. "," .. code_exp(stm[3], i)
    str = str .. "," .. code_exp(stm[4], i) .. " do\n"
    str = str .. code_block(stm[5], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    str = ident("for ", i)
    str = str .. code_idlist(stm[1], i) .. " in "
    str = str .. code_explist(stm[2], i) .. " do\n"
    str = str .. code_block(stm[3], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
    str = ident("repeat\n", i)
    str = str .. code_block(stm[1], i)
    str = str .. ident("until ", i)
    str = str .. code_exp(stm[2], i)
    return str
  elseif tag == "StmFunction" then -- StmFunction FuncName [ID] Type Stm
    str = ident("function ", i) .. code_funcname(stm[1], i)
    str = str .. " (" .. code_idlist(stm[2], i) .. ")\n"
    str = str .. code_block(stm[4], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name [ID] Type Stm
    str = ident("local function ", i) .. stm[1]
    str = str .. " (" .. code_idlist(stm[2], i) .. ")\n"
    str = str .. code_block(stm[4], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "StmLabel" then -- StmLabel Name
    str = ident("::", i) .. stm[1] .. "::"
    return str
  elseif tag == "StmGoTo" then -- StmGoTo Name
    str = ident("goto ", i) .. stm[1]
    return str
  elseif tag == "StmBreak" then -- StmBreak
    str = ident("break", i)
    return str
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    str = spaces(i)
    str = str .. code_varlist(stm[1], i)
    str = str .. " = "
    str = str .. code_explist(stm[2], i)
    return str
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    str = ident("local ", i)
    str = str .. code_idlist(stm[1], i)
    if #stm[2] > 0 then
      str = str .. " = "
      str = str .. code_explist(stm[2], i)
    end
    return str
  elseif tag == "StmRet" then -- StmRet [Exp]
    str = ident("return ", i) .. code_explist(stm[1], i)
    return str
  elseif tag == "StmCall" then -- StmCall Exp
    str = spaces(i) .. code_exp(stm[1], i)
    return str
  else
    error("expecting a statement, but got a " .. tag)
  end
end

function code_block (block, i)
  local tag = block.tag
  if tag ~= "StmBlock" then
    error("expecting a block, but got a " .. tag)
  end
  local l = {}
  for k,v in ipairs(block) do
    l[k] = code_stm(v, i + 1)
  end
  return table.concat(l, "\n") .. "\n"
end

function code.generate (ast)
  assert(type(ast) == "table")
  return code_block(ast, -1)
end

return code

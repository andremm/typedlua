--[[
This file implements the code generator for Typed Lua
]]

local code = {}

local SPACES = 2

local code_block, code_stm, code_exp, code_var, code_funcname
local code_explist, code_varlist, code_fieldlist, code_parlist, code_idlist

local function spaces (n)
  return string.rep(" ", SPACES * n)
end

local function ident (s, n)
  return spaces(n) .. s
end

local function binop (tag)
  local op
  if tag == "ExpAdd" then
    op = "+"
  elseif tag == "ExpSub" then
    op = "-"
  elseif tag == "ExpMul" then
    op = "*"
  elseif tag == "ExpDiv" then
    op = "/"
  elseif tag == "ExpMod" then
    op = "%"
  elseif tag == "ExpPow" then
    op = "^"
  elseif tag == "ExpConcat" then
    op = " .. "
  elseif tag == "ExpNE" then
    op = "~="
  elseif tag == "ExpEQ" then
    op = "=="
  elseif tag == "ExpLT" then
    op = "<"
  elseif tag == "ExpLE" then
    op = "<="
  elseif tag == "ExpGT" then
    op = ">"
  elseif tag == "ExpGE" then
    op = ">="
  elseif tag == "ExpAnd" then
    op = " and "
  elseif tag == "ExpOr" then
    op = " or "
  else
    error("unknown binary operator")
  end
  return op
end

local function unop (tag)
  local op
  if tag == "ExpNot" then
    op = "not "
  elseif tag == "ExpMinus" then
    op = "-"
  elseif tag == "ExpLen" then
    op = "#"
  else
    error("unknown unary operator")
  end
  return op
end

code_idlist = function (idlist, i)
  local l = {}
  for k,v in ipairs(idlist) do
    l[k] = v[1]
  end
  return table.concat(l, ",")
end

code_parlist = function (parlist, i)
  local l = {}
  for k,v in ipairs(parlist) do
    l[k] = v[1]
  end
  if parlist.is_vararg then
    l[#l+1] = "..."
  end
  return " (" .. table.concat(l, ",") .. ")"
end

code_fieldlist = function (fieldlist, i)
  local l = {{},{}}
  for k,v in ipairs(fieldlist[1]) do
    l[1][k] = code_exp(v[1]) .. ","
  end
  for k,v in ipairs(fieldlist[2]) do
    l[2][k] = "[ " .. code_exp(v[1], i) .. " ] = " .. code_exp(v[2], i) .. ","
  end
  return table.concat(l[1], "") .. table.concat(l[2], "")
end

code_varlist = function (varlist, i)
  local l = {}
  for k,v in ipairs(varlist) do
    l[k] = code_var(v, i)
  end
  return table.concat(l, ", ")
end

code_explist = function (explist, i)
  local l = {}
  for k,v in ipairs(explist) do
    l[k] = code_exp(v, i)
  end
  return table.concat(l, ", ")
end

code_funcname = function (funcname, i)
  local l = {}
  local n = 0
  local tag = funcname.tag
  local str = ""
  if tag == "Function" then
    n = #funcname
  elseif tag == "Method" then
    n = #funcname - 1
  else
    error("expecting function or method")
  end
  for k=1,n do
    l[k] = funcname[k]
  end
  str = str .. table.concat(l, ".")
  if tag == "Method" then
    return str .. ":" .. funcname[n+1]
  end
  return str
end

code_var = function (var, i)
  local tag = var.tag
  if tag == "VarID" then -- VarID ID
    return var[1]
  elseif tag == "VarIndex" then -- VarIndex Exp Exp
    return code_exp[1] .. "[ " .. code_exp[2] .. " ]"
  else
    error("expecting a variable, but got a " .. tag)
  end
end

code_exp = function (exp, i)
  local tag = exp.tag
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
    return "[=[" .. exp[1] .. "]=]"
  elseif tag == "ExpVar" then -- ExpVar Var
    return code_var(exp[1], i)
  elseif tag == "ExpFunction" then -- ExpFunction ParList Type Stm
    return "function " .. code_parlist(exp[1], i) .. "\n" .. code_block(exp[3], i) .. ident("end", i)
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    return "{" .. code_fieldlist(exp[1], i) .. "}"
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    return code_exp(exp[1], i) .. ":" .. exp[2] .. "(" .. code_explist(exp[3], i) .. ")"
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    return code_exp(exp[1], i) .. "(" .. code_explist(exp[2], i) .. ")"
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
         tag == "ExpAnd" or -- ExpGE Exp Exp
         tag == "ExpOr" then -- ExpOr Exp Exp
    return "(" .. code_exp(exp[1], i) .. binop(tag) .. code_exp(exp[2], i) .. ")"
  elseif tag == "ExpNot" or -- ExpNot Exp
         tag == "ExpMinus" or -- ExpMinus Exp
         tag == "ExpLen" then -- ExpLen Exp
    return "(" .. unop(tag) .. code_exp(exp[1], i) .. ")"
  else
    error("expecting an expression, but got a " .. tag)
  end
end

code_stm = function (stm, i)
  local tag = stm.tag
  local str
  if tag == "StmBlock" then -- StmBlock [Stm]
    str = ident("do\n", i)
    str = str .. code_block(stm, i+1)
    str = str .. ident("end\n", i)
    return str
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    str = ident("if ", i) .. code_exp(stm[1], 0) .. " then\n"
    str = str .. code_block(stm[2], i+1)
    str = str .. ident("else\n", i)
    if stm[3].tag == "StmBlock" then
      str = str .. code_block(stm[3], i+1) .. ident("end\n", i)
    else
      str = str .. code_stm(stm[3], i+1) .. ident("end\n", i)
    end
    return str
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
    str = ident("while ", i) .. code_exp(stm[1], 0) .. " do\n"
    str = str .. code_block(stm[2], i+1) .. ident("end\n", i)
    return str
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    str = ident("for ", i) .. stm[1][1] .. "=" .. code_exp(stm[2], i)
    str = str .. "," .. code_exp(stm[3], i) .. "," .. code_exp(stm[4], i) .. " do\n"
    str = str .. code_block(stm[5], i+1) .. ident("end\n", i)
    return str
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    str = ident("for ", i) .. code_idlist(stm[1], i) .. " in "
    str = str .. code_explist(stm[2], i) .. " do\n"
    str = str .. code_block(stm[3], i+1) .. ident("end\n", i)
    return str
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
    str = ident("repeat\n", i) .. code_block(stm[1], i+1)
    str = str .. ident("until ", i) .. code_exp(stm[2], i+1)
    return str
  elseif tag == "StmFunction" then -- StmFunction FuncName ParList Type Stm
    str = ident("function ", i) .. code_funcname(stm[1], i)
    str = str .. code_parlist(stm[2], i) .. "\n"
    str = str .. code_block(stm[4], i) .. ident("end\n", i)
    return str
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name ParList Type Stm
    str = ident("local function ", i) .. stm[1]
    str = str .. code_parlist(stm[2], i) .. "\n"
    str = str .. code_block(stm[4], i) .. ident("end\n", i)
    return str
  elseif tag == "StmLabel" then -- StmLabel Name
    str = ident("::", i) .. stm[1] .. "::\n"
    return str
  elseif tag == "StmGoTo" then -- StmGoTo Name
    str = ident("goto ", i) .. stm[1] .. "\n"
    return str
  elseif tag == "StmBreak" then -- StmBreak
    return ident("break\n", i)
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    return spaces(i) .. code_varlist(stm[1], i) .. " = " .. code_explist(stm[2], i) .. "\n"
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    return ident("local ", i) .. code_idlist(stm[1], i) .. " = " .. code_explist(stm[2], i) .. "\n"
  elseif tag == "StmRet" then -- StmRet [Exp]
    return "return " .. code_explist(stm[1], i) .. "\n"
  elseif tag == "StmCall" then -- StmCall Exp
    return spaces(i) .. code_exp(stm[1], i) .. "\n"
  else
    error("expecting a statement, but got a " .. tag)
  end
end

code_block = function (block, i)
  local tag = block.tag
  if tag ~= "StmBlock" then
    error("expecting a block, but got a " .. tag)
  end
  local l = {}
  for k,v in ipairs(block) do
    l[k] = code_stm(v, i)
  end
  return table.concat(l, "\n")
end

function code.generate (ast, filename)
  assert(type(ast) == "table")
  assert(type(filename) == "string")
  local generated_code = code_block(ast, 0)
  local output = assert(io.open(filename, "w"))
  output:write(generated_code)
  output:write("\n")
  output:close()
  print(generated_code)
end

return code

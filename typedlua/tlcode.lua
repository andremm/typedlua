--[[
This file implements the code generator for Typed Lua
]]
local tlcode = {}

local code_block, code_stm, code_exp, code_var
local code_explist, code_varlist, code_fieldlist, code_idlist

local function spaces (n)
  return string.rep(" ", 2 * n)
end

local function ident (s, n)
  return spaces(n) .. s
end

local function iscntrl (x)
  if (x >= 0 and x <= 31) or (x == 127) then return true end
  return false
end

local function isprint (x)
  return not iscntrl(x)
end

local function fix_str (str)
  local new_str = ""
  for i=1,string.len(str) do
    char = string.byte(str, i)
    if char == 34 then new_str = new_str .. string.format("\\\"")
    elseif char == 92 then new_str = new_str .. string.format("\\\\")
    elseif char == 7 then new_str = new_str .. string.format("\\a")
    elseif char == 8 then new_str = new_str .. string.format("\\b")
    elseif char == 12 then new_str = new_str .. string.format("\\f")
    elseif char == 10 then new_str = new_str .. string.format("\\n")
    elseif char == 13 then new_str = new_str .. string.format("\\r")
    elseif char == 9 then new_str = new_str .. string.format("\\t")
    elseif char == 11 then new_str = new_str .. string.format("\\v")
    else
      if isprint(char) then
        new_str = new_str .. string.format("%c", char)
      else
        new_str = new_str .. string.format("\\%03d", char)
      end
    end
  end
  return new_str
end

local op = { add = " + ",
             sub = " - ",
             mul = " * ",
             idiv = " // ",
             div = " / ",
             mod = " % ",
             pow = " ^ ",
             concat = " .. ",
             eq = " == ",
             lt = " < ",
             le = " <= ",
             bor = "|",
             bxor = "~",
             band = "&",
             shl = "<<",
             shr = ">>",
             ["and"] = " and ",
             ["or"] = " or ",
             ["not"] = "not ",
             unm = "-",
             bnot = "~",
             len = "#" }

local function code_call (call, i)
  local l = {}
  for k = 2, #call do
    l[k - 1] = code_exp(call[k], i)
  end
  return code_exp(call[1], i) .. "(" .. table.concat(l, ",") .. ")"
end

local function code_invoke (invoke, i)
  local l = {}
  for k = 3, #invoke do
    l[k - 2] = code_exp(invoke[k], i)
  end
  local str = code_exp(invoke[1], i)
  str = str .. ":" .. invoke[2][1]
  str = str .. "(" .. table.concat(l, ",") .. ")"
  return str
end

local function code_parlist (parlist, i)
  local l = {}
  local len = #parlist
  local is_vararg = false
  if len > 0 and parlist[len].tag == "Dots" then
    is_vararg = true
    len = len - 1
  end
  local k = 1
  for k=1, len do
    l[k] = code_var(parlist[k])
  end
  if is_vararg then
    table.insert(l, "...")
  end
  return table.concat(l, ", ")
end

local function code_fieldlist (fieldlist, i)
  local l = {}
  for k, v in ipairs(fieldlist) do
    if v.tag == "Pair" then
      l[k] = "[" .. code_exp(v[1], i) .. "] = " .. code_exp(v[2], i)
    else
      l[k] = code_exp(v, i)
    end
  end
  return table.concat(l, ", ")
end

function code_var (var, i)
  local tag = var.tag
  if tag == "Id" then
    return var[1]
  elseif tag == "Index" then
    if var[1].tag == "Id" and var[1][1] == "_ENV" and var[2].tag == "String" then
      local v = { tag = "Id", [1] = var[2][1] }
      return code_exp(v, i)
    else
      return code_exp(var[1], i) .. "[" .. code_exp(var[2], i) .. "]"
    end
  else
    error("trying to generate code for a variable, but got a " .. tag)
  end
end

function code_varlist (varlist, i)
  local l = {}
  for k, v in ipairs(varlist) do
    l[k] = code_var(v, i)
  end
  return table.concat(l, ", ")
end

function code_exp (exp, i)
  local tag = exp.tag
  if tag == "Nil" then
    return "nil"
  elseif tag == "Dots" then
    return "..."
  elseif tag == "True" then
    return "true"
  elseif tag == "False" then
    return "false"
  elseif tag == "Number" then
    return tostring(exp[1])
  elseif tag == "String" then
    return '"' .. fix_str(exp[1]) .. '"'
  elseif tag == "Function" then
    local str = "function ("
    str = str .. code_parlist(exp[1], i) .. ")\n"
    if not exp[3] then
      str = str .. code_block(exp[2], i) .. ident("end", i)
    else
      str = str .. code_block(exp[3], i) .. ident("end", i)
    end
    return str
  elseif tag == "Table" then
    local str = "{" .. code_fieldlist(exp, i) .. "}"
    return str
  elseif tag == "Op" then
    local str = ""
    if exp[3] then
      if _VERSION == "Lua 5.3" then
        if exp[2].tag == "Call" and exp[2][1].tag == "Index" and
           exp[2][1][1].tag == "Id" and exp[2][1][1][1] == "_ENV" and
           exp[2][1][2].tag == "String" and exp[2][1][2][1] == "type" and
           exp[3].tag == "String" and exp[3][1] == "integer" then
          str = "math."
        end
      end
      str = str .. code_exp(exp[2], i) .. op[exp[1]] .. code_exp(exp[3], i)
    else
      str = str .. op[exp[1]] .. "(" .. code_exp(exp[2], i) .. ")"
    end
    return str
  elseif tag == "Paren" then
    local str = "(" .. code_exp(exp[1], i) .. ")"
    return str
  elseif tag == "Call" then
    return code_call(exp, i)
  elseif tag == "Invoke" then
    return code_invoke(exp, i)
  elseif tag == "Id" or
         tag == "Index" then
    return code_var(exp, i)
  else
    error("trying to generate code for a expression, but got a " .. tag)
  end
end

function code_explist (explist, i)
  local l = {}
  for k, v in ipairs(explist) do
    l[k] = code_exp(v, i)
  end
  return table.concat(l, ", ")
end

function code_stm (stm, i)
  local tag = stm.tag
  if tag == "Do" then
    local str = ident("do\n", i) .. code_block(stm, i) .. ident("end", i)
    return str
  elseif tag == "Set" then
    local str = spaces(i)
    str = str .. code_varlist(stm[1], i) .. " = " .. code_explist(stm[2], i)
    return str
  elseif tag == "While" then
    local str = ident("while ", i) .. code_exp(stm[1], 0) .. " do\n"
    str = str .. code_block(stm[2], i) .. ident("end", i)
    return str
  elseif tag == "Repeat" then
    local str = ident("repeat\n", i)
    str = str .. code_block(stm[1], i)
    str = str .. ident("until ", i)
    str = str .. code_exp(stm[2], i)
    return str
  elseif tag == "If" then
    local str = ident("if ", i) .. code_exp(stm[1], 0) .. " then\n"
    str = str .. code_block(stm[2], i)
    local len = #stm
    if len % 2 == 0 then
      for k=3, len, 2 do
        str = str .. ident("elseif ", i) .. code_exp(stm[k], 0) .. " then\n"
        str = str .. code_block(stm[k+1], i)
      end
    else
      for k=3, len-1, 2 do
        str = str .. ident("elseif ", i) .. code_exp(stm[k], 0) .. " then\n"
        str = str .. code_block(stm[k+1], i)
      end
      str = str .. ident("else\n", i)
      str = str .. code_block(stm[len], i)
    end
    str = str .. ident("end", i)
    return str
  elseif tag == "Fornum" then
    local str = ident("for ", i)
    str = str .. code_var(stm[1], i) .. " = " .. code_exp(stm[2], i)
    str = str .. ", " .. code_exp(stm[3], i)
    if stm[5] then
      str = str .. ", " .. code_exp(stm[4], i) .. " do\n"
      str = str .. code_block(stm[5], i)
    else
      str = str .. " do\n" .. code_block(stm[4], i)
    end
    str = str .. ident("end", i)
    return str
  elseif tag == "Forin" then
    local str = ident("for ", i)
    str = str .. code_varlist(stm[1], i) .. " in "
    str = str .. code_explist(stm[2], i) .. " do\n"
    str = str .. code_block(stm[3], i)
    str = str .. ident("end", i)
    return str
  elseif tag == "Local" then
    local str = ident("local ", i) .. code_varlist(stm[1], i)
    if #stm[2] > 0 then
      str = str .. " = " .. code_explist(stm[2], i)
    end
    return str
  elseif tag == "Localrec" then
    local str = ident("local function ", i) .. code_var(stm[1][1], i)
    str = str .. " (" .. code_parlist(stm[2][1][1], i) .. ")\n"
    if not stm[2][1][3] then
      str = str .. code_block(stm[2][1][2], i) .. ident("end", i)
    else
      str = str .. code_block(stm[2][1][3], i) .. ident("end", i)
    end
    return str
  elseif tag == "Goto" then
    local str = ident("goto ", i) .. stm[1]
    return str
  elseif tag == "Label" then
    local str = ident("::", i) .. stm[1] .. "::"
    return str
  elseif tag == "Return" then
    local str = ident("return ", i) .. code_explist(stm, i)
    return str
  elseif tag == "Break" then
    return ident("break", i)
  elseif tag == "Call" then
    return ident(code_call(stm, i), i)
  elseif tag == "Invoke" then
    return ident(code_invoke(stm, i), i)
  elseif tag == "Interface" then
    return ""
  else
    error("tyring to generate code for a statement, but got " .. tag)
  end
end

function code_block (block, i)
  local l = {}
  for k, v in ipairs(block) do
    l[k] = code_stm(v, i + 1)
  end
  return table.concat(l, "\n") .. "\n"
end

function tlcode.generate (ast)
  assert(type(ast) == "table")
  return code_block(ast, -1)
end

return tlcode

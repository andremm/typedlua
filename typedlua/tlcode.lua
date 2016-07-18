--[[
This file implements the code generator for Typed Lua
]]
local tlcode = {}

local code_block, code_stm, code_exp, code_var
local code_explist, code_varlist

local function spaces (fmt)
  return string.rep(" ", 2 * fmt.indent)
end

local function indent (s, fmt)
  return spaces(fmt) .. s
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
    local char = string.byte(str, i)
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

local function code_call (call, fmt)
  local l = {}
  for k = 2, #call do
    l[k - 1] = code_exp(call[k], fmt)
  end
  return code_exp(call[1], fmt) .. "(" .. table.concat(l, ",") .. ")"
end

local function code_invoke (invoke, fmt)
  local l = {}
  for k = 3, #invoke do
    l[k - 2] = code_exp(invoke[k], fmt)
  end
  local str = code_exp(invoke[1], fmt)
  str = str .. ":" .. invoke[2][1]
  str = str .. "(" .. table.concat(l, ",") .. ")"
  return str
end

local function code_parlist (parlist, fmt)
  local l = {}
  local len = #parlist
  local is_vararg = false
  if len > 0 and parlist[len].tag == "Dots" then
    is_vararg = true
    len = len - 1
  end
  for k=1, len do
    l[k] = code_var(parlist[k], fmt)
  end
  if is_vararg then
    table.insert(l, "...")
  end
  return table.concat(l, ", ")
end

local function is_simple_key (key)
  return key.tag == "String" and key[1]:match("^[a-zA-Z_][a-zA-Z0-9_]*$")
end

local function code_fieldlist (fieldlist, fmt)
  local l = {}
  for k, v in ipairs(fieldlist) do
    if v.tag == "Pair" then
      if is_simple_key(v[1]) then
        l[k] = v[1][1] .. " = " .. code_exp(v[2], fmt)
      else
        l[k] = "[" .. code_exp(v[1], fmt) .. "] = " .. code_exp(v[2], fmt)
      end
    else
      l[k] = code_exp(v, fmt)
    end
  end
  return table.concat(l, ", ")
end

function code_var (var, fmt)
  local tag = var.tag
  if tag == "Id" then
    return var[1]
  elseif tag == "Index" then
    if var[1].tag == "Id" and var[1][1] == "_ENV" and var[2].tag == "String" then
      local v = { tag = "Id", [1] = var[2][1] }
      return code_exp(v, fmt)
    else
      if is_simple_key(var[2]) then
        return code_exp(var[1], fmt) .. "." .. var[2][1]
      else
        return code_exp(var[1], fmt) .. "[" .. code_exp(var[2], fmt) .. "]"
      end
    end
  else
    error("trying to generate code for a variable, but got a " .. tag)
  end
end

function code_varlist (varlist, fmt)
  local l = {}
  for k, v in ipairs(varlist) do
    l[k] = code_var(v, fmt)
  end
  return table.concat(l, ", ")
end

function code_exp (exp, fmt)
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
    str = str .. code_parlist(exp[1], fmt) .. ") "
    if not exp[3] then
      str = str .. code_block(exp[2], fmt) .. indent("end", fmt)
    else
      str = str .. code_block(exp[3], fmt) .. indent("end", fmt)
    end
    return str
  elseif tag == "Table" then
    local str = "{" .. code_fieldlist(exp, fmt) .. "}"
    return str
  elseif tag == "Op" then
    local str = ""
    if exp[3] then
      str = str .. code_exp(exp[2], fmt) .. op[exp[1]] .. code_exp(exp[3], fmt)
    else
      str = str .. op[exp[1]] .. "(" .. code_exp(exp[2], fmt) .. ")"
    end
    return str
  elseif tag == "Paren" then
    local str = "(" .. code_exp(exp[1], fmt) .. ")"
    return str
  elseif tag == "Call" then
    return code_call(exp, fmt)
  elseif tag == "Invoke" then
    return code_invoke(exp, fmt)
  elseif tag == "Id" or
         tag == "Index" then
    return code_var(exp, fmt)
  else
    error("trying to generate code for a expression, but got a " .. tag)
  end
end

function code_explist (explist, fmt)
  local l = {}
  for k, v in ipairs(explist) do
    l[k] = code_exp(v, fmt)
  end
  return table.concat(l, ", ")
end

function code_stm (stm, fmt)
  local tag = stm.tag
  if tag == "Do" then
    local str = indent("do ", fmt) .. code_block(stm, fmt) .. indent("end", fmt)
    return str
  elseif tag == "Set" then
    local str = spaces(fmt)
    str = str .. code_varlist(stm[1], fmt) .. " = " .. code_explist(stm[2], fmt)
    return str
  elseif tag == "While" then
    local str = indent("while ", fmt) .. code_exp(stm[1], 0) .. " do "
    str = str .. code_block(stm[2], fmt) .. indent("end", fmt)
    return str
  elseif tag == "Repeat" then
    local str = indent("repeat ", fmt)
    str = str .. code_block(stm[1], fmt)
    str = str .. indent("until ", fmt)
    str = str .. code_exp(stm[2], fmt)
    return str
  elseif tag == "If" then
    local str = indent("if ", fmt) .. code_exp(stm[1], 0) .. " then "
    str = str .. code_block(stm[2], fmt)
    local len = #stm
    if len % 2 == 0 then
      for k=3, len, 2 do
        str = str .. indent("elseif ", fmt) .. code_exp(stm[k], 0) .. " then "
        str = str .. code_block(stm[k+1], fmt)
      end
    else
      for k=3, len-1, 2 do
        str = str .. indent("elseif ", fmt) .. code_exp(stm[k], 0) .. " then "
        str = str .. code_block(stm[k+1], fmt)
      end
      str = str .. indent("else ", fmt)
      str = str .. code_block(stm[len], fmt)
    end
    str = str .. indent("end", fmt)
    return str
  elseif tag == "Fornum" then
    local str = indent("for ", fmt)
    str = str .. code_var(stm[1], fmt) .. " = " .. code_exp(stm[2], fmt)
    str = str .. ", " .. code_exp(stm[3], fmt)
    if stm[5] then
      str = str .. ", " .. code_exp(stm[4], fmt) .. " do "
      str = str .. code_block(stm[5], fmt)
    else
      str = str .. " do " .. code_block(stm[4], fmt)
    end
    str = str .. indent("end", fmt)
    return str
  elseif tag == "Forin" then
    local str = indent("for ", fmt)
    str = str .. code_varlist(stm[1], fmt) .. " in "
    str = str .. code_explist(stm[2], fmt) .. " do "
    str = str .. code_block(stm[3], fmt)
    str = str .. indent("end", fmt)
    return str
  elseif tag == "Local" then
    local str = indent("local ", fmt) .. code_varlist(stm[1], fmt)
    if #stm[2] > 0 then
      str = str .. " = " .. code_explist(stm[2], fmt)
    end
    return str
  elseif tag == "Localrec" then
    local str = indent("local function ", fmt) .. code_var(stm[1][1], fmt)
    str = str .. " (" .. code_parlist(stm[2][1][1], fmt) .. ") "
    if not stm[2][1][3] then
      str = str .. code_block(stm[2][1][2], fmt) .. indent("end", fmt)
    else
      str = str .. code_block(stm[2][1][3], fmt) .. indent("end", fmt)
    end
    return str
  elseif tag == "Goto" then
    local str = indent("goto ", fmt) .. stm[1]
    return str
  elseif tag == "Label" then
    local str = indent("::", fmt) .. stm[1] .. "::"
    return str
  elseif tag == "Return" then
    local str = indent("return ", fmt) .. code_explist(stm, fmt)
    return str
  elseif tag == "Break" then
    return indent("break", fmt)
  elseif tag == "Call" then
    return indent(code_call(stm, fmt), fmt)
  elseif tag == "Invoke" then
    return indent(code_invoke(stm, fmt), fmt)
  elseif tag == "Interface" then
    return ""
  else
    error("tyring to generate code for a statement, but got " .. tag)
  end
end

local function resync_line(node, fmt, out)
  while node.l > fmt.line do
    table.insert(out, "\n")
    fmt.line = fmt.line + 1
  end
end

function code_block (block, fmt)
  local l = {}
  local firstline = fmt.line
  local saveindent = fmt.indent
  if block[1] and block[1].l and block[1].l > firstline then
    fmt.indent = fmt.indent + 1
  else
    fmt.indent = 0
  end
  for _, v in ipairs(block) do
    if v.l then
      resync_line(v, fmt, l)
    else
      table.insert(l, "\n")
    end
    table.insert(l, code_stm(v, fmt))
  end
  if fmt.line ~= firstline then
    table.insert(l, "\n")
    fmt.line = fmt.line + 1
  else
    table.insert(l, " ")
  end
  fmt.indent = saveindent
  return table.concat(l)
end

function tlcode.generate (ast)
  assert(type(ast) == "table")
  local fmt = { line = 1, indent = -1 }
  return code_block(ast, fmt) .. "\n"
end

return tlcode

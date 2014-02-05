--[[
This parser uses the Abstract Syntax Tree from Metalua:
https://github.com/fab13n/metalua-parser

block: { stat* }

stat:
  `Do{ stat* }
  | `Set{ {lhs+} {expr+} }                    -- lhs1, lhs2... = e1, e2...
  | `While{ expr block }                      -- while e do b end
  | `Repeat{ block expr }                     -- repeat b until e
  | `If{ (expr block)+ block? }               -- if e1 then b1 [elseif e2 then b2] ... [else bn] end
  | `Fornum{ ident expr expr expr? block }    -- for ident = e, e[, e] do b end
  | `Forin{ {ident+} {expr+} block }          -- for i1, i2... in e1, e2... do b end
  | `Local{ {ident+} {expr+}? }               -- local i1, i2... = e1, e2...
  | `Localrec{ ident expr }                   -- only used for 'local function'
  | `Goto{ <string> }                         -- goto str
  | `Label{ <string> }                        -- ::str::
  | `Return{ <expr*> }                        -- return e1, e2...
  | `Break                                    -- break
  | apply

expr:
  `Nil
  | `Dots
  | `True
  | `False
  | `Number{ <number> }
  | `String{ <string> }
  | `Function{ { `Id{ <string> }* `Dots? } block }
  | `Table{ ( `Pair{ expr expr } | expr )* }
  | `Op{ opid expr expr? }
  | `Paren{ expr }       -- significant to cut multiple values returns
  | apply
  | lhs

apply:
  `Call{ expr expr* }
  | `Invoke{ expr `String{ <string> } expr* }

lhs: `Id{ <string> } | `Index{ expr expr }

opid: 'add' | 'sub' | 'mul' | 'div' | 'mod' | 'pow' | 'concat'
  | 'eq' | 'lt' | 'le' | 'and' | 'or' | 'not' | 'unm' | 'len'
]]

local pp = {}

local block2str, stm2str, exp2str, var2str
local explist2str, varlist2str, parlist2str, fieldlist2str

local function iscntrl (x)
  if (x >= 0 and x <= 31) or (x == 127) then return true end
  return false
end

local function isprint (x)
  return not iscntrl(x)
end

local function fixed_string (str)
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

local function name2str (name)
  return string.format('"%s"', name)
end

local function number2str (n)
  return string.format('"%s"', tostring(n))
end

local function string2str (s)
  return string.format('"%s"', fixed_string(s))
end

function var2str (var)
  local tag = var.tag
  local str = "`" .. tag
  if tag == "Id" then
    str = str .. " " .. name2str(var[1])
  elseif tag == "Index" then
    str = str .. "{ "
    str = str .. exp2str(var[1]) .. ", "
    str = str .. exp2str(var[2])
    str = str .. " }"
  else
    error("expecting a variable, but got a " .. tag)
  end
  return str
end

function varlist2str (varlist)
  local l = {}
  for k, v in ipairs(varlist) do
    l[k] = var2str(v)
  end
  return "{ " .. table.concat(l, ", ") .. " }"
end

function parlist2str (parlist)
  local l = {}
  local len = #parlist
  local is_vararg = false
  if len > 0 and parlist[len].tag == "Dots" then
    is_vararg = true
    len = len - 1
  end
  local i = 1
  while i <= len do
    l[i] = var2str(parlist[i])
    i = i + 1
  end
  if is_vararg then
    l[i] = "`" .. parlist[i].tag
  end
  return "{ " .. table.concat(l, ", ") .. " }"
end

function fieldlist2str (fieldlist)
  local l = {}
  for k, v in ipairs(fieldlist) do
    local tag = v.tag
    if tag == "Pair" then
      l[k] = "`" .. tag .. "{ "
      l[k] = l[k] .. exp2str(v[1]) .. ", " .. exp2str(v[2])
      l[k] = l[k] .. " }"
    else
      l[k] = exp2str(v)
    end
  end
  if #l > 0 then
    return "{ " .. table.concat(l, ", ") .. " }"
  else
    return ""
  end
end

function exp2str (exp)
  local tag = exp.tag
  local str = "`" .. tag
  if tag == "Nil" or
     tag == "Dots" or
     tag == "True" or
     tag == "False" then
  elseif tag == "Number" then
    str = str .. " " .. number2str(exp[1])
  elseif tag == "String" then
    str = str .. " " .. string2str(exp[1])
  elseif tag == "Function" then
    str = str .. "{ "
    str = str .. parlist2str(exp[1]) .. ", "
    str = str .. block2str(exp[2])
    str = str .. " }"
  elseif tag == "Table" then
    str = str .. fieldlist2str(exp)
  elseif tag == "Op" then
    str = str .. "{ "
    str = str .. name2str(exp[1]) .. ", "
    str = str .. exp2str(exp[2])
    if exp[3] then
      str = str .. ", " .. exp2str(exp[3])
    end
    str = str .. " }"
  elseif tag == "Paren" then
    str = str .. "{ " .. exp2str(exp[1]) .. " }"
  elseif tag == "Call" then
    str = str .. "{ "
    str = str .. exp2str(exp[1])
    if exp[2] then
      for i=2, #exp do
        str = str .. ", " .. exp2str(exp[i])
      end
    end
    str = str .. " }"
  elseif tag == "Invoke" then
    str = str .. "{ "
    str = str .. exp2str(exp[1]) .. ", "
    str = str .. exp2str(exp[2])
    if exp[3] then
      for i=3, #exp do
        str = str .. ", " .. exp2str(exp[i])
      end
    end
    str = str .. " }"
  elseif tag == "Id" or tag == "Index" then
    str = var2str(exp)
  else
    error("expecting an expression, but got a " .. tag)
  end
  return str
end

function explist2str (explist)
  local l = {}
  for k, v in ipairs(explist) do
    l[k] = exp2str(v)
  end
  if #l > 0 then
    return "{ " .. table.concat(l, ", ") .. " }"
  else
    return ""
  end
end

function stm2str (stm)
  local tag = stm.tag
  local str = "`" .. tag
  if tag == "Do" then
    local l = {}
    for k, v in ipairs(stm) do
      l[k] = stm2str(v)
    end
    str = str .. "{ " .. table.concat(l, ", ") .. " }"
  elseif tag == "Set" then
    str = str .. "{ "
    str = str .. varlist2str(stm[1]) .. ", "
    str = str .. explist2str(stm[2])
    str = str .. " }"
  elseif tag == "While" then
    str = str .. "{ "
    str = str .. exp2str(stm[1]) .. ", "
    str = str .. block2str(stm[2])
    str = str .. " }"
  elseif tag == "Repeat" then
    str = str .. "{ "
    str = str .. block2str(stm[1]) .. ", "
    str = str .. exp2str(stm[2])
    str = str .. " }"
  elseif tag == "If" then
    str = str .. "{ "
    local len = #stm
    if len % 2 == 0 then
      local l = {}
      for i=1,len-2,2 do
        str = str .. exp2str(stm[i]) .. ", " .. block2str(stm[i+1]) .. ", "
      end
      str = str .. exp2str(stm[len-1]) .. ", " .. block2str(stm[len])
    else
      local l = {}
      for i=1,len-3,2 do
        str = str .. exp2str(stm[i]) .. ", " .. block2str(stm[i+1]) .. ", "
      end
      str = str .. exp2str(stm[len-2]) .. ", " .. block2str(stm[len-1]) .. ", "
      str = str .. block2str(stm[len])
    end
    str = str .. " }"
  elseif tag == "Fornum" then
    str = str .. "{ "
    str = str .. var2str(stm[1]) .. ", "
    str = str .. exp2str(stm[2]) .. ", "
    str = str .. exp2str(stm[3]) .. ", "
    if stm[5] then
      str = str .. exp2str(stm[4]) .. ", "
      str = str .. block2str(stm[5])
    else
      str = str .. block2str(stm[4])
    end
    str = str .. " }"
  elseif tag == "Forin" then
    str = str .. "{ "
    str = str .. varlist2str(stm[1]) .. ", "
    str = str .. explist2str(stm[2]) .. ", "
    str = str .. block2str(stm[3])
    str = str .. " }"
  elseif tag == "Local" then
    str = str .. "{ "
    str = str .. varlist2str(stm[1])
    if #stm[2] > 0 then
      str = str .. ", " .. explist2str(stm[2])
    else
      str = str .. ", " .. "{  }"
    end
    str = str .. " }"
  elseif tag == "Localrec" then
    str = str .. "{ "
    str = str .. "{ " .. var2str(stm[1][1]) .. " }, "
    str = str .. "{ " .. exp2str(stm[2][1]) .. " }"
    str = str .. " }"
  elseif tag == "Goto" or tag == "Label" then
    str = str .. "{ " .. name2str(stm[1]) .. " }"
  elseif tag == "Return" then
    str = str .. explist2str(stm)
  elseif tag == "Break" then
  elseif tag == "Call" then
    str = str .. "{ "
    str = str .. exp2str(stm[1])
    if stm[2] then
      for i=2, #stm do
        str = str .. ", " .. exp2str(stm[i])
      end
    end
    str = str .. " }"
  elseif tag == "Invoke" then
    str = str .. "{ "
    str = str .. exp2str(stm[1]) .. ", "
    str = str .. exp2str(stm[2])
    if stm[3] then
      for i=3, #stm do
        str = str .. ", " .. exp2str(stm[i])
      end
    end
    str = str .. " }"
  else
    error("expecting a statement, but got a " .. tag)
  end
  return str
end

function block2str (block)
  local l = {}
  for k, v in ipairs(block) do
    l[k] = stm2str(v)
  end
  return "{ " .. table.concat(l, ", ") .. " }"
end

function pp.tostring (t)
  assert(type(t) == "table")
  return block2str(t)
end

function pp.print (t)
  assert(type(t) == "table")
  print(pp.tostring(t))
end

function pp.dump (t, i)
  if i == nil then i = 0 end
  io.write(string.format("{\n"))
  io.write(string.format("%s[tag] = %s\n", string.rep(" ", i+2), t.tag))
  io.write(string.format("%s[pos] = %s\n", string.rep(" ", i+2), t.pos))
  for k,v in ipairs(t) do
    io.write(string.format("%s[%s] = ", string.rep(" ", i+2), tostring(k)))
    if type(v) == "table" then
      pp.dump(v,i+2)
    else
      io.write(string.format("%s\n", tostring(v)))
    end
  end
  io.write(string.format("%s}\n", string.rep(" ", i)))
end

return pp

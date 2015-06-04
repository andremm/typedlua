--[[
This module implements Typed Lua AST.
This AST extends the AST format implemented by Metalua.
For more information about Metalua, please, visit:
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
  | `Interface{ <string> type }

expr:
  `Nil
  | `Dots
  | `True
  | `False
  | `Number{ <number> }
  | `String{ <string> }
  | `Function{ { ident* { `Dots type? }? } typelist? block }
  | `Table{ ( `Pair{ expr expr } | expr )* }
  | `Op{ opid expr expr? }
  | `Paren{ expr }       -- significant to cut multiple values returns
  | apply
  | lhs

apply:
  `Call{ expr expr* }
  | `Invoke{ expr `String{ <string> } expr* }

lhs: ident | `Index{ expr expr }

ident: `Id{ <string> type? }

opid: 'add' | 'sub' | 'mul' | 'div' | 'mod' | 'pow' | 'concat'
  | 'eq' | 'lt' | 'le' | 'and' | 'or' | 'not' | 'unm' | 'len'

type:
  `TLiteral{ literal }
  | `TBase{ base }
  | `TNil
  | `TValue
  | `TAny
  | `TSelf
  | `TUnion{ type type type* }
  | `TFunction{ type type }
  | `TTable{ type type* }
  | `TVariable{ <string> }
  | `TRecursive{ <string> type }
  | `TVoid
  | `TUnionlist{ type type type* }
  | `TTuple{ type type* }
  | `TVararg{ type }

literal: false | true | <number> | <string>

base: 'boolean' | 'number' | 'string'

field: `TField{ <string> type }
]]

local tlast = {}

local typealias Type = {
  "tag":string,
  "interface":string?,
  "userdata":string?
}

typealias ID = {
  "tag":string,
  "pos":number,
  1:string,
  2:Type?
}

typealias NameList = {
  "tag":string,
  "pos":number,
  number:ID
}

typealias Exp = {
  "tag":string,
  "pos":number
}

typealias ExpList = {
  "tag":string,
  "pos":number,
  number:Exp
}

typealias Stm = {
  "tag":string
}

typealias StmList = {
  "tag":string,
  "pos":number,
  number:Stm
}

function tlast.namelist (pos:number, id:ID, ...:ID):(NameList)
  local t = { tag = "NameList", pos = pos, ... }
  table.insert(t, 1, id)
  return t
end

function tlast.explist (pos:number, expr:Exp, ...:Exp):(ExpList)
  local t = { tag = "ExpList", pos = pos, ... }
  table.insert(t, 1, expr)
  return t
end

-- stat

function tlast.block (pos:number, ...:Stm):(StmList)
  return { tag = "Block", pos = pos, ... }
end

function tlast.statDo (block:StmList):(Stm)
  block.tag = "Do"
  return block
end

function tlast.statWhile (pos:number, expr:Exp, block:StmList):(Stm)
  return { tag = "While", pos = pos, [1] = expr, [2] = block }
end

function tlast.statRepeat (pos:number, block:StmList, expr:Exp):(Stm)
  return { tag = "Repeat", pos = pos, [1] = block, [2] = expr }
end

function tlast.statIf (pos:number, ...:any):(Stm)
  return { tag = "If", pos = pos, ... }
end

function tlast.statFornum (pos:number, ident:ID, e1:Exp, e2:Exp, e3:Exp, block:StmList):(Stm)
  local s = { tag = "Fornum", pos = pos }
  s[1] = ident
  s[2] = e1
  s[3] = e2
  s[4] = e3
  s[5] = block
  return s
end

function tlast.statForin (pos:number, namelist:NameList, explist:ExpList, block:StmList):(Stm)
  local s = { tag = "Forin", pos = pos }
  s[1] = namelist
  s[2] = explist
  s[3] = block
  return s
end

function tlast.statLocal (pos:number, namelist:NameList, explist:ExpList):(Stm)
  return { tag = "Local", pos = pos, [1] = namelist, [2] = explist }
end

function tlast.statLocalrec (pos:number, ident:ID, expr:Exp):(Stm)
  return { tag = "Localrec", pos = pos, [1] = { ident }, [2] = { expr } }
end

function tlast.statGoto (pos:number, str:string):(Stm)
  return { tag = "Goto", pos = pos, [1] = str }
end

function tlast.statLabel (pos:number, str:string):(Stm)
  return { tag = "Label", pos = pos, [1] = str }
end

function tlast.statReturn (pos:number, ...:Exp):(Stm)
  return { tag = "Return", pos = pos, ... }
end

function tlast.statBreak (pos:number):(Stm)
  return { tag = "Break", pos = pos }
end

function tlast.statFuncSet (pos:number, is_const:boolean, lhs:any, expr:any):(Stm)
  lhs.const = is_const
  if lhs.is_method then
    table.insert(expr[1], 1, { tag = "Id", [1] = "self" })
  end
  return { tag = "Set", pos = pos, [1] = { lhs }, [2] = { expr } }
end

function tlast.statSet (...:Exp):(boolean, Stm?)
  local vl:any = { ... }
  local el = vl[#vl]
  table.remove(vl)
  for k, v in ipairs(vl) do
    if v.tag == "Id" or v.tag == "Index" then
      vl[k] = v
    else
      -- invalid assignment
      return false
    end
  end
  vl.tag = "Varlist"
  vl.pos = vl[1].pos
  return true, { tag = "Set", pos = vl.pos, [1] = vl, [2] = el }
end

function tlast.statApply (expr:Exp):(boolean,Stm?)
  if expr.tag == "Call" or expr.tag == "Invoke" then
    return true, expr
  else
    -- invalid statement
    return false
  end
end

function tlast.statInterface (pos:number, name:string, t:Type):(Stm)
  t.interface = name
  return { tag = "Interface", pos = pos, [1] = name, [2] = t }
end

function tlast.statUserdata (pos:number, name:string, t:Type):(Stm)
  t.userdata = name
  return { tag = "Userdata", pos = pos, [1] = name, [2] = t }
end

function tlast.statLocalTypeDec (stat:any):(Stm)
  stat.is_local = true
  return stat
end

-- parlist

function tlast.parList0 (pos:number):(NameList)
  return { tag = "Parlist", pos = pos }
end

function tlast.parList1 (pos:number, vararg:ID):(NameList)
  return { tag = "Parlist", pos = pos, [1] = vararg }
end

function tlast.parList2 (pos:number, namelist:NameList, vararg:ID?):(NameList)
  if vararg then table.insert(namelist, #namelist + 1, vararg) end
  return namelist
end

-- fieldlist

function tlast.fieldPair (pos:number, e1:Exp, e2:Exp):(Exp)
  return { tag = "Pair", pos = pos, [1] = e1, [2] = e2 }
end

-- expr

function tlast.exprNil (pos:number):(Exp)
  return { tag = "Nil", pos = pos }
end

function tlast.exprDots (pos:number):(Exp)
  return { tag = "Dots", pos = pos }
end

function tlast.exprTrue (pos:number):(Exp)
  return { tag = "True", pos = pos }
end

function tlast.exprFalse (pos:number):(Exp)
  return { tag = "False", pos = pos }
end

function tlast.exprNumber (pos:number, num:number):(Exp)
  return { tag = "Number", pos = pos, [1] = num }
end

function tlast.exprString (pos:number, str:string):(Exp)
  return { tag = "String", pos = pos, [1] = str }
end

function tlast.exprFunction (pos:number, parlist:NameList, rettype:Type|Stm, stat:Stm?):(Exp)
  return { tag = "Function", pos = pos, [1] = parlist, [2] = rettype, [3] = stat }
end

function tlast.exprTable (pos:number, ...:Exp):(Exp)
  return { tag = "Table", pos = pos, ... }
end

function tlast.exprUnaryOp (op:string, e:Exp):(Exp)
  return { tag = "Op", pos = e.pos, [1] = op, [2] = e }
end

function tlast.exprBinaryOp (e1:Exp, op:string?, e2:Exp?):(Exp)
  return { tag = "Op", pos = 1 }
end

function tlast.exprBinaryOp (e1:Exp, op:string?, e2:Exp?):(Exp)
  local e:Exp = { tag = "Op", pos = 1 }
  if not op then
    e = e1
  elseif op == "add" or
         op == "sub" or
         op == "mul" or
         op == "div" or
         op == "mod" or
         op == "pow" or
         op == "concat" or
         op == "eq" or
         op == "lt" or
         op == "le" or
         op == "and" or
         op == "or" then
    e = { tag = "Op", pos = e1.pos, [1] = op, [2] = e1, [3] = e2 }
  elseif op == "ne" then
    e = tlast.exprUnaryOp ("not", tlast.exprBinaryOp(e1, "eq", e2))
  elseif op == "gt" then
    e = { tag = "Op", pos = e1.pos, [1] = "lt", [2] = e2, [3] = e1 }
  elseif op == "ge" then
    e = { tag = "Op", pos = e1.pos, [1] = "le", [2] = e2, [3] = e1 }
  end
  return e
end

function tlast.exprParen (pos:number, e:Exp):(Exp)
  return { tag = "Paren", pos = pos, [1] = e }
end

function tlast.exprSuffixed (e1:Exp, e2:any):(Exp)
  if e2 then
    if e2.tag == "Call" or e2.tag == "Invoke" then
      local e:any = { tag = e2.tag, pos = e1.pos, [1] = e1 }
      for k, v in ipairs(e2) do
        table.insert(e, v)
      end
      local e:Exp = e
      return e
    else
      return { tag = "Index", pos = e1.pos, [1] = e1, [2] = e2[1] }
    end
  else
    return e1
  end
end

function tlast.exprIndex (pos:number, e:Exp):(Exp)
  return { tag = "Index", pos = pos, [1] = e }
end

function tlast.ident (pos:number, str:string, t:Type?):(ID)
  return { tag = "Id", pos = pos, [1] = str, [2] = t }
end

function tlast.index (pos:number, e1:Exp, e2:Exp):(Exp)
  return { tag = "Index", pos = pos, [1] = e1, [2] = e2 }
end

function tlast.identDots (pos:number, t:Type?):(Exp)
  return { tag = "Dots", pos = pos, [1] = t }
end

function tlast.funcName (ident1:ID, ident2:ID, is_method:boolean?):(Exp)
  if ident2 then
    local t:any = { tag = "Index", pos = ident1.pos }
    t[1] = ident1
    t[2] = ident2
    if is_method then t.is_method = is_method end
    local t:Exp = t
    return t
  else
    return ident1
  end
end

-- apply

function tlast.call (pos:number, e1:Exp, ...:Exp):(Exp)
  local a:any = { tag = "Call", pos = pos, [1] = e1 }
  local list = { ... }
  for i = 1, #list do
    a[i + 1] = list[i]
  end
  local a:Exp = a
  return a
end

function tlast.invoke (pos:number, e1:Exp, e2:Exp, ...:Exp):(Exp)
  local a:any = { tag = "Invoke", pos = pos, [1] = e1, [2] = e2 }
  local list = { ... }
  for i = 1, #list do
    a[i + 2] = list[i]
  end
  local a:Exp = a
  return a
end

-- (Exp | ID) -> (Exp | ID)
function tlast.setConst (t:any):(any)
  t.const = true
  return t
end

-- tostring

local block2str, stm2str, exp2str, var2str, type2str
local explist2str, varlist2str, parlist2str, fieldlist2str

local function iscntrl (x:number):(boolean)
  if (x >= 0 and x <= 31) or (x == 127) then return true end
  return false
end

local function isprint (x:number):(boolean)
  return not iscntrl(x)
end

local function fixed_string (str)
  local new_str = ""
  local n:number = string.len(str)
  for i=1,n do
    local char:number = string.byte(str, i)
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

local function name2str (name:string):(string)
  return string.format('"%s"', name)
end

local function number2str (n:number):(string)
  return string.format('"%s"', tostring(n))
end

local function string2str (s:string):(string)
  return string.format('"%s"', fixed_string(s))
end

function type2str (t:any):(string)
  local tag = t.tag
  local str = "`" .. tag
  if tag == "TLiteral" then
    str = str .. " " .. tostring(t[1])
  elseif tag == "TBase" then
    str = str .. " " .. t[1]
  elseif tag == "TNil" or
         tag == "TValue" or
         tag == "TAny" or
         tag == "TSelf" or
         tag == "TVoid" then
  elseif tag == "TUnion" or
         tag == "TUnionlist" then
    local l:any = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    str = str .. "{ " .. table.concat(l, ", ") .. " }"
  elseif tag == "TFunction" then
    str = str .. "{ "
    str = str .. type2str(t[1]) .. ", "
    str = str .. type2str(t[2])
    str = str .. " }"
  elseif tag == "TTable" then
    local l:any = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v[1]) .. ":" .. type2str(v[2])
    end
    str = str .. "{ " .. table.concat(l, ", ") .. " }"
  elseif tag == "TVariable" then
    str = str .. " " .. t[1]
  elseif tag == "TRecursive" then
    str = str .. "{ "
    str = str .. t[1] .. ", "
    str = str .. type2str(t[2])
    str = str .. " }"
  elseif tag == "TTuple" then
    local l:any = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v)
    end
    return str .. "{ " .. table.concat(l, ", ") .. " }"
  elseif tag == "TVararg" then
    return str .. "{ " .. type2str(t[1]) .. " }"
  else
    error("expecting a type, but got a " .. tag)
  end
  return str
end

function var2str (var:any):(string)
  local tag = var.tag
  local str = "`" .. tag
  if tag == "Id" then
    str = str .. " " .. name2str(var[1])
    if var[2] then
      str = str .. ":" .. type2str(var[2])
    end
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

function varlist2str (varlist:any):(string)
  local l:any = {}
  for k, v in ipairs(varlist) do
    l[k] = var2str(v)
  end
  return "{ " .. table.concat(l, ", ") .. " }"
end

function parlist2str (parlist:any):(string)
  local l:any = {}
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
    if parlist[i][1] then
      l[i] = l[i] .. ":" .. type2str(parlist[i][1])
    end
  end
  return "{ " .. table.concat(l, ", ") .. " }"
end

function fieldlist2str (fieldlist:any):(string)
  local l:any = {}
  for k, v in ipairs(fieldlist) do
    local tag = v.tag
    if tag == "Pair" then
      l[k] = "`" .. tag .. "{ "
      l[k] = l[k] .. exp2str(v[1]) .. ", " .. exp2str(v[2])
      l[k] = l[k] .. " }"
    else -- expr
      l[k] = exp2str(v)
    end
  end
  if #l > 0 then
    return "{ " .. table.concat(l, ", ") .. " }"
  else
    return ""
  end
end

function exp2str (exp:any):(string)
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
    str = str .. parlist2str(exp[1])
    if exp[3] then
      str = str .. ":" .. type2str(exp[2])
      str = str .. ", " .. block2str(exp[3])
    else
      str = str .. ", " .. block2str(exp[2])
    end
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
  elseif tag == "Id" or
         tag == "Index" then
    str = var2str(exp)
  else
    error("expecting an expression, but got a " .. tag)
  end
  return str
end

function explist2str (explist:any):(string)
  local l:any = {}
  for k, v in ipairs(explist) do
    l[k] = exp2str(v)
  end
  if #l > 0 then
    return "{ " .. table.concat(l, ", ") .. " }"
  else
    return ""
  end
end

function stm2str (stm:any):(string)
  local tag = stm.tag
  local str = "`" .. tag
  if tag == "Do" then -- `Do{ stat* }
    local l:any = {}
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
  elseif tag == "Goto" or
         tag == "Label" then
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
  elseif tag == "Interface" then
    str = str .. "{ "
    str = str .. stm[1] .. ", "
    str = str .. type2str(stm[2])
    str = str .. " }"
  else
    error("expecting a statement, but got a " .. tag)
  end
  return str
end

function block2str (block:any):(string)
  local l:any = {}
  for k, v in ipairs(block) do
    l[k] = stm2str(v)
  end
  return "{ " .. table.concat(l, ", ") .. " }"
end

function tlast.tostring (block:StmList):(string)
  return block2str(block)
end

function tlast.dump (t:StmList, i:number?):() end

function tlast.dump (t:StmList, i:number?):()
  i = i or 0
  io.write(string.format("{\n"))
  local t:any = t
  for k, v in pairs(t) do
    if type(k) == "string" then
      io.write(string.format("%s[%s] = %s\n", string.rep(" ", i + 2), k, tostring(v)))
    end
  end
  for k, v in ipairs(t) do
    io.write(string.format("%s[%s] = ", string.rep(" ", i + 2), tostring(k)))
    if type(v) == "table" then
      local v:any = v
      tlast.dump(v, i + 2)
    else
      io.write(string.format("%s\n", tostring(v)))
    end
  end
  io.write(string.format("%s}\n", string.rep(" ", i)))
end

return tlast

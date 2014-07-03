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
  | `Interface{ <string> field+ }
  | `LocalInterface{ <string> field+ }

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
  `Literal{ literal }
  | `Base{ base }
  | `Nil
  | `Value
  | `Any
  | `Self
  | `Union{ type type type* }
  | `Function{ typelist typelist }
  | `Table{ type type* }
  | `Variable{ <string> }
  | `Tuple{ type* }
  | `Vararg{ type }

literal: false | true | <number> | <string>

base: 'boolean' | 'number' | 'string'

field: `Field{ <string> type } | `Const{ <string> type }
]]

local tlast = {}

-- namelist : (number, ident, ident*) -> (namelist)
function tlast.namelist (pos, id, ...)
  local t = { tag = "NameList", pos = pos, ... }
  table.insert(t, 1, id)
  return t
end

-- explist : (number, expr, expr*) -> (explist)
function tlast.explist (pos, expr, ...)
  local t = { tag = "ExpList", pos = pos, ... }
  table.insert(t, 1, expr)
  return t
end

-- stat

-- block : (number, stat*) -> (block)
function tlast.block (pos, ...)
  return { tag = "Block", pos = pos, ... }
end

-- statDo : (block) -> (stat)
function tlast.statDo (block)
  block.tag = "Do"
  return block
end

-- statWhile : (number, expr, block) -> (stat)
function tlast.statWhile (pos, expr, block)
  return { tag = "While", pos = pos, [1] = expr, [2] = block }
end

-- statRepeat : (number, block, expr) -> (stat)
function tlast.statRepeat (pos, block, expr)
  return { tag = "Repeat", pos = pos, [1] = block, [2] = expr }
end

-- statIf : (number, any*) -> (stat)
function tlast.statIf (pos, ...)
  return { tag = "If", pos = pos, ... }
end

-- statFornum : (number, ident, expr, expr, expr|block, block?) -> (stat)
function tlast.statFornum (pos, ident, e1, e2, e3, block)
  local s = { tag = "Fornum", pos = pos }
  s[1] = ident
  s[2] = e1
  s[3] = e2
  s[4] = e3
  s[5] = block
  return s
end

-- statForin : (number, namelist, explist, block) -> (stat)
function tlast.statForin (pos, namelist, explist, block)
  local s = { tag = "Forin", pos = pos }
  s[1] = namelist
  s[2] = explist
  s[3] = block
  return s
end

-- statLocal : (number, namelist, explist) -> (stat)
function tlast.statLocal (pos, namelist, explist)
  return { tag = "Local", pos = pos, [1] = namelist, [2] = explist }
end

-- statLocalrec : (number, ident, expr) -> (stat)
function tlast.statLocalrec (pos, ident, expr)
  return { tag = "Localrec", pos = pos, [1] = { ident }, [2] = { expr } }
end

-- statGoto : (number, string) -> (stat)
function tlast.statGoto (pos, str)
  return { tag = "Goto", pos = pos, [1] = str }
end

-- statLabel : (number, string) -> (stat)
function tlast.statLabel (pos, str)
  return { tag = "Label", pos = pos, [1] = str }
end

-- statReturn : (number, expr*) -> (stat)
function tlast.statReturn (pos, ...)
  return { tag = "Return", pos = pos, ... }
end

-- statBreak : (number) -> (stat)
function tlast.statBreak (pos)
  return { tag = "Break", pos = pos }
end

-- statFuncSet : (number, lhs, expr) -> (stat)
function tlast.statFuncSet (pos, lhs, expr)
  if lhs.is_method then
    table.insert(expr[1], 1, { tag = "Id", [1] = "self" })
  end
  return { tag = "Set", pos = pos, [1] = { lhs }, [2] = { expr } }
end

-- statConstFuncSet : (number, lhs, expr) -> (stat)
function tlast.statConstFuncSet (pos, lhs, expr)
  if lhs.is_method then
    table.insert(expr[1], 1, { tag = "Id", [1] = "self" })
  end
  return { tag = "ConstSet", pos = pos, [1] = lhs, [2] = expr }
end

-- statSet : (expr*) -> (boolean, stat?)
function tlast.statSet (...)
  local vl = { ... }
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

-- statConstSet : (expr, expr) -> (boolean, stat?)
function tlast.statConstSet (e1, e2)
  if e1.tag == "Id" or e1.tag == "Index" then
    return true, { tag = "Set", pos = e1.pos, [1] = e1, [2] = e2 }
  else
    -- invalid assignment
    return false
  end
end

-- statApply : (expr) -> (boolean, stat?)
function tlast.statApply (expr)
  if expr.tag == "Call" or expr.tag == "Invoke" then
    return true, expr
  else
    -- invalid statement
    return false
  end
end

-- statInterface : (number, string, any*) -> (stat)
function tlast.statInterface (pos, name, ...)
  local stat = { tag = "Interface", pos = pos, ... }
  table.insert(stat, 1, name)
  return stat
end

-- statLocalInterface : (number, string, any*) -> (stat)
function tlast.statLocalInterface (pos, name, ...)
  local stat = { tag = "LocalInterface", pos = pos, ... }
  table.insert(stat, 1, name)
  return stat
end

-- parlist

-- parList0 : (number) -> (parlist)
function tlast.parList0 (pos)
  return { tag = "Parlist", pos = pos }
end

-- parList1 : (number, ident) -> (parlist)
function tlast.parList1 (pos, vararg)
  return { tag = "Parlist", pos = pos, [1] = vararg }
end

-- parList2 : (number, namelist, ident?) -> (parlist)
function tlast.parList2 (pos, namelist, vararg)
  if vararg then table.insert(namelist, vararg) end
  return namelist
end

-- fieldlist

-- fieldConst : (number, expr, expr) -> (field)
function tlast.fieldConst (pos, e1, e2)
  return { tag = "Const", pos = pos, [1] = e1, [2] = e2 }
end

-- fieldPair : (number, expr, expr) -> (field)
function tlast.fieldPair (pos, e1, e2)
  return { tag = "Pair", pos = pos, [1] = e1, [2] = e2 }
end

-- expr

-- exprNil : (number) -> (expr)
function tlast.exprNil (pos)
  return { tag = "Nil", pos = pos }
end

-- exprDots : (number) -> (expr)
function tlast.exprDots (pos)
  return { tag = "Dots", pos = pos }
end

-- exprTrue : (number) -> (expr)
function tlast.exprTrue (pos)
  return { tag = "True", pos = pos }
end

-- exprFalse : (number) -> (expr)
function tlast.exprFalse (pos)
  return { tag = "False", pos = pos }
end

-- exprNumber : (number, number) -> (expr)
function tlast.exprNumber (pos, num)
  return { tag = "Number", pos = pos, [1] = num }
end

-- exprString : (number, string) -> (expr)
function tlast.exprString (pos, str)
  return { tag = "String", pos = pos, [1] = str }
end

-- exprFunction : (number, parlist, type|stat, stat?) -> (expr)
function tlast.exprFunction (pos, parlist, rettype, stat)
  return { tag = "Function", pos = pos, [1] = parlist, [2] = rettype, [3] = stat }
end

-- exprTable : (number, field*) -> (expr)
function tlast.exprTable (pos, ...)
  return { tag = "Table", pos = pos, ... }
end

-- exprUnaryOp : (string, expr) -> (expr)
function tlast.exprUnaryOp (op, e)
  return { tag = "Op", pos = e.pos, [1] = op, [2] = e }
end

-- exprBinaryOp : (expr, string?, expr?) -> (expr)
function tlast.exprBinaryOp (e1, op, e2)
  if not op then
    return e1
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
    return { tag = "Op", pos = e1.pos, [1] = op, [2] = e1, [3] = e2 }
  elseif op == "ne" then
    return tlast.exprUnaryOp ("not", tlast.exprBinaryOp(e1, "eq", e2))
  elseif op == "gt" then
    return { tag = "Op", pos = e1.pos, [1] = "lt", [2] = e2, [3] = e1 }
  elseif op == "ge" then
    return { tag = "Op", pos = e1.pos, [1] = "le", [2] = e2, [3] = e1 }
  end
end

-- exprParen : (number, expr) -> (expr)
function tlast.exprParen (pos, e)
  return { tag = "Paren", pos = pos, [1] = e }
end

-- exprSuffixed : (expr, expr?) -> (expr)
function tlast.exprSuffixed (e1, e2)
  if e2 then
    if e2.tag == "Call" or e2.tag == "Invoke" then
      local e = { tag = e2.tag, pos = e1.pos, [1] = e1 }
      for k, v in ipairs(e2) do
        table.insert(e, v)
      end
      return e
    else
      return { tag = "Index", pos = e1.pos, [1] = e1, [2] = e2[1] }
    end
  else
    return e1
  end
end

-- exprIndex : (number, expr) -> (lhs)
function tlast.exprIndex (pos, e)
  return { tag = "Index", pos = pos, [1] = e }
end

-- ident : (number, string, type?) -> (ident)
function tlast.ident (pos, str, t)
  return { tag = "Id", pos = pos, [1] = str, [2] = t }
end

-- identDots : (number, type?) -> (expr)
function tlast.identDots (pos, t)
  return { tag = "Dots", pos = pos, [1] = t }
end

-- funcName : (ident, ident, boolean?) -> (lhs)
function tlast.funcName (ident1, ident2, is_method)
  if ident2 then
    local t = { tag = "Index", pos = ident1.pos }
    t[1] = ident1
    t[2] = ident2
    if is_method then t.is_method = is_method end
    return t
  else
    return ident1
  end
end

-- apply

-- call : (number, expr, expr*) -> (apply)
function tlast.call (pos, e1, ...)
  local a = { tag = "Call", pos = pos, [1] = e1 }
  local list = { ... }
  for i = 1, #list do
    a[i + 1] = list[i]
  end
  return a
end

-- invoke : (number, expr, expr, expr*) -> (apply)
function tlast.invoke (pos, e1, e2, ...)
  local a = { tag = "Invoke", pos = pos, [1] = e1, [2] = e2 }
  local list = { ... }
  for i = 1, #list do
    a[i + 2] = list[i]
  end
  return a
end

return tlast

--[[
This module implements a visitor for the Typed Lua AST.
]]

local tltype = require "typedlua.tltype"

local tlvisitor = {}

local visit_block, visit_stm, visit_exp, visit_var, visit_type
local visit_explist, visit_varlist, visit_parlist, visit_fieldlist

function visit_type (visitor, t)
  local tag = t.tag
  if tag == "TLiteral" then
    -- t[1]
  elseif tag == "TBase" then
    -- t[1]
  elseif tag == "TNil" or
         tag == "TValue" or
         tag == "TAny" or
         tag == "TSelf" or
         tag == "TVoid" then
  elseif tag == "TUnion" or
         tag == "TUnionlist" then
    for _, v in ipairs(t) do
      visit_type(visitor, v)
    end
  elseif tag == "TFunction" then
    visit_type(visitor, t[1])
    visit_type(visitor, t[2])
  elseif tag == "TTable" then
    for _, v in ipairs(t) do
      visit_type(visitor, v[1])
      visit_type(visitor, v[2])
    end
  elseif tag == "TVariable" or
         tag == "TGlobalVariable" then
    -- t[1]
  elseif tag == "TRecursive" then
    -- t[1]
    visit_type(visitor, t[2])
  elseif tag == "TTuple" then
    for _, v in ipairs(t) do
      visit_type(visitor, v)
    end
  elseif tag == "TVararg" then
    visit_type(visitor, t[1])
  else
    error("expecting a type, but got a " .. tag)
  end
end

function visit_var (visitor, var)
  local tag = var.tag
  if tag == "Id" then
    visitor:id(var)
  elseif tag == "Index" then
    visitor:index(var)
  else
    error("expecting a variable, but got a " .. tag)
  end
end

function visit_varlist (visitor, varlist)
  for k, v in ipairs(varlist) do
    visit_var(visitor, v)
  end
end

function visit_parlist (visitor, parlist)
  local len = #parlist
  local is_vararg = false
  if len > 0 and parlist[len].tag == "Dots" then
    is_vararg = true
    len = len - 1
  end
  local i = 1
  while i <= len do
    visit_var(visitor, parlist[i])
    i = i + 1
  end
  if is_vararg then
    if parlist[i][1] then
      visit_type(visitor, parlist[i][1])
    end
  end
end

function visit_fieldlist (visitor, fieldlist)
  for k, v in ipairs(fieldlist) do
    local tag = v.tag
    if tag == "Pair" then
      visitor:expression(v[1])
      visitor:expression(v[2])
    else -- expr
      visitor:expression(v)
    end
  end
end

function visit_exp (visitor, exp)
  local tag = exp.tag
  if tag == "Nil" or
     tag == "Dots" or
     tag == "True" or
     tag == "False" or
     tag == "Number" or
     tag == "String" then
  elseif tag == "Function" then
    visit_parlist(visitor, exp[1])
    if exp[3] then
      visit_type(visitor, exp[2])
      visitor:block(exp[3])
    else
      visitor:block(exp[2])
    end
  elseif tag == "Table" then
    visit_fieldlist(visitor, exp)
  elseif tag == "Op" then
    -- opid: exp[1]
    visit_exp(visitor, exp[2])
    if exp[3] then
      visit_exp(visitor, exp[3])
    end
  elseif tag == "Paren" then
    visit_exp(visitor, exp[1])
  elseif tag == "Call" then
    visit_exp(visitor, exp[1])
    if exp[2] then
      for i=2, #exp do
        visit_exp(visitor, exp[i])
      end
    end
  elseif tag == "Invoke" then
    visit_exp(visitor, exp[1])
    visit_exp(visitor, exp[2])
    if exp[3] then
      for i=3, #exp do
        visit_exp(visitor, exp[i])
      end
    end
  elseif tag == "Id" or
         tag == "Index" then
    visit_var(visitor, exp)
  else
    error("expecting an expression, but got a " .. tag)
  end
end

function visit_explist (visitor, explist)
  for k, v in ipairs(explist) do
    visitor:expression(v)
  end
end

function visit_stm (visitor, stm)
  local tag = stm.tag
  if tag == "Do" then -- `Do{ stat* }
    for k, v in ipairs(stm) do
      visit_stm(visitor, v)
    end
  elseif tag == "Set" then
    visit_varlist(visitor, stm[1])
    visit_explist(visitor, stm[2])
  elseif tag == "While" then
    visitor:expression(stm[1])
    visitor:block(stm[2])
  elseif tag == "Repeat" then
    visitor:block(stm[1])
    visitor:expression(stm[2])
  elseif tag == "If" then
    local len = #stm
    if len % 2 == 0 then
      for i=1,len-2,2 do
        visitor:expression(stm[i])
        visitor:block(stm[i+1])
      end
      visitor:expression(stm[len-1])
      visitor:block(stm[len])
    else
      for i=1,len-3,2 do
        visitor:expression(stm[i])
        visitor:block(stm[i+1])
      end
      visitor:expression(stm[len-2])
      visitor:block(stm[len-1])
      visitor:block(stm[len])
    end
  elseif tag == "Fornum" then
    visit_var(visitor, stm[1])
    visitor:expression(stm[2])
    visitor:expression(stm[3])
    if stm[5] then
      visitor:expression(stm[4])
      visitor:block(stm[5])
    else
      visitor:block(stm[4])
    end
  elseif tag == "Forin" then
    visit_varlist(visitor, stm[1])
    visit_explist(visitor, stm[2])
    visitor:block(stm[3])
  elseif tag == "Local" then
    visit_varlist(visitor, stm[1])
    if #stm[2] > 0 then
      visit_explist(visitor, stm[2])
    end
  elseif tag == "Localrec" then
    visit_var(visitor, stm[1][1])
    visitor:expression(stm[2][1])
  elseif tag == "Goto" or
         tag == "Label" then
    visitor:label(stm[1])
  elseif tag == "Return" then
    visit_explist(visitor, stm)
  elseif tag == "Break" then
  elseif tag == "Call" then
    visitor:expression(stm[1])
    if stm[2] then
      for i=2, #stm do
        visitor:expression(stm[i])
      end
    end
  elseif tag == "Invoke" then
    visitor:expression(stm[1])
    visitor:expression(stm[2])
    if stm[3] then
      for i=3, #stm do
        visitor:expression(stm[i])
      end
    end
  elseif tag == "Interface" then
    -- TODO? stm[1]
    visit_type(visitor, stm[2])
  else
    error("expecting a statement, but got a " .. tag)
  end
end

function visit_block (visitor, block)
  for k, v in ipairs(block) do
    visitor:statement(v)
  end
end

function tlvisitor.visit(block, visitor)
  local function nop(visitor) end
  visitor.id = visitor.id or nop
  visitor.index = visitor.id or function(visitor, var)
    visitor:expression(var[1])
    visitor:expression(var[2])
  end
  visitor.label = visitor.label or nop
  visitor.block = visitor.block or visit_block
  visitor.statement = visitor.statement or visit_stm
  visitor.expression = visitor.expression or visit_exp
  visitor:block(block)
end

return tlvisitor

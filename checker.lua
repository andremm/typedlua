--[[
This file implements the type checker for Typed Lua
]]

local parser = require "parser"
local types = require "subtype"

local checker = {}

local st = {} -- symbol table

local function lineno (pos)
  return parser.lineno(st["subject"], pos)
end

local function errormsg (pos)
  local l,c = lineno(pos)
  return string.format("%s:%d:%d:", st["filename"], l, c)
end

local function semerror (msg, pos)
  local error_msg = "%s semantic error, %s"
  error_msg = string.format(error_msg, errormsg(pos), msg)
  table.insert(st["semerror"], error_msg)
end

-- functions that handle the symbol table

local function new_scope ()
  if not st["scope"] then
    st["scope"] = 0
  else
    st["scope"] = st["scope"] + 1
  end
  return st["scope"]
end

local function begin_scope ()
  local scope = new_scope()
  st["maxscope"] = scope
  st[scope] = {} -- new hash for new scope
  st[scope]["label"] = {} -- stores label definitions of a scope
  st[scope]["goto"] = {} -- stores goto definitions of a scope 
  st[scope]["local"] = {} -- stores local variables of a scope
end

local function end_scope ()
  st["scope"] = st["scope"] - 1
end

-- functions that handle invalid use of break

local function begin_loop ()
  if not st["loop"] then
    st["loop"] = 1
  else
    st["loop"] = st["loop"] + 1
  end
end

local function end_loop ()
  st["loop"] = st["loop"] - 1
end

local function insideloop ()
  if st["loop"] and st["loop"] > 0 then
    return true
  end
  return false
end


-- functions that handle identifiers

local function new_id (id_name, id_pos, id_type)
  local id = {}
  id["name"] = id_name
  id["pos"] = id_pos
  id["type"] = id_type
  return id
end

-- functions that handle labels and gotos

local function lookup_label (stm, scope)
  local label = stm[1]
  for s=scope,0,-1 do
    if st[s]["label"][label] then
      return true
    end
  end
  return false
end

local function set_label (stm)
  local scope = st["scope"]
  local label = stm[1]
  local pos = stm["pos"]
  local l = st[scope]["label"][label]
  if not l then
    local t = { name = label, pos = pos }
    st[scope]["label"][label] = t
  else
    local msg = "label '%s' already defined at line %d"
    local line,col = lineno(l["pos"])
    msg = string.format(msg, label, line)
    semerror(msg, pos)
  end
end

local function check_pending_gotos ()
  for s=st["maxscope"],0,-1 do
    for k,v in ipairs(st[s]["goto"]) do
      local label = v[1]
      local pos = v["pos"]
      if not lookup_label(v,s) then
        local msg = "no visible label '%s' for <goto> at line %d"
        local line,col = lineno(pos)
        msg = string.format(msg, label, line)
        semerror(msg, pos)
      end
    end
  end
end

local function set_pending_goto (stm)
  local scope = st["scope"]
  table.insert(st[scope]["goto"], stm)
end

local check_block, check_stm, check_exp, check_var
local check_explist

-- expressions

-- variables

-- statemnts

local function check_assignment (varlist, explist)

end

local function check_break (stm)
  if not insideloop() then
    local msg = "<break> at line %d not inside a loop"
    local pos = stm["pos"]
    local line,col = lineno(pos)
    msg = string.format(msg, line)
    semerror(msg, pos)
  end
end

local function check_call (exp)

end

local function check_for_generic (idlist, explist, stm)
  being_loop()
  check_stm(stm)
  end_loop()
end

local function check_for_numeric (id, exp1, exp2, exp3, stm)
  begin_loop()
  check_stm(stm)
  end_loop()
end

local function check_global_function (fname, idlist, ret_type, stm)
  check_stm(stm)
end

local function check_goto (stm)
  set_pending_goto(stm)
end

local function check_if_else (exp, stm1, stm2)
  check_stm(stm1)
  check_stm(stm2)
end

local function check_label (stm)
  set_label(stm)
end

local function check_local_function (name, idlist, ret_type, stm)
  check_stm(stm)
end

local function check_local_var (idlist, explist)
  check_explist(explist)
end

local function check_repeat (stm, exp)
  begin_loop()
  check_stm(stm)
  end_loop()
end

local function check_return (explist)

end

local function check_while (exp, stm)
  begin_loop()
  check_stm(stm)
  end_loop()
end

function check_explist (explist)
  for k,v in ipairs(explist) do
    check_exp(v)
  end
end

function check_exp (exp)
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
         tag == "ExpPow" then -- ExpPow Exp Exp
  elseif tag == "ExpConcat" then -- ExpConcat Exp Exp
  elseif tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" then -- ExpEQ Exp Exp
  elseif tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" then -- ExpGE Exp Exp
  elseif tag == "ExpAnd" then -- ExpAnd Exp Exp
  elseif tag == "ExpOr" then -- ExpOr Exp Exp
  elseif tag == "ExpNot" then -- ExpNot Exp
  elseif tag == "ExpMinus" then -- ExpMinus Exp
  elseif tag == "ExpLen" then -- ExpLen Exp
  else
    error("cannot type check expression " .. tag)
  end
end

function check_stm (stm)
  local tag = stm.tag
  if tag == "StmBlock" then -- StmBlock [Stm]
    check_block(stm)
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    check_if_else(stm[1], stm[2], stm[3])
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
    check_while(stm[1], stm[2])
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    check_for_numeric(stm[1], stm[2], stm[3], stm[4], stm[5])
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    check_for_generic(stm[1], stm[2], stm[3])
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
    check_repeat(stm[1], stm[2])
  elseif tag == "StmFunction" then -- StmFunction FuncName [ID] Type Stm
    check_global_function(stm[1], stm[2], stm[3], stm[4])
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name [ID] Type Stm
    check_local_function(stm[1], stm[2], stm[3], stm[4])
  elseif tag == "StmLabel" then -- StmLabel Name
    check_label(stm)
  elseif tag == "StmGoTo" then -- StmGoTo Name
    check_goto(stm)
  elseif tag == "StmBreak" then -- StmBreak
    check_break(stm)
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    check_assignment(stm[1], stm[2])
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    check_local_var(stm[1], stm[2])
  elseif tag == "StmRet" then -- StmRet [Exp]
    check_return(stm[1])
  elseif tag == "StmCall" then -- StmCall Exp
    check_call(stm[1])
  else
    error("cannot type check statement " .. tag)
  end
end

function check_block (block)
  local tag = block.tag
  if tag ~= "StmBlock" then
    error("cannot type block " .. tag)
  end
  begin_scope()
  for k,v in ipairs(block) do
    check_stm(v)
  end
  end_scope()
end

local function init_symbol_table (subject, filename)
  st = {} -- reseting the symbol table
  st["subject"] = subject -- store subject for error messages
  st["filename"] = filename -- store filename for error messages
  st["global"] = {} -- store global names
  st["semerror"] = {} -- store semantic errors
  for k,v in pairs(_ENV) do
    local t = type(v)
    local any = types.Any()
    local any_star = types.VarArg(any)
    if t == "string" then
      st["global"][k] = new_id(k, 0, types.ConstantString(v))
    elseif t == "function" then
      st["global"][k] = new_id(k, 0, types.Function(any_star,any))
    else
      st["global"][k] = new_id(k, 0, any)
    end
  end
end

function checker.typecheck (ast, subject, filename)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  local msg = ""
  local typeerror, semerror
  init_symbol_table(subject, filename)
  check_block(ast)
  check_pending_gotos()
  if #st["semerror"] > 0 then
    semerror = true
    msg = msg .. table.concat(st["semerror"], "\n")
  end
  if semerror then
    return nil,msg
  end
  return true
end

return checker

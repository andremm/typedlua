--[[
This file implements the type checker for Typed Lua
]]

local parser = require "parser"
local types = require "types"

local Object = types.Object()
local Any = types.Any()
local Nil = types.Nil()
local False = types.False()
local True = types.True()
local Boolean = types.Boolean()
local Number = types.Number()
local String = types.String()

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
  table.insert(st["messages"], error_msg)
end

local function typeerror (msg, pos)
  local error_msg = "%s type error, %s"
  error_msg = string.format(error_msg, errormsg(pos), msg)
  table.insert(st["messages"], error_msg)
end

local function warning (msg, pos)
  local error_msg = "%s warning, %s"
  error_msg = string.format(error_msg, errormsg(pos), msg)
  table.insert(st["messages"], error_msg)
end

local function name2type (name)
  local t = types.name2type(name)
  if not t then
    local msg = "type '%s' is not defined, so it will be interpreted as 'any'"
    msg = string.format(msg, name)
    return nil,msg
  end
  return t
end

local function get_fill_type (list)
  local len = #list
  if len > 0 then
    local last_type = list[len]["type"]
    if types.isVarArg(last_type) then
      return types.typeofVarArg(last_type)
    end
  end
  return Nil
end

local function get_node_type (node, fill_type)
  if not node then
    return fill_type
  end
  local node_type = node["type"]
  if types.isVarArg(node_type) then
    return types.typeofVarArg(node_type)
  end
  return node_type
end

local function set_node_type (node, node_type)
  node["type"] = node_type
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

local function new_function ()
  if not st["fscope"] then
    st["fscope"] = 0
  else
    st["fscope"] = st["fscope"] + 1
  end
  return st["fscope"]
end

local function begin_function ()
  local fscope = new_function()
  st["function"][fscope] = {}
end

local function end_function ()
  st["fscope"] = st["fscope"] - 1
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

-- variables

local function new_var (name, dec_type, pos, inf_type)
  local var = {}
  var["tag"] = "VarID"
  var[1] = name
  var[2] = dec_type
  var["pos"] = pos
  var["type"] = inf_type
  return var
end

local function id2var (id)
  local var_type,msg = name2type(id[2])
  if not var_type then
    id[2] = "any"
    var_type = Any
    typeerror(msg, id["pos"])
  end
  return new_var(id[1], id[2], id["pos"], var_type)
end

local function idlist2varlist (idlist)
  local list = {}
  for k,v in ipairs(idlist) do
    local var = id2var(v)
    table.insert(list, var)
  end
  return list
end

local function isglobal (name)
  if st["global"][name] then
    return true
  end
  return false
end

local function islocal (name)
  for s=st["scope"],0,-1 do
    if st[s]["local"][name] then
      return true
    end
  end
  return false
end

local function get_global (name)
  return st["global"][name]
end

local function get_local_scope (name)
  local scope = st["scope"]
  for s=scope,0,-1 do
    if st[s]["local"][name] then
      return s
    end
  end
  return nil
end

local function get_visibility (name)
  if islocal(name) then
    return "local"
  elseif isglobal(name) then
    return "global"
  end
  return nil
end

local function set_var (var, inf_type, scope)
  local msg
  local name = var[1]
  local pos = var["pos"]
  local dec_type = var["type"]
  local shadowing
  if types.subtype(inf_type, dec_type) then
    var["type"] = dec_type
  elseif types.isAny(dec_type) then
    if not types.isNil(inf_type) then
      if types.isConstant(inf_type) then
        local c = inf_type[1]
        local t = type(c)
        if t == "boolean" then
          inf_type = Boolean
        elseif t == "number" then
          inf_type = Number
        elseif t == "string" then
          inf_type = String
        else
          inf_type = Nil
        end
      end
      var["type"] = inf_type
    else
      var["type"] = dec_type
    end
    msg = "attempt to cast 'any' to '%s'"
    msg = msg:format(types.tostring(inf_type))
    warning(msg, var["pos"])
  elseif types.isAny(inf_type) then
    var["type"] = dec_type
    msg = "attempt to cast '%s' to 'any'"
    msg = msg:format(types.tostring(dec_type))
    warning(msg, var["pos"])
  else
    var["type"] = dec_type
    msg = "attempt to assign '%s' to '%s'"
    msg = msg:format(types.tostring(inf_type), types.tostring(dec_type))
    typeerror(msg, var["pos"])
  end
  local shadowing, local_or_global
  if scope then -- local
    shadowing = st[scope]["local"][name]
    if shadowing then
      local_or_global = "local"
    end
    st[scope]["local"][name] = var
  else -- global
    shadowing = st["global"][name]
    if shadowing then
      local_or_global = "global"
    end
    st["global"][name] = var
  end
  if shadowing then
    local line,col = lineno(shadowing["pos"])
    local t1,t2 = types.tostring(shadowing["type"]), types.tostring(var["type"])
    msg = "%s '%s' was previously defined on line %d"
    msg = msg:format(local_or_global, name, line)
    warning(msg, var["pos"])
    msg = "shadowing local '%s' from '%s' to '%s'"
    msg = msg:format(name, t1, t2)
    warning(msg, var["pos"])
  end
end

local function update_var (name, pos, inf_type, scope)
  local var
  if scope then -- local
    var = st[scope]["local"][name]
  else -- global
    var = st["global"][name]
  end
  local dec_type = var["type"]
  if types.isAny(dec_type) then
    local msg = "attempt to cast 'any' to 'number'"
    msg = msg:format(types.tostring(inf_type))
    warning(msg, pos)
  elseif types.isAny(inf_type) then
    local msg = "attempt to cast '%s' to 'any'"
    msg = msg:format(types.tostring(dec_type))
    warning(msg, pos)
  elseif not types.subtype(inf_type, dec_type) then
    local msg = "attempt to assign '%s' to '%s'"
    msg = msg:format(types.tostring(inf_type), types.tostring(dec_type))
    typeerror(msg, pos)
  end
end

function check_var (var)
  local tag = var.tag
  if tag == "VarID" then
    local t,msg = name2type(var[2])
    if not t then
      t = Any
      typeerror(msg, var["pos"])
    end
    set_node_type(var, t)
  elseif tag == "VarIndex" then
    local exp1, exp2 = var[1], var[2]
    check_exp(exp1)
    check_exp(exp2)
    set_node_type(var, Any)
  else
    error("cannot type check a variable " .. tag)
  end
end

-- functions

local function set_vararg (btype)
  local fscope = st["fscope"]
  st["function"][fscope]["is_vararg"] = true
  st["function"][fscope]["vararg"] = types.VarArg(btype)
end

local function check_function (idlist, dec_type, stm)
  begin_function()
  begin_scope()
  local varlist = idlist2varlist(idlist)
  local len = #varlist
  local args_type = {}
  if len > 0 then
    if varlist[len][1] == "..." then
      set_vararg(varlist[len]["type"])
      varlist[len]["type"] = types.VarArg(varlist[len]["type"])
    end
    for k,v in ipairs(varlist) do
      table.insert(args_type, v["type"])
    end
  else
    table.insert(args_type, types.VarArg(Object))
  end
  local ret_type,msg = name2type(dec_type)
  if not ret_type then
    ret_type = Any
    typeerror(msg, exp["pos"])
  end
  check_stm(stm)
  end_scope()
  end_function()
  return types.Function(args_type, ret_type)
end

-- expressions

local function explist2typelist (explist)
  local list = {}
  local len = #explist
  if len == 0 then
    table.insert(list, types.VarArg(Object))
  else
    for k,v in ipairs(explist) do
      table.insert(list, explist[k]["type"])
    end
  end
  return list
end

local function check_and (exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(exp1)
  check_exp(exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  set_node_type(exp, types.Union(t1, t2)) -- T-AND
end

local function check_anonymous_function (exp)
  local idlist, dec_type, stm = exp[1], exp[2], exp[3]
  local t = check_function(idlist, dec_type, stm)
  set_node_type(exp, t)
end

local function check_arith (exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(exp1)
  check_exp(exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  if types.subtype(t1, Number) and
     types.subtype(t2, Number) then -- T-ARITH1
    set_node_type(exp, Number)
  elseif types.isAny(t1) or -- T-ARITH2
         types.isAny(t2) then -- T-ARITH3
    set_node_type(exp, Any)
  else
    local wrong
    set_node_type(exp, Any)
    if not types.subtype(t1, Number) and
       not types.isAny(t1) then
      wrong = exp1
    else
      wrong = exp2
    end
    local msg
    msg = "attempt to perform arithmetic on a %s"
    msg = string.format(msg, types.tostring(wrong["type"]))
    typeerror(msg, wrong["pos"])
  end
end

-- function name, parameter number, dec type, given type, pos

local function check_call_arg (fname, k, dtype, gtype, pos)
  local msg
  if not types.subtype(gtype, dtype) then
    msg = "parameter %d of '%s', attempt to assign '%s' to '%s'"
    msg = msg:format(k, fname, types.tostring(gtype), types.tostring(dtype))
    typeerror(msg, pos)
  elseif types.isAny(gtype) then
    msg = "parmeter %d of '%s', attempt to cast 'any' to '%s'"
    msg = msg:format(k, fname, types.tostring(dtype))
    warning(msg, pos)
  elseif types.isAny(dtype) then
    msg = "parameter %d of '%s', attempt to cast '%s' to 'any'"
    msg = msg:format(k, fname, types.tostring(gtype))
    warning(msg, pos)
  end
end

local function check_call_args (fname, args, explist, pos)
  local len_args, len_list = #args, #explist
  local dec_type, given_type
  local fill_type = Nil
  if len_list == 0 then -- calling void
    dec_type = args[1]
    if types.isVarArg(dec_type) then
      dec_type = types.typeofVarArg(dec_type)
      if types.isObject(dec_type) then -- function is void
        return
      end
    end
  end
  if len_list < len_args then
    local i = 1
    while i < len_list do
      pos = explist[i]["pos"]
      dec_type = args[i]
      given_type = explist[i]["type"]
      if types.isVarArg(given_type) then
        given_type = types.typeofVarArg(given_type)
      end
      check_call_arg(fname, i, dec_type, given_type, pos)
      i = i + 1
    end
    local exp = explist[i]
    if not exp then
      given_type = fill_type
    else
      pos = explist[i]["pos"]
      given_type = explist[i]["type"]
      if types.isVarArg(given_type) then
        fill_type = types.typeofVarArg(given_type)
        given_type = fill_type
      end
    end
    local j = i
    while j < len_args do
      dec_type = args[j]
      exp = explist[j]
      if not exp then
        given_type = fill_type
      else
        pos = exp["pos"]
        given_type = exp["type"]
        if types.isVarArg(given_type) then
          given_type = types.typeofVarArg(given_type)
        end
      end
      check_call_arg(fname, j, dec_type, given_type, pos)
      j = j + 1
    end
    if types.isNil(fill_type) then
      check_call_arg(fname, j, args[j], fill_type, pos)
    else
      check_call_arg(fname, j, args[j], explist[i]["type"], pos)
    end
  else
    local i = 1
    while i < len_args do
      dec_type = args[i]
      given_type = explist[i]["type"]
      pos = explist[i]["pos"]
      if types.isVarArg(given_type) then
        given_type = types.typeofVarArg(given_type)
      end
      check_call_arg(fname, i, dec_type, given_type, pos)
      i = i + 1
    end
    dec_type = args[i]
    local j = i
    if types.isVarArg(dec_type) then
      while j < len_list do
        given_type = explist[j]["type"]
        pos = explist[j]["pos"]
        if types.typeofVarArg(given_type) then
          given_type = types.typeofVarArg(given_type)
        end
        check_call_arg(fname, j, dec_type, given_type, pos)
        j = j + 1
      end
      given_type = explist[j]["type"]
      pos = explist[j]["pos"]
      check_call_arg(fname, j, dec_type, given_type, pos)
    else
      check_call_arg(fname, j, args[i], explist[i]["type"], pos)
    end
  end
end

local function check_call_ret (fname, ftype, pos)

end

local function check_call (fname, ftype, explist, pos, visibility)
  local msg
  if types.isAny(ftype) then
    msg = "attempt to call %s '%s' of type 'any'"
    msg = msg:format(local_or_global, var_name)
    warning(msg, pos)
    return Any
  elseif types.isFunction(ftype) then
    check_call_args(fname, ftype[1], explist, pos, visibility)
    check_call_ret(fname, ftype[2], pos)
    return ftype[2]
  else
    msg = "attempt to call %s '%s' of type '%s'"
    msg = msg:format(local_or_global, var_name, types.tostring(var_type))
    typeerror(msg, pos)
    return var_type
  end
end

local function check_calling_method (exp)
  set_node_type(exp, Any)
end

local function check_calling_function (exp)
  local var, explist, pos = exp[1][1], exp[2], exp["pos"]
  local var_name = var[1]
  local isvisible = get_visibility(var_name)
  local ret_type = Nil
  check_explist(explist)
  if isvisible then
    local var_type
    if isvisible == "local" then
      local scope = get_local_scope(var_name)
      var_type = st[scope]["local"][var_name]["type"]
    else
      var_type = st["global"][var_name]["type"]
    end
    ret_type = check_call(var_name, var_type, explist, pos, isvisible)
  else
    local msg = "attempt to call undeclared function '%s'"
    msg = msg:format(var_name)
    typeerror(msg, pos)
  end
  set_node_type(exp, ret_type)
end

local function check_concat (exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(exp1)
  check_exp(exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  if types.subtype(t1, String) and
     types.subtype(t2, String) then -- T-CONCAT1
    set_node_type(exp, String)
  elseif types.isAny(t1) or -- T-CONCAT2
         types.isAny(t2) then -- T-CONCAT3
    set_node_type(exp, Any)
  else
    local wrong
    set_node_type(exp, Any)
    if not types.subtype(t1, String) and
       not types.isAny(t1) then
      wrong = exp1
    else
      wrong = exp2
    end
    local msg
    msg = "attempt to concatenate a %s"
    msg = string.format(msg, types.tostring(wrong["type"]))
    typeerror(msg, wrong["pos"])
  end
end

local function check_equal (exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(exp1)
  check_exp(exp2)
  set_node_type(exp, Boolean) -- T-EQUAL
end

local function check_expvar (exp)
  local var = exp[1]
  local name = var[1]
  local scope = get_local_scope(name)
  local t,msg
  if scope then -- local
    t = st[scope]["local"][name]["type"]
  else -- global
    local g = st["global"][name]
    if g then
      t = g["type"]
    else
      t = types.Nil()
      msg = "using variable '%s' without initialize"
      msg = string.format(msg, name)
      typeerror(msg, exp["pos"])
    end
  end
  set_node_type(exp, t)
end

local function check_len (exp)
  local exp1 = exp[1]
  check_exp(exp1)
  local t1 = exp1["type"]
  if types.subtype(t1, String) then -- T-LEN1
    set_node_type(exp, Number)
  elseif types.isAny(t1) then -- T-LEN2
    set_node_type(exp, Any)
  else
    set_node_type(exp, Any)
    local msg = "attempt to get length of a %s value"
    msg = string.format(msg, types.tostring(t1))
    typeerror(msg, exp1["pos"])
  end
end

local function check_minus (exp)
  local exp1 = exp[1]
  check_exp(exp1)
  local t1 = exp1["type"]
  if types.subtype(t1, Number) then -- T-MINUS1
    set_node_type(exp, Number)
  elseif types.isAny(t1) then -- T-MINUS2
    set_node_type(exp, Any)
  else
    set_node_type(exp, Any)
    local msg
    msg = "attempt to perform arithmetic on a %s"
    msg = string.format(msg, types.tostring(exp1["type"]))
    typeerror(msg, exp1["pos"])
  end
end

local function check_not (exp)
  local exp1 = exp[1]
  check_exp(exp1)
  set_node_type(exp, Boolean) -- T-NOT
end

local function check_or (exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(exp1)
  check_exp(exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  set_node_type(exp, types.Union(t1, t2)) -- T-OR
end

local function check_order (exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(exp1)
  check_exp(exp2)
  local t1, t2 = exp1["type"], exp2["type"]
  set_node_type(exp, Boolean)
  if types.subtype(t1, Number) and
     types.subtype(t2, Number) then -- T-ORDER1
  elseif types.subtype(t1, String) and
         types.subtype(t2, String) then -- T-ORDER2
  elseif types.isAny(t1) or -- T-ORDER3
         types.isAny(t2) then -- T-ORDER4
  else
    local msg = "attempt to compare %s with %s"
    msg = string.format(msg, types.tostring(t1), types.tostring(t2))
    typeerror(msg, exp["pos"])
  end
end

local function check_table (exp)
  set_node_type(exp, Any)
end

local function check_vararg (exp)
  local fscope = st["fscope"]
  local vararg_type
  if st["function"][fscope]["is_vararg"] then
    vararg_type = st["function"][fscope]["vararg"]
  else
    local msg = "cannot use '...' outside a vararg function"
    semerror(msg, exp["pos"])
    vararg_type = types.VarArg(Any)
  end
  set_node_type(exp, vararg_type)
end

-- statemnts

local function check_assignment (varlist, explist)
  check_explist(explist)
  local fill_type = get_fill_type(explist)
  for k,v in ipairs(varlist) do
    check_var(v)
    local inf_type = get_node_type(explist[k], fill_type)
    local scope = get_local_scope(v[1])
    if scope then -- local
      update_var(v[1], v["pos"], inf_type, scope)
    else -- global
      local global = get_global(v[1])
      if not global then
        set_var(v, inf_type)
      else
        update_var(v[1], v["pos"], inf_type)
      end
    end
  end
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

local function check_stmcall (exp)
  check_exp(exp)
end

local function check_for_generic (idlist, explist, stm)
  begin_loop()
  begin_scope()
  check_explist(explist)
  check_stm(stm)
  end_scope()
  end_loop()
end

local function check_for_numeric (id, exp1, exp2, exp3, stm)
  begin_loop()
  begin_scope()
  local var = id2var(id)
  set_var(var, Number, st["scope"])
  check_exp(exp1)
  check_exp(exp2)
  check_exp(exp3)
  local t1, t2, t3 = exp1["type"], exp2["type"], exp3["type"]
  local msg
  if types.isAny(t1) then
    msg = "'for' initial value is any"
    warning(msg, exp1["pos"])
  elseif not types.subtype(t1, Number) then
    msg = "'for' initial value must be a number"
    typeerror(msg, exp1["pos"])
  end
  if types.isAny(t2) then
    msg = "'for' limit value is any"
    warning(msg, exp1["pos"])
  elseif not types.subtype(t2, Number) then
    msg = "'for' limit must be a number"
    typeerror(msg, exp2["pos"])
  end
  if types.isAny(t3) then
    msg = "'for' step value is any"
    warning(msg, exp1["pos"])
  elseif not types.subtype(t3, Number) then
    msg = "'for' step must be a number"
    typeerror(msg, exp3["pos"])
  end
  check_stm(stm)
  end_scope()
  end_loop()
end

local function check_global_function (stm)
  local name, idlist, dec_type, stm1 = stm[1][1], stm[2], stm[3], stm[4]
  local t = check_function(idlist, dec_type, stm1)
  local var = new_var(name, "any", stm["pos"], t)
  set_var(var, t)
end

local function check_goto (stm)
  set_pending_goto(stm)
end

local function check_if_else (exp, stm1, stm2)
  check_exp(exp)
  check_stm(stm1)
  check_stm(stm2)
end

local function check_label (stm)
  set_label(stm)
end

local function check_local_function (stm)
  local name, idlist, dec_type, stm1 = stm[1], stm[2], stm[3], stm[4]
  local t = check_function(idlist, dec_type, stm1)
  local var = new_var(name, "any", stm["pos"], t)
  set_var(var, t, st["scope"])
end

local function check_local_var (idlist, explist)
  local varlist = idlist2varlist(idlist)
  check_explist(explist)
  local fill_type = get_fill_type(explist)
  for k,v in ipairs(varlist) do
    local inf_type = get_node_type(explist[k], fill_type)
    local scope = st["scope"]
    set_var(v, inf_type, scope)
  end
end

local function check_repeat (stm, exp)
  begin_loop()
  check_stm(stm)
  check_exp(exp)
  end_loop()
end

local function check_return (explist)
  check_explist(explist)
end

local function check_while (exp, stm)
  begin_loop()
  check_exp(exp)
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
    set_node_type(exp, Nil)
  elseif tag == "ExpFalse" then
    set_node_type(exp, False)
  elseif tag == "ExpTrue" then
    set_node_type(exp, True)
  elseif tag == "ExpDots" then
    check_vararg(exp)
  elseif tag == "ExpNum" then -- ExpNum Double
    set_node_type(exp, types.ConstantNumber(exp[1]))
  elseif tag == "ExpStr" then -- ExpStr String
    set_node_type(exp, types.ConstantString(exp[1]))
  elseif tag == "ExpVar" then -- ExpVar Var
    check_expvar(exp)
  elseif tag == "ExpFunction" then -- ExpFunction [ID] Type Stm
    check_anonymous_function(exp)
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    check_table(exp)
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    check_calling_method(exp)
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    check_calling_function(exp)
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" then -- ExpPow Exp Exp
    check_arith(exp)
  elseif tag == "ExpConcat" then -- ExpConcat Exp Exp
    check_concat(exp)
  elseif tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" then -- ExpEQ Exp Exp
    check_equal(exp)
  elseif tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" then -- ExpGE Exp Exp
    check_order(exp)
  elseif tag == "ExpAnd" then -- ExpAnd Exp Exp
    check_and(exp)
  elseif tag == "ExpOr" then -- ExpOr Exp Exp
    check_or(exp)
  elseif tag == "ExpNot" then -- ExpNot Exp
    check_not(exp)
  elseif tag == "ExpMinus" then -- ExpMinus Exp
    check_minus(exp)
  elseif tag == "ExpLen" then -- ExpLen Exp
    check_len(exp)
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
    check_global_function(stm)
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name [ID] Type Stm
    check_local_function(stm)
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
    check_stmcall(stm[1])
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
  st["function"] = {} -- store function attributes
  st["global"] = {} -- store global names
  st["messages"] = {} -- store errors and warnings
  for k,v in pairs(_ENV) do
    local t = type(v)
    local any_star = types.VarArg(Any)
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
  init_symbol_table(subject, filename)
  begin_function()
  set_vararg(String)
  check_block(ast)
  end_function()
  check_pending_gotos()
  if #st["messages"] > 0 then
    local msg = table.concat(st["messages"], "\n")
    return nil,msg
  end
  return ast
end

return checker

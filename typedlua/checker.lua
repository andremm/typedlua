--[[
This file implements the type checker for Typed Lua
]]

local parser = require "typedlua.parser"
local st = require "typedlua.st"
local types = require "typedlua.types"

local lineno = st.lineno
local begin_scope, end_scope = st.begin_scope, st.end_scope
local begin_function, end_function = st.begin_function, st.end_function
local begin_loop, end_loop = st.begin_loop, st.end_loop
local insideloop = st.insideloop

local Any = types.Any
local Nil = types.Nil
local False = types.False
local True = types.True
local Boolean = types.Boolean
local Number = types.Number
local String = types.String
local Undefined = types.Undefined
local Void = types.Void

local checker = {}

local function errormsg (env, pos)
  local l,c = lineno(env.subject, pos)
  return string.format("%s:%d:%d:", env["filename"], l, c)
end

local function typeerror (env, msg, pos)
  local error_msg = "%s type error, %s"
  error_msg = string.format(error_msg, errormsg(env, pos), msg)
  table.insert(env["messages"], error_msg)
end

local function warning (env, msg, pos)
  local error_msg = "%s warning, %s"
  error_msg = string.format(error_msg, errormsg(env, pos), msg)
  table.insert(env["messages"], error_msg)
end

local function type2str (t)
  return types.tostring(t)
end

local function check_type_name (env, t)
  if types.isName(t) then
    local msg = "type '%s' is not defined"
    msg = string.format(msg, type2str(t))
    warning(env, msg, t.pos)
    return false
  end
  return true
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

local function get_return_type (env, fscope)
  local ret_type = env["function"][fscope]["ret_type"]
  return ret_type
end

local function set_return_type (env, ret_type)
  local fscope = env.fscope
  env["function"][fscope]["ret_type"] = ret_type
end

local function get_explist_type (explist)
  local list = {}
  local len = #explist
  if len == 0 then
    table.insert(list, types.VarArg(Nil))
    return types.Tuple(list)
  end
  local t
  for i=1,len-1 do
    t = explist[i]["type"]
    if types.VarArg(t) then t = types.typeofVarArg(t) end
    table.insert(list, t)
  end
  t = explist[len]["type"]
  if types.VarArg(t) then
    table.insert(list, t)
    return types.Tuple(list)
  end
  table.insert(list, t)
  table.insert(list, types.VarArg(Nil))
  return types.Tuple(list)
end

local function proj (t)
  if types.isVoid(t) then
    return Nil
  elseif types.isTuple(t) then
    return t[1][1]
  end
  return t
end

local function explist2typelist (explist)
  local len = #explist
  local list = {}
  if len ~= 0 then
    for i=1,len-1 do
      table.insert(list, proj(explist[i]["type"]))
    end
    local last_type = explist[len]["type"]
    if types.isVoid(last_type) then
      table.insert(list, Nil)
    elseif types.isTuple(last_type) then
      for k, v in ipairs(last_type[1]) do
        table.insert(list, v)
      end
    else
      table.insert(list, last_type)
    end
  end
  return list
end

local function arglist2typelist (arglist)
  local list = {}
  for k, v in ipairs(arglist) do
    table.insert(list, v)
  end
  return list
end

local function adjust_typelist (declist, inflist)
  local len_dec, len_inf = #declist, #inflist
  local fill_type = Nil
  if len_inf ~= 0 then
    if types.isVarArg(inflist[len_inf]) then
      fill_type = inflist[len_inf]
    end
  end
  for i=len_inf+1,len_dec do
    table.insert(inflist, fill_type)
  end
end

-- functions that handle identifiers

local check_block, check_stm, check_exp
local check_var, check_var_assignment
local check_explist, check_fieldlist

-- variables

local function var_id (var_name, var_type, var_pos)
  local var = {}
  var.tag = "VarID"
  var[1] = var_name
  var[2] = var_type
  var.pos = var_pos
  return var
end

local function var_index (v, name)
  local pos = v.pos
  local var = {}
  var.tag = "VarIndex"
  var[1] = { tag = "ExpVar", [1] = v, pos = pos }
  var[2] = { tag = "ExpStr", [1] = name, pos = pos }
  var.pos = pos
  return var
end

local function id2var (id)
  return var_id(id[1], id[2], id.pos)
end

local function idlist2varlist (idlist)
  local list = {}
  for k, v in ipairs(idlist) do
    table.insert(list, id2var(v))
  end
  return list
end

local function namelist2var (namelist)
  local var = var_id(namelist[1], Undefined, namelist.pos)
  for i = 2, #namelist do
    var = var_index(var, namelist[i])
  end
  return var
end

local function par2var (env, id)
  if check_type_name(env, id[2]) then
    return var_id(id[1], id[2], id.pos)
  end
  return var_id(id[1], Any, id.pos)
end

local function parlist2varlist (env, idlist)
  local list = {}
  for k, v in ipairs(idlist) do
    table.insert(list, par2var(env, v))
  end
  return list
end

local function isglobal (env, name)
  if env["global"][name] then
    return true
  end
  return false
end

local function islocal (env, name)
  for s=env["scope"],0,-1 do
    if env[s]["local"][name] then
      return true
    end
  end
  return false
end

local function get_global (env, name)
  return env["global"][name]
end

local function get_local_scope (env, name)
  local scope = env["scope"]
  for s=scope,0,-1 do
    if env[s]["local"][name] then
      return s
    end
  end
  return nil
end

local function get_visibility (env, name)
  if islocal(env, name) then
    return "local"
  elseif isglobal(env, name) then
    return "global"
  end
  return nil
end

local function get_var_name (var)
  local tag = var.tag
  if tag == "VarID" then
    return var[1]
  elseif tag == "VarIndex" then
    return get_var_name(var[1][1])
  end
end

local function get_var_type (var)
  local tag = var.tag
  if tag == "VarID" then
    return var[2]
  elseif tag == "VarIndex" then
    return var.type
  end
end

local function get_var_pos (var)
  local tag = var.tag
  if tag == "VarID" then
    return var.pos
  elseif tag == "VarIndex" then
    return var.pos
  end
end

local function set_var (env, var_name, var_type, pos, scope)
  local v = { var_name = var_name, var_type = var_type, pos = pos }
  if scope then
    env[scope]["local"][var_name] = v
  else
    env["global"][var_name] = v
  end
end

local function validate_type_annotation (env, t)
  if types.isName(t) then
    local msg = "type '%s' is not defined"
    msg = string.format(msg, type2str(t))
    warning(env, msg, t.pos)
    return Undefined
  end
  return t
end

local function match_dec_type (env, dec_type, inf_type, pos)
  local msg
  if types.isAny(dec_type) and not types.isAny(inf_type) then
    msg = "attempt to cast 'any' to '%s'"
    msg = string.format(msg, type2str(inf_type))
    warning(env, msg, pos)
  elseif types.isAny(inf_type) and not types.isAny(dec_type) then
    msg = "attempt to cast '%s' to 'any'"
    msg = string.format(msg, type2str(dec_type))
    warning(env, msg, pos)
  elseif not types.csubtype(inf_type, dec_type) then
    msg = "attempt to assign '%s' to '%s'"
    msg = string.format(msg, type2str(inf_type), type2str(dec_type))
    typeerror(env, msg, pos)
  end
end

local function set_global (env, var_name, dec_type, inf_type, pos)
  if types.isUndefined(dec_type) then
    dec_type = types.supertypeof(inf_type)
  end
  match_dec_type(env, dec_type, inf_type, pos)
  if not types.isNil(dec_type) then
    set_var(env, var_name, dec_type, pos)
  else
    local msg = "global '%s' was not declared"
    msg = string.format(msg, var_name)
    warning(env, msg, pos)
  end
end

local function set_local (env, var_name, dec_type, inf_type, pos, scope)
  local msg
  if types.isUndefined(dec_type) then
    if not types.isNil(inf_type) then
      dec_type = types.supertypeof(inf_type)
    else
      msg = "forwarding the declaration of local '%s'"
      msg = string.format(msg, var_name)
      warning(env, msg, pos)
    end
  else
    match_dec_type(env, dec_type, inf_type, pos)
  end
  local shadow = env[scope]["local"][var_name]
  if shadow then
    local line = lineno(env.subject, shadow.pos)
    local t1, t2 = type2str(shadow.var_type), type2str(dec_type)
    msg = "local '%s' was previously defined at line %d"
    msg = string.format(msg, var_name, line)
    warning(env, msg, pos)
    msg = "shadowing local '%s' from '%s' to '%s'"
    msg = string.format(msg, var_name, t1, t2)
    warning(env, msg, pos)
  end
  set_var(env, var_name, dec_type, pos, scope)
end

local function check_par_dec (env, par_type)
  if types.isUndefined(par_type) or
     not check_type_name(env, par_type) then
    return Any
  end
  return par_type
end

local function check_ret_dec (env, ret_type)
  if types.isUndefined(ret_type) or
     not check_type_name(env, ret_type) then
    return types.Tuple({types.VarArg(Any)})
  end
  return ret_type
end

local function update_var (env, var_name, dec_type, inf_type, pos, scope)
  local v, var_scope, msg
  if scope then -- local
    v = env[scope]["local"][var_name]
    var_scope = "local"
  else -- global
    v = env["global"][var_name]
    var_scope = "global"
  end
  local var_type, var_pos = v.var_type, v.var_pos
  if types.isRecord(var_type) then
    var_type = dec_type
  elseif not types.isUndefined(dec_type) and
         not types.subtype(dec_type, var_type) then
    local t1, t2 = type2str(var_type), type2str(dec_type)
    msg = "cannot cast %s '%s' from '%s' to '%s'"
    msg = string.format(msg, var_scope, var_name, t1, t2)
    warning(env, msg, pos)
  end
  match_dec_type(env, var_type, inf_type, pos)
end

local function update_global (env, var, ann_type, inf_type, pos)
  local var_name, dec_type = var.var_name, var.var_type
  if not types.isUndefined(ann_type) and
     not types.subtype(ann_type, dec_type) then
    local msg = "attempt to redeclare global '%s'"
    msg = string.format(msg, var_name)
    typeerror(env, msg, pos)
  end
  match_dec_type(env, dec_type, inf_type, pos)
end

local function update_local (env, var, ann_type, inf_type, pos)
  local var_name, dec_type = var.var_name, var.var_type
  if not types.isUndefined(ann_type) and
     not types.subtype(ann_type, dec_type) then
    local msg = "attempt to cast local '%s' from '%s' to '%s'"
    local t1, t2 = type2str(dec_type), type2str(ann_type)
    msg = string.format(msg, var_name, t1, t2)
    typeerror(env, msg, pos)
  end
  match_dec_type(env, dec_type, inf_type, pos)
end

function check_var (env, var)
  local tag = var.tag
  if tag == "VarID" then
    local name = var[1]
    local scope = get_local_scope(env, name)
    local t, msg
    if scope then -- local
      t = env[scope]["local"][name]["var_type"]
    else -- global
      local g = env["global"][name]
      if g then
        t = g["var_type"]
      else
        t = Nil
        msg = "using variable '%s' without initialize"
        msg = string.format(msg, name)
        typeerror(env, msg, var.pos)
      end
    end
    var.type = t
  elseif tag == "VarIndex" then
    local exp1, exp2 = var[1], var[2]
    check_exp(env, exp1)
    check_exp(env, exp2)
    local var_type, index_type = exp1.type, exp2.type
    local msg = "attmept to index '%s'"
    if types.isAny(var_type) then
      msg = string.format(msg, 'any')
      warning(env, msg, exp1.pos)
      var.type = Any
    elseif types.isRecord(var_type) then
      for k, v in ipairs(var_type[1]) do
        if types.csubtype(index_type, v[1]) then
          var.type = v[2]
          return
        end
      end
      msg = msg .. " with '%s'"
      msg = string.format(msg, type2str(var_type), type2str(index_type))
      typeerror(env, msg, exp2.pos)
      var.type = Nil
    else
      msg = string.format(msg, type2str(var_type))
      typeerror(env, msg, exp1.pos)
      var.type = Nil
    end
  else
    error("canno type check a variable " .. tag)
  end
end

-- functions

local function set_vararg (env, btype)
  local fscope = env["fscope"]
  env["function"][fscope]["is_vararg"] = true
  env["function"][fscope]["vararg"] = types.VarArg(btype)
end

local function check_parameters_list (env, varlist)
  local len = #varlist
  local list = {}
  if len ~= 0 then
    local is_vararg = false
    local vararg_type
    if varlist[len][1] == "..." then
      is_vararg = true
      vararg_type = check_par_dec(env, varlist[len][2])
      set_vararg(env, vararg_type)
      table.remove(varlist)
      len = #varlist
    end
    local scope = env.scope
    for k, v in ipairs(varlist) do
      local var_name = get_var_name(v)
      local var_type = get_var_type(v)
      local var_pos = get_var_pos(v)
      var_type = check_par_dec(env, var_type)
      set_var(env, var_name, var_type, var_pos, scope)
      table.insert(list, var_type)
    end
    if is_vararg then
      table.insert(list, types.VarArg(vararg_type))
    end
  end
  return types.Tuple(list)
end

local function check_function_prototype (env, idlist, ret_type)
  local varlist = parlist2varlist(env, idlist)
  local par_type = check_parameters_list(env, varlist)
  ret_type = check_ret_dec(env, ret_type)
  return types.Function(par_type, ret_type)
end

local function check_ret_type (env, dec_type, inf_type, pos)
  local msg
  if types.isAny(inf_type) and not types.isAny(dec_type) then
    msg = "attempt to return 'any' instead of '%s'"
    msg = string.format(msg, type2str(dec_type))
    warning(env, msg, pos)
  elseif types.isAny(dec_type) and not types.isAny(inf_type) then
    msg = "attempt to return '%s' instead of 'any'"
    msg = string.format(msg, type2str(dec_type))
    warning(env, msg, pos)
  elseif not types.csubtype(inf_type, dec_type) then
    msg = "attempt to return '%s' instead of '%s'"
    msg = string.format(msg, type2str(inf_type), type2str(dec_type))
    typeerror(env, msg, pos)
  end
end

local function check_function_stm (env, stm, ret_type)
  check_stm(env, stm)
  local len = #stm
  if len == 0 or
     stm[len].tag ~= "StmRet" then
    check_ret_type(env, ret_type, Void, stm.pos)
  end
end

-- expressions

local function check_and (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = proj(exp1["type"]), proj(exp2["type"])
  set_node_type(exp, types.Union(t1, t2)) -- T-AND
end

local function check_anonymous_function (env, exp)
  begin_function(env)
  begin_scope(env)
  local idlist, ret_type, stm = exp[1], exp[2], exp[3]
  local t = check_function_prototype(env, idlist, ret_type)
  set_return_type(env, t[2])
  check_function_stm(env, stm, t[2])
  set_node_type(exp, t)
  end_scope(env)
  end_function(env)
end

local function check_arith (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = proj(exp1["type"]), proj(exp2["type"])
  if types.subtype(t1, Number) and
     types.subtype(t2, Number) then -- T-ARITH1
    set_node_type(exp, Number)
  elseif types.isAny(t1) or -- T-ARITH2
         types.isAny(t2) then -- T-ARITH3
    set_node_type(exp, Any)
  else
    local wrong_type, wrong_pos
    set_node_type(exp, Any)
    if not types.subtype(t1, Number) and
       not types.isAny(t1) then
      wrong_type = types.supertypeof(exp1.type)
      wrong_pos = exp1.pos
    else
      wrong_type = types.supertypeof(exp2.type)
      wrong_pos = exp2.pos
    end
    local msg
    msg = "attempt to perform arithmetic on a %s"
    msg = string.format(msg, type2str(wrong_type))
    typeerror(env, msg, wrong_pos)
  end
end

-- function name, parameter number, dec type, given type, pos

local function check_call_arg (env, fname, k, dtype, gtype, pos)
  local msg
  if types.isAny(gtype) and not types.isAny(dtype) then
    msg = "parmeter %d of '%s', attempt to cast 'any' to '%s'"
    msg = msg:format(k, fname, type2str(dtype))
    warning(env, msg, pos)
  elseif types.isAny(dtype) and not types.isAny(gtype) then
    msg = "parameter %d of '%s', attempt to cast '%s' to 'any'"
    msg = msg:format(k, fname, type2str(gtype))
    warning(env, msg, pos)
  elseif not types.csubtype(gtype, dtype) then
    msg = "parameter %d of '%s', attempt to assign '%s' to '%s'"
    msg = msg:format(k, fname, type2str(gtype), type2str(dtype))
    typeerror(env, msg, pos)
  end
end

local function check_call_args (env, fname, args, explist)
  local inflist = explist2typelist(explist)
  local declist = arglist2typelist(args[1])
  local len_dec, len_inf = #declist, #inflist
  local pos = explist.pos
  if len_dec <= len_inf then
    for i=1,len_dec do
      if explist[i] then pos = explist[i].pos end
      check_call_arg(env, fname, i, declist[i], inflist[i], pos)
    end
    if types.isVarArg(declist[len_dec]) then
      for i=len_dec+1,len_inf do
        if explist[i] then pos = explist[i].pos end
        check_call_arg(env, fname, len_dec, declist[len_dec], inflist[i], pos)
      end
    end
  else
    for i=1,len_inf do
      if explist[i] then pos = explist[i].pos end
      check_call_arg(env, fname, i, declist[i], inflist[i], pos)
    end
    local fill_type = Nil
    if len_inf == 0 then
      if not types.isVarArg(declist[1]) then
        for i=1,len_dec do
          if explist[i] then pos = explist[i].pos end
          check_call_arg(env, fname, i, declist[i], fill_type, pos)
        end
      end
    else
      if not types.isVarArg(declist[len_inf]) then
        if types.isVarArg(inflist[len_inf]) then
          fill_type = inflist[len_inf]
        end
        for i=len_inf+1,len_dec do
          if explist[i] then pos = explist[i].pos end
          check_call_arg(env, fname, i, declist[i], fill_type, pos)
        end
      end
    end
  end
end

local function check_call (env, fname, ftype, explist, pos, visibility)
  local msg
  if types.isAny(ftype) then
    msg = "attempt to call %s '%s' of type 'any'"
    msg = msg:format(visibility, fname)
    warning(env, msg, pos)
    return Any
  elseif types.isFunction(ftype) then
    if not types.isVoid(ftype[1]) then
      check_call_args(env, fname, ftype[1], explist)
    end
    return ftype[2]
  else
    msg = "attempt to call %s '%s' of type '%s'"
    msg = msg:format(visibility, fname, type2str(ftype))
    typeerror(env, msg, pos)
    return Nil
  end
end

local function check_calling_function (env, exp)
  local exp1, explist, pos = exp[1], exp[2], exp.pos
  check_exp(env, exp1)
  check_explist(env, explist)
  local func_name = get_var_name(exp1[1])
  local dec_type = exp1.type
  local ret_type = Nil
  if not types.isNil(dec_type) then
    local visibility = get_visibility(env, func_name)
    ret_type = check_call(env, func_name, dec_type, explist, pos, visibility)
  else
    local msg = "attempt to call undeclared function '%s'"
    msg = string.format(msg, func_name)
    typeerror(env, msg, pos)
  end
  set_node_type(exp, ret_type)
end

local function check_calling_method (env, exp)
  local exp1, explist, pos = exp[1], exp[2], exp.pos
  check_exp(env, exp1)
  check_explist(env, explist)
  local meth_name = get_var_name(exp1[1])
  local dec_type = exp1.type
  local ret_type = Nil
  if not types.isNil(dec_type) then
    ret_type = check_call(env, meth_name, dec_type, explist, pos, "method")
  else
    local msg = "attempt to call undeclared method '%s'"
    msg = string.format(msg, meth_name)
    typeerror(env, msg, pos)
  end
  set_node_type(exp, ret_type)
end

local function check_concat (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = proj(exp1["type"]), proj(exp2["type"])
  if types.subtype(t1, String) and
     types.subtype(t2, String) then -- T-CONCAT1
    set_node_type(exp, String)
  elseif types.isAny(t1) or -- T-CONCAT2
         types.isAny(t2) then -- T-CONCAT3
    set_node_type(exp, Any)
  else
    local wrong_type, wrong_pos
    set_node_type(exp, Any)
    if not types.subtype(t1, String) and
       not types.isAny(t1) then
      wrong_type = types.supertypeof(exp1.type)
      wrong_pos = exp1.pos
    else
      wrong_type = types.supertypeof(exp2.type)
      wrong_pos = exp2.pos
    end
    local msg
    msg = "attempt to concatenate a %s"
    msg = string.format(msg, type2str(wrong_type))
    typeerror(env, msg, wrong_pos)
  end
end

local function check_equal (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  set_node_type(exp, Boolean) -- T-EQUAL
end

local function check_expvar (env, exp)
  local var = exp[1]
  check_var(env, var)
  set_node_type(exp, var.type)
end

local function check_len (env, exp)
  local exp1 = exp[1]
  check_exp(env, exp1)
  local t1 = proj(exp1["type"])
  if types.subtype(t1, String) then -- T-LEN1
    set_node_type(exp, Number)
  elseif types.isAny(t1) then -- T-LEN2
    set_node_type(exp, Any)
  else
    set_node_type(exp, Any)
    local msg = "attempt to get length of a %s value"
    local wrong_type = types.supertypeof(t1)
    msg = string.format(msg, type2str(wrong_type))
    typeerror(env, msg, exp1.pos)
  end
end

local function check_minus (env, exp)
  local exp1 = exp[1]
  check_exp(env, exp1)
  local t1 = proj(exp1["type"])
  if types.subtype(t1, Number) then -- T-MINUS1
    set_node_type(exp, Number)
  elseif types.isAny(t1) then -- T-MINUS2
    set_node_type(exp, Any)
  else
    set_node_type(exp, Any)
    local msg
    local wrong_type = types.supertypeof(exp1.type)
    msg = "attempt to perform arithmetic on a %s"
    msg = string.format(msg, type2str(wrong_type))
    typeerror(env, msg, exp1.pos)
  end
end

local function check_not (env, exp)
  local exp1 = exp[1]
  check_exp(env, exp1)
  set_node_type(exp, Boolean) -- T-NOT
end

local function check_or (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = proj(exp1["type"]), proj(exp2["type"])
  set_node_type(exp, types.Union(t1, t2)) -- T-OR
end

local function check_order (env, exp)
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = proj(exp1["type"]), proj(exp2["type"])
  set_node_type(exp, Boolean)
  if types.subtype(t1, Number) and
     types.subtype(t2, Number) then -- T-ORDER1
  elseif types.subtype(t1, String) and
         types.subtype(t2, String) then -- T-ORDER2
  elseif types.isAny(t1) or -- T-ORDER3
         types.isAny(t2) then -- T-ORDER4
  else
    local msg = "attempt to compare %s with %s"
    t1, t2 = types.supertypeof(t1), types.supertypeof(t2)
    msg = string.format(msg, type2str(t1), type2str(t2))
    typeerror(env, msg, exp.pos)
  end
end

local function check_table (env, exp)
  check_fieldlist(env, exp[1])
  set_node_type(exp, exp[1].type)
end

local function check_vararg (env, exp)
  local fscope = env["fscope"]
  local vararg_type
  if env["function"][fscope]["is_vararg"] then
    vararg_type = env["function"][fscope]["vararg"]
  end
  set_node_type(exp, vararg_type)
end

-- statemnts

function check_var_assignment (env, var, inf_type)
  local tag = var.tag
  if tag == "VarID" then
    local var_name = var[1]
    local var_type = validate_type_annotation(env, var[2])
    local pos = var.pos
    local scope = get_local_scope(env, var_name)
    if scope then -- local
      local l = env[scope]["local"][var_name]
      update_local(env, l, var_type, inf_type, pos)
    else -- global
      local g = get_global(env, var_name)
      if g then
        update_global(env, g, var_type, inf_type, pos)
      else
        set_global(env, var_name, var_type, inf_type, pos)
      end
    end
  elseif tag == "VarIndex" then
    check_var(env, var)
    match_dec_type(env, var.type, inf_type, var.pos)
  else
    error("cannot assign to variable " .. tag)
  end
end

local function check_assignment (env, varlist, explist)
  check_explist(env, explist)
  local typelist = explist2typelist(explist)
  adjust_typelist(varlist, typelist)
  for k, v in ipairs(varlist) do
    check_var_assignment(env, v, typelist[k])
  end
end

local function check_stmcall (env, exp)
  check_exp(env, exp)
end

local function check_for_generic (env, idlist, explist, stm)
  begin_scope(env)
  check_explist(env, explist)
  check_stm(env, stm)
  end_scope(env)
end

local function check_for_numeric (env, id, exp1, exp2, exp3, stm)
  begin_scope(env)
  set_var(env, id[1], Number, id.pos, env.scope)
  check_exp(env, exp1)
  check_exp(env, exp2)
  check_exp(env, exp3)
  local t1, t2, t3 = exp1["type"], exp2["type"], exp3["type"]
  local msg
  if types.isAny(t1) then
    msg = "'for' initial value is any"
    warning(env, msg, exp1["pos"])
  elseif not types.subtype(t1, Number) then
    msg = "'for' initial value must be a number"
    typeerror(env, msg, exp1["pos"])
  end
  if types.isAny(t2) then
    msg = "'for' limit value is any"
    warning(env, msg, exp1["pos"])
  elseif not types.subtype(t2, Number) then
    msg = "'for' limit must be a number"
    typeerror(env, msg, exp2["pos"])
  end
  if types.isAny(t3) then
    msg = "'for' step value is any"
    warning(env, msg, exp1["pos"])
  elseif not types.subtype(t3, Number) then
    msg = "'for' step must be a number"
    typeerror(env, msg, exp3["pos"])
  end
  check_stm(env, stm)
  end_scope(env)
end

local function check_global_function (env, stm)
  local pos = stm.pos
  begin_function(env)
  begin_scope(env)
  local namelist, idlist, ret_type, stm1 = stm[1], stm[2], stm[3], stm[4]
  local var = namelist2var(namelist)
  local t = check_function_prototype(env, idlist, ret_type)
  check_var_assignment(env, var, t)
  set_return_type(env, t[2])
  check_function_stm(env, stm1, t[2])
  end_scope(env)
  end_function(env)
end

local function check_if_else (env, exp, stm1, stm2)
  check_exp(env, exp)
  check_stm(env, stm1)
  check_stm(env, stm2)
end

local function check_local_function (env, stm)
  local scope, pos = env.scope, stm.pos
  begin_function(env)
  begin_scope(env)
  local name, idlist, ret_type, stm1 = stm[1], stm[2], stm[3], stm[4]
  local t = check_function_prototype(env, idlist, ret_type)
  set_var(env, name, t, scope)
  set_return_type(env, t[2])
  check_function_stm(env, stm1, t[2])
  end_scope(env)
  end_function(env)
end

local function check_local_var (env, idlist, explist)
  local scope = env.scope
  local varlist = idlist2varlist(idlist)
  check_explist(env, explist)
  local typelist = explist2typelist(explist)
  adjust_typelist(varlist, typelist)
  for k, v in ipairs(varlist) do
    set_local(env, v[1], v[2], typelist[k], v.pos, scope)
  end
end

local function check_repeat (env, stm, exp)
  check_stm(env, stm)
  check_exp(env, exp)
end

local function check_return (env, stm)
  local explist = stm[1]
  check_explist(env, explist)
  local typelist = explist2typelist(explist)
  local ret_type = get_return_type(env, env.fscope)
  check_ret_type(env, ret_type, types.Tuple(typelist), stm.pos)
end

local function check_while (env, exp, stm)
  check_exp(env, exp)
  check_stm(env, stm)
end

function check_fieldlist (env, fieldlist)
  local l = {}
  local i = 1
  for k, v in ipairs(fieldlist[1]) do
    check_exp(env, v[1])
    table.insert(l, { [1] = Number, [2] = v[1].type })
  end
  for k, v in ipairs(fieldlist[2]) do
    check_exp(env, v[1])
    check_exp(env, v[2])
    table.insert(l, { [1] = v[1].type, [2] = v[2].type })
  end
  if #l == 0 then
    table.insert(l, { [1] = Any, [2] = types.VarArg(Any) })
  end
  fieldlist.type = types.Record(l)
end

function check_explist (env, explist)
  for k, v in ipairs(explist) do
    check_exp(env, v)
  end
end

function check_exp (env, exp)
  local tag = exp.tag
  if tag == "ExpNil" then
    set_node_type(exp, Nil)
  elseif tag == "ExpFalse" then
    set_node_type(exp, False)
  elseif tag == "ExpTrue" then
    set_node_type(exp, True)
  elseif tag == "ExpDots" then
    check_vararg(env, exp)
  elseif tag == "ExpNum" then -- ExpNum Double
    set_node_type(exp, types.ConstantNumber(exp[1]))
  elseif tag == "ExpStr" then -- ExpStr String
    set_node_type(exp, types.ConstantString(exp[1]))
  elseif tag == "ExpVar" then -- ExpVar Var
    check_expvar(env, exp)
  elseif tag == "ExpFunction" then -- ExpFunction [ID] Type Stm
    check_anonymous_function(env, exp)
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    check_table(env, exp)
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp [Exp]
    check_calling_method(env, exp)
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    check_calling_function(env, exp)
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" then -- ExpPow Exp Exp
    check_arith(env, exp)
  elseif tag == "ExpConcat" then -- ExpConcat Exp Exp
    check_concat(env, exp)
  elseif tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" then -- ExpEQ Exp Exp
    check_equal(env, exp)
  elseif tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" then -- ExpGE Exp Exp
    check_order(env, exp)
  elseif tag == "ExpAnd" then -- ExpAnd Exp Exp
    check_and(env, exp)
  elseif tag == "ExpOr" then -- ExpOr Exp Exp
    check_or(env, exp)
  elseif tag == "ExpNot" then -- ExpNot Exp
    check_not(env, exp)
  elseif tag == "ExpMinus" then -- ExpMinus Exp
    check_minus(env, exp)
  elseif tag == "ExpLen" then -- ExpLen Exp
    check_len(env, exp)
  else
    error("cannot type check expression " .. tag)
  end
end

function check_stm (env, stm)
  local tag = stm.tag
  if tag == "StmBlock" then -- StmBlock [Stm]
    check_block(env, stm)
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    check_if_else(env, stm[1], stm[2], stm[3])
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
    check_while(env, stm[1], stm[2])
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    check_for_numeric(env, stm[1], stm[2], stm[3], stm[4], stm[5])
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    check_for_generic(env, stm[1], stm[2], stm[3])
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
    check_repeat(env, stm[1], stm[2])
  elseif tag == "StmFunction" then -- StmFunction FuncName [ID] Type Stm
    check_global_function(env, stm)
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name [ID] Type Stm
    check_local_function(env, stm)
  elseif tag == "StmLabel" or -- StmLabel Name
         tag == "StmGoTo" or -- StmGoTo Name
         tag == "StmBreak" then -- StmBreak
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    check_assignment(env, stm[1], stm[2])
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    check_local_var(env, stm[1], stm[2])
  elseif tag == "StmRet" then -- StmRet [Exp]
    check_return(env, stm)
  elseif tag == "StmCall" then -- StmCall Exp
    check_stmcall(env, stm[1])
  else
    error("cannot type check statement " .. tag)
  end
end

function check_block (env, block)
  local tag = block.tag
  if tag ~= "StmBlock" then
    error("cannot type block " .. tag)
  end
  begin_scope(env)
  for k,v in ipairs(block) do
    check_stm(env, v)
  end
  end_scope(env)
end

local function init_symbol_table (env, subject, filename)
  env["subject"] = subject -- store subject for error messages
  env["filename"] = filename -- store filename for error messages
  env["function"] = {} -- store function attributes
  env["global"] = {} -- store global names
  env["messages"] = {} -- store errors and warnings
  local any_star = types.VarArg(Any)
  local args_and_ret = types.Tuple({any_star})
  local ftype = types.Function(args_and_ret, args_and_ret)
  for k,v in pairs(_ENV) do
    local t = type(v)
    if t == "string" then
      set_var(env, k, types.ConstantString(v), 0)
    elseif t == "function" then
      set_var(env, k, ftype, 0)
    else
      set_var(env, k, Any, 0)
    end
  end
end

function checker.typecheck (ast, subject, filename)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  local env = {}
  init_symbol_table(env, subject, filename)
  begin_function(env)
  set_return_type(env, types.Tuple({types.VarArg(Any)}))
  set_vararg(env, String)
  check_block(env, ast)
  end_function(env)
  if #env["messages"] > 0 then
    local msg = table.concat(env["messages"], "\n")
    return ast, msg
  end
  return ast
end

return checker

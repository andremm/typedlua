--[[
This file implements Typed Lua type checker
]]

local unpack = table.unpack or unpack

local tlchecker = {}

local tlast = require "typedlua.tlast"
local tlst = require "typedlua.tlst"
local tltype = require "typedlua.tltype"
local tlparser = require "typedlua.tlparser"
local tldparser = require "typedlua.tldparser"
local tlfilter = require "typedlua.tlfilter"

local Value = tltype.Value()
local Any = tltype.Any()
local Nil = tltype.Nil()
local Self = tltype.Self()
local False = tltype.False()
local True = tltype.True()
local Boolean = tltype.Boolean()
local Number = tltype.Number()
local String = tltype.String()
local Integer = tltype.Integer(false)

local check_block, check_stm, check_exp, check_var, check_var_exps

local acolor = {
  red     = "\27[31;1m",
  magenta = "\27[35;1m",
  bold    = "\27[1m",
  reset   = "\27[0m"
}

local typeerror = tltype.typeerror

local function set_type (node, t)
  node["type"] = t
end

local function set_ubound (var, t)
  var.ubound = t
end

local function get_type (node)
  return node and tltype.unfold(node["type"]) or Nil
end

local function get_ubound (node)
  return node and tltype.unfold(node.ubound) or Nil
end

local check_self_field

local function check_self (env, torig, t, pos)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = string.format("self type appearing in a place that is not a first parameter or a return type inside type " .. bold_token, tltype.tostring(torig))
  if tltype.isSelf(t) then
    typeerror(env, "self", msg, pos)
    return tltype.Any()
  elseif tltype.isRecursive(t) then
    local r = tltype.Recursive(t[1], check_self(env, torig, t[2], pos))
    r.name = t.name
    return r
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
   local r = { tag = t.tag, name = t.name }
   for k, v in ipairs(t) do
     r[k] = check_self(env, torig, v, pos)
   end
   return r
  elseif tltype.isFunction(t) then
    local r = tltype.Function(check_self(env, torig, t[1], pos),
                              check_self(env, torig, t[2], pos))
    r.name = t.name
    return r
  elseif tltype.isVararg(t) then
    local r = tltype.Vararg(check_self(env, torig, t[1], pos))
    r.name = t.name
    return r
  elseif tltype.isTable(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.Field(v.const, v[1], check_self_field(env, torig, v[2], pos)))
    end
    local r = tltype.Table(unpack(l))
    r.unique = t.unique
    r.open = t.open
    return r
  else
    return t
  end
end

function check_self_field(env, torig, t, pos)
  if tltype.isRecursive(t) then
    local r = tltype.Recursive(t[1], check_self_field(env, torig, t[2], pos))
    r.name = t.name
    return r
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
   local r = { tag = t.tag, name = t.name }
   for k, v in ipairs(t) do
     r[k] = check_self_field(env, torig, v, pos)
   end
   return r
  elseif tltype.isFunction(t) then
    local input = t[1]
    assert(tltype.isTuple(input), "BUG: function input type is not a tuple")
    if tltype.isSelf(input[1]) then -- method
      local ninput = { tag = input.tag, tltype.Self() }
      for i = 2, #input do
        ninput[i] = check_self(env, torig, input[i], pos)
      end
      local r = tltype.Function(ninput, t[2])
      r.name = t.name
      return r
    else
      local r = tltype.Function(check_self(env, torig, t[1], pos),
                                check_self(env, torig, t[2], pos))
      r.name = t.name
      return r
    end
  elseif tltype.isTable(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.Field(v.const, v[1], check_self_field(env, torig, v[2], pos)))
    end
    local r = tltype.Table(unpack(l))
    r.unique = t.unique
    r.open = t.open
    return r
  else
    return check_self(env, torig, t, pos)
  end
end

local function get_interface (env, name, pos)
  local t = tlst.get_interface(env, name)
  if not t then
    return tltype.GlobalVariable(env, name, pos, typeerror)
  else
    return t
  end
end

local function replace_names (env, t, pos, ignore)
  ignore = ignore or {}
  if tltype.isRecursive(t) then
    local link = ignore[t[1]]
    ignore[t[1]] = true
    local r = tltype.Recursive(t[1], replace_names(env, t[2], pos, ignore))
    r.name = t.name
    ignore[t[1]] = link
    return r
  elseif tltype.isLiteral(t) or
     tltype.isBase(t) or
     tltype.isNil(t) or
     tltype.isValue(t) or
     tltype.isAny(t) or
     tltype.isSelf(t) or
     tltype.isVoid(t) then
    return t
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
    local r = { tag = t.tag, name = t.name }
    for k, _ in ipairs(t) do
      r[k] = replace_names(env, t[k], pos, ignore)
    end
    return r
  elseif tltype.isFunction(t) then
    t[1] = replace_names(env, t[1], pos, ignore)
    t[2] = replace_names(env, t[2], pos, ignore)
    return t
  elseif tltype.isTable(t) then
    for k, _ in ipairs(t) do
      t[k][2] = replace_names(env, t[k][2], pos, ignore)
    end
    return t
  elseif tltype.isVariable(t) then
    if not ignore[t[1]] then
      local r = replace_names(env, get_interface(env, t[1], pos), pos, ignore)
      r.name = t[1]
      return r
    else
      return t
    end
  elseif tltype.isVararg(t) then
    t[1] = replace_names(env, t[1], pos, ignore)
    return t
  else
    return t
  end
end

local function close_type (t)
  if tltype.isUnion(t) or
     tltype.isUnionlist(t) or
     tltype.isTuple(t) then
    for _, v in ipairs(t) do
      close_type(v)
    end
  else
    if t.open then t.open = nil end
  end
end

local function is_global_function_call (exp, fn_name)
  if exp.tag == "Call" then
      local t = tltype.first(get_type(exp[1]))
      return tltype.isPrim(t) and t[1] == fn_name
  end
  return false
end

local function searchpath (name, path)
  if package.searchpath then
    return package.searchpath(name, path)
  else
    local error_msg = ""
    name = string.gsub(name, '%.', '/')
    for tldpath in string.gmatch(path, "([^;]*);") do
      tldpath = string.gsub(tldpath, "?", name)
      local f = io.open(tldpath, "r")
      if f then
        f:close()
        return tldpath
      else
        error_msg = error_msg .. string.format("no file '%s'\n", tldpath)
      end
    end
    return nil, error_msg
  end
end

local function infer_return_type (env)
  local l = tlst.get_return_type(env)
  if #l == 0 then
    return tltype.Tuple({ Nil }, true)
  else
    local r = tltype.Unionlist(unpack(l))
    if tltype.isAny(r) then r = tltype.Tuple({ Any }, true) end
    close_type(r)
    return r
  end
end

local function check_masking (env, local_name, pos)
  local function lineno (s, i)
    if i == 1 then return 1, 1 end
    local rest, num = s:sub(1,i):gsub("[^\n]*\n", "")
    local r = #rest
    return 1 + num, r ~= 0 and r or 1
  end

  local masked_local = tlst.masking(env, local_name)
  if masked_local then
    local l = lineno(env.subject, masked_local.pos)
    local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
    local msg = "masking previous declaration of local " .. bold_token .. " on line %d"
    msg = string.format(msg, local_name, l)
    typeerror(env, "mask", msg, pos)
  end
end

local function check_unused_locals (env)
  local l = tlst.unused(env)
  for k, v in pairs(l) do
    local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
    local msg = string.format("unused local " .. bold_token, k)
    typeerror(env, "unused", msg, v.pos)
  end
end

local function check_tl (env, name, path, pos)
  local file = io.open(path, "r")
  local subject = file:read("*a")
  local s, f = env.subject, env.filename
  io.close(file)
  local ast, msg = tlparser.parse(subject, path, env.strict, env.integer)
  if not ast then
    typeerror(env, "syntax", msg, pos)
    return Any
  end
  env.subject = subject
  env.filename = path
  tlst.begin_function(env)
  check_block(env, ast)
  local t1 = tltype.first(infer_return_type(env))
  tlst.end_function(env)
  env.subject = s
  env.filename = f
  return t1
end

local function check_interface (env, stm)
  local name, t, is_local = stm[1], stm[2], stm.is_local
  if tlst.get_interface(env, name) then
    local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
    local msg = "attempt to redeclare interface " .. bold_token
    msg = string.format(msg, name)
    typeerror(env, "alias", msg, stm.pos)
  else
    check_self(env, t, t, stm.pos)
    local t = replace_names(env, t, stm.pos)
    t.name = name
    tlst.set_interface(env, name, t, is_local)
  end
  return false
end

local function check_userdata (env, stm)
  local name, t, is_local = stm[1], stm[2], stm.is_local
  if tlst.get_userdata(env, name) then
    local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
    local msg = "attempt to redeclare userdata " .. bold_token
    msg = string.format(msg, name)
    typeerror(env, "alias", msg, stm.pos)
  else
    check_self(env, t, t, stm.pos)
    t.name = name
    local t = replace_names(env, t, stm.pos)
    tlst.set_userdata(env, name, t, is_local)
  end
end

local function check_tld (env, name, path, pos)
  local ast, msg = tldparser.parse(path, env.strict, env.integer)
  if not ast then
    typeerror(env, "syntax", msg, pos)
    return Any
  end
  local t = tltype.Table()
  for _, v in ipairs(ast) do
    local tag = v.tag
    if tag == "Id" then
      table.insert(t, tltype.Field(v.const, tltype.Literal(v[1]), replace_names(env, v[2], pos)))
    elseif tag == "Interface" then
      check_interface(env, v)
    elseif tag == "Userdata" then
      check_userdata(env, v)
    else
      error("trying to check a description item, but got a " .. tag)
    end
  end
  return t
end

local function check_require (env, name, pos, extra_path)
  extra_path = extra_path or ""
  if not env["loaded"][name] then
    local path = string.gsub(package.path..";", "[.]lua;", ".tl;")
    local filepath, msg1 = searchpath(extra_path .. name, path)
    if filepath then
      if not env.parent[name] then
        env.parent[name] = true
        env["loaded"][name] = check_tl(env, name, filepath, pos)
      else
        typeerror(env, "load", "circular require", pos)
        env["loaded"][name] = Any
      end
    else
      path = string.gsub(package.path..";", "[.]lua;", ".tld;")
      local msg2
      filepath, msg2 = searchpath(extra_path .. name, path)
      if filepath then
        env["loaded"][name] = check_tld(env, name, filepath, pos)
      else
        env["loaded"][name] = Any
        local s, m = pcall(require, name)
        if not s then
          if string.find(m, "syntax error") then
            typeerror(env, "syntax", m, pos)
          else
            local msg = "could not load '%s'%s%s%s"
            msg = string.format(msg, name, msg1, msg2, m)
            typeerror(env, "load", msg, pos)
          end
        end
      end
    end
  end
  return env["loaded"][name]
end

local function check_arith (env, exp, op)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to perform arithmetic on a " .. bold_token
  if tltype.subtype(t1, tltype.Integer(true)) and
     tltype.subtype(t2, tltype.Integer(true)) then
    if op == "div" or op == "pow" then
      set_type(exp, Number)
    else
      set_type(exp, Integer)
    end
  elseif tltype.subtype(t1, Number) and tltype.subtype(t2, Number) then
    set_type(exp, Number)
    if op == "idiv" then
      local msg = "integer division on floats"
      typeerror(env, "arith", msg, exp1.pos)
    end
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "any", msg, exp1.pos)
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t2))
    typeerror(env, "any", msg, exp2.pos)
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = tltype.general(t1), exp1.pos
    if tltype.subtype(t1, Number) or tltype.isAny(t1) then
      wrong_type, wrong_pos = tltype.general(t2), exp2.pos
    end
    msg = string.format(msg, tltype.tostring(wrong_type))
    typeerror(env, "arith", msg, wrong_pos)
  end
end

local function check_bitwise (env, exp, op)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to perform bitwise %s on a " .. bold_token
  if tltype.subtype(t1, tltype.Integer(true)) and
     tltype.subtype(t2, tltype.Integer(true)) then
    set_type(exp, Integer)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, op or "", tltype.tostring(t1))
    typeerror(env, "any", msg, exp1.pos)
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, op or "", tltype.tostring(t2))
    typeerror(env, "any", msg, exp2.pos)
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = tltype.general(t1), exp1.pos
    if tltype.subtype(t1, Number) or tltype.isAny(t1) then
      wrong_type, wrong_pos = tltype.general(t2), exp2.pos
    end
    msg = string.format(msg, op or "", tltype.tostring(wrong_type))
    typeerror(env, "arith", msg, wrong_pos)
  end
end

local function check_concat (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to concatenate a " .. bold_token
  if tltype.subtype(t1, String) and tltype.subtype(t2, String) then
    set_type(exp, String)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "any", msg, exp1.pos)
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t2))
    typeerror(env, "any", msg, exp2.pos)
  else
    set_type(exp, Any)
    local wrong_type, wrong_pos = tltype.general(t1), exp1.pos
    if tltype.subtype(t1, String) or tltype.isAny(t1) then
      wrong_type, wrong_pos = tltype.general(t2), exp2.pos
    end
    msg = string.format(msg, tltype.tostring(wrong_type))
    typeerror(env, "concat", msg, wrong_pos)
  end
end

local function check_equal (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  set_type(exp, Boolean)
  if exp1.tag == "Index" and exp1[1].tag == "Id" and
     exp1[2].tag == "String" and tltype.isStr(get_type(exp2)) then
    local var, floc, _ = tlst.get_local(env, exp1[1][1])
    if var and floc and not var.assigned then
      return tlfilter.set_single(var, tlfilter.filter_fieldliteral(exp1[2][1], get_type(exp2)))
    end
  elseif is_global_function_call(exp1, "type") and exp1[2].tag == "Id" and tltype.isStr(get_type(exp2)) then
    local var, floc, _ = tlst.get_local(env, exp1[2][1])
    if var and floc and not var.assigned then
      return tlfilter.set_single(var, tlfilter.filter_tag(get_type(exp2)[1]))
    end
  elseif is_global_function_call(exp1, "math_type") and exp1[2].tag == "Id"
      and tltype.isStr(get_type(exp2)) and get_type(exp2)[1] == "integer" then
    local var, floc, _ = tlst.get_local(env, exp1[2][1])
    if var and floc and not var.assigned then
      return tlfilter.set_single(var, tlfilter.filter_integer)
    end
  elseif exp1.tag == "Id" and exp2.tag == "Nil" then
    local var, floc, _ = tlst.get_local(env, exp1[1])
    if var and floc and not var.assigned then
      return tlfilter.set_single(var, tlfilter.filter_nil)
    end
  end
  return {}
end

local function check_order (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to compare " .. bold_token .. " with " .. bold_token
  if tltype.subtype(t1, Number) and tltype.subtype(t2, Number) then
    set_type(exp, Boolean)
  elseif tltype.subtype(t1, String) and tltype.subtype(t2, String) then
    set_type(exp, Boolean)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "any", msg, exp1.pos)
  elseif tltype.isAny(t2) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "any", msg, exp2.pos)
  else
    set_type(exp, Any)
    t1, t2 = tltype.general(t1), tltype.general(t2)
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "order", msg, exp.pos)
  end
end

local function apply_filters (env, inout, fset, pos)
  local has_void = false
  for var, filter in pairs(fset) do
    if not var.assigned and not tlst.isupvalue(env, var) then
      local t = get_type(var)
      if not tltype.isProj(t) then
        local tin, tout = filter(t)
        local tf = inout and tin or tout
        has_void = has_void or tltype.isVoid(tf)
        if not tltype.isVoid(tf) then
          tlst.backup_vartype(env, var, pos)
          set_type(var, tf)
        end
      else
        local label, idx = t[1], t[2]
        local vproj = tlst.get_local(env, label)
        local proj = vproj.type
        if tltype.isUnionlist(proj) then
          local nproj = {}
          for _, tup in ipairs(proj) do
            local tv = tup[idx]
            local tin, tout = filter(tv)
            local tf = inout and tin or tout
            if not tltype.isVoid(tf) then
              local ntup = tltype.Tuple(tup)
              ntup[idx] = tf
              nproj[#nproj+1] = ntup
            end
          end
          local nproj = tltype.Unionlist(unpack(nproj))
          has_void = has_void or tltype.isVoid(nproj)
          if not tltype.isVoid(nproj) then
            tlst.backup_vartype(env, vproj, pos)
            set_type(vproj, nproj)
          end
        elseif tltype.isTuple(proj) then
          local tv = proj[idx]
          local tin, tout = filter(tv)
          local tf = inout and tin or tout
          has_void = has_void or tltype.isVoid(tf)
          if not tltype.isVoid(tf) then
            local nproj = tltype.Tuple(proj)
            nproj[idx] = tf
            if not tltype.isVoid(nproj) then
              tlst.backup_vartype(env, vproj, pos)
              set_type(vproj, nproj)
            end
          end
        else
          error("BUG: projection for variable " .. var[1] .. " has type " .. tltype.tostring(proj))
        end
      end
    end
  end
  return has_void
end

local function check_and (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  local sf1 = check_exp(env, exp1)
  tlst.push_backup(env)
  apply_filters(env, true, sf1 or {}, exp.pos)
  local sf2 = check_exp(env, exp2)
  tlst.pop_backup(env)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  if tltype.isNil(t1) or tltype.isFalse(t1) then
    set_type(exp, t1)
  elseif tltype.isUnion(t1, Nil) then
    set_type(exp, tltype.Union(t2, Nil))
  elseif tltype.isUnion(t1, False) then
    set_type(exp, tltype.Union(t2, False))
  elseif tltype.isBoolean(t1) then
    set_type(exp, tltype.Union(t2, False))
  else
    set_type(exp, tltype.Union(t1, t2))
  end
  return tlfilter.set_and(sf1, sf2)
end

local function check_or (env, exp)
  local exp1, exp2 = exp[2], exp[3]
  local sf1 = check_exp(env, exp1)
  tlst.push_backup(env)
  apply_filters(env, false, sf1 or {}, exp.pos)
  local sf2 = check_exp(env, exp2)
  tlst.pop_backup(env)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  if tltype.isNil(t1) or tltype.isFalse(t1) then
    set_type(exp, t2)
  elseif tltype.isUnion(t1, Nil) and tltype.isVoid(t2) then
    apply_filters(env, true, sf1 or {}, exp.pos)
    set_type(exp, tltype.filterUnion(t1, Nil))
  elseif tltype.isUnion(t1, Nil) then
    set_type(exp, tltype.Union(tltype.filterUnion(t1, Nil), t2))
  elseif tltype.isUnion(t1, False) then
    set_type(exp, tltype.Union(tltype.filterUnion(t1, False), t2))
  else
    set_type(exp, tltype.Union(t1, t2))
  end
  return tlfilter.set_or(sf1, sf2)
end

local function check_binary_op (env, exp)
  local op = exp[1]
  if op == "add" or op == "sub" or
     op == "mul" or op == "idiv" or op == "div" or op == "mod" or
     op == "pow" then
    check_arith(env, exp, op)
  elseif op == "concat" then
    check_concat(env, exp)
  elseif op == "eq" then
    return check_equal(env, exp)
  elseif op == "lt" or op == "le" then
    check_order(env, exp)
  elseif op == "and" then
    return check_and(env, exp)
  elseif op == "or" then
    return check_or(env, exp)
  elseif op == "band" or op == "bor" or op == "bxor" or
         op == "shl" or op == "shr" then
    check_bitwise(env, exp)
  else
    error("cannot type check binary operator " .. op)
  end
end

local function check_not (env, exp)
  local exp1 = exp[2]
  local sf = check_exp(env, exp1)
  set_type(exp, Boolean)
  return tlfilter.set_not(sf)
end

local function check_bnot (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  local t1 = tltype.first(get_type(exp1))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to perform bitwise on a " .. bold_token
  if tltype.subtype(t1, tltype.Integer(true)) then
    set_type(exp, Integer)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "any", msg, exp1.pos)
  else
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "bitwise", msg, exp1.pos)
  end
end

local function check_minus (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  local t1 = tltype.first(get_type(exp1))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to perform arithmetic on a " .. bold_token
  if tltype.subtype(t1, Integer) then
    set_type(exp, Integer)
  elseif tltype.subtype(t1, Number) then
    set_type(exp, Number)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "any", msg, exp1.pos)
  else
    set_type(exp, Any)
    t1 = tltype.general(t1)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "arith", msg, exp1.pos)
  end
end

local function check_len (env, exp)
  local exp1 = exp[2]
  check_exp(env, exp1)
  local t1 = tltype.first(get_type(exp1))
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to get length of a " .. bold_token
  if tltype.subtype(t1, String) or
     tltype.subtype(t1, tltype.Table()) then
    set_type(exp, Integer)
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "any", msg, exp1.pos)
  else
    set_type(exp, Any)
    t1 = tltype.general(t1)
    msg = string.format(msg, tltype.tostring(t1))
    typeerror(env, "len", msg, exp1.pos)
  end
end

local function check_unary_op (env, exp)
  local op = exp[1]
  if op == "not" then
    return check_not(env, exp)
  elseif op == "bnot" then
    check_bnot(env, exp)
  elseif op == "unm" then
    check_minus(env, exp)
  elseif op == "len" then
    check_len(env, exp)
  else
    error("cannot type check unary operator " .. op)
  end
end

local function check_op (env, exp)
  if exp[3] then
    return check_binary_op(env, exp)
  else
    return check_unary_op(env, exp)
  end
end

local function check_paren (env, exp)
  local exp1 = exp[1]
  check_exp(env, exp1)
  local t1 = get_type(exp1)
  set_type(exp, tltype.first(t1))
end

local function check_parameters (env, parlist, pos)
  local len = #parlist
  if len == 0 then
    if env.strict then
      return tltype.Tuple({ Nil }, true)
    else
      return tltype.Tuple({ Value }, true)
    end
  else
    local l = {}
    if parlist[1][1] == "self" and not parlist[1][2] then
      parlist[1][2] = Self
    end
    for i = 1, len do
      if not parlist[i][2] then parlist[i][2] = Any end
      l[i] = replace_names(env, parlist[i][2], pos)
    end
    if parlist[len].tag == "Dots" then
      local t = parlist[len][1] or Any
      l[len] = t
      tlst.set_vararg(env, t)
      return tltype.Tuple(l, true)
    else
      if env.strict then
        l[len + 1] = Nil
        return tltype.Tuple(l, true)
      else
        l[len + 1] = Value
        return tltype.Tuple(l, true)
      end
    end
  end
end

local function check_explist (env, explist, lselfs)
  lselfs = lselfs or {}
  local fsets = {}
  -- Lua (and LuaJIT) evaluates an expression list left-to-right
  for k, v in ipairs(explist) do
    fsets[k] = check_exp(env, v, lselfs[k])
  end
  return fsets
end

local function check_return_type (env, inf_type, dec_type, pos)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "return type " .. bold_token .. " does not match " .. bold_token
  if tltype.isUnionlist(dec_type) then
    dec_type = tltype.unionlist2tuple(dec_type)
  end
  dec_type = tltype.unfold(dec_type)
  if tltype.subtype(inf_type, dec_type) then
    return
  elseif tltype.consistent_subtype(inf_type, dec_type) then
    msg = string.format(msg, tltype.tostring(inf_type), tltype.tostring(dec_type))
    typeerror(env, "any", msg, pos)
  else
    msg = string.format(msg, tltype.tostring(inf_type), tltype.tostring(dec_type))
    typeerror(env, "ret", msg, pos)
  end
end

local function check_function (env, exp, tself)
  local oself = env.self
  env.self = tself
  local idlist, ret_type, block = exp[1], replace_names(env, exp[2], exp.pos), exp[3]
  local infer_return = false
  if not block then
    block = ret_type
    ret_type = tltype.Tuple({ Nil }, true)
    infer_return = true
  end
  tlst.begin_function(env)
  tlst.begin_scope(env)
  local input_type = check_parameters(env, idlist, exp.pos)
  local t = tltype.Function(input_type, ret_type)
  local len = #idlist
  if len > 0 and idlist[len].tag == "Dots" then len = len - 1 end
  for k = 1, len do
    local v = idlist[k]
    v[2] = replace_names(env, v[2], exp.pos)
    set_type(v, v[2])
    set_ubound(v, v[2])
    check_masking(env, v[1], v.pos)
    tlst.set_local(env, v)
  end
  local r = check_block(env, block)
  if not r then tlst.set_return_type(env, tltype.Tuple({ Nil }, true)) end
  check_unused_locals(env)
  tlst.end_scope(env)
  local inferred_type = infer_return_type(env)
  if infer_return then
    ret_type = inferred_type
    t = tltype.Function(input_type, ret_type)
    set_type(exp, t)
  end
  if env.self then
    t = check_self_field(env, t, t, exp.pos)
  else
    t = check_self(env, t, t, exp.pos)
  end
  check_return_type(env, inferred_type, ret_type, exp.pos)
  tlst.end_function(env)
  set_type(exp, t)
  env.self = oself
end

local function check_table (env, exp)
  local l = {}
  local i = 1
  local len = #exp
  for k, v in ipairs(exp) do
    local tag = v.tag
    local t1, t2
    if tag == "Pair" then
      local exp1, exp2 = v[1], v[2]
      check_exp(env, exp1)
      check_exp(env, exp2)
      t1, t2 = get_type(exp1), tltype.general(get_type(exp2))
      if tltype.subtype(Nil, t1) then
        t1 = Any
        local msg = "table index can be nil"
        typeerror(env, "table", msg, exp1.pos)
      elseif not (tltype.subtype(t1, Boolean) or
                  tltype.subtype(t1, Number) or
                  tltype.subtype(t1, String)) then
        t1 = Any
        local msg = "table index is dynamic"
        typeerror(env, "any", msg, exp1.pos)
      end
    else
      local exp1 = v
      check_exp(env, exp1)
      t1, t2 = tltype.Literal(i), tltype.general(get_type(exp1))
      if k == len and tltype.isVararg(t2) then
        t1 = Integer
      end
      i = i + 1
    end
    if t2.open then t2.open = nil end
    t2 = tltype.first(t2)
    l[k] = tltype.Field(v.const, t1, t2)
  end
  local t = tltype.Table(unpack(l))
  t.unique = true
  set_type(exp, t)
end

local function var2name (env, var)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local tag = var.tag
  if tag == "Id" then
    return string.format("local " .. bold_token, var[1])
  elseif tag == "Index" then
    if var[1].tag == "Id" and var[1][1] == "_ENV" and var[2].tag == "String" then
      return string.format("global " .. bold_token, var[2][1])
    else
      return string.format("field " .. bold_token, var[2][1])
    end
  else
    return "value"
  end
end

local function explist2typegen (explist, limit)
  local len = limit or #explist
  return function (i)
    if i <= len then
      local t = get_type(explist[i])
      return tltype.first(t)
    else
      local t = Nil
      if len > 0 then t = get_type(explist[len]) end
      if tltype.isTuple(t) then
        if i <= #t then
          t = t[i]
        else
          t = t[#t]
          if not tltype.isVararg(t) then t = Nil end
        end
      else
        t = Nil
      end
      if tltype.isVararg(t) then
        return tltype.first(t)
      else
        return t
      end
    end
  end
end

local function arglist2type (explist)
  local len = #explist
  if len == 0 then
    return tltype.Tuple({ Nil }, true)
  else
    local l = {}
    for i = 1, len do
      l[i] = tltype.first(get_type(explist[i]))
    end
    if not tltype.isVararg(explist[len]) then
      l[len + 1] = Nil
    end
    return tltype.Tuple(l, true)
  end
end

local function check_arguments (env, func_name, dec_type, infer_type, pos)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to pass " .. bold_token .. " to %s of input type " .. bold_token
  if tltype.subtype(infer_type, dec_type) then
    return
  elseif tltype.consistent_subtype(infer_type, dec_type) then
    msg = string.format(msg, tltype.tostring(infer_type), func_name, tltype.tostring(dec_type))
    typeerror(env, "any", msg, pos)
  else
    msg = string.format(msg, tltype.tostring(infer_type), func_name, tltype.tostring(dec_type))
    typeerror(env, "args", msg, pos)
  end
end

local function replace_self (env, t, tself)
  tself = tself or Nil
  if tltype.isSelf(t) then
    return tself
  elseif tltype.isRecursive(t) then
    local r = tltype.Recursive(t[1], replace_self(env, t[2], tself))
    r.name = t.name
    return r
  elseif tltype.isLiteral(t) or
     tltype.isBase(t) or
     tltype.isNil(t) or
     tltype.isValue(t) or
     tltype.isAny(t) or
     tltype.isTable(t) or
     tltype.isVariable(t) or
     tltype.isVoid(t) then
    return t
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
    local r = { tag = t.tag, name = t.name }
    for k, v in ipairs(t) do
      r[k] = replace_self(env, v, tself)
    end
    return r
  elseif tltype.isFunction(t) then
    return tltype.Function(replace_self(env, t[1], tself), replace_self(env, t[2], tself))
  elseif tltype.isVararg(t) then
    return tltype.Vararg(replace_self(env, t[1], tself))
  else
    return t
  end
end

local function check_call (env, exp)
  local exp1 = exp[1]
  local explist = {}
  for i = 2, #exp do
    explist[i - 1] = exp[i]
  end
  check_exp(env, exp1)
  local fsets = check_explist(env, explist)
  local t = replace_self(env, tltype.first(get_type(exp1)), env.self)
  local inferred_type = replace_self(env, arglist2type(explist), env.self)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to call %s of type " .. bold_token
  if tltype.isPrim(t) then
    if t[1] == "assert" then
      if fsets[1] then
        apply_filters(env, true, fsets[1], exp.pos)
      end
      set_type(exp, arglist2type(explist))
      return {}
    elseif t[1] == "require" and #explist == 1 and tltype.isStr(get_type(explist[1])) then
      set_type(exp, check_require(env, get_type(explist[1])[1], exp.pos))
      return {}
    elseif t[1] == "setmetatable" and #explist == 2 and
        not tltype.isNil(tltype.getField(tltype.Literal("__index"), get_type(explist[2]))) then
      local _, t2 = get_type(explist[1]), get_type(explist[2])
      local t3 = tltype.getField(tltype.Literal("__index"), t2)
      if tltype.isTable(t3) then t3.open = true end
      set_type(exp, t3)
      return {}
    else
      t = t[2]
    end
  end
  if tltype.isFunction(t) then
    check_arguments(env, var2name(env, exp1), t[1], inferred_type, exp.pos)
    set_type(exp, t[2])
  elseif tltype.isAny(t) then
    set_type(exp, Any)
    msg = string.format(msg, var2name(env, exp1), tltype.tostring(t))
    typeerror(env, "any", msg, exp.pos)
  else
    set_type(exp, Nil)
    msg = string.format(msg, var2name(env, exp1), tltype.tostring(t))
    typeerror(env, "call", msg, exp.pos)
  end
  return {}
end

local function check_invoke (env, exp)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local exp1, exp2 = exp[1], exp[2]
  local explist = {}
  for i = 3, #exp do
    explist[i - 2] = exp[i]
  end
  check_exp(env, exp1)
  check_exp(env, exp2)
  check_explist(env, explist)
  local t1, t2 = get_type(exp1), get_type(exp2)
  t1 = replace_self(env, t1, env.self)
  table.insert(explist, 1, { type = t1 })
  if tltype.isTable(t1) or
     tltype.isString(t1) or
     tltype.isStr(t1) then
    local inferred_type = replace_self(env, arglist2type(explist), env.self)
    local t3
    if tltype.isTable(t1) then
      t3 = replace_self(env, tltype.getField(t2, t1), t1)
      --local s = env.self or Nil
      --if not tltype.subtype(s, t1) then env.self = t1 end
    else
      local string_userdata = env["loaded"]["string"] or tltype.Table()
      t3 = replace_self(env, tltype.getField(t2, string_userdata), t1)
      inferred_type[1] = String
    end
    local msg = "attempt to call method " .. bold_token .. " of type " .. bold_token
    if tltype.isFunction(t3) then
      check_arguments(env, "field", t3[1], inferred_type, exp.pos)
      set_type(exp, t3[2])
    elseif tltype.isAny(t3) then
      set_type(exp, Any)
      msg = string.format(msg, exp2[1], tltype.tostring(t3))
      typeerror(env, "any", msg, exp.pos)
    else
      set_type(exp, Nil)
      msg = string.format(msg, exp2[1], tltype.tostring(t3))
      typeerror(env, "invoke", msg, exp.pos)
    end
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    local msg = "attempt to index " .. bold_token .. " with " .. bold_token
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "any", msg, exp.pos)
  else
    set_type(exp, Nil)
    local msg = "attempt to index " .. bold_token .. " with " .. bold_token
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "index", msg, exp.pos)
  end
  return false
end

local function check_local_var (env, id, inferred_type, close_local)
  local local_name, local_type, pos = id[1], id[2], id.pos
  if tltype.isMethod(inferred_type) then
    local msg = "attempt to create a method reference"
    typeerror(env, "local", msg, pos)
    inferred_type = Nil
  end
  if not local_type then
    if tltype.isNil(inferred_type) then -- pretend that the user declared it as value
      set_type(id, inferred_type)
      set_ubound(id, tltype.Value())
      id.narrow = true -- we should narrow this if we get the chance
    else
      local_type = tltype.general(inferred_type)
      --if not local_type.name then local_type.name = local_name end
      if inferred_type.unique then
        local_type.unique = nil
        local_type.open = true
      end
      if close_local then local_type.open = nil end
      set_type(id, local_type)
      set_ubound(id, local_type)
    end
  else
    check_self(env, local_type, local_type, pos)
    local_type = replace_names(env, local_type, pos)
    local local_type = tltype.unfold(local_type)
    if tltype.subtype(inferred_type, local_type) then
      if tltype.isUnion(local_type) then -- downcast, but leave option to upcast later
        set_type(id, tltype.general(inferred_type))
        set_ubound(id, local_type)
        id.narrow = true -- we should narrow this if we get the chance
      else
        set_type(id, local_type)
        set_ubound(id, local_type)
      end
    else
      local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
      local msg = "attempt to assign " .. bold_token .. " to " .. bold_token
      msg = string.format(msg, tltype.tostring(inferred_type), tltype.tostring(local_type))
      if tltype.consistent_subtype(inferred_type, local_type) then
        typeerror(env, "any", msg, pos)
        set_type(id, local_type)
        set_ubound(id, local_type)
      elseif tltype.isNil(inferred_type) then -- pretend that the user declared it as t|nil
        set_type(id, inferred_type)
        set_ubound(id, tltype.Union(local_type, Nil))
        id.narrow = true -- we should narrow this if we get the chance
      else
        typeerror(env, "local", msg, pos)
        set_type(id, inferred_type)
        set_ubound(id, inferred_type)
      end
    end
  end
  check_masking(env, id[1], id.pos)
  tlst.set_local(env, id)
end

local function unannotated_idlist (idlist, start)
  if start > #idlist then
    return false
  end
  for i = start, #idlist do
    if idlist[i][2] then return false end
  end
  return true
end

local function match_unionlist (t, max)
  max = (max or 0) + 1
  for _, tt in ipairs(t) do
    if #tt > max then
      max = #tt
    end
  end
  for _, tt in ipairs(t) do
    while #tt < max do
      local last = tt[#tt]
      tt[#tt] = tltype.Union(last[1], Nil)
      tt[#tt+1] = last
    end
  end
  return true
end

local function check_local (env, idlist, explist)
  check_explist(env, explist)
  if tltype.isUnionlist(get_type(explist[#explist])) and
     unannotated_idlist(idlist, #explist) and
     match_unionlist(get_type(explist[#explist]), #idlist - #explist) then
    local t = get_type(explist[#explist])
    local label = "$PROJ" .. tostring({})
    for i = #explist, #idlist do
      local v = idlist[i]
      set_type(v, tltype.Proj(label, i - #explist + 1))
      check_masking(env, v[1], v.pos)
      tlst.set_local(env, v)
      tlst.set_local(env, { label, type = t, ubound = t }, 1)
    end
    local tuple = explist2typegen(explist, #explist-1)
    for k = 1, #explist-1 do
      local t = tuple(k)
      local close_local = explist[k] and explist[k].tag == "Id" and tltype.isTable(t)
      check_local_var(env, idlist[k], t, close_local)
    end
  else
    local tuple = explist2typegen(explist)
    for k, v in ipairs(idlist) do
      local t = tuple(k)
      local close_local = explist[k] and explist[k].tag == "Id" and tltype.isTable(t)
      check_local_var(env, v, t, close_local)
    end
  end
  return false
end

local function check_localrec (env, id, exp)
  local idlist, ret_type, block = exp[1], replace_names(env, exp[2], exp.pos), exp[3]
  local infer_return = false
  if not block then
    block = ret_type
    ret_type = tltype.Tuple({ Nil }, true)
    infer_return = true
  end
  tlst.set_local(env, id)
  tlst.begin_function(env)
  tlst.begin_scope(env)
  local input_type = check_parameters(env, idlist, exp.pos)
  local t = tltype.Function(input_type, ret_type)
  id[2] = t
  set_type(id, t)
  set_ubound(id, t)
  check_masking(env, id[1], id.pos)
  local len = #idlist
  if len > 0 and idlist[len].tag == "Dots" then len = len - 1 end
  for k = 1, len do
    local v = idlist[k]
    v[2] = replace_names(env, v[2], exp.pos)
    set_type(v, v[2])
    set_ubound(v, v[2])
    check_masking(env, v[1], v.pos)
    tlst.set_local(env, v)
  end
  local r = check_block(env, block)
  if not r then tlst.set_return_type(env, tltype.Tuple({ Nil }, true)) end
  check_unused_locals(env)
  tlst.end_scope(env)
  local inferred_type = infer_return_type(env)
  if infer_return then
    ret_type = inferred_type
    t = tltype.Function(input_type, ret_type)
    id[2] = t
    set_type(id, t)
    set_ubound(id, t)
    set_type(exp, t)
  end
  check_return_type(env, inferred_type, ret_type, exp.pos)
  tlst.end_function(env)
  return false
end

local function explist2typelist (explist)
  local len = #explist
  if len == 0 then
    return tltype.Tuple({ Nil }, true)
  else
    local l = {}
    for i = 1, len - 1 do
      table.insert(l, tltype.first(get_type(explist[i])))
    end
    local last_type = get_type(explist[len])
    if tltype.isUnionlist(last_type) then
      last_type = tltype.unionlist2tuple(last_type)
    end
    if tltype.isTuple(last_type) then
      for _, v in ipairs(last_type) do
        if not tltype.isVararg(v) then
          table.insert(l, tltype.first(v))
        else
          table.insert(l, v)
        end
      end
    else
      table.insert(l, last_type)
    end
    if not tltype.isVararg(l[#l]) then
      table.insert(l, tltype.Vararg(Nil))
    end
    return tltype.Tuple(l)
  end
end

local function check_return (env, stm)
  check_explist(env, stm)
  local t = explist2typelist(stm)
  if not tltype.isVoid(t) then
    tlst.set_return_type(env, tltype.general(t))
  end
  return true
end

local function check_assignment (env, varlist, explist)
  local lselfs = {}
  for k, v in ipairs(varlist) do
    if v.tag == "Index" and v[1].tag == "Id" and v[2].tag == "String" then
      local l = tlst.get_local(env, v[1][1])
      local t = get_type(l)
      -- a brittle hack to type a method definition in the right-hand side?
      if tltype.isTable(t) then lselfs[k] = t end
    end
  end
  check_explist(env, explist, lselfs)
  local l = {}
  -- evaluation of expressions in lvalues goes left-to-right, but the
  -- actual assignment is right-to-left, so this needs two passes
  for _, v in ipairs(varlist) do
    check_var_exps(env, v)
  end
  for i = #varlist, 1, -1 do
    check_var(env, varlist[i], explist[i])
    l[i] = get_type(varlist[i])
  end
  table.insert(l, tltype.Vararg(Value))
  local var_type, exp_type = tltype.Tuple(l), explist2typelist(explist)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to assign " .. bold_token .. " to " .. bold_token
  if tltype.subtype(exp_type, var_type) then
    return
  elseif tltype.consistent_subtype(exp_type, var_type) then
    msg = string.format(msg, tltype.tostring(exp_type), tltype.tostring(var_type))
    typeerror(env, "any", msg, varlist[1].pos)
  else
    msg = string.format(msg, tltype.tostring(exp_type), tltype.tostring(var_type))
    typeerror(env, "set", msg, varlist[1].pos)
  end
  return false
end

local function check_while (env, stm)
  local exp1, stm1 = stm[1], stm[2]
  tlst.push_backup(env, true)
  local fs = check_exp(env, exp1) or {}
  tlst.push_break(env)
  if apply_filters(env, true, fs, exp1.pos) then -- while block is unreacheable
    typeerror(env, "dead", "'while' body is unreacheable", stm.pos)
    tlst.pop_backup(env)
    return false
  else
    local r, didgoto = check_block(env, stm1, true)
    local frame = tlst.pop_backup(env)
    for var, ty in pairs(frame) do
      if not tltype.subtype(ty.type, var.type) then
        local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
        local msg = "variable " .. bold_token .. " is looping back with type " .. bold_token .. " incompatible with type " .. bold_token .. " that it entered loop"
        typeerror(env, "loop", string.format(msg, var[1], tltype.tostring(ty.type), tltype.tostring(var.type)), ty.pos)
      end
    end
    local snapshots = tlst.pop_break(env)
    if not r then
      snapshots[#snapshots+1] = frame
    end
    snapshots[#snapshots+1] = {}
    local newtypes = tlst.join_snapshots(snapshots, stm.pos)
    for var, ty in pairs(newtypes) do
      tlst.commit_type(env, var, ty, stm.pos)
    end
    return false, didgoto -- while always can not return if does not execute once
  end
end

local function check_repeat (env, stm)
  local stm1, exp1 = stm[1], stm[2]
  tlst.push_backup(env, true)
  tlst.push_break(env)
  local r, didgoto = check_block(env, stm1, true)
  local fs = check_exp(env, exp1) or {}
  local snapshots = tlst.pop_break(env)
  if r or apply_filters(env, true, fs, exp1.pos) then
    tlst.pop_backup(env)
  else
    local frame = tlst.pop_backup(env)
    for var, ty in pairs(frame) do
      if not tltype.subtype(ty.type, var.type) then
        local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
        local msg = "variable " .. bold_token .. " is looping back with type " .. bold_token .. " incompatible with type " .. bold_token .. " that it entered loop"
        typeerror(env, "loop", string.format(msg, var[1], tltype.tostring(ty.type), tltype.tostring(var.type)), ty.pos)
      end
    end
    snapshots[#snapshots+1] = frame
  end
  local newtypes = tlst.join_snapshots(snapshots, stm.pos)
  for var, ty in pairs(newtypes) do
    tlst.commit_type(env, var, ty, stm.pos)
  end
  return r, didgoto
end

local function check_if (env, stm)
  local rl, dg = {}, {}
  local prevfs = {}
  local snapshots = {}
  local last = #stm % 2 == 0 and #stm + 1 or #stm
  for i = 1, last, 2 do
    tlst.push_backup(env, false)
    local has_void = false
    for _, fs_pos in ipairs(prevfs) do
      has_void = has_void or apply_filters(env, false, fs_pos[1], fs_pos[2])
    end
    if has_void then -- rest of the if is unreacheable (previous condition is always true)
      if stm[i] then
        if i == last then
          typeerror(env, "dead", "'else' block is unreacheable", stm[i].pos)
        elseif i == last - 1 then
          typeerror(env, "dead", "this arm of the 'if' is unreacheable", stm[i].pos)
        end
      end
      tlst.pop_backup(env)
      break
    end
    local exp, block = stm[i], stm[i + 1]
    local has_void, fs = false, {}
    if block then
      fs = check_exp(env, exp) or {}
      has_void = apply_filters(env, true, fs, exp.pos)
      prevfs[#prevfs+1] = { fs, exp.pos }
    else
      block = exp
    end
    local r, didgoto = true, false
    if not has_void then -- "then" block of this condition is reacheable
      if block then
        r, didgoto = check_block(env, block)
      else
        r, didgoto = false, false
      end
      rl[#rl+1] = didgoto and false or r
      dg[#dg+1] = didgoto
    else
      if exp then
        typeerror(env, "dead", "this arm of the 'if' is unreacheable", exp.pos)
      end
    end
    local frame = tlst.pop_backup(env)
    if not r then
      snapshots[#snapshots+1] = frame
    end
  end
  local newtypes = tlst.join_snapshots(snapshots, stm[1].pos)
  for var, tyub in pairs(newtypes) do
    tlst.commit_type(env, var, tyub, stm[1].pos)
  end
  local r = true
  for _, v in ipairs(rl) do
    r = r and v
  end
  local didgoto = false
  for _, v in ipairs(dg) do
    didgoto = didgoto or v
  end
  return r, didgoto
end

local function infer_int(t)
  return tltype.isInt(t) or tltype.isInteger(t)
end

local function check_fornum (env, stm)
  local id, exp1, exp2, exp3, block = stm[1], stm[2], stm[3], stm[4], stm[5]
  check_exp(env, exp1)
  local t1 = get_type(exp1)
  local for_text = env.color and acolor.bold .. "'for'" .. acolor.reset or "'for'"
  local msg = for_text .. " initial value must be a number"
  if not tltype.subtype(t1, Number) then
    if tltype.consistent_subtype(t1, Number) then
      typeerror(env, "any", msg, exp1.pos)
    else
      typeerror(env, "fornum", msg, exp1.pos)
    end
  end
  check_exp(env, exp2)
  local t2 = get_type(exp2)
  msg = for_text .. " limit must be a number"
  if not tltype.subtype(t2, Number) then
    if tltype.consistent_subtype(t2, Number) then
      typeerror(env, "any", msg, exp2.pos)
    else
      typeerror(env, "fornum", msg, exp2.pos)
    end
  end
  local int_step = true
  if block then
    check_exp(env, exp3)
    local t3 = get_type(exp3)
    msg = for_text .. " step must be a number"
    if not infer_int(t3) then
      int_step = false
    end
    if not tltype.subtype(t3, Number) then
      if tltype.consistent_subtype(t3, Number) then
        typeerror(env, "any", msg, exp3.pos)
      else
        typeerror(env, "fornum", msg, exp3.pos)
      end
    end
  else
    block = exp3
  end
  tlst.push_backup(env, true)
  tlst.push_break(env)
  tlst.begin_scope(env)
  tlst.set_local(env, id)
  if infer_int(t1) and infer_int(t2) and int_step then
    set_type(id, Integer)
    set_ubound(id, Integer)
  else
    set_type(id, Number)
    set_ubound(id, Number)
  end
  local r, didgoto = check_block(env, block, true)
  check_unused_locals(env)
  tlst.end_scope(env)
  local snapshots = tlst.pop_break(env)
  local frame = tlst.pop_backup(env)
  if not r then
    for var, ty in pairs(frame) do
      if not tltype.subtype(ty.type, var.type) then
        local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
        local msg = "variable " .. bold_token .. " is looping back with type " .. bold_token .. " incompatible with type " .. bold_token .. " that it entered loop"
        typeerror(env, "loop", string.format(msg, var[1], tltype.tostring(ty.type), tltype.tostring(var.type)), ty.pos)
      end
    end
    snapshots[#snapshots+1] = frame
  end
  snapshots[#snapshots+1] = {} -- can run 0 times
  local newtypes = tlst.join_snapshots(snapshots, stm.pos)
  for var, ty in pairs(newtypes) do
    tlst.commit_type(env, var, ty, stm.pos)
  end
  return r, didgoto
end

local function check_forin (env, idlist, explist, block)
  tlst.push_backup(env, true)
  tlst.push_break(env)
  tlst.begin_scope(env)
  check_explist(env, explist)
  local t = tltype.first(get_type(explist[1]))
  local tuple = explist2typegen({})
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local msg = "attempt to iterate over " .. bold_token
  if tltype.isFunction(t) then
    local l = {}
    for k, v in ipairs(t[2]) do
      l[k] = {}
      set_type(l[k], v)
    end
    tuple = explist2typegen(l)
  elseif tltype.isAny(t) then
    msg = string.format(msg, tltype.tostring(t))
    typeerror(env, "any", msg, idlist.pos)
  else
    msg = string.format(msg, tltype.tostring(t))
    typeerror(env, "forin", msg, idlist.pos)
  end
  for k, v in ipairs(idlist) do
    local t = tltype.filterUnion(tuple(k), Nil)
    check_local_var(env, v, t, false)
  end
  local r, didgoto = check_block(env, block, true)
  check_unused_locals(env)
  tlst.end_scope(env)
  local snapshots = tlst.pop_break(env)
  local frame = tlst.pop_backup(env)
  if not r then
    for var, ty in pairs(frame) do
      if not tltype.subtype(ty.type, var.type) then
        local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
        local msg = "variable " .. bold_token .. " is looping back with type " .. bold_token .. " incompatible with type " .. bold_token .. " that it entered loop"
        typeerror(env, "loop", string.format(msg, var[1], tltype.tostring(ty.type), tltype.tostring(var.type)), ty.pos)
      end
    end
    snapshots[#snapshots+1] = frame
  end
  snapshots[#snapshots+1] = {} -- can run 0 times
  local newtypes = tlst.join_snapshots(snapshots, block.pos)
  for var, ty in pairs(newtypes) do
    tlst.commit_type(env, var, ty, block.pos)
  end
  return r, didgoto
end

local function check_id (env, exp)
  local name = exp[1]
  local l, floc, _ = tlst.get_local(env, name)
  local t = get_type(l)
  if tltype.isProj(t) then
    local label, idx = t[1], t[2]
    l = tlst.get_local(env, label)
    t = tltype.unionlist2union(get_type(l), idx)
  end
  if not floc then -- upvalue, type is ubound of the var
    t = get_ubound(l)
  end
  set_type(exp, t)
  local l, floc, _ = tlst.get_local(env, name)
  if l and floc and not l.assigned then
    return tlfilter.set_single(l, tlfilter.filter_truthy)
  else
    return {}
  end
end

local function check_index (env, exp)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local exp1, exp2 = exp[1], exp[2]
  check_exp(env, exp1)
  check_exp(env, exp2)
  local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
  local msg = "attempt to index " .. bold_token .. " with " .. bold_token
  t1 = replace_self(env, t1, env.self)
  if tltype.isTable(t1) then
    -- FIX: methods should not leave objects, this is unsafe!
    local field_type = tltype.unfold(replace_self(env, tltype.getField(t2, t1), tltype.Any()))
    if not tltype.isNil(field_type) then
      set_type(exp, field_type)
    else
      if exp1.tag == "Id" and exp1[1] == "_ENV" and exp2.tag == "String" then
        msg = "attempt to access undeclared global " .. bold_token
        msg = string.format(msg, exp2[1])
      else
        msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      end
      typeerror(env, "index", msg, exp.pos)
      set_type(exp, Nil)
    end
  elseif tltype.isAny(t1) then
    set_type(exp, Any)
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "any", msg, exp.pos)
  else
    set_type(exp, Nil)
    msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
    typeerror(env, "index", msg, exp.pos)
  end
  if exp1.tag == "Id" and exp2.tag == "String" then
    local var, floc, _ = tlst.get_local(env, exp[1])
    if var and floc and not var.assigned then
      return tlfilter.set_single(exp1[1], tlfilter.filter_field(exp2[1]))
    end
  end
  return {}
end

-- only checks the r-values that appear in l-values, but does
-- not check the actual assignment
function check_var_exps (env, var)
  local tag = var.tag
  if tag == "Index" then
    local exp1, exp2 = var[1], var[2]
    check_exp(env, exp1)
    check_exp(env, exp2)
  end
end

-- check the assignment to an lvalue, rvalues inside the lvalue have already been checked
function check_var (env, var, exp)
  local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
  local tag = var.tag
  if tag == "Id" then
    local name = var[1]
    local l, floc, lloc = tlst.get_local(env, name)
    local t = get_type(l)
    if tltype.isProj(t) then
      tlst.break_projection(env, l)
      t = get_type(l)
    end
    if not floc then -- upvalue
      l.assigned = true
      t = get_ubound(l)
      if not lloc and not tltype.subtype(t, get_type(l)) then
        local bold_token = env.color and acolor.bold .. "'%s'" .. acolor.reset or "'%s'"
        local msg = "attempt to assign to filtered upvalue " .. bold_token .. " across a loop"
        msg = string.format(msg, l[1])
        typeerror(env, "set", msg, var.pos)
      end
      set_type(l, t)
    end
    if not l.assigned then
      local tr = get_type(exp)
      if tltype.subtype(tr, t) then
        if tltype.isUnion(t) and (not tltype.subtype(t, tr)) then
          t = tltype.general(tr)
          tlst.commit_type(env, l, { type = t }, var.pos)
        end
      else
        local ubound = get_ubound(l)
        if tltype.subtype(tr, ubound) then
          t = tltype.general(tr)
          tlst.commit_type(env, l, { type = t }, var.pos)
        else
          t = ubound
        end
      end
    end
    if exp and exp.tag == "Id" and tltype.isTable(t) then t.open = nil end
    set_type(var, t)
  elseif tag == "Index" then
    local exp1, exp2 = var[1], var[2]
    local t1, t2 = tltype.first(get_type(exp1)), tltype.first(get_type(exp2))
    local msg = "attempt to index " .. bold_token .. " with " .. bold_token
    t1 = replace_self(env, t1, env.self)
    if tltype.isTable(t1) then
      local oself = env.self
      -- another brittle hack for defining methods
      if exp1.tag == "Id" and exp1[1] ~= "_ENV" then env.self = t1 end
      local field_type = tltype.getField(t2, t1)
      if not tltype.isNil(field_type) then
        set_type(var, field_type)
      else
        if t1.open then
          if exp then
            local t3 = tltype.general(get_type(exp))
            local t = tltype.general(t1)
            table.insert(t, tltype.Field(var.const, t2, t3))
            if tltype.subtype(t, t1) then
              table.insert(t1, tltype.Field(var.const, t2, t3))
            else
              msg = "could not include field " .. bold_token
              msg = string.format(msg, tltype.tostring(t2))
              typeerror(env, "open", msg, var.pos)
            end
            if t3.open then t3.open = nil end
            set_type(var, t3)
          else
            set_type(var, Nil)
          end
        else
          if exp1.tag == "Id" and exp1[1] == "_ENV" and exp2.tag == "String" then
            msg = "attempt to access undeclared global " .. bold_token
            msg = string.format(msg, exp2[1])
          else
            msg = "attempt to use " .. bold_token .. " to index closed table"
            msg = string.format(msg, tltype.tostring(t2))
          end
          typeerror(env, "open", msg, var.pos)
          set_type(var, Nil)
        end
      end
      env.self = oself
    elseif tltype.isAny(t1) then
      set_type(var, Any)
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, "any", msg, var.pos)
    else
      set_type(var, Nil)
      msg = string.format(msg, tltype.tostring(t1), tltype.tostring(t2))
      typeerror(env, "index", msg, var.pos)
    end
  else
    error("cannot type check variable " .. tag)
  end
end

function check_exp (env, exp, tself)
  if exp.type then return end
  local tag = exp.tag
  if tag == "Nil" then
    set_type(exp, Nil)
  elseif tag == "Dots" then
    set_type(exp, tltype.Vararg(tlst.get_vararg(env)))
  elseif tag == "True" then
    set_type(exp, True)
  elseif tag == "False" then
    set_type(exp, False)
  elseif tag == "Number" then
    set_type(exp, tltype.Literal(exp[1]))
  elseif tag == "String" then
    set_type(exp, tltype.Literal(exp[1]))
  elseif tag == "Function" then
    check_function(env, exp, tself)
  elseif tag == "Table" then
    check_table(env, exp)
  elseif tag == "Op" then
    return check_op(env, exp)
  elseif tag == "Paren" then
    check_paren(env, exp)
  elseif tag == "Call" then
    check_call(env, exp)
  elseif tag == "Invoke" then
    check_invoke(env, exp)
  elseif tag == "Id" then
    return check_id(env, exp)
  elseif tag == "Index" then
    check_index(env, exp)
  else
    error("cannot type check expression " .. tag)
  end
end

function check_stm (env, stm)
  local tag = stm.tag
  if tag == "Do" then
    return check_block(env, stm)
  elseif tag == "Set" then
    return check_assignment(env, stm[1], stm[2])
  elseif tag == "While" then
    return check_while(env, stm)
  elseif tag == "Repeat" then
    return check_repeat(env, stm)
  elseif tag == "If" then
    return check_if(env, stm)
  elseif tag == "Fornum" then
    return check_fornum(env, stm)
  elseif tag == "Forin" then
    return check_forin(env, stm[1], stm[2], stm[3])
  elseif tag == "Local" then
    return check_local(env, stm[1], stm[2])
  elseif tag == "Localrec" then
    return check_localrec(env, stm[1][1], stm[2][1])
  elseif tag == "Goto" then
    return false, true
  elseif tag == "Label" then
    return false
  elseif tag == "Return" then
    return check_return(env, stm)
  elseif tag == "Break" then
    tlst.push_break_snapshot(env)
    return false
  elseif tag == "Call" then
    check_call(env, stm)
    if tltype.isVoid(get_type(stm)) then
      return true
    else
      return false
    end
  elseif tag == "Invoke" then
    return check_invoke(env, stm)
  elseif tag == "Interface" then
    return check_interface(env, stm)
  else
    error("cannot type check statement " .. tag)
  end
end

local function check_stms (env, block)
  local r = false
  local bkp = env.self
  local didgoto = false
  for i, v in ipairs(block) do
    local isret, isgoto = check_stm(env, v)
    r = r or isret
    didgoto = didgoto or isgoto
    env.self = bkp
    if r and not didgoto then
      if i ~= #block then
        typeerror(env, "dead", "unreacheable statement", block[i+1].pos)
      end
      break
    end -- rest of the block is unreacheable
  end
  if didgoto then r = false end
  check_unused_locals(env)
  return r, didgoto
end

function check_block (env, block, loop)
  tlst.begin_scope(env, loop)
  local r, didgoto = check_stms(env, block)
  tlst.end_scope(env)
  return r, didgoto
end

local function load_lua_env (env)
  local path = "typedlua/"
  local l = {}
  if _VERSION == "Lua 5.1" then
    path = path .. "lsl51/"
    l = { "coroutine", "package", "string", "table", "math", "io", "os", "debug" }
  elseif _VERSION == "Lua 5.2" then
    path = path .. "lsl52/"
    l = { "coroutine", "package", "string", "table", "math", "bit32", "io", "os", "debug" }
  elseif _VERSION == "Lua 5.3" then
    path = path .. "lsl53/"
    l = { "coroutine", "package", "string", "utf8", "table", "math", "io", "os", "debug" }
  else
    error("Typed Lua does not support " .. _VERSION)
  end
  local t = check_require(env, "base", 0, path)
  for _, v in ipairs(l) do
    local t1 = tltype.Literal(v)
    local t2 = check_require(env, v, 0, path)
    local f = tltype.Field(false, t1, t2)
    table.insert(t, f)
  end
  t.open = true
  local lua_env = tlast.ident(0, "_ENV", t)
  set_type(lua_env, t)
  set_ubound(lua_env, t)
  tlst.set_local(env, lua_env)
  tlst.get_local(env, "_ENV")
end

function tlchecker.typecheck (ast, subject, filename, strict, integer, color)
  assert(type(ast) == "table")
  assert(type(subject) == "string")
  assert(type(filename) == "string")
  assert(type(strict) == "boolean")
  assert(type(color) == "boolean")
  local env = tlst.new_env(subject, filename, strict, color)
  if integer and _VERSION == "Lua 5.3" then
    Integer = tltype.Integer(true)
    env.integer = true
    tltype.integer = true
  end
  tlst.begin_function(env)
  tlst.begin_scope(env)
  tlst.set_vararg(env, String)
  load_lua_env(env)
  check_stms(env, ast)
  tlst.end_scope(env)
  tlst.end_function(env)
  return env.messages
end

local function get_source_line(filename, l)
  local i = 1
  for source_line in io.lines(filename) do
    if i == l then
      return (string.gsub(source_line, "\t", " "))
    end
    i = i + 1
  end
end

local function get_source_arrow(c, color, is_warning)
  if color then
    local color_code = is_warning and acolor.magenta or acolor.red
    return string.rep(" ", c - 1) .. color_code .. "^" .. acolor.reset
  else
    return string.rep(" ", c - 1) .. "^"
  end
end

function tlchecker.error_msgs (messages, warnings, color, line_preview)
  assert(type(messages) == "table")
  assert(type(warnings) == "boolean")
  local l = {}
  local msg = color and acolor.bold .. "%s:%d:%d:" .. acolor.reset .. " %s, %s" or "%s:%d:%d: %s, %s"
  local skip_error = { any = true,
    mask = true,
    unused = true,
  }
  local n = 0
  for _, v in ipairs(messages) do
    local tag = v.tag
    if skip_error[tag] then
      if warnings then
        local warning_text = color and acolor.magenta .. "warning" .. acolor.reset or "warning"
        table.insert(l, string.format(msg, v.filename, v.l, v.c, warning_text, v.msg))
        if line_preview then
          table.insert(l, get_source_line(v.filename, v.l))
          table.insert(l, get_source_arrow(v.c, color, true))
        end
      end
    else
      local error_text = color and acolor.red .. "type error" .. acolor.reset or "type error"
      table.insert(l, string.format(msg, v.filename, v.l, v.c, error_text, v.msg))
      if line_preview then
        table.insert(l, get_source_line(v.filename, v.l))
        table.insert(l, get_source_arrow(v.c, color, false))
      end
      n = n + 1
    end
  end
  if #l == 0 then
    return nil, n
  else
    return table.concat(l, "\n"), n
  end
end

return tlchecker

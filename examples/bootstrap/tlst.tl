--[[
This module implements Typed Lua symbol table.
]]

local tlst = {}

local typealias Type = {
  "tag":string
}

local typealias Id = {
  "pos":number,
  1:string,
  2:Type?
}

typealias Scope = {
  "goto":{number:{"tag":string}},
  "label":{string:boolean?},
  "local":any,
  "unused":{string:{1:string}},
  "interface":{string:Type},
  "userdata":{string:Type}
}

typealias FScope = {
  "return_type":{Type},
  "vararg":Type?
}

typealias Env = {
  "subject":string,
  "filename":string,
  "strict":boolean,
  "messages":{string?},
  "maxscope":number,
  "scope":number,
  "fscope":number,
  "loop":number,
  "function":{FScope},
  "interface":{string:Type},
  "userdata":{string:Type},
  "loaded":{},
  number:Scope?
}

function tlst.new_env (subject:string, filename:string, strict:boolean):(Env)
  local env = {}
  env.subject = subject
  env.filename = filename
  env.strict = strict
  env.messages = {}
  env.maxscope = 0
  env.scope = 0
  env.fscope = 0
  env.loop = 0
  env["function"] = {}
  env["interface"] = {}
  env["userdata"] = {}
  env["loaded"] = {}
  local env:Env = env
  return env
end

local function new_scope ():(Scope)
  local senv = {}
  senv["goto"] = {}
  senv["label"] = {}
  senv["local"] = {}
  senv["unused"] = {}
  senv["interface"] = {}
  senv["userdata"] = {}
  return senv
end

function tlst.begin_scope (env:Env):()
  local scope = env.scope
  if scope > 0 then
    local s = env[scope]
    if s then
      for k, v in pairs(s["local"]) do
        if v["type"] and v["type"].open then
          v["type"].open = nil
          v["type"].reopen = true
        end
      end
    end
  end
  env.scope = scope + 1
  env.maxscope = env.scope
  env[env.scope] = new_scope()
end

function tlst.end_scope (env:Env):()
  env.scope = env.scope - 1
  local scope = env.scope
  if scope > 0 then
    local s = env[scope]
    if s then
      for k, v in pairs(s["local"]) do
        if v.bkp then
          v["type"] = v.bkp
        end
        if v["type"] and v["type"].reopen then
          v["type"].reopen = nil
          v["type"].open = true
        end
      end
    end
  end
end

function tlst.set_pending_goto (env:Env, stm:{"tag":string}):()
  local s = env[env.scope]
  if s then table.insert(s["goto"], #s["goto"] + 1, stm) end
end

function tlst.get_pending_gotos (env:Env, scope:number):({{"tag":string}}?)
  local s = env[scope]
  if s then return s["goto"] end
end

function tlst.get_maxscope (env:Env):(number)
  return env.maxscope
end

function tlst.set_label (env:Env, name:string):(boolean)
  local scope = env.scope
  local s = env[scope]
  if s then
    local label = s["label"][name]
    if not label then
      s["label"][name] = true
      return true
    end
  end
  return false
end

function tlst.exist_label (env:Env, scope:number, name:string):(boolean)
  for s = scope, 1, -1 do
    local ss = env[s]
    if ss then
      if ss["label"][name] then return true end
    end
  end
  return false
end

function tlst.set_local (env:Env, id:{1:string}):()
  local scope = env.scope
  local local_name = id[1]
  local s = env[scope]
  if s then
    s["local"][local_name] = id
    s["unused"][local_name] = id
  end
end

function tlst.get_local (env:Env, local_name:string):({1:string}?)
  local scope = env.scope
  for s = scope, 1, -1 do
    local ss = env[s]
    if ss then
      local l:{1:string}? = ss["local"][local_name]
      if l then
        ss["unused"][local_name] = nil
        return l
      end
    end
  end
  return nil
end

function tlst.masking (env:Env, local_name:string):(Id?)
  local scope = env.scope
  local s = env[scope]
  if s then
    local id:Id? = s["local"][local_name]
    return id
  end
end

function tlst.unused (env:Env):({string:{1:string}}?)
  local scope = env.scope
  local s = env[scope]
  if s then return s["unused"] end
end

function tlst.set_interface (env:Env, name:string, t:Type, is_local:boolean?):()
  if is_local then
    local scope = env.scope
    local s = env[scope]
    if s then s["interface"][name] = t end
  else
    env["interface"][name] = t
  end
end

function tlst.get_interface (env:Env, name:string):(Type?)
  local scope = env.scope
  for s = scope, 1, -1 do
    local ss = env[s]
    if ss then
      local t = ss["interface"][name]
      if t then return t end
    end
  end
  if env["interface"][name] then
    return env["interface"][name]
  end
  for s = scope, 1, -1 do
    local ss = env[s]
    if ss then
      local t = ss["userdata"][name]
      if t then return t end
    end
  end
  return env["userdata"][name]
end

function tlst.set_userdata (env:Env, name:string, t:Type, is_local:boolean?):()
  if is_local then
    local scope = env.scope
    local s = env[scope]
    if s then s["userdata"][name] = t end
  else
    env["userdata"][name] = t
  end
end

function tlst.get_userdata (env:Env, name:string):(Type?)
  local scope = env.scope
  for s = scope, 1, -1 do
    local ss = env[s]
    if ss then
      local t = ss["userdata"][name]
      if t then return t end
    end
  end
  return env["userdata"][name]
end

local function new_fenv ():(FScope)
  local fenv = {}
  fenv["return_type"] = {}
  local fenv:FScope = fenv
  return fenv
end

function tlst.begin_function (env:Env):()
  env.fscope = env.fscope + 1
  env["function"][env.fscope] = new_fenv()
end

function tlst.end_function (env:Env):()
  env.fscope = env.fscope - 1
end

function tlst.set_vararg (env:Env, t:Type):()
  local s = env["function"][env.fscope]
  if s then s["vararg"] = t end
end

function tlst.get_vararg (env:Env)
  local s = env["function"][env.fscope]
  if s then return s["vararg"] end
end

function tlst.is_vararg (env:Env):(boolean)
  local t = tlst.get_vararg(env)
  if t then return true else return false end
end

function tlst.set_return_type (env:Env, t:Type):()
  local s = env["function"][env.fscope]
  if s then
    table.insert(s["return_type"], #s["return_type"] + 1, t)
  end
end

function tlst.get_return_type (env:Env):({Type}?)
  local s = env["function"][env.fscope]
  if s then return s["return_type"] end
end

function tlst.begin_loop (env:Env):()
  env.loop = env.loop + 1
end

function tlst.end_loop (env:Env):()
  env.loop = env.loop - 1
end

function tlst.insideloop (env:Env):(boolean)
  return env.loop > 0
end

return tlst

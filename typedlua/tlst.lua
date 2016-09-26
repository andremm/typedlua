--[[
This module implements Typed Lua symbol table.
]]

local tltype = require "typedlua.tltype"

local tlst = {}

-- new_env : (string, string, boolean) -> (env)
function tlst.new_env (subject, filename, strict, color)
  local env = {}
  env.subject = subject
  env.filename = filename
  env.parent = {}
  env.strict = strict
  env.color = color
  env.integer = false
  env.messages = {}
  env.maxscope = 0
  env.scope = 0
  env.fscope = 0
  env.loop = 0
  env["function"] = {}
  env["interface"] = {}
  env["userdata"] = {}
  env["loaded"] = {}
  env["projections"] = {}
  env["backups"] = {}
  env["breaks"] = {}
  return env
end

function tlst.new_projection(env, t)
  local label = {}
  env["projections"][label] = t
  return label
end

function tlst.get_projection(env, label)
  return env["projections"][label]
end

function tlst.set_projection(env, label, t)
  env["projections"][label] = t
end

-- new_scope : () -> (senv)
local function new_scope ()
  local senv = {}
  senv["goto"] = {}
  senv["label"] = {}
  senv["local"] = {}
  senv["unused"] = {}
  senv["interface"] = {}
  senv["userdata"] = {}
  return senv
end

-- begin_scope : (env, boolean) -> ()
--  second argument indicates if this is a loop scope
function tlst.begin_scope (env, loop)
  local scope = env.scope
  if scope > 0 then
    for _, v in pairs(env[scope]["local"]) do
      if v["type"] and v["type"].open then
        v["type"].open = nil
        v["type"].reopen = true
      end
    end
  end
  env.scope = scope + 1
  env.maxscope = env.scope
  env[env.scope] = new_scope()
  env[env.scope]["function"] = env["function"][env.fscope]
  env[env.scope].loop = loop
end

-- end_scope : (env) -> ()
function tlst.end_scope (env)
  env.scope = env.scope - 1
  local scope = env.scope
  if scope > 0 then
    for _, v in pairs(env[scope]["local"]) do
      if v["type"] and v["type"].reopen then
        v["type"].reopen = nil
        v["type"].open = true
      end
    end
  end
end

-- set_pending_goto : (env, stm) -> ()
function tlst.set_pending_goto (env, stm)
  table.insert(env[env.scope]["goto"], stm)
end

-- get_pending_gotos : (env, number) -> ({number:stm})
function tlst.get_pending_gotos (env, scope)
  return env[scope]["goto"]
end

-- get_maxscope : (env) -> (number)
function tlst.get_maxscope (env)
  return env.maxscope
end

-- set_label : (env, string) -> (boolean)
function tlst.set_label (env, name)
  local scope = env.scope
  local label = env[scope]["label"][name]
  if not label then
    env[scope]["label"][name] = true
    return true
  else
    return false
  end
end

-- exist_label : (env, number, string) -> (boolean)
function tlst.exist_label (env, scope, name)
  for s = scope, 1, -1 do
    if env[s]["label"][name] then return true end
  end
  return false
end

-- set_local : (env, id, integer) -> ()
function tlst.set_local (env, id, scope)
  scope = scope or env.scope
  local local_name = id[1]
  id.scope = scope
  id.bkp = {}
  env[scope]["local"][local_name] = id
  env[scope]["unused"][local_name] = id
end

-- get_local : (env, string) -> (id, boolean, boolean)
--   second return value is boolean indicating if is local to this function
--   third return value indicates if the function definition is crossing a loop
function tlst.get_local (env, local_name)
  local scope = env.scope
  local currfunc = env[env.scope]["function"]
  local no_loop = true
  for s = scope, 1, -1 do
    local l = env[s]["local"][local_name]
    if l then
      env[s]["unused"][local_name] = nil
      return l, env[s]["function"] == currfunc, no_loop
    end
    no_loop = no_loop and (not env[s].loop)
  end
  return nil, false, false
end

-- masking : (env, string) -> (id|nil)
function tlst.masking (env, local_name)
  local scope = env.scope
  return env[scope]["local"][local_name]
end

-- unused : (env) -> ({string:id})
function tlst.unused (env)
  local scope = env.scope
  return env[scope]["unused"]
end

-- set_interface : (env, string, type, boolean?) -> ()
function tlst.set_interface (env, name, t, is_local)
  if is_local then
    local scope = env.scope
    env[scope]["interface"][name] = t
  else
    env["interface"][name] = t
  end
end

-- get_interface : (env) -> (type?)
function tlst.get_interface (env, name)
  local scope = env.scope
  for s = scope, 1, -1 do
    local t = env[s]["interface"][name]
    if t then return t end
  end
  if env["interface"][name] then
    return env["interface"][name]
  end
  for s = scope, 1, -1 do
    local t = env[s]["userdata"][name]
    if t then return t end
  end
  return env["userdata"][name]
end

-- set_userdata : (env, string, type, boolean?) -> ()
function tlst.set_userdata (env, name, t, is_local)
  if is_local then
    local scope = env.scope
    env[scope]["userdata"][name] = t
  else
    env["userdata"][name] = t
  end
end

-- get_userdata : (env) -> (type?)
function tlst.get_userdata (env, name)
  local scope = env.scope
  for s = scope, 1, -1 do
    local t = env[s]["userdata"][name]
    if t then return t end
  end
  return env["userdata"][name]
end

-- new_fenv : () -> (fenv)
local function new_fenv ()
  local fenv = {}
  fenv["return_type"] = {}
  return fenv
end

-- begin_function : (env) -> ()
function tlst.begin_function (env)
  env.fscope = env.fscope + 1
  env["function"][env.fscope] = new_fenv()
end

-- end_function : (env) -> ()
function tlst.end_function (env)
  env.fscope = env.fscope - 1
end

-- set_vararg : (env, type) -> ()
function tlst.set_vararg (env, t)
  env["function"][env.fscope]["vararg"] = t
end

-- get_vararg : (env) -> (type?)
function tlst.get_vararg (env)
  return env["function"][env.fscope]["vararg"]
end

-- is_vararg : (env) -> (boolean)
function tlst.is_vararg (env)
  local t = tlst.get_vararg(env)
  if t then return true else return false end
end

-- set_return_type : (env, type) -> ()
function tlst.set_return_type (env, t)
  table.insert(env["function"][env.fscope]["return_type"], t)
end

-- get_return_type : (env) -> ({type})
function tlst.get_return_type (env)
  return env["function"][env.fscope]["return_type"]
end

-- begin_loop : (env) -> ()
function tlst.begin_loop (env)
  env.loop = env.loop + 1
end

-- end_loop : (env) -> ()
function tlst.end_loop (env)
  env.loop = env.loop - 1
end

-- insideloop : (env) -> (boolean)
function tlst.insideloop (env)
  return env.loop > 0
end

-- pushes a frame for backing up types for filters/refinement
-- (env, boolean) -> (frame)
function tlst.push_backup(env)
  local bkps = env["backups"]
  local frame = { scope = env.scope + 1 }
  bkps[#bkps+1] = frame
  return frame
end

-- revert types of backed-up vars (returns frame with the types before reversal)
function tlst.revert_types(env, frame)
  local curr = {}
  for var, ty in pairs(frame) do
    if var ~= "scope" then
      if var.scope <= env.scope then
        curr[var] = { type = var.type, pos = ty.pos }
      end
      var.type = ty.type
    end
  end
  return curr
end

-- pops a backup frame, reverting the types, returns another frame with the current types
function tlst.pop_backup(env)
  local bkps = env["backups"]
  local frame = bkps[#bkps]
  bkps[#bkps] = nil
  return tlst.revert_types(env, frame)
end

-- backup current type of var, return false is there is no
--   active backup frame for var
function tlst.backup_vartype(env, var, pos)
  assert(pos, "position must not be nil")
  local bkps = env["backups"]
  if #bkps == 0 then return false end
  local frame = bkps[#bkps]
  if frame.scope < var.scope then
    return false
  end
  if frame[var] then -- keep current backup but update position
    frame[var].pos = pos
    return true
  else
    frame[var] = { type = var.type, pos = pos }
    return true
  end
end

-- breaks link of var with other variables in the same projection
function tlst.break_projection (env, var)
  local label, idx = var["type"][1], var["type"][2]
  local proj = tlst.get_local(env, label)
  for _, frame in ipairs(env["backups"]) do
    if frame[proj] then
      local t, pos = frame[proj].type, frame[proj].pos
      frame[var] = { type = tltype.unionlist2union(t, idx), pos = pos }
    end
  end
  var.type = tltype.unionlist2union(proj.type, idx)
  var.ubound = tltype.unionlist2union(proj.ubound, idx)
end

-- join two sets of saved types
function tlst.join_types(type1, type2, pos)
  return { type = tltype.Union(type1.type, type2.type), pos = pos }
end

-- join all the snapshots of types
-- (if a variable is not present in a snapshot its current type is taken)
function tlst.join_snapshots(snaps, pos)
  assert(pos, "position must not be null")
  if #snaps == 0 then return {} end
  local joined = {}
  for var, ty in pairs(snaps[1]) do
    joined[var] = ty
  end
  for i = 2, #snaps do
    for var, ty in pairs(joined) do
      if not snaps[i][var] then
        joined[var] = tlst.join_types({ type = var.type, pos = var.pos }, ty, pos)
      else
        joined[var] = tlst.join_types(snaps[i][var], ty, pos)
      end
    end
    for var, ty in pairs(snaps[i]) do
      if not joined[var] then
        joined[var] = tlst.join_types({ type = var.type, pos = var.pos }, ty, pos)
      end
    end
  end
  return joined
end

function tlst.isupvalue(env, var)
  return env[env.scope]["function"] ~= env[var.scope]["function"]
end

function tlst.commit_type(env, var, ty, pos)
  assert(pos, "position must not be nil")
  local inframe = tlst.backup_vartype(env, var, pos)
  var.type = ty.type
  if var.narrow and not inframe then
    var.narrow = false
    var.ubound = var.type
  end
end

-- pushes a frame form storing break snapshots
function tlst.push_break(env)
  local brks = env["breaks"]
  local frame = { scope = env.scope + 1 }
  brks[#brks+1] = frame
  return frame
end

-- pops the frame with the break snapshots
function tlst.pop_break(env)
  local brks = env["breaks"]
  local frame = brks[#brks]
  brks[#brks] = nil
  return frame
end

-- push snapshot of current backed-up types in topmost break frame
function tlst.push_break_snapshot(env)
  local bkps = env["backups"]
  local brks = env["breaks"]
  local brk_frame = brks[#brks]
  local snap = {}
  for i = #bkps, 1, -1 do
    if bkps[i].scope < brk_frame.scope then break end -- we escaped this break's scope
    local frame = bkps[i]
    for var, ty in pairs(frame) do
      if var ~= "loop" and var ~= "scope" then
        snap[var] = { type = var.type, pos = ty.pos }
      end
    end
  end
  brk_frame[#brk_frame+1] = snap
end

return tlst

--[[
This file implements functions that handle the symbol table
]]

local st = {}

function st.lineno (s, i)
  if i == 1 then return 1, 1 end
  local l, lastline = 0, ""
  s = s:sub(1, i) .. "\n"
  for line in s:gmatch("[^\n]*[\n]") do
    l = l + 1
    lastline = line
  end
  local c = lastline:len() - 1
  return l, c ~= 0 and c or 1
end

function st.begin_scope (env)
  if not env.scope then
    env.scope = 0
  else
    env.scope = env.scope + 1
  end
  local scope = env.scope
  env.maxscope = scope
  env[scope] = {}
  env[scope]["label"] = {}
  env[scope]["local"] = {}
  env[scope]["goto"] = {}
  env[scope]["type"] = {}
end

function st.end_scope (env)
  env.scope = env.scope - 1
end

function st.begin_function (env)
  if not env.fscope then
    env.fscope = 0
  else
    env.fscope = env.fscope + 1
  end
  local fscope = env.fscope
  env["function"][fscope] = {}
end

function st.end_function (env)
  env.fscope = env.fscope - 1
end

function st.begin_loop (env)
  if not env.loop then
    env.loop = 1
  else
    env.loop = env.loop + 1
  end
end

function st.end_loop (env)
  env.loop = env.loop - 1
end

function st.insideloop (env)
  if env.loop and env.loop > 0 then
    return true
  end
  return false
end

return st

--[[
This module implements functions that handle scoping rules 
]]
local scope = {}

function scope.lineno (s, i)
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

function scope.new_scope (env)
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
end

function scope.begin_scope (env)
  env.scope = env.scope + 1
end

function scope.end_scope (env)
  env.scope = env.scope - 1
end

function scope.new_function (env)
  if not env.fscope then
    env.fscope = 0
  else
    env.fscope = env.fscope + 1
  end
  local fscope = env.fscope
  env["function"][fscope] = {}
end

function scope.begin_function (env)
  env.fscope = env.fscope + 1
end

function scope.end_function (env)
  env.fscope = env.fscope - 1
end

function scope.begin_loop (env)
  if not env.loop then
    env.loop = 1
  else
    env.loop = env.loop + 1
  end
end

function scope.end_loop (env)
  env.loop = env.loop - 1
end

function scope.insideloop (env)
  return env.loop and env.loop > 0
end

return scope


if #arg ~= 1 then
  print("Usage: top.lua <rockspec_dir>")
  os.exit(1)
end

local dir = arg[1]

local deps = require "luarocks.deps"

local packages = {}

-- loads latest version of each package

for rockspec in io.popen("ls -1 " .. dir .. "/*.rockspec"):lines() do
  local env = {}
  loadfile(rockspec, "t", env)()
  local p = string.lower(env.package)
  if not packages[p] then
    packages[p] = env
    packages[p].count = 0
  else
    if deps.compare_versions(env.version, packages[p].version) then
      packages[p] = env
      packages[p].count = 0
    end
  end
end

-- counts dependencies

for k, v in pairs(packages) do
  if v.dependencies then
    for i, d in ipairs(v.dependencies) do
      local j = string.find(d, " ")
      if j then d = string.lower(string.sub(d, 1, j - 1)) end
      if packages[d] then
        packages[d].count = packages[d].count + 1
      end
    end
  end
end

-- identifies and prints the ranking

local l = {}

for k, v in pairs(packages) do
  table.insert(l, string.format("%.4d_", v.count) .. k)
end

table.sort(l, function (a, b) return a > b end)

for i = 1, 10 do
  print(string.match(l[i], "(%d+)_(.+)"))
end

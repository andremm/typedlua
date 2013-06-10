
local LOG = "infra.log"

local function mkdir (dir_name)
  assert(type(dir_name) == "string")
  local MKDIR = string.format("mkdir -p %s >> %s 2>&1", dir_name, LOG)
  local ret = os.execute(MKDIR)
  if ret == nil then
    error("could not create directory: " .. dir_name)
  end
end

local function cleandir (dir_name)
  assert(type(dir_name) == "string")
  local RM = string.format("rm -rf %s >> %s 2>&1", dir_name, LOG)
  local ret = os.execute(RM)
  if ret == nil then
    error("could not remove directory: " .. dir_name)
  end
end

local function rsync (host_name, dir_name)
  assert(type(host_name) == "string")
  assert(type(dir_name) == "string")
  local RSYNC = string.format("rsync -av %s %s >> %s 2>&1", host_name, dir_name, LOG)
  local ret = os.execute(RSYNC)
  if ret == nil then
    error("could not rsync to " .. dir_name)
  end
end

local function luarocks_unpack (repo_dir, unpack_dir)
  assert(type(repo_dir) == "string")
  assert(type(unpack_dir) == "string")
  local manifest = repo_dir .. "/manifest"
  dofile(manifest)
  assert(type(repository) == "table")
  for k,v in pairs(repository) do
    print(string.format("unpacking %s", k))
    -- it is ok if the package is already unpack
    os.execute(string.format("cd %s ; luarocks --only-server=%s unpack %s >> %s 2>&1",
      unpack_dir, repo_dir, k, LOG))
  end
end

local function grep_v_dirs (unpack_dir)
  assert(type(unpack_dir) == "string")
  local str = ""
  local dirs = { "doc", "docs", "test", "tests" }
  for k,v in ipairs(dirs) do
    str = string.format("%s|grep -v '%s/[^/]*/[^/]*/%s/'", str, unpack_dir, v)
  end
  return str
end

local function get_moon_projects (unpack_dir)
  assert(type(unpack_dir) == "string")
  local FIND = "find %s -name '*.%s'"
  local dotlua = {}
  local moon_projects = {}
  for i in io.popen(string.format(FIND, unpack_dir, "lua")):lines() do
    dotlua[i] = true
  end
  for i in io.popen(string.format(FIND, unpack_dir, "moon")):lines() do
    local luafile = string.gsub(i, ".moon", ".lua")
    if dotlua[luafile] then
      local p = string.gsub(luafile, unpack_dir .. '/[^/]*/([^/]*)/.*', '%1')
      if not moon_projects[p] then moon_projects[p] = true end
    end
  end
  return moon_projects
end

local function grep_v_moon_projects (unpack_dir)
  assert(type(unpack_dir) == "string")
  local str = ""
  for k,v in pairs(get_moon_projects(unpack_dir)) do
    str = string.format("%s|grep -v '/%s/'", str, k)
  end
  return str
end

local function build_luac_list (unpack_dir, luac_list)
  assert(type(unpack_dir) == "string")
  assert(type(luac_list) == "string")
  local file = assert(io.open(luac_list, "w"))
  local FIND = string.format("find %s -name '*.lua' %s %s",
    unpack_dir, grep_v_dirs(unpack_dir), grep_v_moon_projects(unpack_dir))
  for dotlua in io.popen(FIND):lines() do
    local ret = os.execute(string.format("luac '%s' >> %s 2>&1", dotlua, LOG))
    if ret then
      file:write(dotlua .. "\n")
    end
  end
  io.close(file)
  os.remove("luac.out")
end

if #arg ~= 1 then
  print("Usage: infra.lua <dir_name>")
  os.exit(1)
end

local path = arg[1]
local repo_dir = path .. "/rocks"
local unpack_dir = path .. "/unpack"
local luarocks_host = "luarocks.org::rocks"
local luac_list = "luac.csv"

print("> removing old log")
LOG = os.getenv("PWD") .. "/" .. LOG
os.remove(LOG)
print(string.format("> creating rocks directory: %s", repo_dir))
mkdir(repo_dir)
print(string.format("> rsynching from %s to %s ...", luarocks_host, repo_dir))
rsync(luarocks_host, repo_dir)
print(string.format("> preparing unpack directory: %s", unpack_dir))
cleandir(unpack_dir)
mkdir(unpack_dir)
print(string.format("> unpacking rocks from %s to %s", repo_dir, unpack_dir))
luarocks_unpack(repo_dir, unpack_dir)
print(string.format("> bulding luac listing to %s", luac_list))
build_luac_list(unpack_dir, luac_list)

os.exit(0)


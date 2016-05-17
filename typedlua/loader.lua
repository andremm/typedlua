--[[
This file implements a Typed Lua module loader
]]
local lua_loadstring, lua_load, lua_loadfile, lua_dofile, lua_searchpath = loadstring or load, load, loadfile, dofile, package.searchpath


-------------------------------------------------------------------------------
-- Typed Lua compiler
-------------------------------------------------------------------------------

local tlparser  = require "typedlua.tlparser"
local tlchecker = require "typedlua.tlchecker"
local tlcode    = require "typedlua.tlcode"

local tlloader = {}

local opts =
{
  STRICT = true,
  INTEGER = false,
  SHOW_WARNINGS = true
}
if _VERSION == "Lua 5.3" then opts.INTEGER = true end

-- Compiles Typed Lua into Lua code
function tlloader.compile (code, chunkname)
  local code_t = type(code)
  if code_t ~= "string" then
    return nil, "expecting string (got " .. code_t .. ")"
  end

  -- Parse
  local ast, err = tlparser.parse(code, chunkname, opts.STRICT, opts.INTEGER)
  if not ast then
    return nil, err
  end

  -- Typecheck
  local messages = tlchecker.typecheck(ast, code, chunkname, opts.STRICT, opts.INTEGER)

  -- Check if there were errors
  local has_errors = tlchecker.error_msgs(messages, false)

  -- Produce output with warnings when needed
  local errors_and_warnings
  if has_errors or opts.SHOW_WARNINGS then
    errors_and_warnings = tlchecker.error_msgs(messages, true)
  end

  if has_errors then
    -- Abort execution
    return nil, errors_and_warnings
  elseif opts.SHOW_WARNINGS and errors_and_warnings then
    -- Print the warnings if requested
    print(errors_and_warnings)
  end

  -- Emit the Lua code
  return tlcode.generate(ast)
end


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

local function readfile (filename)
  local file, err = io.open(filename)
  if not file then
    return nil, err
  end
  local contents = assert(file:read("*a"))
  file:close()
  return contents
end

-- Copied from compat53
local function searchpath (name, path, sep, rep)
  sep = (sep or "."):gsub("(%p)", "%%%1")
  rep = (rep or package.config:sub(1, 1)):gsub("(%%)", "%%%1")
  local pname = name:gsub(sep, rep):gsub("(%%)", "%%%1")
  local msg = {}
  for subpath in path:gmatch("[^;]+") do
    local fpath = subpath:gsub("%?", pname)
    local f = io.open(fpath, "r")
    if f then
      f:close()
      return fpath
    end
    msg[#msg+1] = "\n\tno file '" .. fpath .. "'"
  end
  return nil, table.concat(msg)
end

local function findfile (name, pname, dirsep)
  local path = package[pname]
  if type(path) ~= "string" then
    error("'package." .. pname .. "' must be a string")
  end
  return package.searchpath(name, path, dirsep)
end

local function create_load (loadstring_func, default_chunkname)
  return function (ld, chunkname, ...)
    if type(ld) == "function" then
      local chunk = ""
      
      repeat
        local piece = ld()
	if piece == "" then
	  piece = nil
	end
	if piece then
	  assert(type(piece) == "string")
	  chunk = chunk .. piece
	end
      until not piece

      ld = chunk
      chunkname = chunkname or default_chunkname
    elseif type(ld) == "string" then
      chunkname = chunkname or ld
    end

    assert(type(ld) == "string")
    return loadstring_func(ld, chunkname, ...)
  end
end

local function create_loadfile (loadstring_func)
  return function (filename, ...)
    local contents, err = readfile(filename)
    if not contents then
      return nil, err
    end
    return loadstring_func(contents, tostring(filename), ...)
  end
end

local function create_dofile (loadfile_func)
  return function (filename, ...)
    local f = assert(loadfile_func(filename, ...))
    return f()
  end
end


-------------------------------------------------------------------------------
-- Typed Lua counterparts of Lua's load functions
-------------------------------------------------------------------------------

function tlloader.loadstring (string, chunkname, ...)
  chunkname = chunkname or "=(typedlua.loadstring)"
  local lua_code, err = tlloader.compile(string, chunkname)
  if not lua_code then
    return nil, err
  end
  return lua_loadstring(lua_code, chunkname, ...)
end

tlloader.load     = create_load    (tlloader.loadstring, "=(typedlua.load)")
tlloader.loadfile = create_loadfile(tlloader.loadstring)
tlloader.dofile   = create_dofile  (tlloader.loadfile)


-------------------------------------------------------------------------------
-- Reimplementations of internal Lua functions.
-- These are just reimplemented so all load actions go through loadstring,
-- which decides whether the string is Typed Lua or plain Lua based on the
-- chunk name: the ".tl" extension is linked to Typed Lua
-------------------------------------------------------------------------------

local function custom_loadstring (string, chunkname, ...)
  local ext = string.match(chunkname, "%.([^%.\\/]*)$")
  if ext == "tl" then
    return tlloader.loadstring(string, chunkname, ...)
  else
    return lua_loadstring(string, chunkname, ...)
  end
end

local custom_load     = create_load    (function(...) return loadstring(...) end, "=(load)")
local custom_loadfile = create_loadfile(function(...) return loadstring(...) end)
local custom_dofile   = create_dofile  (function(...) return loadfile(...) end)

local function custom_loader (name)
  local filename, err = findfile(name, "path")
  if filename == nil then
    return err
  end

  local ret
  ret, err = loadfile(filename)
  if ret then
    return ret, filename
  else
    error("error loading module " .. name .. " from file " .. filename .. ":\n\t" .. err, 3)
  end
end


-------------------------------------------------------------------------------
-- Insert / remove the Typed Lua loader.
-------------------------------------------------------------------------------

function tlloader.insert (pos)
  -- 2 is the position of the default Lua loader
  pos = pos or 2

  -- Replace the global functions
  if not package.searchpath then
    package.searchpath = searchpath
  end
  loadstring = custom_loadstring
  loadfile   = custom_loadfile
  dofile     = custom_dofile
  load       = custom_load

  -- Insert the loader
  local loaders = package.loaders or package.searchers
  for _, loader in ipairs(loaders) do
    if loader == custom_loader then
      return false
    end
  end
  table.insert(loaders, pos, custom_loader)
  return true
end

function tlloader.remove ()
  -- Restore the global functions
  package.searchpath = lua_searchpath
  loadstring         = lua_loadstring
  loadfile           = lua_loadfile
  dofile             = lua_dofile
  load               = lua_load

  -- Remove the loader
  local loaders = package.loaders or package.searchers
  for i, loader in ipairs(loaders) do
    if loader == custom_loader then
      table.remove(loaders, i)
      return true
    end
  end
  return false
end

-- Insert the loader in the default slot. It can be removed and inserted again
-- in the desired slot.
tlloader.insert()


return tlloader

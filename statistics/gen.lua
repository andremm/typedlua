
local statistics = require "statistics"

local function help ()
  print("Usage: gen.lua <filename>")
end

if #arg ~= 1 then
  help()
  os.exit(1)
end

local filename = arg[1]

local result = statistics.generate(filename)
statistics.log_result(filename, result)

os.exit(0)

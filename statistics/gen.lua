
local statistics = require "statistics"

local function help ()
  print("Usage: gen.lua <opt> <filename>")
  print("--csv", "prints in csv style")
  print("--help", "prints this message")
  print("--log", "prints in the old style log")
end

if #arg ~= 2 then
  help()
  os.exit(1)
end

local opt = arg[1]
local filename = arg[2]

if opt ~= "--csv" and opt ~= "--log" then
  help()
  os.exit(1)
end

local result = statistics.generate(filename)

if opt == "--csv" then
  statistics.print_header()
  statistics.print_result(filename, result)
elseif opt == "--log" then
  statistics.log_result(filename, result)
end

os.exit(0)

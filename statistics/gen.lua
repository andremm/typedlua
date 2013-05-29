
local statistics = require "statistics"

if #arg ~= 1 then
  print("Usage: gen.lua <filename>")
  os.exit(1)
end

filename = arg[1]

statistics.generate(filename)

os.exit(0)

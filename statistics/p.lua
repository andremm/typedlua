
if #arg ~= 2 then
  print ("Usage: p.lua <total> <x>")
  os.exit(1)
end

local total = tonumber(arg[1])
local x = tonumber(arg[2])
local p = (100*x) / total

print(string.format("%.0f%%", p))

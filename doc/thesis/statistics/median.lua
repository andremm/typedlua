
if #arg ~= 1 then
  print("Usage: lua median.lua <file>")
  os.exit(1)
end

local filename = arg[1]

local t = {}

for line in io.lines(filename) do
  table.insert(t, tonumber(line))
end

table.sort(t)

local n = #t

if n % 2 == 0 then
  print((t[n / 2] + t[n / 2 + 1]) / 2)
else
  print(t[(n + 1) / 2])
end

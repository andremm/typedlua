
local f = "../statistics/database.csv"

local h = {}

for line in io.lines(f) do
  line = string.gsub(line, "/Users/andre/tmp/luarocks_repo//unpack/", "")
  --local k = string.match(line, "[^/]+[/]([^/]+)")
  local k = string.match(line, "([^/]+)[/]")
  if not h[k] then h[k] = true end
end

local l = {}

for k, v in pairs(h) do
  table.insert(l, k)
end

print(l[math.random(1,#l)])

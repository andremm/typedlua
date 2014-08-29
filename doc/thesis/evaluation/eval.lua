
local list = {
  "lsl.base",
  "lsl.coroutine",
  "lsl.package",
  "lsl.string",
  "lsl.table",
  "lsl.math",
  "lsl.bit32",
  "lsl.io",
  "lsl.os",
  "md5.md5",
}

local cat = { "easy", "over", "poly", "hard" }

local function p (x, total)
  return string.format("%.0f%%", ((100 * x) / total))
end

local result_by_module = {}
local total_by_module = {}

local result_by_library = {}
local total_by_library = {}

for i, m in ipairs(list) do
  local mod = require(m)
  result_by_module[i] = { 0, 0, 0, 0 }
  total_by_module[i] = 0
  for k, c in pairs(mod) do
    result_by_module[i][c] = result_by_module[i][c] + 1
    total_by_module[i] = total_by_module[i] + 1
  end
  local l = string.match(m, "(%w+)[.]%w+")
  if not result_by_library[l] then
    result_by_library[l] = { 0, 0, 0, 0 }
    total_by_library[l] = 0
  end
  for j, v in ipairs(result_by_module[i]) do
    result_by_library[l][j] = result_by_library[l][j] + v
    total_by_library[l] = total_by_library[l] + v
  end
end

for k, l in pairs(result_by_library) do
  print(k)
  for i, v in ipairs(l) do
    print(cat[i], v)
  end
  print("total", total_by_library[k], p(l[1] + l[2], total_by_library[k]))
  print("---")
end

for i, m in ipairs(result_by_module) do
  print(list[i])
  for j, v in ipairs(m) do
    print(cat[j], v)
  end
  print("total", total_by_module[i], p(m[1] + m[2], total_by_module[i]))
  print("---")
end



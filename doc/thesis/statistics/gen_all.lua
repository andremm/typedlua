
local statistics = require "statistics"

local file = io.open("unpack.csv")
local unpack_dir = file:read("*l") .. "/"
file:close()

local merge = statistics.init_merge()

for i in io.lines("database.csv") do
  local project = string.gsub(string.gsub(i, unpack_dir, ""), "/.*", "")
  local result = statistics.generate(i)
  statistics.log_result(i, result)
  statistics.merge(result, merge, project)
end

statistics.log_merge(merge)

statistics.print_merge(merge)

os.exit(0)

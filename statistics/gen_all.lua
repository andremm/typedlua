
local statistics = require "statistics"

local merge = statistics.init_merge()

for i in io.lines("database.csv") do
  local result = statistics.generate(i)
  statistics.log_result(i, result)
  statistics.merge(result, merge)
end

statistics.log_merge(merge)

os.exit(0)

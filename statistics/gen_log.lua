
local statistics = require "statistics"

for i in io.lines("luac.csv") do
  local result = statistics.generate(i)
  statistics.log_result(i, result)
end

os.exit(0)

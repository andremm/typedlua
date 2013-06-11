
local statistics = require "statistics"

statistics.print_header()

for i in io.lines("luac.csv") do
  local result = statistics.generate(i)
  statistics.print_result(i, result)
end

os.exit(0)

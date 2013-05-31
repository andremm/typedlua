
local statistics = require "statistics"

for i in io.lines("luac.csv") do
  print(i)
  statistics.generate(i)
end

os.exit(0)

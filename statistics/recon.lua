local parser = require "parser"

for i in io.lines("luac.csv") do
  if parser.parse(i) then
    print(i .. ",OK")
  else
    print(i .. ",NOT")
  end
end

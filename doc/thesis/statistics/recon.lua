local parser = require "parser"

for i in io.lines("compile.csv") do
  if parser.parse_from_file(i) then
    print(i .. ",OK")
  else
    print(i .. ",NOT")
  end
end

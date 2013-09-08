package.path = "../?.lua;" .. package.path

local parser = require "typedlua.parser"

for i in io.lines("compile.csv") do
  if parser.parse_from_file(i) then
    print(i .. ",OK")
  else
    print(i .. ",NOT")
  end
end

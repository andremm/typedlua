local lfs = require "lfs"

local path, testfile = ...

testfile = testfile or "test.lua"

assert(path)

lfs.chdir(path)

local function gen_test(file)
  local out = { "-- " .. file, "", "s = [=[" }
  for line in io.lines(file) do
    out[#out+1] = line
  end
  out[#out+1] = "]=]"
  out[#out+1] = ""
  out[#out+1] = "e = [=["
  os.execute("tlc " .. file .. " > out.txt 2> out.txt")
  for line in io.lines("out.txt") do
    if line:match(file) then
      out[#out+1] = line:gsub(file, testfile)
    end
  end
  out[#out+1] = "]=]"
  out[#out+1] = ""
  out[#out+1] = "r = typecheck(s)"
  out[#out+1] = "check(e, r)"
  out[#out+1] = ""
  os.remove("out.txt")
  print(table.concat(out, "\n"))
end

for file in lfs.dir(".") do
  if lfs.attributes(file, "mode") == "file" and file:match("%.tl$") then
    gen_test(file)
  end
end

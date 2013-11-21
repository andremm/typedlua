local FIND = "find /Users/andre/tmp/luarocks_repo/unpack/ -name '*.lua'"
os.remove("total.csv")
os.remove("compile.csv")
for dotlua in io.popen(FIND):lines() do
  os.execute(string.format("echo %s >> total.csv", dotlua))
  local ret = os.execute(string.format("luac '%s' >> luac.log 2>&1", dotlua))
  if ret then
    os.execute(string.format("echo %s >> compile.csv", dotlua))
  end
end

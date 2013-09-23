local FIND = "find /Users/andre/tmp/luarocks_repo/unpack/ -name '*.lua'"
for dotlua in io.popen(FIND):lines() do
  local ret = os.execute(string.format("luac '%s' >> luac.log 2>&1", dotlua))
  if ret then
    print(dotlua)
  end
end

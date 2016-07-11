local input = io.open("test")

if input then
  local stuff = input:read("a")
  input:close()
end

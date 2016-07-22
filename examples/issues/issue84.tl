local input = io.open("test")

if input then
  local stuff = input:read("a")
  input:close()
  print(input + 10) -- error, input is file
end

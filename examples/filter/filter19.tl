local x: number | string | nil = 10
local y: number | string = 20

while type(x) == "number" do
  local function f()
    x = 10 -- error, assigning to filtered upvalue across loop
  end
  print(x + 10) -- error, x n|s|nil
  if type(y) == "string" then -- dead
    print(x + y)
    print(x .. "foo")
    print(y .. "bar")
  end
end

x = x + 10 -- error, x n|s|nil because of f

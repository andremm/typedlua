local x: number | string | nil = 10
local y: number | string = 20

while type(x) == "number" do
  print(x + 10) -- ok
  if type(y) == "string" then -- dead
    print(x + y)
    print(x .. "foo")
    print(y .. "bar")
  end
end

x = x + 10 -- ok

local x: number | string | nil = 10 -- x is actually integer
local y: number | string = 20 -- y is actually integer

while type(x) == "number" and type(y) ~= "number" do -- while block unreacheable
  print(x + 10)
  print(x + y)
  print(x .. "foo")
  print(y .. "bar")
  x = "foo"
  print(x + 10)
end

x = x + 10 -- ok

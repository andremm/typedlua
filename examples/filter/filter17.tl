local x: number | string | nil = 10
local y: number | string = 20

if x then
  while type(x) == "number" and type(y) ~= "number" do -- while body unreacheable
    print(x + 10)
    print(x + y)
    print(x .. "foo")
    print(y .. "bar")
    x = "foo"
    x = nil
    print(x + 10)
  end
  print(x .. "foo") -- error, x is integer
end
x = x + 10 -- ok

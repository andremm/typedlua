local x: number | string | nil = 10
local y: number | string = 20

while type(x) == "number" and type(y) ~= "number" do -- dead
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  print(x .. "foo") -- error, x: number
  print(y .. "bar") -- ok
end

x = x + 10 -- ok

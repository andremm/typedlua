local function foo(x: number|string|nil, y: number|string)

while type(x) == "number" and type(y) ~= "number" do
  print(x + 10) -- ok
  print(x + y) -- error, y is string
  print(x .. "foo") -- error, x is number
  print(y .. "bar") -- ok
  x = "foo" -- ok, x is now string
  print(x + 10) -- error, x is string
end

x = x + 10 -- error, x is number|string|nil

end

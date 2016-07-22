local function f(x: number | string | nil, y: number | string)

while type(x) == "number" do
  print(x + 10) -- ok
  if type(y) == "string" then
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
  end
end

x = x + 10 -- error, x: number|string|nil
end

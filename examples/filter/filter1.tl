local function foo(x: number|string|nil, y: number|string)

if type(x) == "number" and type(y) ~= "number" then
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  print(x .. "foo") -- error, x: number
  print(y .. "bar") -- ok
elseif x then
  print(x + 10) -- error, x: number|string
  if type(x) == "string" then
    print(x .. "foo") -- ok
  end
  print(x .. "foo") -- error, x: number|string
else
  print(x .. "foo") -- error, x: nil
  print(y + 10) -- error, y: number|string
end

x = x + 10 -- error, x: number|string|nil

end

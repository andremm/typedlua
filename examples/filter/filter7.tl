local function f(x: number | string | nil, y: number | string)

if type(x) == "number" and type(y) ~= "number" then
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  local function f()
    x = 10 -- ok, x is now n|s|nil
  end
  print(x .. "foo") -- error, x is n|s|nil
  print(y .. "bar") -- ok
elseif x then -- x cannot downcast anymore because of f
  print(x + 10) -- error, x: number|string|nil
  if type(x) == "string" then
    print(x .. "foo") -- error, x: number|string|nil
  end
  print(x .. "foo") -- error, x: number|string|nil
else
  print(x .. "foo") -- error, x: number|string|nil
  print(y + 10) -- error, y: number|string
end

x = x + 10 -- error, x: number|string|nil

end

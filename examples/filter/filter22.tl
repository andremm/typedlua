local x: number | string | nil = 10
local y: number | string = 20

local function f()
  x = 10
end

-- the previous function makes x unfilterable

if type(x) == "number" and type(y) ~= "number" then -- dead because of y
  print(x + 10)
  print(x + y)
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- error, x: number|string|nil
  if type(x) == "string" then
    print(x .. "foo") -- error, x: number|string|nil
  end
  print(x .. "foo") -- error, x: number|string|nil
else
  print(x .. "foo") -- error, x: number|string|nil
  print(y + 10) -- ok
end

x = x + 10 -- error, x: number|string|nil

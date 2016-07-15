local x: number | string | nil = 10
local y: number | string = 20

if type(x) == "number" and type(y) ~= "number" then
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  local function f()
    print(x + 10) -- error because x: number|string|nil here, but does not change x
  end
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

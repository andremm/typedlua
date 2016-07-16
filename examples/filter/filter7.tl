local x: number | string | nil = 10
local y: number | string = 20

if type(x) == "number" then --and type(y) ~= "number" then
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  local function f()
    x = 10 -- reverts x to original type, from now on x is unfilterable
  end
  print(x .. "foo") -- error, x: number|string|nil
  print(y .. "bar") -- ok
elseif x then
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

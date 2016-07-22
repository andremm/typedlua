local function f(x: number | string | nil, y: number | string)

local f = function () end

while type(x) == "number" do
  f()
  print(x + 10) -- ok
  f = function ()
    x = "foo" -- error, cannot revert across loop
  end
  if type(y) == "string" then
    print(x + y)  -- error, x n|s|nil
    print(x .. "foo") -- error, x n|s|nil
    print(y .. "bar") -- ok
  end
end

x = x + 10 -- error, x: number|string|nil
end

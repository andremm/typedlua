local x: number | string | nil = 10
local y: number | string = 20

if type(x) == "number" and type(y) ~= "number" then -- dead
  print(x + 10)
  print(x + y)
  local function f()
    print(x + 10)
  end
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- ok
  if type(x) == "string" then -- dead
    print(x .. "foo")
  end
  print(x .. "foo") -- error, x integer
else -- dead
  print(x .. "foo") -- error, x: nil
  print(y + 10) -- error, y: number|string
end

x = x + 10 -- ok

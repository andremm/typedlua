local x: number|string|nil = 10 -- x is actually integer
local y: number|string = 20 -- y is actually integer

if type(x) == "number" and type(y) ~= "number" then -- this arm is dead
  print(x + 10)
  print(x + y)
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- ok
  if type(x) == "string" then -- this arm is dead
    print(x .. "foo")
  end
  print(x .. "foo") -- error, x integer
else -- else block is dead
  print(x .. "foo")
  print(y + 10)
end

x = x + 10 -- ok

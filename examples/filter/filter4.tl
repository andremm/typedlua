local x: number | string | nil = 10
local y: number | string = 20

if x then
  while type(x) == "number" and type(y) ~= "number" do
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
    x = "foo" -- x reverts to number|string
    x = nil   -- error, cannot revert across loop
    print(x + 10) -- error, x:number|string
  end
end
x = x + 10 -- error, x: number|string|nil

local function f(x: number | string | nil, y: number | string)

if x then
  while type(x) == "number" and type(y) ~= "number" do
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
    local function g() print(x+10) end -- error, x string|number|nil
    x = "foo" -- x now is string
    print(x + 10) -- error, x string
    x = nil   -- error, x was string|number when entered loop
    print(x + 10) -- error, x string
  end
end
x = x + 10 -- error, x: number|string|nil

end

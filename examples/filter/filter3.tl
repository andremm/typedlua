local x:number? -- we know that x is actually nil
local y:string? -- ditto for y

if type(x) == "number" then -- dead
  x = x + 1
elseif type(y) == "string" then -- dead
  y = y .. "hello"
else
  x = x + 1 -- error, x nil
  y = y + 1 -- error, y nil
end

x = x + 1 -- error, x nil
y = y .. "hello" -- ditto

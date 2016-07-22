local function f(x:number?, y:string?)

if type(x) == "number" then
  x = x + 1
elseif type(y) == "string" then
  y = y .. "hello"
else
  x = x + 1 -- error, x nil
  y = y + 1 -- error, y nil
end

x = x + 1 -- error, x number?
y = y .. "hello" -- error, y string?

end

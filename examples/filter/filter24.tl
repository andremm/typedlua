local x: number|string|nil = 10

if math.random() > 10 then
  print(x+10) -- ok
elseif math.random() > 10 then
  error("is a number")
elseif type(x) == "number" then
    error("is a number")
end

x = x + 10 -- ok

if not x then -- dead
  print(x+10) -- error, x is nil
elseif type(x) == "number" then
  error("is a number")
end

-- dead code
x = x .. "foo"

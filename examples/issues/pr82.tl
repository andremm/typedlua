
local function factorial(n: integer): integer
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end
local x = 5

print(factorial(x))
print(factorial(x, nil))
print(factorial(x, 5))

print(factorial + 10) -- just to force an error

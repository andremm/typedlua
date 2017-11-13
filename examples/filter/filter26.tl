local function f(x: boolean?)
  if x then
    print(x + 10)
  else
    print(x .. "foo")
  end
end

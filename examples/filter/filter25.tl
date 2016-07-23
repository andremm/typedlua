local function f(t: { "foo": string }?)
  local x: string = t and t.foo or "bar"
  print(x + 10)
  local t = t or error("nil!")
  print(t.foo + 10)
end

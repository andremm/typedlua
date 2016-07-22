local function f(i: integer?) -- to force i to be integer?
  print(1 + (i or 0)) -- ok
  print(i .. "foo") -- error, i integer?
end

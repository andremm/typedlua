local f, err = io.open("wednesday")
do
  f = f or io.stdin -- breaks f out of the projection
  print(f + 10) -- f is a file
  print(err + 10) -- err is string?
  if type(err) == "string" then
    print(f + 10) -- f is still file
  end
end
print(f + 10) -- f is file
print(err + 10) -- err is string?
if f then
  print(err + 10) -- err is still string?
end

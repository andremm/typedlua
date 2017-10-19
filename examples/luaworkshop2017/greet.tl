local function greet (greeting:string?):string
  greeting = greeting or "Hello"
  return greeting .. " Lua Workshop 2017!"
end

print(greet("Good Afternoon"))
print(greet())
print(greet({}))

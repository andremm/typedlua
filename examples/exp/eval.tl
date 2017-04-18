local parser = require "parser"

local function eval (exp:Exp):number
  if exp.tag == "Num" then
    return exp[1]
   elseif exp.tag == "Add" then
     return eval(exp[1]) + eval(exp[2])
   elseif exp.tag == "Sub" then
     return eval(exp[1]) - eval(exp[2])
   elseif exp.tag == "Mul" then
     return eval(exp[1]) * eval(exp[2])
   elseif exp.tag == "Div" then
     return eval(exp[1]) / eval(exp[2])
  end
end

if #arg ~= 1 then
  print("Uso: eval.lua <exp>")
  os.exit(1)
end

local input:string = arg[1]

local ast, msg = parser.parse(input)
if ast then
  print(eval(ast))
  os.exit(0)
else
  print(msg)
  os.exit(1)
end

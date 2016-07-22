typealias Cmd = string|{string}

function f(cmd:Cmd) -- if I change to cmd:string|{string}, it works
   local c: {string} = {}
   if type(cmd) == "string" then
      table.insert(c, cmd)
      print(cmd + 10) -- error, cmd is string
   else
      c = cmd
      print(cmd + 10) -- error, cmd is array 
   end
end

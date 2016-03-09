

f = function (cmd) 
  local c = {}
  if type(cmd) == "string" then 
    table.insert(c,cmd)
  else 
    c = cmd
  end
end



interface IStackOverflow
   const my_method: (IStackOverflow) -> ()
end

function new(): IStackOverflow
   local obj: IStackOverflow = {}
   obj.my_method = my_method
   return obj
end

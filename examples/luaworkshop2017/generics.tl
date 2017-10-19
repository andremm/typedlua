class Stack<T>
  contents: {T}
  constructor new ()
    self.contents = {}
  end
  method push (x:T)
    self.contents[#self.contents + 1] = x
  end
  method pop ():T?
    local top = self.contents[#self.contents]
    self.contents[#self.contents] = nil
    return top
  end
end

local stack1 = Stack.new<string>()
local stack2 = Stack.new<integer>()
stack1:push("hello lua workshop")
stack2:push(2017)
print(tostring(stack1:pop()) .. " " .. tostring(stack2:pop()))

interface SubA
  subaname: string
end

local A = {}
A.aname = '<class A proto>'
A.child = {subaname='the subaname'}

function A:new(): self
  local t = {}
  t.aname = '<class A>'
  t.child = {subaname='the subaname'}

  local s = setmetatable(t, {__index = self})
  return s
end

function A:afoo(): self
  local s: string = self.aname
  print('Hello ' .. s)
  return self
end

function A:arun(name: string)
  print('arun', name)
end

function A:awrong(x: self) -- error, self cannot appear here!
end

function A:abar(obj: SubA)
  self:arun(obj.subaname) -- Does not compile cleanly.

  -- This works:
  local s = obj.subaname
  self:arun(s)

  -- This also works:
  self.arun(self, obj.subaname)
end

return A

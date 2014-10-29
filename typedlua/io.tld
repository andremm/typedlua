--[[
Typed Lua description file for input and output facilities
]]

userdata file
  close : (self) -> (boolean?, string?, number?)
  flush : (self) -> (boolean)
  lines : (self, string|number*) -> (() -> (string|number*))
  read : (self, string|number*) -> (string|number*)
  seek : (self, string?, number?) -> (number) | (nil, string, number)
  setvbuf : (self, string, number?) -> (boolean) | (nil, string, number)
  write : (self, string|number*) -> (self) | (nil, string, number)
end

stdin : file
stdout : file
stderr : file

close : (file?) -> (boolean?, string?, number?)
flush : () -> (boolean)
input : (string|file?) -> (file)
lines : (string*) -> (() -> (string|number*))
open : (string, string?) -> (file) | (nil, string, number)
output : (string|file?) -> (file)
popen : (string, string?) -> (file) | (nil, string, number)
read : (string|number*) -> (string|number*)
tmpfile : () -> (file)
type : (value) -> (string?)
write : (string|number*) -> (file) | (nil, string, number)

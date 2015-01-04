--[[
Typed Lua description file for input and output facilities
]]

userdata file
  close : (self) -> (boolean?, string?, number?)
  flush : (self) -> (boolean)
  lines : (self, string|number*) -> (() -> (string|number*))
  read : (self, string|number*) -> (string|number*)
  seek : (self, string?, integer?) -> (integer) | (nil, string, integer)
  setvbuf : (self, string, integer?) -> (boolean) | (nil, string, integer)
  write : (self, string|number*) -> (self) | (nil, string, integer)
end

stdin : file
stdout : file
stderr : file

close : (file?) -> (boolean?, string?, integer?)
flush : () -> (boolean)
input : (string|file?) -> (file)
lines : (string*) -> (() -> (string|number*))
open : (string, string?) -> (file) | (nil, string, integer)
output : (string|file?) -> (file)
popen : (string, string?) -> (file) | (nil, string, integer)
read : (string|number*) -> (string|number*)
tmpfile : () -> (file)
type : (value) -> (string?)
write : (string|number*) -> (file) | (nil, string, integer)

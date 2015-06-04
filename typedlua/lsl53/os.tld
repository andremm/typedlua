--[[
Typed Lua description file for operating system facilities
]]

clock : () -> (number)
date : (string?, integer?) -> (string) | (nil) |
                             ({"year":integer, "month":integer, "day":integer,
                               "hour":integer, "min":integer, "sec":integer,
                               "wday":integer, "yday":integer, "isdst":boolean})
difftime : (integer, integer) -> (number)
execute : (string?) -> (boolean?, string?, integer?)
exit : (boolean|integer?, value) -> ()
getenv : (string) -> (string?)
remove : (string) -> (boolean) | (nil, string, integer)
rename : (string, string) -> (boolean) | (nil, string, integer)
setlocale : (string?, string?) -> (string?)
time : ({"year":integer, "month":integer, "day":integer,
         "hour":integer?, "min":integer?, "sec":integer?,
         "wday":integer?, "yday":integer?, "isdst":boolean?}?) -> (integer?)
tmpname : () -> (string)

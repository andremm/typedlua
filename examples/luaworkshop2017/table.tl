local array:{string} =
  { "typedlua", "at", "lua", "workshop" }

local map:{string:integer} =
  { moscow = 2014, san_francisco = 2017 }

local record:{"d":integer, "m":integer, "y":integer} =
  { d = 10, m = 6, y = 2021 }

local record_with_array:{"z":integer, string} =
  { z = 2049, "K", "Joe" }

local va:string? = array[2]
local vm:integer? = map.san_francisco
local vr:integer = record.y

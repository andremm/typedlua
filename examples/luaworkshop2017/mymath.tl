local mymath = {}

local RADIANS_PER_DEGREE = 3.14 / 180.0

function mymath.deg (r:number):number
  return r / RADIANS_PER_DEGREE
end

function mymath.rad (d:number):number
  return d * RADIANS_PER_DEGREE
end

mymath.pow = function (x:number, y:number):number
  return x ^ y
end

return mymath

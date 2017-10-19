local is_even, is_odd

function is_even (n: integer):boolean
  if (n == 0) then
    return true
  else
    return is_odd(n - 1)
  end
end

function is_odd (n: integer):boolean
  if (n == 0) then
    return false
  else
    return is_even(n - 1)
  end
end

print(is_even(8))
print(is_odd(8))

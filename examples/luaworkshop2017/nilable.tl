function sum_list (xs:{integer}):integer
  local sum = 0
  for i = 1, #xs do
      sum = sum + xs[i]
  end
  return sum
end

print(sum_list({[1] = 1, [2] = 2, [4] = 3}))

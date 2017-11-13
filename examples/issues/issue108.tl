local function w (filename:string, data:string):boolean
  local scratch = filename .. "~"
  local fh, open_err = io.open(scratch, "w")
  if not fh then return false end
  local write_ok, write_err = fh:write(data)
  if not write_ok then return false end
  local close_ok, close_err = fh:close()
  if not close_ok then return false end
  return true
end

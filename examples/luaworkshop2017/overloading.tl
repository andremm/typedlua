local function get_upload_server (server:string|{"upload_server":string?}):(string,string)|(nil,string)
  if type(server) == "string" then
    return server, "specific"
  else
    local server = server.upload_server
    if server then
      return server, "default"
    else
      return nil, "no upload server set"
    end
  end
end

local server, mode_or_error = get_upload_server({})
if not server then
  print("ERROR: " .. mode_or_error)
else
  print("using " .. mode_or_error .. " server " .. server)
end

local function get_upload_server (cfg:{"upload_server":string?}, server:string?):(string)|(nil,string)
  server = server or cfg.upload_server
  if not server then
    return nil, "No server specified and no default configured with upload_server."
  else
    return server
  end
end

local server, error = get_upload_server({})
if server then
  print("SERVER: " .. server)
else
  print("ERROR: " .. error)
end


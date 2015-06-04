--[[
local md5sum = nil

do -- select MD5 library

  local ok,mod = pcall(require,"crypto")
  if ok then
    local digest = (mod.evp or mod).digest
    if digest then
      md5sum = function(str) return digest("md5",str) end
    end
  end

  if not md5sum then
    ok,mod = pcall(require,"md5")
    if ok then md5sum = mod.sumhexa or mod.digest end
  end

  if not md5sum then
    ok,mod = pcall(require,"digest") -- last because using globals
    if ok and md5 then md5sum = md5.digest end
  end

end

assert(md5sum,"cannot find supported md5 module")
]]
local md5 = require "examples.lmd5.md5"
local md5sum = md5.digest
local s_http = require "examples.luasocket.socket.http"
local s_url = require "examples.luasocket.socket.url"
local ltn12 = require "examples.luasocket.ltn12"

local hash = function(...:number|string)
  return md5sum(table.concat({...},":"))
end

local parse_header = function(h:string)
  local r:{string:string} = {}
  for k,v in (h .. ','):gmatch("(%w+)=(.-),") do
    if v:sub(1,1) == '"' then -- strip quotes
      r[k:lower()] = v:sub(2,-2)
    else r[k:lower()] = v end
  end
  return r
end

local make_digest_header = function(t:{{1:string, 2:string?, "unquote":boolean?}})
  local s:{string|number} = {}
  for i=1,#t do
    local x:{1:string, 2:string?, "unquote":boolean?}? = t[i]
    if x then
      local u = x.unquote
      local x1 = x[1]
      local x2 = x[2] or ""
      if u then
        s[i] = x1 .. '=' .. x2
      else
        s[i] = x1 .. '="' .. x2 .. '"'
      end
    end
  end
  return "Digest " .. table.concat(s,', ')
end

local hcopy = function(t:url_request):url_request
  local r = {}
  --for k,v in pairs(t) do r[k] = v end
  r.url = t.url
  r.sink = t.sink
  r.method = t.method
  r.headers = t.headers
  r.source = t.source
  r.step = t.step
  r.proxy = t.proxy
  r.redirect = t.redirect
  r.create = t.create
  return r
end

local _request = function(t:url_request)
  if not t.url then error("missing URL") end
  local url = s_url.parse(t.url)
  local user:string?,password:string?
  if url then
    user,password = url.user,url.password
    if not (user and password) then
      error("missing credentials in URL")
    end
    url.user,url.password,url.authority,url.userinfo = nil,nil,nil,nil
    t.url = s_url.build(url)
    local ghost_source:nil|() -> (string?)
    local source = t.source
    if source then
      local ghost_chunks:{string} = {}
      local ghost_capture = function(x:string?)
        if x then ghost_chunks[#ghost_chunks+1] = x end
        return x
      end
      local ghost_i = 0
      ghost_source = function()
        ghost_i = ghost_i+1
        return ghost_chunks[ghost_i]
      end
      t.source = ltn12.source.chain(source,ghost_capture)
    end
    local b:number|string?,c:number|string?,h:{string:string}? = s_http.request(t)
    local h1:{string:string} = {}
    h = h or h1
    if (c == 401) and h["www-authenticate"] then
      local ht = parse_header(h["www-authenticate"] or "")
      assert(ht.realm and ht.nonce and ht.opaque)
      if ht.qop ~= "auth" then
        return nil, string.format("unsupported qop (%s)",tostring(ht.qop))
      end
      local algo = ht.algorithm
      if algo then
        if algo:lower() ~= "md5" then
          return nil, string.format("unsupported algo (%s)",algo)
        end
      end
      local nc,cnonce = "00000001",string.format("%08x",os.time())
      local uri = s_url.build{path = url.path,query = url.query}
      local method = t.method or "GET"
      local response = hash(
        hash(user,ht.realm,password),
        ht.nonce,
        nc,
        cnonce,
        "auth",
        hash(method,uri)
      )
      t.headers = t.headers or {}
      --t.headers.authorization = make_digest_header{
      local auth = make_digest_header{
        {"username", user},
        {"realm", ht.realm},
        {"nonce", ht.nonce},
        {"uri", uri},
        {"cnonce", cnonce},
        {"nc", nc, unquote=true},
        {"qop", "auth"},
        {"algorithm", "MD5"},
        {"response", response},
        {"opaque", ht.opaque},
      }
      if t.source then t.source = ghost_source end
      local b:number|string?,c:number|string?,h:{string:string}? = s_http.request(t)
      return b,c,h
    else return b,c,h end
  end
end

local request = function(x:string|url_request)
  if type(x) == "string" then
    local r = {}
    local _,c,h = _request{url = x,sink = ltn12.sink.table(r)}
    return table.concat(r),c,h
  else
    return _request(hcopy(x))
  end
  error(string.format("unexpected type %s",type(x)))
end

return {
  request = request,
}

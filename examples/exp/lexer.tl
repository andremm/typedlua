local lexer = {}

typealias Token = { "tipo":string, "lexama":number? }

function lexer.scan (input:string):({Token})?
  local i:integer = 1
  local l:{Token} = {}
  while i <= #input do
    local t:Token?
    local b:integer = string.byte(input, i) or 0
    local c:string = string.char(b)
    if c == ' ' or
       c == '\t' or
       c == '\n' then
       -- pula brancos
       b = string.byte(input, i + 1) or 0
       local e:string = string.char(b)
       while e == ' ' or e  == '\t' or e == '\n' do
         i = i + 1
         b = string.byte(input, i + 1) or 0
         e = string.char(b)
       end
    elseif c >= '0' and c <= '9' then
      local n:string = c
      b = string.byte(input, i + 1) or 0
      local d:string = string.char(b)
      while d >= '0' and d <= '9' do
        n = n .. d
        i = i + 1
        b = string.byte(input, i + 1) or 0
        d = string.char(b)
      end
      t = { tipo = "Num", lexema = tonumber(n) }
    elseif c == '+' then
      t = { tipo = "+" }
    elseif c == '-' then
      t = { tipo = "-" }
    elseif c == '*' then
      t = { tipo = "*" }
    elseif c == '/' then
      t = { tipo = "/" }
    elseif c == '(' then
      t = { tipo = "(" }
    elseif c == ')' then
      t = { tipo = ")" }
    else
      return nil, string.format("token invalido: '%s'", c)
    end
    if t then table.insert(l, t) end
    i = i + 1
  end
  if #l == 0 then
    return nil, "token invalido: string vazia"
  else
    return l
  end
end

function lexer.scanner (input:string):(() -> (Token))?
  local scanner, msg = lexer.scan(input)
  if scanner then
    local i:integer = 1
    return function ():Token -- get_token
      local t:Token = { tipo = "EOF" }
      if i <= #scanner then
        t = scanner[i]
        i = i + 1
      end
      return t
    end
  else
    return nil, msg
  end
end

return lexer

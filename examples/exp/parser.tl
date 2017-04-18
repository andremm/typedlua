local parser = {}

local lexer = require "lexer"

typealias Exp = { "tag":"Num", 1:number }
              | { "tag":"Add", 1:number, 2:number }
              | { "tag":"Sub", 1:number, 2:number }
              | { "tag":"Mul", 1:number, 2:number }
              | { "tag":"Div", 1:number, 2:number }

local token:Token, get_token:() -> (Token)

local exp, termo, fator

local function le (t:string):(Token)?
  if token.tipo == t then
    local tk:Token = token
    token = get_token()
    return tk
  else
    return nil, string.format("expressao invalida: li '%s', mas esperava '%s'", token.tipo, t)
  end
end

function exp ():(Exp)?
  local exp1, msg = termo()
  if exp1 then
    while token.tipo == '+' or
          token.tipo == '-' do
      if token.tipo == '+' then
        local op, msg = le('+')
        if op then
          local exp2, msg = termo()
          if exp2 then
            exp1 = { tag = "Add", [1] = exp1, [2] = exp2 }
          else
            return nil, msg
          end
        else
          return nil, msg
        end
      else
        local op, msg = le('-')
        if op then
          local exp2, msg = termo()
          if exp2 then
          exp1 = { tag = "Sub", [1] = exp1, [2] = exp2 }
          else
            return nil, msg
          end
        else
          return nil, msg
        end
      end
    end
    return exp1
  else
    return nil, msg
  end
end

function termo ():(Exp)?
  local exp1, msg = fator()
  if exp1 then
    while token.tipo == '*' or
          token.tipo == '/' do
      if token.tipo == '*' then
        local op, msg = le('*')
        if op then
          local exp2, msg = fator()
          if exp2 then
            exp1 = { tag = "Mul", [1] = exp1, [2] = exp2 }
          else
            return nil, msg
          end
        else
          return nil, msg
        end
      else
        local op, msg = le('/')
        if op then
          local exp2, msg = fator()
          if exp2 then
          exp1 = { tag = "Div", [1] = exp1, [2] = exp2 }
          else
            return nil, msg
          end
        else
          return nil, msg
        end
      end
    end
    return exp1
  else
    return nil, msg
  end
end

function fator ():(Exp)?
  if token.tipo == 'Num' then
    local t, msg = le('Num')
    if t then
      return { tag = "Num", [1] = t.lexema }
    else
      return nil, msg
    end
  elseif token.tipo == '(' then
    local t, msg = le('(')
    if t then
      local exp1, msg = exp()
      if exp1 then
        local t, msg = le(')')
        if t then
          return exp1
        else
          return nil, msg
        end
      else
        return nil, msg
      end
    else
      return nil, msg
    end
  end
end

function parser.parse (input:string)
  local scanner, msg = lexer.scanner(input)
  if scanner then
    get_token = scanner
    token = get_token()
    local ast, msg = exp()
    if ast then
      local t, msg = le('EOF')
      if t then
        return ast
      else
        return nil, msg
      end
    else
      return nil, msg
    end
  else
    return nil, msg
  end
end

return parser

local tltype = require "typedlua.tltype"

local tlfilter = {}

--
-- Type filters
--
-- A type filter is a function (Type) -> (Type, Type)
--   the first type is the resulting of filtering "in", the second filtering "out"
--
-- There are filters that do nothing (filter_pass), filter only nil (filter_nil),
-- filter nil or false (filter_falsy), filter numbers (filter_number),
-- filter strings (filter_string), and filter tables (filter_table)
--
-- There are filter "constructors" for dynamic type tags (filter_tag),
-- literal fields (filter_field), literal fields and values (filter_fieldliteral),
-- and boolean operations on filters (filter_and, filter_or, filter_not).
-- The result of these constructors is always another filter
--

function tlfilter.filter_pass(t)
  return t, t
end

function tlfilter.filter_nil(t)
  if tltype.isValue(t) or tltype.isAny(t) then
    return tltype.Nil(), t -- there is no top type for functions besides value/any
  elseif tltype.isNil(t) then
    return t, tltype.Void()
  elseif tltype.isUnion(t) then
    local l, hasnil = {}, false
    for _, tt in ipairs(t) do
      if tltype.isNil(tt) then
        hasnil = true
      else
        l[#l+1] = tt
      end
    end
    if hasnil then
      return tltype.Nil(), tltype.Union(table.unpack(l))
    else
      return tltype.Void(), tltype.Union(table.unpack(l))
    end
  elseif tltype.isGlobalVariable(t) then
    return tltype.filter_nil(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_falsy(t)
  if tltype.isValue(t) or tltype.isAny(t) then
    return tltype.Union(tltype.Nil(), tltype.False()), t -- there is no top type for functions besides value/any
  elseif tltype.isNil(t) or tltype.isFalse(t) then
    return t, tltype.Void()
  elseif tltype.isBoolean(t) then
    return tltype.False(), tltype.True()
  elseif tltype.isUnion(t) then
    local l, lf = {}, {}
    for _, tt in ipairs(t) do
      if tltype.isNil(tt) or tltype.isFalse(tt) then
        lf[#lf+1] = tt
      elseif tltype.isBoolean(tt) then
        lf[#lf+1] = tltype.isFalse()
        l[#l+1] = tltype.isTrue()
      else
        l[#l+1] = tt
      end
    end
    return tltype.Union(table.unpack(lf)), tltype.Union(table.unpack(l))
  elseif tltype.isGlobalVariable(t) then
    return tltype.filter_falsy(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_number(t)
  if tltype.isValue(t) or tltype.isAny(t) then
    return tltype.Number(), t
  elseif tltype.isNum(t) or tltype.isNumber(t) or tltype.isInteger(t) then
    return t, tltype.Void()
  elseif tltype.isUnion(t) then
    local l, tn = {}, nil
    for _, tt in ipairs(t) do
      if tltype.isNumber(tt) then
        tn = tt
      elseif tltype.isNum(tt) or tltype.isInteger(tt) then
        tn = tn or tt
      else
        l[#l+1] = tt
      end
    end
    tn = tn or tltype.Void()
    return tn, tltype.Union(table.unpack(l))
  elseif tltype.isGlobalVariable(t) then
    return tltype.filter_number(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_integer(t)
  if tltype.isValue(t) or tltype.isAny(t) or tltype.isNumber(t) then
    return tltype.Integer(), t
  elseif (tltype.isNum(t) and math.type(t[1]) == "integer") or tltype.isInteger(t) then
    return t, tltype.Void()
  elseif tltype.isUnion(t) then
    local l, tn = {}, nil
    for _, tt in ipairs(t) do
      if tltype.isNumber(tt) then
        tn = tt
      elseif tltype.isNum(tt) or tltype.isInteger(tt) then
        tn = tn or tt
      else
        l[#l+1] = tt
      end
    end
    tn = tn or tltype.Void()
    return tn, tltype.Union(table.unpack(l))
  elseif tltype.isGlobalVariable(t) then
    return tltype.filter_integer(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_string(t)
  if tltype.isValue(t) or tltype.isAny(t) then
    return tltype.String(), t
  elseif tltype.isStr(t) or tltype.isString(t) then
    return t, tltype.Void()
  elseif tltype.isUnion(t) then
    local l, ts = {}, nil
    for _, tt in ipairs(t) do
      if tltype.isString(tt) then
        ts = tt
      elseif tltype.isStr(tt) then
        ts = ts or tt
      else
        l[#l+1] = tt
      end
    end
    ts = ts or tltype.Void()
    return ts, tltype.Union(table.unpack(l))
  elseif tltype.isGlobalVariable(t) then
    return tltype.filter_string(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_boolean(t)
  if tltype.isValue(t) or tltype.isAny(t) then
    return tltype.Boolean(), t
  elseif tltype.isTrue(t) or tltype.isFalse(t) or tltype.isBoolean(t) then
    return t, tltype.Void()
  elseif tltype.isUnion(t) then
    local l, tb = {}, nil
    for _, tt in ipairs(t) do
      if tltype.isBoolean(tt) then
        tb = tt
      elseif tltype.isTrue(tt) then
        if tb and tltype.isFalse(tb) then
          tb = tltype.Boolean()
        end
        tb = tb or tt
      elseif tltype.isFalse(tt) then
        if tb and tltype.isTrue(tb) then
          tb = tltype.Boolean()
        end
        tb = tb or tt
      else
        l[#l+1] = tt
      end
    end
    tb = tb or tltype.Void()
    return tb, tltype.Union(table.unpack(l))
  elseif tltype.isGlobalVariable(t) then
    return tltype.filter_boolean(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_table(t)
  if tltype.isValue(t) or tltype.isAny(t) then
    return tltype.Table(), t
  elseif tltype.isTable(t) then
    return t, tltype.Void()
  elseif tltype.isUnion(t) then
    local l, lt = {}, {}
    for _, tt in ipairs(t) do
      if tltype.isTable(tt) then
        lt[#lt+1] = tt
      else
        l[#l+1] = tt
      end
    end
    return tltype.Union(table.unpack(lt)), tltype.Union(table.unpack(l))
  elseif tltype.isRecursive(t) or tltype.isGlobalVariable(t) then
    return tltype.filter_table(tltype.unfold(t))
  else
    return tltype.Void(), t
  end
end

function tlfilter.filter_tag(tag)
  if tag == "number" then
    return tlfilter.filter_number
  elseif tag == "string" then
    return tlfilter.filter_string
  elseif tag == "nil" then
    return tlfilter.filter_nil
  elseif tag == "boolean" then
    return tlfilter.filter_boolean
  elseif tag == "table" then
    return tlfilter.filter_table
  else
    return tlfilter.filter_pass
  end
end

function tlfilter.filter_field(name)
  local function filter (t)
    if tltype.isValue(t) or tltype.isAny(t) then
      return t, t -- might lose the field at any time, so cannot say anything
    elseif tltype.isTable(t) then
      for _, field in ipairs(t) do
        if tltype.isLiteral(field[1]) and field[1][1] == name then -- definitely has the field
          return t, tltype.Void()
        end
      end
      if tltype.isNil(tltype.getField(tltype.Literal(name), t)) then -- definitely does not have the field
        return tltype.Void(), t
      else -- may have the field, may have not, or might lose it at any time
        return t, t
      end
    elseif tltype.isUnion(t) then
      local lt, lf = {}, {}
      for _, tt in ipairs(t) do
        local ttt, ttf = filter(tt)
        if not tltype.isVoid(ttt) then lt[#lt+1] = ttt end
        if not tltype.isVoid(ttf) then lf[#lf+1] = ttf end
      end
      return tltype.Union(table.unpack(lt)), tltype.Union(table.unpack(lf))
    elseif tltype.isRecursive(t) or tltype.isGlobalVariable(t) then
      return filter(tltype.unfold(t))
    else
      return tltype.Void(), t
    end
  end
  return filter
end

function tlfilter.filter_fieldliteral(name, littype)
  local function filter (t)
    if tltype.isValue(t) or tltype.isAny(t) then
      return t, t
    elseif tltype.isTable(t) then
      for _, field in ipairs(t) do
        if (not field.const) and tltype.isLiteral(field[1]) and field[1][1] == name and
           tltype.isLiteral(field[2]) and field[2][1] == littype[1] then -- definitely has the field
          return t, tltype.Void()
        end
      end
      if tltype.isNil(tltype.getField(tltype.Literal(name), t)) then -- definitely does not have the field
        return tltype.Void(), t
      else -- may have the field, may have not, or might lose it
        return t, t
      end
    elseif tltype.isUnion(t) then
      local lt, lf = {}, {}
      for _, tt in ipairs(t) do
        local ttt, ttf = filter(tt)
        if not tltype.isVoid(ttt) then lt[#lt+1] = ttt end
        if not tltype.isVoid(ttf) then lf[#lf+1] = ttf end
      end
      return tltype.Union(table.unpack(lt)), tltype.Union(table.unpack(lf))
    elseif tltype.isRecursive(t) or tltype.isGlobalVariable(t) then
      return filter(tltype.unfold(t))
    else
      return tltype.Void(), t
    end
  end
  return filter
end

function tlfilter.filter_not(f)
  return function (t)
    local tt, tf = f(t)
    return tf, tt
  end
end

function tlfilter.filter_and(f1, f2)
  return function (t)
    local tt1, tf1 = f1(t)
    local tt2, tf2 = f2(t)
    local ttt, _ = f2(tt1)
    return ttt, tltype.Union(tf1, tf2)
  end
end

function tlfilter.filter_or(f1, f2)
  return function (t)
    local tt1, tf1 = f1(t)
    local tt2, tf2 = f2(t)
    local _, tff = f2(tf1)
    return tltype.Union(tt1, tt2), tff
  end
end

tlfilter.filter_truthy = tlfilter.filter_not(tlfilter.filter_falsy)

--
-- Filter sets
--
-- A filter set is a set of names (strings) to filters, and
-- denotes the filters that might be applied to a set of local
-- variables at the same time
--
-- Again, there are constructors for boolean operations between
-- filter sets
--

function tlfilter.set_single(name, filter)
  return { [name] = filter }
end

function tlfilter.set_not(set)
  local nset = {}
  for n, f in pairs(set or {}) do
    nset[n] = tlfilter.filter_not(f)
  end
  return nset
end

function tlfilter.set_and(set1, set2)
  local nset = {}
  set1, set2 = set1 or {}, set2 or {}
  for n, f in pairs(set1) do
    if set2[n] then
      nset[n] = tlfilter.filter_and(f, set2[n])
    else
      nset[n] = function (t)
        local tt = f(t)
        return tt, t
      end
    end
  end
  for n, f in pairs(set2) do
    if not set1[n] then
      nset[n] = function (t)
        local tt = f(t)
        return tt, t
      end
    end
  end
  return nset
end

function tlfilter.set_or(set1, set2)
  local nset = {}
  set1, set2 = set1 or {}, set2 or {}
  for n, f in pairs(set1) do
    if set2[n] then
      nset[n] = tlfilter.filter_or(f, set2[n])
    else
      nset[n] = function (t)
        local _, tf = f(t)
        return t, tf
      end
    end
  end
  for n, f in pairs(set2) do
    if not set1[n] then
      nset[n] = function (t)
        local _, tf = f(t)
        return t, tf
      end
    end
  end
  return nset
end

return tlfilter

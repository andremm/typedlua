
local terms = {
-- A
-- B
  "bottom type",
-- C
  "closed table type",
  "coercion",
  "consistency",
  "consistent-subtyping",
  "contravariant",
  "covariant",
-- D
  "depth subtyping",
  "dynamic type",
-- E
-- F
  "filter type",
  "fixed table type",
  "flow typing",
  "free assigned variable",
-- G
  "gradual type system",
  "gradual typing",
-- H
-- I
  "invariant",
-- J
-- K
-- L
-- M
  "metatable",
-- N
  "nominal type system",
-- O
  "open table type",
  "optional type system",
-- P
  "projection environment",
  "projection type",
  "prototype object",
-- Q
-- R
-- S
  "self-like delegation",
  "sound type system",
  "structural type system",
-- T
  "table refinement",
  "top type",
  "type environment",
  "type tag",
-- U
  "unique table type",
  "unsound type system",
  "userdata",
-- V
  "vararg expression",
  "variadic function",
  "variance",
-- W
  "width subtyping",
-- X
-- Y
-- Z
}

local def = {
}

table.sort(terms)

io.write("\\begin{description}\n")
for k, v in ipairs(terms) do
  io.write(string.format("\\item[%s] %s\n", v, def[v] or "TO DO."))
end
io.write("\\end{description}\n")

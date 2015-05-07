
local terms = {
-- A
  "algebraic data type",
  "associative array",
  "assertion",
  "axiom",
-- B
  "bottom type",
-- C
  "coercion",
  "consistency",
  "consistent-subtyping",
  "contract",
  "contravariant",
  "covariant",
-- D
  "deduction system",
  "depth subtyping",
  "downcast",
  "dynamic type",
  "dynamic typing",
  "dynamically typed language",
-- E
  "effect system",
-- F
  "flow typing",
  "free assigned variable",
-- G
  "gradual type system",
  "gradual typing",
  "global type inference",
-- H
  "higher-order function",
-- I
  "immutable field",
  "invariant",
-- J
-- K
-- L
  "local type inference",
-- M
  "metaprogramming",
  "metatable",
  "mutable field",
-- N
  "nominal type system",
  "nilable",
-- O
  "optional type system",
  "optional typing",
-- P
  "pattern matching",
  "prototype object",
-- Q
-- R
  "receiver",
  "referenced variable",
  "run-time check",
-- S
  "self-like delegation",
  "sound type system",
  "static analysis",
  "static typing",
  "statically typed language",
  "structural type system",
  "subtype",
  "subtyping",
  "supertype",
-- T
  "table constructor",
  "table refinement",
  "top type",
  "type alias",
  "type annotation",
  "type cast",
  "type checker",
  "type checking",
  "type environment",
  "type error",
  "type inference",
  "type safety",
  "type system",
  "type tag",
  "typing relation",
-- U
  "unsound type system",
  "upcast",
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

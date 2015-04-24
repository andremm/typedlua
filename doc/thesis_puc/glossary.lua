
local terms = {
-- A
  "associate array",
  "application language",
  "arity mismatch",
  "assertion",
  "axiom",
-- B
  "blame annotation",
  "blame calculus",
  "bottom type",
  "brand",
  "bug",
-- C
  "coercion",
  "consistency",
  "consistent-subtyping",
  "contract",
  "contravariant",
  "covariant",
-- D
  "dependent type",
  "deployment",
  "depth subtyping",
  "downcast",
  "dynamic type",
  "dynamic typing",
  "dynamically typed language",
-- E
-- F
  "first-class function",
  "first-level type",
  "flow typing",
-- G
  "gradual type system",
  "gradual typing",
  "global type inference",
-- H
  "higher-order function",
  "hybrid type checking",
-- I
  "immutable field",
  "imperative language",
  "inheritance",
-- J
-- K
-- L
  "lambda calculus",
  "lexical scope",
  "local type inference",
-- M
  "metaprogramming",
  "metatable",
  "mutable field",
-- N
  "nominal type system",
-- O
  "objetc-oriented language",
  "operator overloading",
  "optional type system",
  "optional typing",
  "overloading",
-- P
  "pattern matching",
  "polymorphism",
  "projection type",
  "prototype object",
-- Q
  "quasi-static type system",
-- R
  "receiver",
  "refinement type",
  "run-time check",
-- S
  "script",
  "scripting language",
  "second-level type",
  "self",
  "self-like delegation",
  "simpy typed lambda calculus",
  "single inheritance",
  "soft typing",
  "sound",
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
  "type",
  "type alias",
  "type annotation",
  "type cast",
  "type checker",
  "type checking",
  "type error",
  "type inference",
  "type refinement",
  "type safety",
  "type system",
  "type tag",
  "typed lambda calculus",
-- U
  "unsound",
  "untyped lambda calculus",
  "upcast",
  "userdata",
-- V
  "vararg expression",
  "variadic function",
-- W
  "width subtyping",
-- X
-- Y
-- Z
}

local def = {
}

table.sort(terms)

io.write("Should we include Strongtalk, Dart, TypeScript, etc?\n")
io.write("\\begin{description}\n")
for k, v in ipairs(terms) do
  io.write(string.format("\\item[%s] %s\n", v, def[v] or "TODO."))
end
io.write("\\end{description}\n")

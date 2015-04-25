
local terms = {
-- A
  "abstract syntax",
  "associative array",
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
  "concrete syntax",
  "consistency",
  "consistent-subtyping",
  "contract",
  "contravariant",
  "covariant",
-- D
  "deduction system",
  "dependent type",
  "deployment",
  "depth subtyping",
  "downcast",
  "dynamic type",
  "dynamic typing",
  "dynamically typed language",
-- E
  "effect system",
-- F
  "first-class function",
  "first-level type",
  "flow typing",
  "format string",
  "free assigned variable",
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
  "invariant",
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
  "nilable",
-- O
  "objetc-oriented language",
  "occurence typing",
  "operator overloading",
  "optional type system",
  "optional typing",
  "overloading",
-- P
  "parametric polymorphism",
  "partial auxiliary function",
  "pattern matching",
  "polymorphism",
  "projection environment",
  "projection type",
  "prototype object",
-- Q
  "quasi-static type system",
-- R
  "receiver",
  "referenced variable",
  "refinement type",
  "reflection",
  "run-time check",
-- S
  "scope",
  "script",
  "scripting language",
  "second-level type",
  "self",
  "self-like delegation",
  "simpy typed lambda calculus",
  "single inheritance",
  "soft typing",
  "sound",
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
  "type",
  "type alias",
  "type annotation",
  "type cast",
  "type checker",
  "type checking",
  "type environment",
  "type error",
  "type inference",
  "type parameter",
  "type predicates",
  "type refinement",
  "type safety",
  "type system",
  "type tag",
  "typed lambda calculus",
  "typing relation",
-- U
  "unsound",
  "untyped lambda calculus",
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

io.write("Should we include Strongtalk, Dart, TypeScript, etc?\n")
io.write("\\begin{description}\n")
for k, v in ipairs(terms) do
  io.write(string.format("\\item[%s] %s\n", v, def[v] or "TO DO."))
end
io.write("\\end{description}\n")

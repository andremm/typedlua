
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
-- A
-- B
  ["bottom type"] = [[
It is a type that is subtype of all types.
]],
-- C
  ["closed table type"] = [[
It is a table type that does not allow adding fields.
]],
  ["coercion"] = [[
It is a relation that allows converting values from one type to values
of another type without error.
]],
  ["consistency"] = [[
It is a relation that gradual typing uses to control the insertion of
run-time checks between dynamically typed and statically typed code.
]],
  ["consistent-subtyping"] = [[
It is a relation that combines consistency and subtyping.
]],
  ["contravariant"] = [[
Subtyping is contravariant when it reverses the subtyping order,
that is, it orders types from a more generic type to a more specific type.
]],
  ["covariant"] = [[
Subtyping is covariant when it preserves the subtyping order,
that is, it orders types from a more specific type to a more general type.
]],
-- D
  ["depth subtyping"] = [[
It allows changing the type of individual fields of a table type.
]],
  ["dynamic type"] = [[
It is a type that allows combining dynamic and static typing in the same code.
It is neither the bottom nor the top type in the subtyping relation, but
a subtype only of itself.
Gradual typing uses the dynamic type along with the consistency relation to
identify where run-time casts should be inserted to prevent that dynamically
typed code violates statically typed code.
]],
-- E
-- F
  ["filter type"] = [[
It is a type that allows Typed Lua to discriminate the type of local variables
inside control flow statements.
]],
  ["fixed table type"] = [[
It is a table type that does not allow adding fields. It also does not allow
width subtyping to make single inheritance safe.
]],
  ["flow typing"] = [[
It is a combination of static typing and flow analysis.
]],
  ["free assigned variable"] = [[
It is a free variable that appears in an assignment.
]],
-- G
  ["gradual type system"] = [[
It is a type system that uses the consistency relation to check the interaction
among the dynamic type and other types.
]],
  ["gradual typing"] = [[
It is an approach that uses a gradual type system to allow static and dynamic
typing in the same code, but inserting run-time checks between statistically
typed and dynamically typed code.
]],
-- H
-- I
  ["invariant"] = [[
Subtyping is invariant when it does not allow ordering types, that is,
it is a way that express type equality through subtyping.
]],
-- J
-- K
-- L
-- M
  ["metatable"] = [[
It is a Lua table that allows changing its behavior.
]],
-- N
  ["nominal type system"] = [[
It is a type system that uses name declarations to define the equivalence among types.
]],
-- O
  ["open table type"] = [[
It is a table type that allows adding fields, and usually refers to table types that
have aliases.
]],
  ["optional type system"] = [[
It is a type system that allows combining static and dynamic typing in the same
language, but without affecting the run-time semantics.
]],
-- P
  ["projection environment"] = [[
It is a type environment that Typed Lua uses to assign projection variables to
second-level types.
]],
  ["projection type"] = [[
It is a type that allows Typed Lua to discriminate the type of local variables
that have a dependency relation.
]],
  ["prototype object"] = [[
It is an object that works like a class, that is, it is a prototype for creating
new objects of a given class.
]],
-- Q
-- R
-- S
  ["self-like delegation"] = [[
It is a technique to implement inheritance in dynamically typed languages.
]],
  ["sound type system"] = [[
It is a type system that does not type check all programs that contain a type error.
]],
  ["structural type system"] = [[
It is a type system that uses the type structure to define the equivalence among types.
]],
-- T
  ["table refinement"] = [[
It is an operation that allows changing a table type to include new fields or
to specialize existing fields.
]],
  ["top type"] = [[
It is a type that is supertype of all types.
]],
  ["type environment"] = [[
It is an environment that assigns variable names to types.
]],
  ["type tag"] = [[
In dynamically typed languages a type tag describes the type of a value during
run-time.
]],
-- U
  ["unique table type"] = [[
It is a table type that allows adding fields, and it is also the table type
that describe the type of the table constructor.
]],
  ["unsound type system"] = [[
It is a type system that type checks certain programs that contain type errors.
]],
  ["userdata"] = [[
It is a way to define new types in an application or library that is written in C.
]],
-- V
  ["vararg expression"] = [[
It is a Lua expression that can result in an arbitrary number of values.
]],
  ["variadic function"] = [[
It is a Lua function that can receive an arbitrary number of arguments.
]],
  ["variance"] = [[
It is the way types are ordered.
]],
-- W
  ["width subtyping"] = [[
It allows adding more fields to a table type.
]],
-- X
-- Y
-- Z
}

table.sort(terms)

io.write("\\begin{description}\n")
for k, v in ipairs(terms) do
  io.write(string.format("\\item[%s] %s\n", v, def[v] or "TO DO."))
end
io.write("\\end{description}\n")

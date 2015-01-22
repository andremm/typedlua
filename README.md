# Typed Lua

Typed Lua is a typed superset of Lua that compiles to plain Lua.
It provides optional type annotations, compile-time type checking, and
class-based object oriented programming through the definition of classes,
interfaces, and modules.

# Requirements for running the compiler

1. lua >= 5.1
1. [lpeg](http://www.inf.puc-rio.br/%7Eroberto/lpeg/) >= 0.12

# Install

Typed Lua must be installed in a standard location; luarocks will do this.

        $ [install luarocks]
        $ luarocks pack typedlua-scm-1.rockspec
        $ luarocks install typedlua-scm-1.src.rock

# Usage

        $ tlc [options] [filename]

# Compiler options

        -d      dump the AST
        -g      generate code without type checking
        -h      print this help
        -o name output to file 'name' (default is 'tlc.lua')
        -p      print the AST
        -v      print current version

# License

Released under the MIT License (MIT)

Copyright (c) 2013 Andre Murbach Maidl

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

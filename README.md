# Typed Lua

Typed Lua is a typed superset of Lua that compiles to plain Lua.
It provides optinal type annotations, compile-time type checking, and
class-based object oriented programming through the definition of classes,
interfaces, and modules.

# Requirements

    lua >= 5.2.2
    lpeg >= 0.12

# Usage


    $ tlc [options] [filename]

# Compiler options

    -d	    dump the AST
    -g	    generate code without type checking
    -h	    print this help
    -o name output to file 'name' (default is 'tlc.lua')
    -p	    print the AST
    -v	    print current version

# Major TODO list

* Add table type
* Add intersection type
* Add recursive type
* Add classes, interfaces, and modules
* Reimplement consitent-subtyping

# Typed Lua Syntax
<p>
<pre>
chunk ::= block

block ::= {stat} [retstat]

stat ::= &lsquo;<b>;</b>&rsquo; |
         varlist &lsquo;<b>=</b>&rsquo; explist |
         functioncall |
         label |
         <b>break</b> |
         <b>goto</b> Name |
         <b>do</b> block <b>end</b> |
         <b>while</b> exp <b>do</b> block <b>end</b> |
         <b>repeat</b> block <b>until</b> exp |
         <b>if</b> exp <b>then</b> block {<b>elseif</b> exp <b>then</b> block} [<b>else</b> block] <b>end</b> |
         <b>for</b> Name &lsquo;<b>=</b>&rsquo; exp &lsquo;<b>,</b>&rsquo; exp [&lsquo;<b>,</b>&rsquo; exp] <b>do</b> block <b>end</b> |
         <b>for</b> namelist <b>in</b> explist <b>do</b> block <b>end</b> |
         <b>function</b> funcname funcbody |
         <b>local</b> <b>function</b> Name funcbody |
         <b>local</b> typednamelist [&lsquo;<b>=</b>&rsquo; explist]

retstat ::= <b>return</b> [explist] [&lsquo;<b>;</b>&rsquo;]

label ::= &lsquo;<b>::</b>&rsquo; Name &lsquo;<b>::</b>&rsquo;

funcname ::= Name {&lsquo;<b>.</b>&rsquo; Name} [&lsquo;<b>:</b>&rsquo; Name]

varlist ::= var {&lsquo;<b>,</b>&rsquo; var}

var ::=  Name [&lsquo;<b>:</b>&rsquo; type] | prefixexp &lsquo;<b>[</b>&rsquo; exp &lsquo;<b>]</b>&rsquo; | prefixexp &lsquo;<b>.</b>&rsquo; Name

typednamelist ::= Name [&lsquo;<b>:</b>&rsquo; type] {&lsquo;<b>,</b>&rsquo; Name [&lsquo;<b>:</b>&rsquo; type]}

namelist ::= Name {&lsquo;<b>,</b>&rsquo; Name}

explist ::= exp {&lsquo;<b>,</b>&rsquo; exp}

exp ::=  <b>nil</b> | <b>false</b> | <b>true</b> | Number | String | &lsquo;<b>...</b>&rsquo; | functiondef |
         prefixexp | tableconstructor | exp binop exp | unop exp

prefixexp ::= var | functioncall | &lsquo;<b>(</b>&rsquo; exp &lsquo;<b>)</b>&rsquo;

functioncall ::=  prefixexp args | prefixexp &lsquo;<b>:</b>&rsquo; Name args

args ::=  &lsquo;<b>(</b>&rsquo; [explist] &lsquo;<b>)</b>&rsquo; | tableconstructor | String

functiondef ::= <b>function</b> funcbody

funcbody ::= &lsquo;<b>(</b>&rsquo; [parlist] &lsquo;<b>)</b>&rsquo; [&lsquo;<b>:</b>&rsquo; 2ndclasstype] block <b>end</b>

parlist ::= typednamelist [&lsquo;<b>,</b>&rsquo; &lsquo;<b>...</b>&rsquo; [&lsquo;<b>:</b>&rsquo; type]] | &lsquo;<b>...</b>&rsquo; [&lsquo;<b>:</b>&rsquo; type]

tableconstructor ::= &lsquo;<b>{</b>&rsquo; [fieldlist] &lsquo;<b>}</b>&rsquo;

fieldlist ::= field {fieldsep field} [fieldsep]

field ::= &lsquo;<b>[</b>&rsquo; exp &lsquo;<b>]</b>&rsquo; &lsquo;<b>=</b>&rsquo; exp | Name &lsquo;<b>=</b>&rsquo; exp | exp

fieldsep ::= &lsquo;<b>,</b>&rsquo; | &lsquo;<b>;</b>&rsquo;

binop ::= &lsquo;<b>+</b>&rsquo; | &lsquo;<b>-</b>&rsquo; | &lsquo;<b>*</b>&rsquo; | &lsquo;<b>/</b>&rsquo; | &lsquo;<b>^</b>&rsquo; | &lsquo;<b>%</b>&rsquo; | &lsquo;<b>..</b>&rsquo; |
          &lsquo;<b>&lt;</b>&rsquo; | &lsquo;<b>&lt;=</b>&rsquo; | &lsquo;<b>&gt;</b>&rsquo; | &lsquo;<b>&gt;=</b>&rsquo; | &lsquo;<b>==</b>&rsquo; | &lsquo;<b>~=</b>&rsquo; |
          <b>and</b> | <b>or</b>

unop ::= &lsquo;<b>-</b>&rsquo; | <b>not</b> | &lsquo;<b>#</b>&rsquo;

type ::= <b>object</b> | <b>any</b> | <b>nil</b> | basetype | uniontype | functiontype

basetype ::= <b>boolean</b> | <b>number</b> | <b>string</b>

uniontype ::= type &lsquo;<b>|</b>&rsquo; type

functiontype ::= &lsquo;<b>(</b>&rsquo; [2ndclasstype] &lsquo;<b>)</b>&rsquo; &lsquo;<b>-&gt;</b>&rsquo; 2ndclasstype

2ndclasstype ::= type {&lsquo;<b>,</b>&rsquo; type} [&lsquo;<b>*</b>&rsquo;]
</pre>
<p>


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

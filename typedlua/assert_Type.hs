import Type
import Control.Exception

-- constant types for tests

nil = TConstant CNil
false = TConstant CFalse
true = TConstant CTrue
double = TConstant CDouble
integer = TConstant CInteger
str = TConstant CString

-- base type for tests

boolean = TBase BBoolean
number = TBase BNumber
string = TBase BString

-- other types for tests

object = TObject
anyt = TAny

f1 = TFunction (TFirstClass number) (TFirstClass number)

u1 = TUnion number nil

main :: IO ()
main = do
  print $ assert (subtype object object == True) "Object <: Object";
  print $ assert (subtype nil object == True) "nil <: Object";
  print $ assert (subtype false object == True) "false <: Object";
  print $ assert (subtype true object == True) "true <: Object";
  print $ assert (subtype double object == True) "1.1 <: Object";
  print $ assert (subtype integer object == True) "1 <: Object";
  print $ assert (subtype str object == True) "'w' <: Object";
  print $ assert (subtype boolean object == True) "Boolean <: Object";
  print $ assert (subtype number object == True) "Number <: Object";
  print $ assert (subtype string object == True) "String <: Object";
  print $ assert (subtype anyt object == True) "Any <: Object";
  print $ assert (subtype f1 object == True) "(Number) -> Number <: Object";
  print $ assert (subtype u1 object == True) "Number | nil <: Object";
  print $ assert (subtype object nil == False) "Object <:/ nil";
  print $ assert (subtype object false == False) "Object <:/ false";
  print $ assert (subtype object true == False) "Object <:/ true";
  print $ assert (subtype object double == False) "Object <:/ 1.1";
  print $ assert (subtype object integer == False) "Object <:/ 1";
  print $ assert (subtype object str == False) "Object <:/ 'w'";
  print $ assert (subtype object boolean == False) "Object <:/ boolean";
  print $ assert (subtype object number == False) "Object <:/ number";
  print $ assert (subtype object string == False) "Object <:/ string";
  print $ assert (subtype object anyt == False) "Object <:/ Any";
  print $ assert (subtype object f1 == False) "Object <:/ (Number) -> Number";
  print $ assert (subtype object u1 == False) "Object <:/ Number | nil";

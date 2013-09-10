module Type where

data Const = CNil
           | CFalse
           | CTrue
           | CDouble
           | CInteger
           | CString
           deriving (Eq,Show)

data Base = BBoolean
          | BNumber
          | BString
          deriving (Eq,Show)

data Type = TConstant Const
          | TBase Base
          | TObject
          | TAny
          | TFunction SndClassType SndClassType
          | TUnion Type Type
          deriving (Show)

data SndClassType = TFirstClass Type
                  | TVarArg Type
                  | TProd Type SndClassType
                  deriving (Show)

subtype :: Type -> Type -> Bool
subtype t1 t2 =
  subtype_top t1 t2

subtype_top :: Type -> Type -> Bool
subtype_top t1 t2 =
  case t2 of TObject -> True
             _ -> False



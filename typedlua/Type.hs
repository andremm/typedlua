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
  subtype_top t1 t2 ||
  subtype_any t1 t2 ||
  subtype_constants t1 t2 ||
  subtype_base t1 t2 ||
  subtype_union t1 t2 ||
  subtype_function t1 t2

subtype_top :: Type -> Type -> Bool
subtype_top t1 t2 =
  case t2 of TObject -> True
             _ -> False

subtype_any :: Type -> Type -> Bool
subtype_any t1 t2 =
  case t1 of
    TAny -> case t2 of
              TAny -> True
              _ -> False
    _ -> False

subtype_constants :: Type -> Type -> Bool
subtype_constants t1 t2 =
  case t1 of
    TConstant CNil -> case t2 of
                        TConstant CNil -> True
                        _ -> False
    TConstant CFalse -> case t2 of
                          TConstant CFalse -> True
                          TBase BBoolean -> True
                          _ -> False
    TConstant CTrue -> case t2 of
                         TConstant CTrue -> True
                         TBase BBoolean -> True
                         _ -> False
    TConstant CDouble -> case t2 of
                           TConstant CDouble -> True
                           TBase BNumber -> True
                           _ -> False
    TConstant CInteger -> case t2 of
                            TConstant CInteger -> True
                            TConstant CDouble -> True
                            TBase BNumber -> True
                            _ -> False
    TConstant CString -> case t2 of
                           TConstant CString -> True
                           TBase BString -> True
                           _ -> False
    _ -> False

subtype_base :: Type -> Type -> Bool
subtype_base t1 t2 =
  case t1 of
    TBase BBoolean -> case t2 of
                        TBase BBoolean -> True
                        _ -> False
    TBase BNumber -> case t2 of
                       TBase BNumber -> True
                       _ -> False
    TBase BString -> case t2 of
                       TBase BString -> True
                       _ -> False
    _ -> False

subtype_union :: Type -> Type -> Bool
subtype_union t1 t2 =
  case t1 of
    TUnion t1' t1'' -> (subtype t1' t2) && (subtype t1'' t2)
    t1 -> case t2 of
            TUnion t2' t2'' -> (subtype t1 t2') || (subtype t1 t2'')
            _ -> False

subtype_function :: Type -> Type -> Bool
subtype_function t1 t2 =
  case t1 of
    TFunction s1 s2 -> case t2 of
                         TFunction s3 s4 -> (subtype2 s3 s1) && (subtype2 s2 s4)
                         _ -> False
    _ -> False

subtype2 :: SndClassType -> SndClassType -> Bool
subtype2 a b =
  case a of
    TFirstClass t1 -> case b of
                        TFirstClass t2 -> subtype t1 t2
                        TVarArg t2 -> subtype t1 t2
                        _ -> False
    TVarArg t1 -> case b of
                    TVarArg t2 -> subtype t1 t2
                    TFirstClass t2 -> subtype t1 t2
                    TProd t2 s2 -> (subtype t1 t2) && (subtype2 a s2)
                    _ -> False
    TProd t1 s1 -> case b of
                     TProd t2 s2 -> (subtype t1 t2) && (subtype2 s1 s2)
                     TVarArg t2 -> (subtype t1 t2) && (subtype2 s1 b)
                     _ -> False
    _ -> False

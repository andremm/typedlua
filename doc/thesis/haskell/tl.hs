
data TType = TValue
  | TNil
  | TBool Bool
  | TInt Int
  | TFloat Float
  | TStr String
  | TBoolean
  | TInteger
  | TNumber
  | TString
  | TUnion TType TType
  deriving (Eq, Show)

data SType = TType TType
  | TVararg TType
  | TTuple TType SType
  deriving (Show)

type Id = (String,TType)

data Stm = SSkip
  | SSeq Stm Stm
  | SLocal (Id,[Id]) [Exp] Stm
  | SAss1 LHS Exp
  | SAss (LHS,[LHS]) (Exp,[Exp])
  deriving (Show)

data Exp = ENil
  | EInt Int
  | EFloat Float
  | EStr String
  | EVar LHS
  deriving (Show)

data LHS = Var String deriving (Show)

type Env = String -> TType

emptyEnv :: Env
emptyEnv v = TNil

update :: String -> TType -> Env -> Env
update v t g = \v' -> if v == v' then t else g v'

showEnv :: Env -> TType
showEnv g = g ""

subtype1 :: TType -> TType -> Bool
subtype1 (TBool _) TBoolean = True
subtype1 (TInt _) TInteger = True
subtype1 (TInt _) TNumber = True
subtype1 (TFloat _) TNumber = True
subtype1 (TStr _) TString = True
subtype1 TInteger TNumber = True
subtype1 (TUnion t1 t2) t = (subtype1 t1 t) && (subtype1 t2 t)
subtype1 t (TUnion t1 t2) = (subtype1 t t1) || (subtype1 t t2)
subtype1 t TValue = True
subtype1 t1 t2 = t1 == t2

subtype2 :: SType -> SType -> Bool
subtype2 (TType t1) (TType t2) = subtype1 t1 t2
subtype2 (TTuple t1 s1) (TTuple t2 s2) = (subtype1 t1 t2) && (subtype2 s1 s2)
subtype2 (TVararg t1) (TVararg t2) = subtype1 (TUnion t1 TNil) (TUnion t2 TNil)
subtype2 (TType t1) (TVararg t2) = subtype1 t1 (TUnion t2 TNil)
subtype2 (TVararg t1) (TType t2) = subtype1 (TUnion t1 TNil) t2
subtype2 (TVararg t1) (TTuple t2 s2) = (subtype1 (TUnion t1 TNil) t2) && (subtype2 (TVararg t1) s2)
subtype2 (TTuple t1 s1) (TVararg t2) = (subtype1 t1 (TUnion t2 TNil)) && (subtype2 s1 (TVararg t2))
subtype2 _ _ = False

idlist2tuple :: [Id] -> SType
idlist2tuple [] = TVararg TValue
idlist2tuple ((n,t):l) = TTuple t (idlist2tuple l)

updateEnv :: [Id] -> Env -> Env
updateEnv [] g = g
updateEnv ((n,t):l) g = updateEnv l (update n t g)

checkStm :: Stm -> Env -> Env
checkStm SSkip g = g
checkStm (SSeq s1 s2) g = checkStm s2 (checkStm s1 g)
--checkStm (SLocal1 (n,t) e s) g = if subtype1 (checkExp e g) t
--                              then checkStm s (update n t g)
--                              else error("local: " ++ (show (checkExp e g) ++ " </: " ++ (show t)))
checkStm (SLocal (id,idl) el s) g = if subtype2 (checkExpList el g) (idlist2tuple (id:idl))
                                    then checkStm s (updateEnv (id:idl) g)
                                    else error("local: " ++ (show (checkExpList el g)) ++ " </: " ++ (show (idlist2tuple (id:idl))))
--checkStm (SAss1 l e) g = if subtype1 (checkExp e g) (checkVar l g)
--                        then g
--                        else error("assign: " ++ (show (checkExp e g)) ++ " </: " ++ (show (checkVar l g)))
checkStm (SAss (l,ll) (e,el)) g = if subtype2 (checkExpList (e:el) g) (checkVarList (l:ll) g)
                                  then g
                                  else error("assign: " ++ (show (checkExpList (e:el) g)) ++ " </: " ++ (show (checkVarList (l:ll) g)))

checkVar :: LHS -> Env -> TType
checkVar (Var n) g = g n

checkVarList :: [LHS] -> Env -> SType
checkVarList [] g = TVararg TValue
checkVarList (v:vl) g = TTuple (checkVar v g) (checkVarList vl g)

checkExp :: Exp -> Env -> TType
checkExp ENil g = TNil
checkExp (EInt i) g = TInt i
checkExp (EFloat f) g = TFloat f
checkExp (EStr s) g = TStr s
checkExp (EVar l) g = checkVar l g

checkExpList :: [Exp] -> Env -> SType
checkExpList [] _ = TVararg TNil
checkExpList (e:es) g = TTuple (checkExp e g) (checkExpList es g)

typecheck :: Stm -> String
typecheck s = case showEnv (checkStm s emptyEnv) of
  TNil -> "OK"
  e -> show(e)


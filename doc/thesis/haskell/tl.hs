
data TType = TValue
  | TNil
  | TFalse
  | TTrue
  | TInt Int
  | TFloat Float
  | TStr String
  | TBoolean
  | TInteger
  | TNumber
  | TString
  | TUnion TType TType
  | TFunction SType SType
  | UTable [(TType,Bool,TType)]
  | OTable [(TType,Bool,TType)]
  | CTable [(TType,Bool,TType)]
  deriving (Eq, Show)

data SType = TVoid
  | TType TType
  | TVararg TType
  | TTuple TType SType
  deriving (Eq, Show)

type Id = (String,TType)

type Vararg = (Bool,TType)

data Stm = SSkip
  | SSeq Stm Stm
  | SLocal (Id,[Id]) ExpList0 Stm
  | SAss (LHS,[LHS]) ExpList1
  | SRet ExpList0
  | SApp Exp ExpList0
  deriving (Show)

data Exp = ENil
  | EFalse
  | ETrue
  | EInt Int
  | EFloat Float
  | EStr String
  | EVararg
  | EFun [Id] Vararg SType Stm
  | EApp Exp ExpList0
  | EVar LHS
  deriving (Show)

data MExp = MVararg
  | MApp Exp ExpList0
  deriving (Show)

data LHS = Var String deriving (Show)

data ExpList0 = ZNothing
  | OEL ExpList1
  deriving (Show)

data ExpList1 = OExp Exp
  | OMExp MExp
  | LExp Exp ExpList1
  deriving (Show)

type Env = String -> TType

emptyEnv :: Env
emptyEnv v = TNil

update :: String -> TType -> Env -> Env
update v t g = \v' -> if v == v' then t else g v'

initEnv :: Env
initEnv = update "..." TString emptyEnv

updateEnv :: [Id] -> Env -> Env
updateEnv [] g = g
updateEnv ((n,t):l) g = updateEnv l (update n t g)

showEnv :: Env -> TType
showEnv g = g ""

first :: SType -> TType
first (TType t) = t
first (TVararg t) = TUnion t TNil
first (TTuple t s) = t
first TVoid = error("first(TVoid)")

subtype_closed_fields k b v [] = False
subtype_closed_fields k2 False v2 ((k1,False,v1):t) = (subtype1 k1 k2 && subtype1 k2 k1 && subtype1 v1 v2 && subtype1 v2 v1) || subtype_closed_fields k2 False v2 t
subtype_closed_fields k2 True v2 ((k1,True,v1):t) = (subtype1 k1 k2 && subtype1 k2 k1 && subtype1 v1 v2) || subtype_closed_fields k2 True v2 t
subtype_closed_fields k2 True v2 ((k1,False,v1):t) = (subtype1 k1 k2 && subtype1 k2 k1 && subtype1 v1 v2) || subtype_closed_fields k2 True v2 t
subtype_closed_fields k2 False v2 ((k1,True,v1):t) = False || subtype_closed_fields k2 False v2 t

subtype_closed_tables [] [] = True
subtype_closed_tables t [] = True
subtype_closed_tables [] t = False
subtype_closed_tables t1 ((k,b,v):t2) = subtype_closed_fields k b v t1 && subtype_closed_tables t1 t2

subtype_unique_fields _ _ v [] = subtype1 TNil v
subtype_unique_fields k1 b1 v1 ((k2,b2,v2):t) = (subtype1 k1 k2 && subtype1 v1 v2) || subtype_unique_fields k1 b1 v1 t

subtype_unique [] = True
subtype_unique ((k,b,v):t) = subtype1 TNil v && subtype_unique t

subtype_unique_tables [] [] = True
subtype_unique_tables t [] = True
subtype_unique_tables [] t = subtype_unique t
subtype_unique_tables ((k,b,v):t1) t2 = subtype_unique_fields k b v t2 && subtype_unique_tables t1 t2

subtype_open_fields _ _ v [] = subtype1 TNil v
subtype_open_fields k1 False v1 ((k2,False,v2):t) = (subtype1 k1 k2 && subtype1 v1 v2 && subtype1 v2 v1) || subtype_open_fields k1 False v1 t
subtype_open_fields k1 True v1 ((k2,True,v2):t) = (subtype1 k1 k2 && subtype1 v1 v2) || subtype_open_fields k1 True v1 t
subtype_open_fields k1 False v1 ((k2,True,v2):t) = (subtype1 k1 k2 && subtype1 v1 v2) || subtype_open_fields k1 False v1 t
subtype_open_fields k1 True v1 ((k2,False,v2):t) = False || subtype_open_fields k1 True v1 t

subtype_open_tables [] [] = True
subtype_open_tables t [] = True
subtype_open_tables [] t = subtype_unique t
subtype_open_tables ((k,b,v):t1) t2 = subtype_open_fields k b v t2 && subtype_open_tables t1 t2

subtype1 :: TType -> TType -> Bool
subtype1 t TValue = True
subtype1 TFalse TBoolean = True
subtype1 TTrue TBoolean = True
subtype1 (TInt _) TInteger = True
subtype1 (TInt _) TNumber = True
subtype1 (TFloat _) TNumber = True
subtype1 (TStr _) TString = True
subtype1 TInteger TNumber = True
subtype1 (TUnion t1 t2) t = (subtype1 t1 t) && (subtype1 t2 t)
subtype1 t (TUnion t1 t2) = (subtype1 t t1) || (subtype1 t t2)
subtype1 (TFunction p1 r1) (TFunction p2 r2) = (subtype2 p2 p1) && (subtype2 r1 r2)
subtype1 (CTable t1) (CTable t2) = subtype_closed_tables t1 t2
subtype1 (UTable t1) (CTable t2) = subtype_unique_tables t1 t2
subtype1 (UTable t1) (OTable t2) = subtype_unique_tables t1 t2
subtype1 (UTable t1) (UTable t2) = subtype_unique_tables t1 t2
subtype1 (OTable t1) (CTable t2) = subtype_open_tables t1 t2
subtype1 (OTable t1) (OTable t2) = subtype_open_tables t1 t2
subtype1 t1 t2 = t1 == t2

subtype2 :: SType -> SType -> Bool
--subtype2 (TVararg TNil) TVoid = True
--subtype2 TVoid (TVararg TValue) = True
subtype2 (TType t1) (TType t2) = subtype1 t1 t2
subtype2 (TTuple t1 s1) (TTuple t2 s2) = (subtype1 t1 t2) && (subtype2 s1 s2)
subtype2 (TVararg t1) (TVararg t2) = subtype1 (TUnion t1 TNil) (TUnion t2 TNil)
subtype2 (TType t1) (TVararg t2) = subtype1 t1 (TUnion t2 TNil)
subtype2 (TVararg t1) (TType t2) = subtype1 (TUnion t1 TNil) t2
subtype2 (TVararg t1) (TTuple t2 s2) = (subtype1 (TUnion t1 TNil) t2) && (subtype2 (TVararg t1) s2)
subtype2 (TTuple t1 s1) (TVararg t2) = (subtype1 t1 (TUnion t2 TNil)) && (subtype2 s1 (TVararg t2))
subtype2 s1 s2 = s1 == s2

idlist2tuple :: [Id] -> SType
idlist2tuple [] = TVararg TValue
idlist2tuple ((n,t):l) = TTuple t (idlist2tuple l)

parlist2tuple :: [Id] -> Vararg -> SType
parlist2tuple [] (False,_) = TVoid
parlist2tuple [] (True,t) = TVararg t
parlist2tuple ((n,t):l) v = parlist2tuple1 t l v

parlist2tuple1 :: TType -> [Id] -> Vararg -> SType
parlist2tuple1 t1 [] (False,_) = TType t1
parlist2tuple1 t1 [] (True,t2) = TTuple t1 (TVararg t2)
parlist2tuple1 t1 ((n,t2):l) v = TTuple t1 (parlist2tuple1 t2 l v)

adjustRetType :: SType -> SType
adjustRetType TVoid = TVoid
adjustRetType (TType t) = TTuple t (TVararg TNil)
adjustRetType (TVararg t) = TVararg t
adjustRetType (TTuple t s) = TTuple t (adjustRetType s)

checkStm :: Stm -> Env -> SType
checkStm SSkip g = TVoid
checkStm (SSeq s1 s2) g = case checkStm s1 g of
  TVoid -> checkStm s2 g
  _ -> error("seq: ")
checkStm (SLocal (id,idl) el s) g = if subtype2 (checkExpList0 el g) (idlist2tuple (id:idl))
                                    then checkStm s (updateEnv (id:idl) g)
                                    else error("local: " ++ (show (checkExpList0 el g)) ++ " </: " ++ (show (idlist2tuple (id:idl))))
checkStm (SAss (l,ll) el) g = if subtype2 (checkExpList1 el g) (checkVarList (l:ll) g)
                                  then TVoid
                                  else error("assign: " ++ (show (checkExpList1 el g)) ++ " </: " ++ (show (checkVarList (l:ll) g)))
checkStm (SRet el) g = checkExpList0 el g
checkStm (SApp e el) g = case checkMApp e el g of
  TVoid -> TVoid
  TVararg t -> TVoid
  e -> error(show(e))

checkVar :: LHS -> Env -> TType
checkVar (Var n) g = g n

checkVarList :: [LHS] -> Env -> SType
checkVarList [] g = TVararg TValue
checkVarList (v:vl) g = TTuple (checkVar v g) (checkVarList vl g)

checkExp :: Exp -> Env -> TType
checkExp ENil g = TNil
checkExp EFalse g = TFalse
checkExp ETrue g = TTrue
checkExp (EInt i) g = TInt i
checkExp (EFloat f) g = TFloat f
checkExp (EStr s) g = TStr s
checkExp EVararg g = case g "..." of
  TNil -> error("not vararg function")
  t -> TUnion t TNil
checkExp (EFun [] (False,t) r s) g = if subtype2 (checkStm s g) r
                                     then TFunction (parlist2tuple [] (False,t)) r
                                     else error(show(checkStm s g) ++ " </: " ++ show(r))
checkExp (EFun [] (True,t) r s) g = if subtype2 (checkStm s (updateEnv [("...",t)] g)) r
                                    then TFunction (parlist2tuple [] (True,t)) r
                                    else error(show(checkStm s (updateEnv [("...",t)] g)) ++ " </: " ++ show(r))
checkExp (EFun (id:idl) (False,t) r s) g = if subtype2 (checkStm s (updateEnv (id:idl) g)) r
                                           then TFunction (parlist2tuple (id:idl) (False,t)) r
                                           else error(show(checkStm s (updateEnv (id:idl) g)) ++ " </: " ++ show(r))
checkExp (EFun (id:idl) (True,t) r s) g = if subtype2 (checkStm s (updateEnv (("...",t):(id:idl)) g)) r
                                          then TFunction (parlist2tuple (id:idl) (True,t)) r
                                          else error(show(checkStm s (updateEnv (("...",t):(id:idl)) g)) ++ " </: " ++ show(r))
checkExp (EApp e el) g = first (checkMApp e el g)
checkExp (EVar l) g = checkVar l g

checkMExp :: MExp -> Env -> SType
checkMExp MVararg g = case g "..." of
  TNil -> error("not a vararg function")
  t -> TVararg t
checkMExp (MApp e el) g = checkMApp e el g

checkMApp :: Exp -> ExpList0 -> Env -> SType
checkMApp e el g = case g "strict" of
  TNil -> case checkExp e g of
            (TFunction p r) -> if subtype2 (checkExpList0 el g) p
                               then r
                               else error("app_{?}: " ++ show(checkExpList0 el g) ++ " </: " ++ show(p))
            e -> error("app_{?}: " ++ show(e))
  TTrue -> case checkExp e g of
             (TFunction p r) -> if subtype2 (adjustTuple (checkArgList0 el g) p) p
                                then r
                                else error("app_{?}: " ++ show(adjustTuple (checkArgList0 el g) p) ++ " </: " ++ show(p))
             e -> error("app_{?}: " ++ show(e))
  TFalse -> case checkExp e g of
              (TFunction p r) -> if subtype2 (checkArgList0 el g) p
                                 then r
                                 else error("app_{?}: " ++ show(checkArgList0 el g) ++ " </: " ++ show(p))
              e -> error("app_{?}: " ++ show(e))
  e -> error(show(e))

checkExpList0 :: ExpList0 -> Env -> SType
checkExpList0 ZNothing _ = TVararg TNil
checkExpList0 (OEL el) g = checkExpList1 el g

checkExpList1 :: ExpList1 -> Env -> SType
checkExpList1 (OExp e) g = TTuple (checkExp e g) (TVararg TNil)
checkExpList1 (OMExp m) g = checkMExp m g
checkExpList1 (LExp e l) g = TTuple (checkExp e g) (checkExpList1 l g)

checkArgList0 :: ExpList0 -> Env -> SType
checkArgList0 ZNothing _ = TVoid
checkArgList0 (OEL el) g = checkArgList1 el g

checkArgList1 :: ExpList1 -> Env -> SType
checkArgList1 (OExp e) g = TType (checkExp e g)
checkArgList1 (OMExp m) g = checkMExp m g
checkArgList1 (LExp e l) g = TTuple (checkExp e g) (checkArgList1 l g)

adjustTuple :: SType -> SType -> SType
adjustTuple TVoid TVoid = TVoid
adjustTuple TVoid s = adjustTuple (TType TNil) s
adjustTuple (TType t1) (TType t2) = TType t1
adjustTuple (TType t1) (TVararg t2) = TType t1
adjustTuple (TType t1) (TTuple t2 s) = TTuple t1 (adjustTuple (TType TNil) s)
adjustTuple (TTuple t1 s1) (TTuple t2 s2) = TTuple t1 (adjustTuple s1 s2)
adjustTuple t1 t2 = t1

tc :: Stm -> String
tc s = show (checkStm s initEnv)

stc :: Stm -> String
stc s = show (checkStm s (update "strict" TFalse initEnv))

astc :: Stm -> String
astc s = show (checkStm s (update "strict" TTrue initEnv))

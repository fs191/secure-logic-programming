{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC
  ( secrecCode
  ) where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Control.Lens hiding(Empty)

import Data.List
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc

import qualified DatalogProgram as DP
import DBClause
import Expr
import Rule

newtype SCProgram = SCProgram
  { _pStatements :: [TopStatement]
  }
  deriving (Semigroup, Monoid)

instance Pretty SCProgram where
  pretty p = vsep $ pretty <$> _pStatements p

data SCKind
  = SCShared3pKind

instance Pretty SCKind where
  pretty SCShared3pKind = "shared3p"

data SCDomain
  = SCShared3p | SCPublic | SCDynamic (Maybe Int)
  deriving (Show, Eq)

instance Pretty SCDomain where
  pretty SCShared3p  = "pd_shared3p"
  pretty SCPublic    = "public"
  pretty (SCDynamic Nothing)  = "D"
  pretty (SCDynamic (Just i)) = "D" <> pretty i

angled prettyContent = encloseSep (langle <> space) (rangle <> space) comma prettyContent

data SCType
  = SCUInt32
  | SCUInt
  | SCUInt8
  | SCXorUInt32
  | SCXorUInt8
  | SCInt32
  | SCBool
  | SCString
  | SCDynamicT (Maybe Int)
  | SCDynamicS (Maybe Int)
  | SCColumn SCDomain SCType SCType
  | SCSubst SCDomain SCType
  | SCStruct Text SCTemplate
  | SCArray Int SCType
  deriving (Show, Eq)

instance Pretty SCType where
  pretty SCUInt32    = "uint32"
  pretty SCUInt      = "uint"
  pretty SCUInt8     = "uint8"
  pretty SCXorUInt32 = "xor_uint32"
  pretty SCXorUInt8  = "xor_uint32"
  pretty SCInt32     = "int32"
  pretty SCBool      = "bool"
  pretty SCString    = "string"

  -- we are using two values for each string column: the true value S, and a hash T used for comparisons
  pretty (SCDynamicT Nothing)  = "T"
  pretty (SCDynamicT (Just i)) = "T" <> pretty i
  pretty (SCDynamicS Nothing)  = "S"
  pretty (SCDynamicS (Just i)) = "S" <> pretty i

  -- the column template defines its privacy type and the two types of values
  pretty (SCColumn pt dt st) = "relColumn" <> angled [pretty pt, pretty dt, pretty st]
  pretty (SCSubst pt dt)  = "subst" <> angled [pretty pt, pretty dt]
  pretty (SCStruct s t)     = pretty s <> pretty t
  pretty (SCArray n sctype) = pretty sctype <+> "[[" <> pretty n <> "]]"

data SCTemplate
  = SCTemplateDecl (Maybe ([SCDomain], [SCType]))
  | SCTemplateUse  (Maybe ([SCDomain], [SCType]))
  deriving (Show, Eq)

instance Pretty SCTemplate where
  pretty (SCTemplateDecl Nothing) = ""
  pretty (SCTemplateUse  Nothing) = ""
  pretty (SCTemplateDecl (Just (ds,xs))) = line <> "template" <> angled (pds ++ pxs)
      where pds = map ("domain" <+>) $ pretty <$> ds
            pxs = map ("type" <+>)   $ pretty <$> xs
  pretty (SCTemplateUse  (Just (ds,xs))) = angled (pds ++ pxs)
      where pds = pretty <$> ds
            pxs = pretty <$> xs

-- since SecreC syntax is different from Datalog's, let us define SecreC's own expression type
-- we will only need to define pretty printing for it
data SCExpr
  = SCConstInt   Int
  | SCConstFloat Float
  | SCConstStr   String
  | SCConstBool  Bool
  | SCVarName    Text
  | SCNot SCExpr
  | SCNeg SCExpr
  | SCInv SCExpr
  | SCDiv SCExpr SCExpr
  | SCSub SCExpr SCExpr
  | SCLt  SCExpr SCExpr
  | SCLe  SCExpr SCExpr
  | SCEq  SCExpr SCExpr
  | SCGt  SCExpr SCExpr
  | SCGe  SCExpr SCExpr
  | SCMul SCExpr SCExpr
  | SCAdd SCExpr SCExpr
  | SCMin SCExpr SCExpr
  | SCMax SCExpr SCExpr
  | SCAnd SCExpr SCExpr
  | SCOr  SCExpr SCExpr
  | SCSum  [SCExpr]
  | SCProd [SCExpr]
  | SCOrs  [SCExpr]
  | SCAnds [SCExpr]
  | SCTypeCast SCType SCExpr
  | SCFunCall Text [SCExpr]

instance Pretty SCExpr where
  pretty (SCConstInt x)   = pretty x
  pretty (SCConstStr x)   = dquotes $ pretty x
  pretty (SCConstBool True)  = "true"
  pretty (SCConstBool False) = "false"
  pretty (SCVarName x)    = pretty x
  pretty (SCNot e)        = "!" <> parens (pretty e)
  pretty (SCNeg e)        = "-" <> parens (pretty e)
  pretty (SCInv e)        = "1 / " <> parens (pretty e)
  pretty (SCDiv x y)      = parens (pretty x) <+> "/" <+> parens (pretty y)
  pretty (SCSub x y)      = parens (pretty x) <+> "-" <+> parens (pretty y)
  pretty (SCLt x y)       = parens (pretty x) <+> "<" <+> parens (pretty y)
  pretty (SCLe x y)       = parens (pretty x) <+> "<=" <+> parens (pretty y)
  pretty (SCEq x y)       = parens (pretty x) <+> "==" <+> parens (pretty y)
  pretty (SCGt x y)       = parens (pretty x) <+> ">" <+> parens (pretty y)
  pretty (SCGe x y)       = parens (pretty x) <+> ">=" <+> parens (pretty y)
  pretty (SCMul x y)      = parens (pretty x) <+> "*" <+> parens (pretty y)
  pretty (SCAdd x y)      = parens (pretty x) <+> "+" <+> parens (pretty y)
  pretty (SCOr x y)       = parens (pretty x) <+> "|" <+> parens (pretty y)
  pretty (SCAnd x y)      = parens (pretty x) <+> "&" <+> parens (pretty y)
  pretty (SCSum xs)       = hsep . punctuate "+" $ pretty <$> xs
  pretty (SCProd xs)      = hsep . punctuate "*" $ pretty <$> xs
  pretty (SCAnds xs)      = hsep . punctuate "&" $ pretty <$> xs
  pretty (SCOrs xs)       = hsep . punctuate "|" $ pretty <$> xs
  pretty (SCTypeCast t x) = parens (pretty t) <> pretty x
  pretty (SCFunCall f xs) = pretty f <> xs'
    where xs' = tupled $ pretty <$> xs

-- | Top-level statements
data TopStatement
  -- Function declaration
  = Funct FunctionDecl 
  -- Struct declaration
  | Struct StructDecl
  -- Import statement
  | Import String
  -- SecreC domain statement
  | Domain SCDomain SCKind
  -- Empty line
  | Empty

instance Pretty TopStatement where
  pretty (Funct f)    = pretty f
  pretty (Struct s)   = pretty s
  pretty (Import s)   = "import" <+> pretty s <> semi
  pretty (Domain d k) = "domain" <+> pretty d <+> pretty k <> semi
  pretty Empty = ""

-- | Statements with a return type
-- TODO use GADTs to express the return type
data Statement 
  = Comment String
  -- Variable declaration
  | VarDecl SCVar
  -- Variable assignment
  | VarAsgn Text SCExpr
  -- Variable initialization
  | VarInit SCVar SCExpr
  -- Function call
  | FunCall String [SCExpr]
  -- Return Statement
  | Return SCExpr
  -- We decide to leave empty rows inside functions as well (for better readability)
  | SCEmpty

instance Pretty Statement where
  pretty (Comment t) = "//" <> pretty t
  pretty (VarDecl v)   = pretty v <> semi
  pretty (VarAsgn v e) = pretty v <+> "=" <+> pretty e <> semi
  pretty (VarInit v e) = pretty v <+> "=" <+> pretty e <> semi
  pretty (Return e)       = "return" <+> pretty e <> semi
  pretty (SCEmpty)       = ""
  pretty (FunCall n pars) = pretty n <> pars' <> semi
    where pars' = tupled $ pretty <$> pars

data SCVar = SCVar
  { _vdDomain :: SCDomain
  , _vdType   :: SCType
  , _vdName   :: Text
  }

instance Pretty SCVar where
  pretty v = k <+> t <+> n
    where
      k = pretty $ _vdDomain v
      t = pretty $ _vdType v
      n = pretty $ _vdName v

data FunctionDecl = FunctionDecl
  { _fdTemplate   :: SCTemplate
  , _fdReturnType :: Maybe SCType
  , _fdName       :: Text
  , _fdParams     :: [SCVar]
  , _fdBody       :: [Statement]
  }

instance Pretty FunctionDecl where
  pretty fd = vsep
    [ template
    , rt <+> n <+> pars
    , lbrace
    , indent 4 (vsep body)
    , rbrace
    ]
    where
      template = pretty $ _fdTemplate fd
      n    = pretty $ _fdName fd
      pars = tupled $ pretty <$> _fdParams fd
      body = pretty <$> _fdBody fd
      rt   = case _fdReturnType fd of
        Just x  -> pretty x
        Nothing -> "void"

data StructDecl = StructDecl
  { _sdTemplate   :: SCTemplate
  , _sdName       :: Text
  , _sdMembers    :: [SCVar]
  }

instance Pretty StructDecl where
  pretty sd = vsep
    [ template
    , "struct" <+> n
    , lbrace
    , indent 4 (vsep ms)
    , rbrace
    ]
    where
      template = pretty $ _sdTemplate sd
      n  = pretty $ _sdName sd
      ms = pretty <$> map VarDecl (_sdMembers sd)

-- some variable/function names that are used multiple times
nameTableStruct p    = pack $ "table_" ++ p
nameGetTableStruct p = pack $ "getTable_" ++ p
nameTableCat p       = pack $ "cat_" ++ p
nameTableExt p       = pack $ "extend_" ++ p
nameGoalComp p l     = pack $ "goal_" ++ p ++ "_" ++ show l

-- fixed names that are used globally in different components
nameArg  i = pack $ "arg" ++ show i
nameBB     = pack $ "b"
nameMM     = pack $ "m"

nameTableBB  t   = pack $ unpack t ++ "." ++ unpack nameBB
nameTableArg t i = pack $ unpack t ++ "." ++ unpack (nameArg i)
nameIndex t i    = pack $ unpack t ++ show i

nameM    i = pack $ "m" ++ show i
nameN    i = pack $ "n" ++ show i
nameB    i = pack $ "b" ++ show i
nameB0   i = pack $ "b0" ++ show i
nameTheta i = pack $ "theta" ++ show i
nameTable i = pack $ "table" ++ show i

-- some functions that are already defined in SecreC
funExtCol   = SCFunCall "extendColumn"
funGetDBCol = SCFunCall "getDBColumn"
funCat      = SCFunCall "myCat"
funConstCol = SCFunCall "constColumn"
funReshape  = SCFunCall "reshape"
funTrueCol  = SCFunCall "trueColumn"
funFreeVCol = SCFunCall "freeVarColumn"
funUnify    = SCFunCall "unify"
funSize     = SCFunCall "size"
funSum      = SCFunCall "sum"
funGetArg   = SCFunCall "argument"
funShuffle  = SCFunCall "lpShuffle"
funArithOp  = SCFunCall "aop"
funBoolOp   = SCFunCall "bop"

funFilterTrue     = SCFunCall "filterTrue"
funDeclassify     = SCFunCall "declassifyIfNeed"
funTdbGetRowCount = SCFunCall "tdbGetRowCount"

funPublishArg         = FunCall "publishArg"
funTdbOpenConnection  = FunCall "tdbOpenConnection"
funTdbCloseConnection = FunCall "tdbCloseConnection"

-- use this Sharemind dataset by default
strDataset = SCConstStr "DS1"

secrecCode :: DP.DatalogProgram -> SCProgram
secrecCode dp = program $

  header
  ++ map (Struct . extPredDecl) extPreds
  ++ map (Funct . extPredGet) extPreds
  ++ map Struct (zipWith intPredDecl intPredPs intPredNs)
  ++ map Funct (zipWith intPredExt intPredPs intPredNs)
  ++ map Funct (ruleFuns rules)
  ++ map Funct (zipWith intPredCat intPredPs intPredNs)
  ++ map Funct (zipWith3 intPredGet lss intPredPs intPredNs)
  ++ [Funct goal]
 where
   rules = dp ^. DP.dpRules
   goal  = case (dp ^. DP.dpGoal) of
       Just g -> concreteGoal rules (DP.inputs g) (DP.outputs g) (DP.formula g)
       Nothing -> defaultGoal
   extPreds = dp ^. DP.dpDBClauses
   (intPredPs, intPredNs) = unzip $ nub $ map (\p -> (predicateName p, predicateArity p)) $ map (\r -> r ^. ruleHead) rules

   lss = [ls | r <- rules, let ls = filter (>= 0) $ zipWith (\r' l -> if ruleName r == ruleName r' then l else -1) rules [0..]]


header :: [TopStatement]
header =
  [ Import "stdlib"
  , Import "shared3p"
  , Import "shared3p_string"
  , Import "shared3p_table_database"
  , Import "table_database"
  , Empty
  , Import "lp_essentials"
  , Empty
  -- TODO the domain is currently defined already in lp_essentials.sc, but this may change later
  --, Domain SCShared3p SCShared3pKind
  , Empty
  ]

-- type rewrite function
scDomain :: Maybe Int -> Maybe PPDomain -> SCDomain
scDomain _ (Just Private) = SCShared3p
scDomain _ (Just Public)  = SCPublic
scDomain i Nothing        = SCDynamic i

scDomainFromAnn :: Ann -> SCDomain
scDomainFromAnn ann = scDomain Nothing (ann ^. domain)

scStructType :: (SCDomain -> SCType -> SCType -> SCType) -> Int -> Ann -> SCType
scStructType f i ann =
  let dom    = ann ^. domain in
  let dtype  = ann ^. annType in
  case (dtype, dom) of
      (Just PPBool, _) -> f (scDomain (Just i) dom) SCBool  SCBool
      (Just PPInt,  _) -> f (scDomain (Just i) dom) SCInt32 SCInt32
      (Just PPStr,  Just Private) -> f (scDomain (Just i) dom) SCXorUInt32 SCXorUInt8
      (Just PPStr,  Just Public)  -> f (scDomain (Just i) dom) SCUInt32    SCUInt8
      (Just PPStr,  Nothing)      -> error $ "cannot determine data type for a string of unknown domain"
      (Nothing,     _)            -> f (scDomain (Just i) dom) (SCDynamicT (Just i)) (SCDynamicS (Just i))

scColType :: Int -> Ann -> SCType
scColType = scStructType SCColumn

scConstType :: Expr -> SCExpr
scConstType (ConstInt   _ c) = SCConstInt c
scConstType (ConstFloat _ c) = SCConstFloat c
scConstType (ConstBool  _ c) = SCConstBool c
scConstType (ConstStr   _ c) = SCConstStr c
scConstType e                 = error $ "Expecting a constant, not" ++ show e

scSubstType :: Int -> Ann -> SCType
scSubstType i ann = SCSubst (scDomain (Just i) (ann ^. domain)) (scColType i ann)

-- TODO it seems that we can reduce it directly to type inference over column parameters
-- so that we do not need to define type inference for columns
-- we prefer private to public, and everything to dynamic
joinType :: SCType -> SCType -> SCType
joinType (SCColumn (SCDynamic _) _ _) c2 = c2
joinType c1 (SCColumn (SCDynamic _) _ _) = c1
joinType (SCColumn SCPublic _ _) c2 = c2
joinType c1 (SCColumn SCPublic _ _) = c1
joinType c1@(SCColumn d1 t1 s1) c2@(SCColumn d2 t2 s2) = if (t1 == t2 && s1 == s2) then c1
                                                         else error $ "could not match types for different rules for the goal relation"

dynamicColD i = SCDynamic  (Just i)
dynamicColT i = SCDynamicT (Just i)
dynamicColS i = SCDynamicS (Just i)
dynamicColumn i = SCColumn (dynamicColD i) (dynamicColT i) (dynamicColS i)
dynamicSubst  i = SCSubst  (dynamicColD i) (dynamicColumn i)

extPredDecl :: DBClause -> StructDecl
extPredDecl dbc = struct (SCTemplateDecl Nothing) (nameTableStruct p) (y:ys)
  where
    p  = name dbc
    xs = vars dbc
    y  = variable SCPublic (SCArray 1 SCBool) nameBB
    is = [0..length xs - 1]
    ys = zipWith (\x i -> case x of {(DBCol pptype _) -> variable SCPublic (scColType i pptype) (nameArg i) ; _ -> error $ "Expected a DBCol, but see " ++ show x}) xs is

extPredGet :: DBClause -> FunctionDecl
extPredGet dbc = function (SCTemplateDecl Nothing) returnType fname fargs fbody
  where
    result = "result"
    ds = "ds"
    m  = "m"
    mi = "mi"
    ni = "ni"
    p = name dbc
    n = length $ vars dbc
    is = [0..n-1]
    returnType = Just $ SCStruct (nameTableStruct p) (SCTemplateUse Nothing)
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCString ds, variable SCPublic SCUInt m, variable SCPublic SCUInt mi, variable SCPublic SCUInt ni]
    fbody = [ VarDecl $ variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse Nothing)) result
            , VarAsgn (nameTableBB result) (funTrueCol [SCVarName m])
            ]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funGetDBCol [SCVarName ds, SCConstStr p, SCConstInt i, SCVarName m, SCVarName mi, SCVarName ni])) is
            ++ [Return (SCVarName result)]

intPredDecl :: String -> Int -> StructDecl
intPredDecl p n = struct template (nameTableStruct p) (y:ys)
  where
    is = [0..n - 1]
    template = SCTemplateDecl $ Just ([SCDynamic Nothing], map dynamicColT is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) nameBB
    ys = map (\i -> variable SCPublic (SCDynamicT (Just i)) (nameArg i)) is

intPredExt :: String -> Int -> FunctionDecl
intPredExt p n = function template returnType fname fargs fbody
  where
    result = "result"
    m  = "m"
    mi = "mi"
    ni = "ni"
    is = [0..n - 1]
    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameTableExt p
    fargs = [variable SCPublic (dynamicColT 1) result, variable SCPublic SCUInt m, variable SCPublic SCUInt mi, variable SCPublic SCUInt ni]
    fbody = [VarAsgn (nameTableBB result) (funExtCol [SCVarName (nameTableBB result), SCVarName m, SCVarName mi, SCVarName ni])]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funExtCol [SCVarName (nameTableArg result i), SCVarName m, SCVarName mi, SCVarName ni])) is
            ++ [Return (SCVarName result)]


intPredCat :: String -> Int -> FunctionDecl
intPredCat p n = function template returnType fname fargs fbody
  where
    input1 = "t1"
    input2 = "t2"
    result = "t0"
    is = [0..n - 1]
    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1, dynamicColT 2])
    returnType = Just $ dynamicColT 0
    fname = nameTableCat p
    fargs = [variable SCPublic (dynamicColT 1) input1, variable SCPublic (dynamicColT 2) input2]
    fbody = [VarDecl $ variable SCPublic (SCDynamicT (Just 0)) result,
             VarAsgn (nameTableBB result) (funCat [SCVarName (nameTableBB input1), SCVarName (nameTableBB input2)])]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funCat [SCVarName (nameTableArg input1 i), SCVarName (nameTableArg input2 i)])) is
            ++ [Return (SCVarName result)]

intPredGet :: [Int] -> String -> Int -> FunctionDecl
intPredGet (l':ls) p n = function template returnType fname fargs fbody
  where
    ds = "ds"
    input  = "args"
    result = "result"

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCString ds, variable SCPublic (dynamicColT 1) input]
    fbody = map (\l -> VarInit (variable SCPublic (SCDynamicT (Just 0)) (nameIndex result l)) (SCFunCall (nameGoalComp p l) [SCVarName ds, SCVarName input])) (l':ls)
            ++ [VarInit (variable SCPublic (SCDynamicT (Just 0)) result) (SCVarName (nameIndex result l'))]
            ++ map (\l -> VarAsgn result (SCFunCall (nameTableCat p) [SCVarName result, SCVarName (nameIndex result l)])) ls
            ++ [Return (SCVarName result)]

defaultGoal :: FunctionDecl
defaultGoal = mainFun $
  [ VarDecl $ variable SCShared3p SCUInt32 "dummy"
  , Comment "TODO: state your own goal here"
  , FunCall "publish" 
      [ SCConstStr "NOTICE: no goal specified in the main function"
      , SCConstBool False
      ]
  ]


concreteGoal :: [Rule] -> [Expr] -> [Expr] -> Expr -> FunctionDecl
concreteGoal rules xs ys (Pred _ p zs) = mainFun $

  -- get the arguments of a goal
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColType i xtype)        (nameArg i)) (funConstCol [funGetArg [SCConstStr x]])) setX ++
  map (\(z,i)           -> VarInit (variable SCPublic ((scColType i . getAnn) z) (nameArg i)) (funConstCol [scConstType z])) setC ++
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColType i xtype)        (nameArg i)) (funFreeVCol [])) setY ++

  -- create an input data structure that corresponds to particular goal
  [ VarDecl $ variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCPublic], inTypes))) input
  , VarAsgn (nameTableBB input) (funTrueCol [])
  ] ++
  map (\i -> VarAsgn (nameTableArg input i) (SCVarName (nameArg i))) is ++

  -- establish database connection
  [ VarInit (variable SCPublic SCString ds) strDataset
  , funTdbOpenConnection [SCVarName ds]

  -- call the goal, read updated valuation of free variables
  , VarInit (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCPublic], outTypes))) result) (SCFunCall (nameGetTableStruct p) [SCVarName ds, SCVarName input])

  -- close connection
  , funTdbCloseConnection [SCVarName ds]

  -- shuffle the results and leave only those whose truth bit is 1
  , VarInit (variable SCPublic SCUInt32 n) (funDeclassify [funSum [SCTypeCast SCUInt32 (SCVarName (nameTableBB result))]])
  , VarInit (variable outDomain (SCArray 1 SCUInt32) pi) (funShuffle [SCVarName (nameTableBB result)])
  ] ++
  map (\(Var _ zi,i) -> funPublishArg
               [ SCConstInt i
               , SCConstStr zi
               , (funFilterTrue [SCVarName pi, SCVarName n, SCVarName (nameTableArg result i)])
               ]
  ) setY

  where
    ds     = "ds"
    input  = "args"
    result = "result"
    pi     = "pi"
    n      = "n"

    is = [0..length zs - 1]
    (setZ,setC) = partition (\(zi,_) -> case zi of {Var _ _ -> True; _ -> False}) (zip zs is)
    setX = [(zi,i) | (zi@(Var _ z),i) <- setZ, (Var _ x) <- xs, z == x]
    setY = [(zi,i) | (zi@(Var _ z),i) <- setZ, (Var _ y) <- ys, z == y]
    -- which types the args of p have according to zs (intputs/outputs)
    inTypes = zipWith (\z i -> scColType i (getAnn z)) zs is

    rs = filter (\r -> ruleName r == p) rules
    -- which types the args of p have according to rules for relation p
    outTypesLs = map (\r -> zipWith scColType is (ruleSchema r)) rs
    -- we eventually take the strongest of these two to avoid privacy leakage of both DB and the user's inputs
    outTypes   = foldr (zipWith joinType) (map dynamicColumn is) (inTypes : outTypesLs)

    -- if the truthness condition of at least one rule has private type, then so has the final answer
    doms = map (\r -> let q = r ^. ruleTail in ((getAnn q) ^.) domain) rules
    outDomain = if elem (Just Private) doms then SCShared3p else SCPublic

ruleFuns :: [Rule] -> [FunctionDecl]
ruleFuns rules = zipWith ruleFun rules [0..]

ruleFun :: Rule -> Int -> FunctionDecl
ruleFun r j = function template returnType fname fargs fbody
  where
    ds    = "ds"
    input = "args"
    rhead = r ^. ruleHead
    rtail = r ^. ruleTail
    p     = ruleName r
    zs    = args r
    n     = length zs

    template   = SCTemplateDecl $ Just ((SCDynamic Nothing) :  map (SCDynamic . Just) [0..n-1], (SCDynamicT Nothing) : (map (SCDynamicT . Just) [0..n-1] ++ map (SCDynamicS . Just) [0..n-1]))
    returnType = Just $ SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn [0..n-1]))
    fname      = nameGoalComp p j
    fargs      = [SCVar SCPublic SCString ds, SCVar SCPublic (SCDynamicT Nothing) input]
    fbody      = ruleBody ds input p zs rtail

ruleBody :: Text -> Text -> String -> [Expr] -> Expr -> [Statement]
ruleBody ds input0 p zs q =

  [ SCEmpty, Comment "compute the number of solutions in used predicates"
  , VarInit (variable SCPublic SCUInt (nameM 0)) (funSize [SCVarName (nameTableBB input0)])
  ] ++ getRowCounts ++
  [ VarInit (variable SCPublic SCUInt nameMM) $ SCProd (SCVarName (nameM 0) : (map (\(_,i) -> SCVarName (nameM i)) ts))
  ] ++ getNs ++
  [ SCEmpty, Comment "extend the initial args to appropriate size"
  , VarInit (variable SCPublic (SCDynamicT Nothing) input) (SCFunCall (nameTableExt p) [SCVarName input0, SCVarName nameMM, SCVarName (nameM 0), SCVarName (nameN 0)])
  ] ++
  [SCEmpty, Comment "evaluate all underlying predicates"] ++ getTables ++ 
  evalBody ++ 
  [SCEmpty, Comment "unify args"] ++ unifyArgs ++

  [ SCEmpty, Comment "output the updated predicate arguments"
  , VarInit (variable (SCDynamic Nothing) (SCArray 1 SCBool) result_b) (SCAnd (SCVarName (nameTableBB input)) (SCAnd (SCAnds (map (\i -> SCVarName (nameB0 i)) [0..length zs-1])) (SCAnds (map (\i -> SCVarName (nameB i)) [1..length qs]))))
  , VarDecl (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn [0..n-1]))) result)
  , VarAsgn (nameTableBB result) (SCVarName result_b)
  ] ++ asgnOutput ++
  [Return (SCVarName result)]

  where
    input    = "table0"
    result   = "result"
    result_b = "b"

    n  = length zs
    qs = andsToList q
    ts = map (\(qi,i) -> (predicateName qi, i)) $ filter (\(qi,i) -> case qi of {Pred _ _ _ -> True; _ -> False}) $ zip qs [1..]
    is = 0 : (map snd ts)

    getRowCounts = map (\(ti,i) -> VarInit (variable SCPublic SCUInt (nameM i)) (funTdbGetRowCount [SCVarName ds, SCConstStr ti])) ts
    getNs        = map (\i      -> let js = (filter (i >=) is) in
                                   VarInit (variable SCPublic SCUInt (nameN i)) (SCDiv (SCVarName nameMM) (SCProd (map (\j -> SCVarName (nameM j)) js))) ) is
    getTables    = map (\(ti,i) -> VarInit (variable SCPublic (SCStruct (nameTableStruct ti) (SCTemplateUse Nothing)) (nameTable i)) (SCFunCall (nameGetTableStruct ti) [SCVarName ds, SCVarName nameMM, SCVarName (nameM i), SCVarName (nameN i)])) ts

    evalBody   = concat $ zipWith predToSC qs [1..]
    unifyArgs  = concat $ zipWith (\z i -> [ VarInit (variable SCPublic (dynamicSubst i) (nameTheta i)) (funUnify [SCVarName (nameTableArg input i), exprToSC z])
                                           , VarInit (variable (SCDynamic (Just i)) (SCArray 1 SCBool) (nameB0 i)) (SCVarName  (nameTableBB (nameTheta i)))
                                           , VarInit (variable SCPublic (dynamicColumn i) (nameArg i)) (SCVarName (nameTableArg (nameTheta i) 0))
                                           ] ) zs [0..]

    asgnOutput = map (\i -> VarAsgn (nameTableArg result i) (SCVarName (nameArg i))) [0..length zs - 1]



predToSC :: Expr -> Int -> [Statement]
predToSC e j =
  [SCEmpty, Comment ("q" ++ show j)] ++ predCode
  where
    predCode = case e of

        ConstBool ann b -> let dom = scDomainFromAnn ann in
                          [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (funReshape [SCConstBool b, SCVarName nameMM])]

        -- here we assume that constants are not passed directly as arguments, but a comparison is added later
        -- TODO we will need the annotation if we do the comparisons immediately
        Pred _ p zs    -> zipWith (\z i -> VarInit (variable SCPublic (scColType i (getAnn z)) (pack $ getVarName z)) (SCVarName (nameTableArg (nameTable j) i))) zs [0..]
                          ++ [VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) (funReshape [SCConstBool True, SCVarName nameMM])]

        Not ann (Pred _ p zs) ->
            let dom = scDomainFromAnn ann in
            [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName (nameTableArg (nameTable j) i))) zs [0..]))]

        Lt  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC e)]
        Le  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC e)]
        Eq  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC e)]
        Gt  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC e)]
        Ge  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC e)]

        _ -> error $ "Cannot determine truthness for " ++ show e

exprToSC :: Expr -> SCExpr
exprToSC e =
  case e of 
    ConstInt   _ c -> funConstCol [SCConstInt c, SCVarName nameMM]
    ConstFloat _ c -> funConstCol [SCConstFloat c, SCVarName nameMM]
    ConstStr   _ c -> funConstCol [SCConstStr c, SCVarName nameMM]
    ConstBool  _ c -> funConstCol [SCConstBool c, SCVarName nameMM]

    -- TODO should we distinguish between DB and Free variables?
    DBCol _ x -> SCVarName $ pack x
    Var   _ x -> SCVarName $ pack x

    Not  _ e0 -> funBoolOp [SCConstStr "not", exprToSC e0]
    Neg  _ e0 -> funArithOp [SCConstStr "neg", exprToSC e0]
    Inv  _ e0 -> funArithOp [SCConstStr "inv", exprToSC e0]

    Div  _ e1 e2 -> funArithOp [SCConstStr "/", exprToSC e1, exprToSC e2]
    Sub  _ e1 e2 -> funArithOp [SCConstStr "-", exprToSC e1, exprToSC e2]
    Lt   _ e1 e2 -> funBoolOp [SCConstStr "<", exprToSC e1, exprToSC e2]
    Le   _ e1 e2 -> funBoolOp [SCConstStr "<=", exprToSC e1, exprToSC e2]
    Eq   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2]
    Gt   _ e1 e2 -> funBoolOp [SCConstStr ">", exprToSC e1, exprToSC e2]
    Ge   _ e1 e2 -> funBoolOp [SCConstStr ">=", exprToSC e1, exprToSC e2]
    Mul  _ e1 e2 -> funArithOp [SCConstStr "*", exprToSC e1, exprToSC e2]
    Add  _ e1 e2 -> funArithOp [SCConstStr "+", exprToSC e1, exprToSC e2]
    Min  _ e1 e2 -> funArithOp [SCConstStr "min", exprToSC e1, exprToSC e2]
    Max  _ e1 e2 -> funArithOp [SCConstStr "max", exprToSC e1, exprToSC e2]
    And  _ e1 e2 -> funBoolOp [SCConstStr "and", exprToSC e1, exprToSC e2]
    Or   _ e1 e2 -> funBoolOp [SCConstStr "or", exprToSC e1, exprToSC e2]

    Pred _ _ _ -> error $ "High order predicates are not supported"

function :: SCTemplate -> Maybe SCType -> Text -> [SCVar] -> [Statement] -> FunctionDecl
function = FunctionDecl

mainFun :: [Statement] -> FunctionDecl
mainFun = function (SCTemplateDecl Nothing) Nothing "main" []

struct :: SCTemplate -> Text -> [SCVar] -> StructDecl
struct = StructDecl

variable :: SCDomain -> SCType -> Text -> SCVar
variable = SCVar

program :: [TopStatement] -> SCProgram
program = SCProgram


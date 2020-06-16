{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC
--  ( secrecCode
--  ) where
where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Control.Lens hiding(Empty)

import Data.List
import Data.Text (Text, pack)
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
  pretty p = hsep $ punctuate semi $ pretty <$> _pStatements p

data SCKind
  = SCShared3pKind

instance Pretty SCKind where
  pretty SCShared3pKind = "shared3p"

data SCDomain
  = SCShared3p | SCPublic | SCDynamic (Maybe Int)

instance Pretty SCDomain where
  pretty SCShared3p  = "pd_shared3p"
  pretty SCPublic    = ""
  pretty (SCDynamic Nothing)  = "D"
  pretty (SCDynamic (Just i)) = "D" <+> pretty i

angled prettyContent = langle <> prettyContent <> rangle

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
  | SCSubst SCDomain SCType SCType
  | SCStruct String SCTemplate
  | SCArray Int SCType

instance Pretty SCType where
  pretty SCUInt32    = "uint32"
  pretty SCUInt      = "uint"
  pretty SCUInt8     = "uint8"
  pretty SCXorUInt32 = "xor_uint32"
  pretty SCXorUInt8  = "xor_uint32"
  pretty SCInt32     = "int32"
  pretty SCBool      = "bool"
  pretty SCString    = "string"
  pretty (SCDynamicT Nothing)  = "T"
  pretty (SCDynamicT (Just i)) = "T" <+> pretty i
  pretty (SCDynamicS Nothing)  = "S"
  pretty (SCDynamicS (Just i)) = "S" <+> pretty i
  pretty (SCColumn pt dt st) = "relColumn" <> angled (tupled [pretty pt, pretty dt, pretty st])
  pretty (SCSubst pt dt st)  = "subst" <> angled (tupled [pretty pt, pretty dt, pretty st])
  pretty (SCStruct s t)     = pretty s <> pretty t
  pretty (SCArray n sctype) = pretty sctype <+> "[[" <+> pretty n <+> "]]"

data SCTemplate
  = SCTemplateDecl (Maybe ([SCDomain], [SCType]))
  | SCTemplateUse  (Maybe ([SCDomain], [SCType]))

instance Pretty SCTemplate where
  pretty (SCTemplateDecl Nothing) = ""
  pretty (SCTemplateUse  Nothing) = ""
  pretty (SCTemplateDecl (Just (ds,xs))) = "template" <> angled (tupled (pds ++ pxs))
      where pds = map ("domain" <+>) $ pretty <$> ds
            pxs = map ("type" <+>)   $ pretty <$> xs
  pretty (SCTemplateUse  (Just (ds,xs))) = angled (tupled (pds ++ pxs))
      where pds = pretty <$> ds
            pxs = pretty <$> xs

-- since SecreC syntax is different from Datalog's, let us define SecreC's own expression type
-- we will only need to define pretty printing for it
data SCExpr
  = SCConstInt   Int
  | SCConstFloat Float
  | SCConstStr   String
  | SCConstBool  Bool
  | SCVarName    String
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
  | SCFunCall String [SCExpr]

instance Pretty SCExpr where
  pretty (SCConstInt x)   = pretty x
  pretty (SCConstStr x)   = "\"" <> pretty x <> "\""
  pretty (SCConstBool x)  = pretty x
  pretty (SCVarName x)    = pretty x
  pretty (SCNot e)        = "!" <> pretty e
  pretty (SCNeg e)        = "-" <> pretty e
  -- TODO check if it is the right SecreC syntax for floating point inverse
  pretty (SCInv e)        = "1 / (" <> pretty e <> ")"
  pretty (SCDiv x y)      = pretty x <+> "/" <+> pretty y
  pretty (SCSub x y)      = pretty x <+> "-" <+> pretty y
  pretty (SCLt x y)       = pretty x <+> "<" <+> pretty y
  pretty (SCLe x y)       = pretty x <+> "<=" <+> pretty y
  pretty (SCEq x y)       = pretty x <+> "==" <+> pretty y
  pretty (SCGt x y)       = pretty x <+> ">" <+> pretty y
  pretty (SCGe x y)       = pretty x <+> ">=" <+> pretty y
  pretty (SCMul x y)      = pretty x <+> "*" <+> pretty y
  pretty (SCAdd x y)      = pretty x <+> "+" <+> pretty y
  pretty (SCOr x y)       = pretty x <+> " | " <+> pretty y
  pretty (SCAnd x y)      = pretty x <+> " & " <+> pretty y
  pretty (SCSum xs)       = encloseSep "" "" "+" (pretty <$> xs)
  pretty (SCProd xs)      = encloseSep "" "" "*" (pretty <$> xs)
  pretty (SCAnds xs)      = encloseSep "" "" "&" (pretty <$> xs)
  pretty (SCOrs xs)       = encloseSep "" "" "|" (pretty <$> xs)
  pretty (SCTypeCast t x) = "(" <> pretty t <> ")" <> pretty x
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
  pretty (Import s)   = "import" <+> pretty s
  pretty (Domain d k) = "domain" <+> pretty d <+> pretty k

-- | Statements with a return type
-- TODO use GADTs to express the return type
data Statement 
  = Comment String
  -- Variable declaration
  | VarDecl SCVar
  -- Variable assignment
  | VarAsgn String SCExpr
  -- Variable initialization
  | VarInit SCVar SCExpr
  -- Function call
  | FunCall String [SCExpr]
  -- Return Statement
  | Return SCExpr

instance Pretty Statement where
  pretty (Comment t) = "//" <> pretty t
  pretty (VarDecl v)   = pretty v <> semi
  pretty (VarAsgn v e) = pretty v <+> "=" <+> pretty e <> semi
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
    , lbracket
    , indent 4 body
    , rbracket
    ]
    where
      template = pretty $ _fdTemplate fd
      n    = pretty $ _fdName fd
      pars = tupled $ pretty <$> _fdParams fd
      body = vsep $ pretty <$> _fdBody fd
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
    , lbracket
    , vsep ms
    , rbracket
    ]
    where
      template = pretty $ _sdTemplate sd
      n  = pretty $ _sdName sd
      ms = punctuate semi $ pretty <$> _sdMembers sd

secrecCode :: DP.DatalogProgram -> SCProgram
secrecCode dp = program $
{-
  header
  ++ -}
  map (Struct . extPredDecl) extPreds
  ++ map (Struct . intPredDecl) intPreds
  ++ map (Funct . intPredExt) intPreds
  ++ map Funct (ruleFuns rules)
  ++ map (Funct . intPredCat) intPreds
  ++ map (Funct . intPredGet ls) intPreds
  ++ [Funct goal]
 where
   rules = dp ^. DP.dpRules
   goal  = case (dp ^. DP.dpGoal) of
       Just g -> concreteGoal (DP.inputs g) (DP.outputs g) (DP.formula g)
       Nothing -> defaultGoal
   extPreds = dp ^. DP.dpDBClauses
   intPreds = map (\r -> r ^. ruleHead) rules
   -- TODO we need to extract own ls for each intPred
   ls = [0..length intPreds]


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
  , Domain SCShared3p SCShared3pKind
  , Empty
  ]

-- type rewrite function
scColType :: Ann -> SCType
scColType (Ann (Just PPBool) (Just Private)) = SCColumn SCShared3p (SCArray 1 SCBool)      (SCArray 2 SCBool)
scColType (Ann (Just PPBool) (Just Public))  = SCColumn SCPublic   (SCArray 1 SCBool)      (SCArray 2 SCBool)
scColType (Ann (Just PPInt)  (Just Private)) = SCColumn SCShared3p (SCArray 1 SCInt32)     (SCArray 2 SCInt32)
scColType (Ann (Just PPInt)  (Just Public))  = SCColumn SCPublic   (SCArray 1 SCInt32)     (SCArray 2 SCInt32)
scColType (Ann (Just PPStr)  (Just Private)) = SCColumn SCShared3p (SCArray 1 SCXorUInt32) (SCArray 2 SCXorUInt8)
scColType (Ann (Just PPStr)  (Just Public))  = SCColumn SCPublic   (SCArray 1 SCUInt32)    (SCArray 2 SCUInt8)

scSubstType :: Ann -> SCType
scSubstType (Ann (Just PPBool) (Just Private)) = SCSubst SCShared3p (SCArray 1 SCBool)      (SCArray 2 SCBool)
scSubstType (Ann (Just PPBool) (Just Public))  = SCSubst SCPublic   (SCArray 1 SCBool)      (SCArray 2 SCBool)
scSubstType (Ann (Just PPInt)  (Just Private)) = SCSubst SCShared3p (SCArray 1 SCInt32)     (SCArray 2 SCInt32)
scSubstType (Ann (Just PPInt)  (Just Public))  = SCSubst SCPublic   (SCArray 1 SCInt32)     (SCArray 2 SCInt32)
scSubstType (Ann (Just PPStr)  (Just Private)) = SCSubst SCShared3p (SCArray 1 SCXorUInt32) (SCArray 2 SCXorUInt8)
scSubstType (Ann (Just PPStr)  (Just Public))  = SCSubst SCPublic   (SCArray 1 SCUInt32)    (SCArray 2 SCUInt8)

dynamicColD i = SCDynamic  (Just i)
dynamicColT i = SCDynamicT (Just i)
dynamicColS i = SCDynamicS (Just i)
dynamicColumn i = SCColumn (dynamicColD i) (dynamicColT i) (dynamicColS i)
dynamicSubst  i = SCSubst  (dynamicColD i) (dynamicColT i) (dynamicColS i)

packj s j = pack (s ++ show j)

extPredDecl :: DBClause -> StructDecl
extPredDecl dbc = struct (SCTemplateDecl Nothing) (pack p) (y:ys)
  where
    p  = name dbc
    xs = vars dbc
    y  = variable SCPublic (SCArray 1 SCBool) "b"
    ys = map (\(Var pptype x) -> variable SCPublic (scColType pptype) (pack x)) xs

intPredDecl :: Expr -> StructDecl
intPredDecl (Pred _ p xs) = struct template (pack p) (y:ys)
  where
    is = [0..length xs - 1]
    template = SCTemplateDecl $ Just ((SCDynamic Nothing) : (map dynamicColD is), map dynamicColT is ++ map dynamicColS is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) "b"
    ys = zipWith (\(Var pptype _) i -> variable SCPublic (dynamicColumn i) (pack ("arg" ++ show i))) xs is

intPredExt :: Expr -> FunctionDecl
intPredExt (Pred _ p xs) = function template returnType fname fargs fcode
  where
    is = [0..length xs - 1]
    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = pack ("extend_" ++ show p)
    fargs = [variable SCPublic (dynamicColT 1) "table", variable SCPublic SCUInt "m", variable SCPublic SCUInt "mi", variable SCPublic SCUInt "ni"]
    fcode = [VarAsgn "table.b" (SCFunCall "extendColumn" [SCVarName "table.b", SCVarName "m", SCVarName "mi", SCVarName "ni"])]
            ++ map (\i -> VarAsgn ("table.arg" ++ show i) (SCFunCall "extendColumn" [SCVarName ("table.arg" ++ show i), SCVarName "m", SCVarName "mi", SCVarName "ni"])) is
            ++ [Return (SCVarName "table")]

intPredCat :: Expr -> FunctionDecl
intPredCat (Pred _ p xs) = function template returnType fname fargs fcode
  where
    is = [0..length xs - 1]
    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1, dynamicColT 2])
    returnType = Just $ dynamicColT 0
    fname = pack ("cat_" ++ show p)
    fargs = [variable SCPublic (dynamicColT 1) "t1", variable SCPublic (dynamicColT 2) "t2"]
    fcode = [VarDecl $ variable SCPublic (SCDynamicT (Just 0)) "t0",
             VarAsgn "t0.b" (SCFunCall "myCat" [SCVarName "t1.b", SCVarName "t2.b"])]
            ++ map (\i -> VarAsgn ("t0.arg" ++ show i) (SCFunCall "myCat" [SCVarName ("t1.arg" ++ show i), SCVarName ("t2.arg" ++ show i)])) is
            ++ [Return (SCVarName "t0")]

intPredGet :: [Int] -> Expr -> FunctionDecl
intPredGet ls (Pred _ p xs) = function template returnType fname fargs fcode
  where
    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = pack ("getTable_" ++ show p)
    fargs = [variable SCPublic (dynamicColT 1) "args"]
    fcode = map (\l -> VarInit (variable SCPublic (SCDynamicT (Just 0)) (pack ("res" ++ show l))) (SCFunCall ("goal_" ++ p ++ "_" ++ show l) [SCVarName "args"])) ls
            ++ [VarDecl $ variable SCPublic (SCDynamicT (Just 0)) "res"]
            ++ map (\l -> VarAsgn "res" (SCFunCall ("cat_" ++ p) [SCVarName "res", (SCFunCall ("goal_" ++ p ++ "_" ++ show l) [SCVarName "args"])])) ls
            ++ [Return (SCVarName "res")]

defaultGoal :: FunctionDecl
defaultGoal = mainFun $
  [ VarDecl $ variable SCShared3p SCUInt32 "dummy"
  , Comment "TODO: state your own goal here"
  , FunCall "publish" 
      [ SCConstStr "NOTICE: no goal specified in the main function"
      , SCConstBool False
      ]
  ]

concreteGoal :: [Expr] -> [Expr] -> Expr -> FunctionDecl
concreteGoal xs ys (Pred _ p zs) = mainFun $
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColType xtype) (pack ("arg" ++ show i))) (SCFunCall "constColumn" [SCFunCall "argument" [SCConstStr x]])) setX ++
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColType xtype) (pack ("arg" ++ show i))) (SCFunCall "constColumn" [SCVarName x])) setC ++
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColType xtype) (pack ("arg" ++ show i))) (SCFunCall "freeVarColumn" [])) setY ++
  [ VarDecl $ variable SCPublic (SCStruct ("table" ++ p) (SCTemplateUse $ Just ([SCPublic], inTypes))) "args"
  , VarAsgn "args.b" (SCFunCall "trueColumn" [])
  ] ++
  map (\i -> VarAsgn ("args.arg" ++ show i) (SCVarName ("arg" ++ show i))) is ++
  [ VarInit (variable SCPublic (SCStruct ("table" ++ p) (SCTemplateUse $ Just ([SCPublic], outTypes))) "res") (SCFunCall ("getTable_" ++ p) [SCVarName "args"])
  , VarInit (variable SCPublic SCUInt32 "n") (SCFunCall "declassifyIfNeed" [SCFunCall "sum" [SCTypeCast SCUInt32 (SCVarName "res.b")]])
  , VarInit (variable outDomain (SCArray 1 SCUInt32) "pi") (SCFunCall ("lpShuffle") [SCVarName "res.b"])
  ] ++
  map (\(Var _ zi,i) -> FunCall "publishArg" 
               [ SCConstInt i
               , SCConstStr zi
               , (SCFunCall "filterTrue" [SCVarName "pi", SCVarName "n", SCVarName ("res.arg" ++ show i ++ ".val")])
               , (SCFunCall "filterTrue" [SCVarName "pi", SCVarName "n", SCVarName ("res.arg" ++ show i ++ ".str")])
               ]
  ) setY
  where
    is = [1..length zs]
    (setZ,setC) = partition (\(zi,_) -> case zi of {Var _ _ -> True; _ -> False}) (zip zs is)
    setX = [(zi,i) | (zi@(Var _ z),i) <- setZ, (Var _ x) <- xs, z == x]
    setY = [(zi,i) | (zi@(Var _ z),i) <- setZ, (Var _ y) <- ys, z == y]
    inTypes  = [scColType ztype | (Var ztype _, _) <- setZ]

    -- TODO this is temporary, we actually need to apply information about types of all rules here!
    outTypes = map (scColType . getAnn) zs
    outDomain = SCShared3p

ruleFuns :: [Rule] -> [FunctionDecl]
ruleFuns rules = zipWith ruleFun rules [1..]

ruleFun :: Rule -> Int -> FunctionDecl
ruleFun r j = function template returnType fname args body
  where
    rhead = r ^. ruleHead
    rtail = r ^. ruleTail
    (p,zs) = case rhead of
        (Pred _ p zs) -> (p,zs)
        _             -> error $ "rule head must be a single predicate"
    -- TODO do we actually need dynamic type here?
    inTypes    = map (scColType . getAnn) zs
    template   = SCTemplateDecl $ Just ([SCDynamic Nothing], (SCDynamicT Nothing) : inTypes)
    returnType = Just $ SCStruct ("table" ++ p) (SCTemplateUse $ Just ([SCDynamic Nothing], inTypes))
    fname      = pack $ "goal_" ++ p ++ "_" ++ show j
    args       = [SCVar SCPublic (SCDynamicT Nothing) "args"]
    body       = ruleBody p zs rtail

ruleBody :: String -> [Expr] -> Expr -> [Statement]
ruleBody p zs q = decl ++ ruleComputation ++ setArgs ++ closing
  where
    qs = andsToList q
    ts = map (\(qi,i) -> (predicateName qi, i)) $ filter (\(qi,i) -> case qi of {Pred _ _ _ -> True; _ -> False}) $ zip qs [1..]
    -- TODO do we actually need dynamic type here?
    inTypes      = map (scColType . getAnn) zs
    getRowCounts = map (\(ti,i) -> VarInit (variable SCPublic SCUInt (pack ("m" ++ show i))) (SCFunCall "tdbGetRowCount" [SCVarName "ds", SCConstStr ti])) ts
    getNs        = map (\(_, i) -> VarInit (variable SCPublic SCUInt (pack ("n" ++ show i))) (SCDiv (SCVarName "m") (SCProd (map (\i -> SCVarName ("m" ++ show i)) [0..i]))) ) ts
    getTables    = map (\(ti,i) -> VarInit (variable SCPublic (SCStruct ("table_" ++ ti) (SCTemplateUse Nothing)) (pack ("table" ++ show i))) (SCFunCall ("getTable_" ++ ti) [SCVarName "ds", SCVarName "m", SCVarName ("m" ++ show i), SCVarName ("n" ++ show i)])) ts
    setArgs      = concat $ zipWith (\z i -> [ VarInit (variable SCPublic (dynamicSubst i) (pack ("theta0" ++ show i))) (SCFunCall "unify" [SCVarName ("table0.arg" ++ show i), exprToSC z])
                                    , VarInit (variable (SCDynamic (Just i)) (SCArray 1 SCBool) (pack ("b0" ++ show i))) (SCVarName ("theta0" ++ show i ++ ".b"))
                                    , VarInit (variable SCPublic (dynamicColumn i) (pack ("arg" ++ show i))) (SCVarName ("theta0" ++ show i ++ ".z"))
                                    ] ) zs [0..]
    asgnArgs     = map (\i -> VarAsgn ("result.arg" ++ show i) (SCVarName ("arg" ++ show i))) [0..length zs - 1]
    ruleComputation = concat $ zipWith predToSC qs [1..]
    closing =
      [ FunCall "tdbCloseConnection" [SCVarName "ds"]
      , VarInit (variable (SCDynamic Nothing) (SCArray 1 SCBool) "b") (SCAnd (SCVarName "table0.b") (SCAnd (SCAnds (map (\(_,i) -> SCVarName ("b0" ++ show i)) ts)) (SCAnds (map (\i -> SCVarName ("b" ++ show i)) [1..length qs]))))
      , VarDecl (variable SCPublic (SCStruct ("table" ++ p) (SCTemplateUse $ Just ([SCDynamic Nothing], inTypes))) "result")
      , VarAsgn "result.b" (SCVarName "b")
      ] ++ setArgs ++
      [Return (SCVarName "result")]
    decl =
      [ VarInit (variable SCPublic SCUInt "m") (SCConstInt 0)
      , VarInit (variable SCPublic SCString "ds") (SCConstStr "DS1")
      , FunCall "tdbOpenConnection" [SCVarName "ds"]
      ] ++ getRowCounts ++
      [ VarAsgn "m" $ SCProd (SCVarName "m0" : (map (\(_,i) -> SCVarName ("m" ++ show i)) ts))
      ] ++ getNs ++
      [ VarInit (variable SCPublic (SCDynamicT Nothing) "table0") (SCFunCall ("extend_" ++ p) [SCVarName "args", SCVarName "m", SCVarName "m0", SCVarName "n0"])
      ] ++ getTables

predToSC :: Expr -> Int -> [Statement]
predToSC e j =
  case e of

    ConstBool  _ b -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (SCFunCall "reshape" [SCConstBool b, SCVarName "m"])]

    -- here we assume that constants are not passed directly as arguments, but a comparison is added later
    Pred _ p zs    -> zipWith (\z i -> VarInit (variable SCPublic (scColType (getAnn z)) (packj "arg" i)) (SCVarName ("table" ++ show j ++ ".arg" ++ show i))) zs [0..]

    Not  _ (Pred _ p zs) -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName ("table" ++ show j ++ ".arg" ++ show i))) zs [0..]))]

    Lt   _ _ _ -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (exprToSC e)]
    Le   _ _ _ -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (exprToSC e)]
    Eq   _ _ _ -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (exprToSC e)]
    Gt   _ _ _ -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (exprToSC e)]
    Ge   _ _ _ -> [VarInit (variable SCPublic (SCArray 1 SCBool) (packj "b" j)) (exprToSC e)]

    _ -> error $ "Cannot determine truthness for " ++ show e

exprToSC :: Expr -> SCExpr
exprToSC e =
  case e of 
    ConstInt   _ c -> SCFunCall "constColumn" [SCConstInt c, SCVarName "m"]
    ConstFloat _ c -> SCFunCall "constColumn" [SCConstFloat c, SCVarName "m"]
    ConstStr   _ c -> SCFunCall "constColumn" [SCConstStr c, SCVarName "m"]
    ConstBool  _ c -> SCFunCall "constColumn" [SCConstBool c, SCVarName "m"]

    -- TODO should we distinguish between DB and Free variables?
    DBCol _ x -> SCVarName x
    Var   _ x -> SCVarName x

    Not  _ e0 -> SCFunCall "bop" [SCConstStr "not", exprToSC e0]
    Neg  _ e0 -> SCFunCall "aop" [SCConstStr "neg", exprToSC e0]
    Inv  _ e0 -> SCFunCall "aop" [SCConstStr "inv", exprToSC e0]

    Div  _ e1 e2 -> SCFunCall "aop" [SCConstStr "div", exprToSC e1, exprToSC e2]
    Sub  _ e1 e2 -> SCFunCall "aop" [SCConstStr "sub", exprToSC e1, exprToSC e2]
    Lt   _ e1 e2 -> SCFunCall "bop" [SCConstStr "lt", exprToSC e1, exprToSC e2]
    Le   _ e1 e2 -> SCFunCall "bop" [SCConstStr "le", exprToSC e1, exprToSC e2]
    Eq   _ e1 e2 -> SCFunCall "bop" [SCConstStr "eq", exprToSC e1, exprToSC e2]
    Gt   _ e1 e2 -> SCFunCall "bop" [SCConstStr "gt", exprToSC e1, exprToSC e2]
    Ge   _ e1 e2 -> SCFunCall "bop" [SCConstStr "ge", exprToSC e1, exprToSC e2]
    Mul  _ e1 e2 -> SCFunCall "aop" [SCConstStr "mul", exprToSC e1, exprToSC e2]
    Add  _ e1 e2 -> SCFunCall "aop" [SCConstStr "add", exprToSC e1, exprToSC e2]
    Min  _ e1 e2 -> SCFunCall "aop" [SCConstStr "min", exprToSC e1, exprToSC e2]
    Max  _ e1 e2 -> SCFunCall "aop" [SCConstStr "max", exprToSC e1, exprToSC e2]
    And  _ e1 e2 -> SCFunCall "bop" [SCConstStr "and", exprToSC e1, exprToSC e2]
    Or   _ e1 e2 -> SCFunCall "bop" [SCConstStr "or", exprToSC e1, exprToSC e2]

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


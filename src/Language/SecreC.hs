{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC
  ( secrecCode
  , csvImportCode
  ) where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Control.Lens hiding(Empty)

import Data.List
import qualified Data.Set as S

import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc

import Annotation
import qualified DatalogProgram as DP
import DBClause
import Expr
import Rule
import Table(getTableData)

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

angled :: [Doc ann] -> Doc ann
angled prettyContent = encloseSep (langle <> space) (rangle <> space) comma prettyContent

data SCType
  = SCUInt32
  | SCUInt
  | SCUInt8
  | SCXorUInt32
  | SCXorUInt8
  | SCInt32
  | SCFloat32
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
  pretty SCFloat32   = "float32"
  pretty SCUInt32    = "uint32"
  pretty SCUInt      = "uint"
  pretty SCUInt8     = "uint8"
  pretty SCXorUInt32 = "xor_uint32"
  pretty SCXorUInt8  = "xor_uint8"
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
  | SCConstArr   [SCExpr]
  | SCConstAny   String
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
  pretty (SCConstFloat x) = pretty x
  pretty (SCConstStr x)   = dquotes $ pretty x
  pretty (SCConstBool True)  = "true"
  pretty (SCConstBool False) = "false"
  pretty (SCConstArr xs)  = if length xs > 0 then
                                lbrace <> (hsep . punctuate ",") (pretty <$> xs) <> rbrace
                            else
                                pretty $ funReshape [SCConstInt 0, SCConstInt 0]
  pretty (SCConstAny x)   = pretty x
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

-- some shorthand notation
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

-- some variable/function names that are used multiple times
nameInTableStruct p    = pack $ "in_" ++ p
nameOutTableStruct p   = pack $ "out_" ++ p
nameGetTableStruct p = pack $ "getTable_" ++ p
nameDedup p          = pack $ "deduplicate_" ++ p
nameTableCat p       = pack $ "cat_" ++ p
nameTablePermute p   = pack $ "permute_" ++ p
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
nameArgs i = pack $ "args" ++ show i
nameRes  i = pack $ "res" ++ show i
nameResUn i = pack $ "resUnique" ++ show i
nameTheta i = pack $ "theta" ++ show i
nameTable i = pack $ "table" ++ show i

-- some functions that are already defined in SecreC
funExtCol   = SCFunCall "extendColumn"
funGetDBCol = SCFunCall "getDBColumn"
funCat      = SCFunCall "myCat"
funPermute  = SCFunCall "applyPermutation"
funReshape  = SCFunCall "reshape"
funConstIntCol   = SCFunCall "constIntColumn"
funConstFloatCol = SCFunCall "constFloatColumn"
funConstBoolCol  = SCFunCall "constBoolColumn"
funConstStrCol   = SCFunCall "constStrColumn"
funConstCol      = SCFunCall "constColumn"
funTrueCol  = SCFunCall "trueColumn"
funFreeVCol = SCFunCall "freeVarColumn"
funColSize  = SCFunCall "colSize"
funUnify    = SCFunCall "unify"
funSize     = SCFunCall "size"
funSum      = SCFunCall "sum"
funGetArg   = SCFunCall "argument"
funShuffle  = SCFunCall "lpShuffle"
funArithOp  = SCFunCall "aop"
funBoolOp   = SCFunCall "bop"
funFindRep  = SCFunCall "findRepeating"
funCopyCol  = SCFunCall "copyColumn"
funCountSort = SCFunCall "countSortPermutation"
funQuickSort = SCFunCall "quickSortPermutation"

funAggr s         = SCFunCall (pack (s ++ "_filter"))
funFilterTrue     = SCFunCall "filterTrue"
funDeclassify     = SCFunCall "declassifyIfNeed"
funTdbGetRowCount = SCFunCall "tdbGetRowCount"

funPublishCol         = FunCall "publishCol"
funPublishVal         = FunCall "publishVal"
funCreateTable        = FunCall "createTable"
funWriteToTable       = FunCall "writePublicToTable"
funTdbOpenConnection  = FunCall "tdbOpenConnection"
funTdbCloseConnection = FunCall "tdbCloseConnection"

-- use this Sharemind dataset by default
strDataset = SCConstStr "DS1"

-- type rewrite function
scDomain :: Maybe Int -> PPDomain -> SCDomain
scDomain _ Private = SCShared3p
scDomain _ Public  = SCPublic
scDomain i Unknown = SCDynamic i

scDomainFromAnn :: Ann -> SCDomain
scDomainFromAnn ann = scDomain Nothing (ann ^. domain)

scVarType :: Ann -> (SCDomain, SCType)
scVarType ann =
  let dom    = ann ^. domain in
  let dtype  = ann ^. annType in

  let scDom = scDomain Nothing dom in
  let sctype = case (dtype, dom) of
          (PPBool, _) -> SCBool
          (PPInt,  _) -> SCInt32
          (PPStr,  Private) -> SCArray 1 SCXorUInt8
          (PPStr,  Public)  -> SCString
          (PPStr,  Unknown) -> error $ "cannot determine data type for a string of unknown domain"
          (PPFloat, _)      -> SCFloat32
          _                 -> SCDynamicT Nothing
  in (scDom, sctype)

scStructType :: (SCDomain -> SCType -> SCType -> SCType) -> Maybe Int -> Ann -> SCType
scStructType f i ann =
  let dom    = ann ^. domain in
  let dtype  = ann ^. annType in
  case (dtype, dom) of
      (PPBool, _)       -> f (scDomain i dom) SCBool  SCBool
      (PPInt,  _)       -> f (scDomain i dom) SCInt32 SCInt32
      (PPStr,  Private) -> f (scDomain i dom) SCXorUInt32 SCXorUInt8
      (PPStr,  Public)  -> f (scDomain i dom) SCUInt32    SCUInt8
      (PPStr,  Unknown) -> error $ "cannot determine data type for a string of unknown domain"
      (PPFloat, _)      -> f (scDomain i dom) SCFloat32 SCFloat32
      _                 -> f (scDomain i dom) (SCDynamicT i) (SCDynamicS i)

scStructPrivateType :: (SCDomain -> SCType -> SCType -> SCType) -> Maybe Int -> Ann -> SCType
scStructPrivateType f i ann =
  let dtype  = ann ^. annType in
  case dtype of
      PPBool -> f SCShared3p SCBool  SCBool
      PPInt  -> f SCShared3p SCInt32 SCInt32
      PPStr  -> f SCShared3p SCXorUInt32 SCXorUInt8
      PPFloat -> f SCShared3p SCFloat32 SCFloat32
      _      -> f SCShared3p (SCDynamicT i) (SCDynamicS i)

scColTypeI :: Int -> Ann -> SCType
scColTypeI i = scStructType SCColumn (Just i)

scColPrivateTypeI :: Int -> Ann -> SCType
scColPrivateTypeI i = scStructPrivateType SCColumn (Just i)

scColType :: Ann -> SCType
scColType = scStructType SCColumn Nothing

scColPrivateType :: Ann -> SCType
scColPrivateType = scStructPrivateType SCColumn Nothing

scConstType :: Expr -> SCExpr
scConstType (ConstInt   _ c) = SCConstInt c
scConstType (ConstFloat _ c) = SCConstFloat c
scConstType (ConstBool  _ c) = SCConstBool c
scConstType (ConstStr   _ c) = SCConstStr c
scConstType (Attribute  _ c) = SCConstStr c
scConstType e                 = error $ "Expecting a constant, not " ++ show e

dynamicColD i = SCDynamic  (Just i)
dynamicColT i = SCDynamicT (Just i)
dynamicColS i = SCDynamicS (Just i)
dynamicColumn i = SCColumn (dynamicColD i) (dynamicColT i) (dynamicColS i)
dynamicSubst  i = SCSubst  (dynamicColD i) (dynamicColumn i)

---------------------------------

-- all bounded variables in predicate head are inputs
-- all free variables in predicate head are outputs
partitionInputsOutputs :: [Expr] -> ([(Expr,Int)], [(Expr,Int)])
partitionInputsOutputs zs =
    let is = [0..length zs-1] in
    partition (\(z,_) -> z ^. annotation ^. annBound) $ zip zs is

--------------------------------------------------
-- convert a program to SecreC (transformation S^P)
secrecCode :: DP.DatalogProgram -> SCProgram
secrecCode dp = program $

  header
  ++ map (Struct . extPredDecl) extPreds
  ++ map (Funct . extPredGet) extPreds
  ++ map Struct (zipWith intPredInDecl intPredPs intPredXss)
  ++ map Struct (zipWith intPredOutDecl intPredPs intPredYss)
  ++ map Funct (zipWith intPredExt intPredPs intPredXss)
  ++ map Funct (zipWith ruleToSC rules [0..])
  ++ map Funct (zipWith intPredCat intPredPs intPredYss)
  ++ map Funct (zipWith intPredPermute intPredPs intPredYss)
  ++ map Funct (zipWith3 intPredGet lss intPredPs intPredYss)
  ++ map Funct (zipWith intPredDedup intPredPs intPredYss)
  ++ [Funct goal]
 where
   rules = dp ^. DP.dpRules
   goal  = concreteGoal dp
   extPreds = dp ^.. DP.dpDBClauses
   (intPredPs, intPredXss, intPredYss) = unzip3 $ nub 
                                         $ map (\p -> let zs = predicateVars p in
                                                      let (xs',ys') = partitionInputsOutputs zs in
                                                      let xs = map snd xs' in
                                                      let ys = map snd ys' in
                                                      (p ^. predName, xs, ys)) $ map (\r -> r ^. ruleHead) rules

   lss = [ls | r <- rules
               , let ls' = zipWith (\r' l -> let p = ruleName r' in
                                             let boolType = scDomainFromAnn (ruleAnn r') in
                                             let argTypes = map scColType (ruleSchema r') in
                                             (if ruleName r == p then l else -1, (boolType, argTypes))
                                   ) rules [0..]
               , let ls = filter ((>= 0) . fst) ls']

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

extPredDecl :: DBClause -> StructDecl
extPredDecl dbc = struct (SCTemplateDecl Nothing) (nameOutTableStruct p) (y:ys)
  where
    p  = name dbc
    xs = vars dbc
    y  = variable SCPublic (SCArray 1 SCBool) nameBB
    is = [0..length xs - 1]
    ys = zipWith (\x i -> case x of {(Attribute pptype _) -> variable SCPublic (scColTypeI i pptype) (nameArg i) ; _ -> error $ "Expected an attribute, but got " ++ show x}) xs is

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
    returnType = Just $ SCStruct (nameOutTableStruct p) (SCTemplateUse Nothing)
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCString ds, variable SCPublic SCUInt m, variable SCPublic SCUInt mi, variable SCPublic SCUInt ni]
    fbody = [ VarDecl $ variable SCPublic (SCStruct (nameOutTableStruct p) (SCTemplateUse Nothing)) result
            , VarAsgn (nameTableBB result) (funTrueCol [SCVarName m])
            ]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funGetDBCol [SCVarName ds, SCConstStr p, SCConstInt i, SCVarName m, SCVarName mi, SCVarName ni])) is
            ++ [Return (SCVarName result)]

intPredInDecl :: String -> [Int] -> StructDecl
intPredInDecl p is = struct template (nameInTableStruct p) (y:ys)
  where
    template = SCTemplateDecl $ Just ([SCDynamic Nothing], map dynamicColT is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) nameBB
    ys = map (\i -> variable SCPublic (SCDynamicT (Just i)) (nameArg i)) is

intPredOutDecl :: String -> [Int] -> StructDecl
intPredOutDecl p is = struct template (nameOutTableStruct p) (y:ys)
  where
    template = SCTemplateDecl $ Just ([SCDynamic Nothing], map dynamicColT is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) nameBB
    ys = map (\i -> variable SCPublic (SCDynamicT (Just i)) (nameArg i)) is


intPredExt :: String -> [Int] -> FunctionDecl
intPredExt p is = function template returnType fname fargs fbody
  where
    result = "result"
    m  = "m"
    mi = "mi"
    ni = "ni"

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameTableExt p
    fargs = [variable SCPublic (dynamicColT 1) result, variable SCPublic SCUInt m, variable SCPublic SCUInt mi, variable SCPublic SCUInt ni]
    fbody = [VarAsgn (nameTableBB result) (funExtCol [SCVarName (nameTableBB result), SCVarName m, SCVarName mi, SCVarName ni])]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funExtCol [SCVarName (nameTableArg result i), SCVarName m, SCVarName mi, SCVarName ni])) is
            ++ [Return (SCVarName result)]


intPredCat :: String -> [Int] -> FunctionDecl
intPredCat p is = function template returnType fname fargs fbody
  where
    input1 = "t1"
    input2 = "t2"
    result = "t0"

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1, dynamicColT 2])
    returnType = Just $ dynamicColT 0
    fname = nameTableCat p
    fargs = [variable SCPublic (dynamicColT 1) input1, variable SCPublic (dynamicColT 2) input2]
    fbody = [VarDecl $ variable SCPublic (SCDynamicT (Just 0)) result,
             VarAsgn (nameTableBB result) (funCat [SCVarName (nameTableBB input1), SCVarName (nameTableBB input2)])]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funCat [SCVarName (nameTableArg input1 i), SCVarName (nameTableArg input2 i)])) is
            ++ [Return (SCVarName result)]

intPredGet :: [(Int, (SCDomain, [SCType]))] -> String -> [Int] -> FunctionDecl
intPredGet ls0 p is = function template returnType fname fargs fbody
  where
    ds = "ds"
    input  = "args"
    result = "result"

    --we only need to put output columns into the template
    ls1 = map (\(l,(bt,at)) -> (l, (bt, map fst $ filter (\(_, i) -> elem i is) $ zip at [0..length at-1]))) ls0
    ls  = map (\(l,(bt,at)) -> (l, SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([bt], at)))) ls1

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCString ds, variable SCPublic (dynamicColT 1) input]
    fbody = [ VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)]
            ++ map (\(l,lt) -> VarInit (variable SCPublic lt (nameIndex result l)) (SCFunCall (nameGoalComp p l) [SCVarName ds, SCVarName input])) ls
            ++ map (\(l,_)  -> VarAsgn result $ SCFunCall (nameTableCat p) [SCVarName result, SCVarName (nameIndex result l)]) ls
            ++ [Return (SCVarName result)]

intPredPermute :: String -> [Int] -> FunctionDecl
intPredPermute p is = function template returnType fname fargs fbody
  where
    table  = "t"
    result = "result"
    pi     = "pi"

    template = SCTemplateDecl $ Just ([SCDynamic Nothing], [SCDynamicT Nothing, SCDynamicT (Just 0), SCDynamicT (Just 1)])
    returnType = Just $ SCDynamicT (Just 0)
    fname = nameTablePermute p
    fargs = [variable SCPublic (SCDynamicT (Just 1)) table, variable (SCDynamic Nothing) (SCArray 1 (SCDynamicT Nothing)) pi]
    fbody = [ VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)
            , VarAsgn (nameTableBB result) (funPermute [SCVarName (nameTableBB table), SCVarName pi])]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funPermute [SCVarName (nameTableArg table i), SCVarName pi])) is
            ++ [Return (SCVarName result)]

-- TODO this works correctly as far as we pass a single one choice of inputs (which is the case so far)
-- we would need piecewise deduplication otherwise
intPredDedup :: String -> [Int] -> FunctionDecl
intPredDedup p is = function template returnType fname fargs fbody
  where
    pi = "pi"
    table  = "t"
    result = "result"

    template = SCTemplateDecl $ Just ([], [SCDynamicT (Just 0), SCDynamicT (Just 1)])
    returnType = Just $ SCDynamicT (Just 0)
    fname = nameDedup p
    fargs = [variable SCPublic (SCDynamicT (Just 1)) table]

    fbody = [ VarDecl (variable SCShared3p (SCArray 1 SCUInt32) pi)
            , VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)
            , VarAsgn (nameTableBB result) (SCVarName (nameTableBB table))]
            ++ map (\i -> VarAsgn (nameTableArg result i) (funCopyCol [SCVarName (nameTableArg table i)])) is ++
            [VarAsgn pi (funCountSort [SCVarName (nameTableBB result)])
            , VarAsgn result (SCFunCall (nameTablePermute p) [SCVarName result, SCVarName pi])]

            ++ concat (map (\i -> [ VarAsgn pi (funQuickSort [SCVarName (nameTableArg result i)])
                                   , VarAsgn result (SCFunCall (nameTablePermute p) [SCVarName result, SCVarName pi])]
                      ) is)
            ++
            if length is > 0 then
                let (r:rs) = map (\i -> funFindRep [SCVarName (nameTableArg result i)]) is in
                [ VarAsgn (nameTableBB result) (SCAnd (SCVarName (nameTableBB result)) (SCNot (foldr (\x y -> SCAnd x y) r rs)))
                , Return (SCVarName result)]
            else
                [Return (SCVarName result)]

--------------------------------------------------
-- convert a predicate to SecreC (transformation S^G)
concreteGoal :: DP.DatalogProgram -> FunctionDecl
concreteGoal dp = mainFun $

  -- get user inputs
  map (\(Var xtype x) -> let (xdom,xsctype) = scVarType xtype in VarInit (variable xdom xsctype (pack x)) (funGetArg [SCConstStr x])) xs ++

  -- establish database connection
  [ VarInit (variable SCPublic SCString ds) strDataset
  , funTdbOpenConnection [SCVarName ds]]

  -- construct a table that contains results for goalPred

  ++ (prepareGoal ds xnames fullGoal j) ++

  -- close connection
  [funTdbCloseConnection [SCVarName ds]

  -- shuffle the results and leave only those whose truth bit is 1
  , VarInit (variable SCPublic SCUInt32 n) (funDeclassify [funSum [SCTypeCast SCUInt32 (SCVarName (nameB j))]])
  , VarInit (variable SCShared3p (SCArray 1 SCUInt32) pi) (funShuffle [SCVarName (nameB j)])
  ] ++

  zipWith (\y i -> funPublishCol
                       [ SCConstInt i
                       , SCConstStr y
                       , (funFilterTrue [SCVarName pi, SCVarName n, SCVarName (pack y)])
                       ]

  -- TODO this is for testing aggregations, remove after we implement parsing aggregations
  -- ) ["Y"] [0]
  ) ynames [0..length ynames-1]

  where
    xs       = dp ^.. DP.inputs
    ys       = dp ^.. DP.outputs
    fullGoal = dp ^.  DP.dpFullGoal
    ds       = "ds"
    pi       = "pi"
    n        = "n"

    -- a dummy index (not important if we have one statement in the goal)
    j = 1

    xnames = S.fromList $ map (\(Var _ x) -> x) xs
    ynames =              map (\(Var _ y) -> y) ys
    -- since we added de-duplication, the final bits will be private unless everything is public (and we would not need secure MPC in the latter case)
    outDomain = SCShared3p --scDomainFromAnn (goalPred ^. annotation)


--------------------------------------------------
-- convert a rule (a Horn Clause) to SecreC function (transformation S^C)
ruleToSC :: Rule -> Int -> FunctionDecl
ruleToSC r j = function template resTableType fname fargs fbody
  where
    ds    = "ds"
    input = "args"
    rhead = r ^. ruleHead
    rtail = r ^. ruleTail
    p     = ruleName r
    ann   = ruleAnn r
    zs    = args r

    -- we assume that all bounded variables are inputs, and free variables are outputs
    (xs,ys) = partitionInputsOutputs zs

    argTypes = map (\(x,i) -> scColTypeI i (x ^. annotation)) xs
    resTypes = map (\(y,i) -> scColTypeI i (y ^. annotation)) ys

    argTableType = SCStruct (nameInTableStruct p) (SCTemplateUse $ Just ([SCPublic],   argTypes))
    resTableType = Just $ SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], resTypes))

    -- TODO template may still be needed if we allow unknown types
    -- js = map fst $ filter snd $ map (\(z,i) -> (i, case scDomainFromAnn (z ^. annotation) of {SCDynamic _ -> True; _ -> False})) xs
    -- template = SCTemplateDecl $ Just (map (SCDynamic . Just) js, (SCDynamicT Nothing) : (map (SCDynamicT . Just) js ++ map (SCDynamicS . Just) js))
    template = SCTemplateDecl Nothing
    --

    fname      = nameGoalComp p j
    fargs      = [SCVar SCPublic SCString ds, SCVar SCPublic argTableType input]
    fbody      = ruleBodyToSC ann argTableType ds input p xs ys rtail

--------------------------------------------------
-- convert body of a rule (a Horn Clause) to SecreC function (a subtransformation of S^C)
ruleBodyToSC :: Ann -> SCType -> Text -> Text -> String -> [(Expr,Int)] -> [(Expr,Int)] -> Expr -> [Statement]
ruleBodyToSC ann argTableType ds input p xs ys q =

  [ SCEmpty, Comment "compute the number of solutions in used predicates"
  , VarInit (variable SCPublic SCUInt (nameM 0)) (funSize [SCVarName (nameTableBB input)])
  ] ++ getRowCounts ++
  [ VarInit (variable SCPublic SCUInt nameMM) $ SCProd (SCVarName (nameM 0) : (map (\(_,i) -> SCVarName (nameM i)) ts))
  ] ++ getNs ++
  [ SCEmpty, Comment "extend the initial args to appropriate size"
  , VarInit (variable SCPublic argTableType inputTable) (SCFunCall (nameTableExt p) [SCVarName input, SCVarName nameMM, SCVarName (nameM 0), SCVarName (nameN 0)])
  ] ++
  [SCEmpty, Comment "evaluate all underlying predicates"] ++ getTables ++
  [SCEmpty, Comment "assign input variables"] ++ asgnInputArgs ++
  [SCEmpty, Comment "evaluate the clause body"] ++ evalBody ++

  [ SCEmpty, Comment "output the updated predicate arguments"
  , VarInit (variable (scDomainFromAnn ann) (SCArray 1 SCBool) result_b) (SCAnd (SCVarName (nameTableBB inputTable)) (SCAnds (map (\i -> SCVarName (nameB i)) [1..length qs])))

  --, VarDecl (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn [0..n-1]))) result)
  , VarDecl (variable SCPublic (SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], map (\(y,i) -> scColTypeI i (y ^. annotation)) ys))) result)
  --

  , VarAsgn (nameTableBB result) (SCVarName result_b)
  ] ++ asgnOutputArgs ++
  [Return (SCVarName result)]

  where
    inputTable = "table0"
    result   = "result"
    result_b = "b"

    asgnInputArgs  = map (\((Var xtype x),i) -> VarInit (variable SCPublic (scColType xtype) (pack x)) (SCVarName $ nameTableArg inputTable i)) xs
    asgnOutputArgs = map (\(z,i) -> VarAsgn (nameTableArg result i) (exprToSC z)) ys

    qs = andsToList q
    ts = map (\(qj,j) -> (qj ^. predName, j)) $ filter (\(qj,j) -> case qj of {Pred _ _ _ -> True; _ -> False}) $ zip qs [1..]
    ks = 0 : (map snd ts)

    getRowCounts = map (\(tk,k) -> VarInit (variable SCPublic SCUInt (nameM k)) (funTdbGetRowCount [SCVarName ds, SCConstStr tk])) ts
    getNs        = map (\k      -> let js = (filter (k >=) ks) in
                                   VarInit (variable SCPublic SCUInt (nameN k)) (SCDiv (SCVarName nameMM) (SCProd (map (\j -> SCVarName (nameM j)) js))) ) ks
    getTables    = map (\(tk,k) -> VarInit (variable SCPublic (SCStruct (nameOutTableStruct tk) (SCTemplateUse Nothing)) (nameTable k)) (SCFunCall (nameGetTableStruct tk) [SCVarName ds, SCVarName nameMM, SCVarName (nameM k), SCVarName (nameN k)])) ts

    evalBody = concat $ zipWith (\qj j -> formulaToSC ds qj j) qs [1..]


---------------------------------------------------------------------
-- construct SecreC statements computing all valuations of predicate

-- TODO this works correctly as far as we pass a single one choice of inputs (which is the case so far)
-- we would need to take into account the entire table for deduplication before aggregation
intPredToSC :: Bool -> Text -> Expr -> Int -> [Statement]
intPredToSC isSetSemantics ds (Pred ptype p zs) j =

      -- declared variables are the bounded arguments
      let dv = map (\z -> z  ^. annotation ^. annBound) zs in

      -- link inputs, outputs, and constants to indices of zs
      let is = [0..length zs - 1] in

      -- separate constants and variables
      let (setZ',setC) = partition (\(zi,_) -> case zi of {Var _ _ -> True; Attribute _ _ -> True; Hole _ -> True; _ -> False}) (zip zs is) in
      let setZ = map (\(z,i) -> case z of {Attribute zann zval -> (Var zann zval,i); _ -> (z,i)}) setZ' in

      -- all bounded variables will be inputs
      -- all free variables will be assigned in this execution
      let (setX,setY) = partition (\(z,i) -> case z of
                                                 Var _ _ -> dv !! i
                                                 _       -> False) $ setZ
      in

      -- the input table
      let argTypes = map (\(z,i) -> scColTypeI i (z ^. annotation)) setX ++ map (\(z,i) -> scColTypeI i (z ^. annotation)) setC in
      let argTableType = SCStruct (nameInTableStruct p) (SCTemplateUse $ Just ([SCPublic], argTypes)) in
      let argTableName = nameArgs j in

      -- the output table before deduplication
      let resTypes = map (\(z,i) -> scColTypeI i (z ^. annotation)) setY in
      let resDomain = scDomainFromAnn ptype in
      let resTableType = SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([resDomain],  resTypes)) in
      let resTableName = nameRes j in

      -- the output table after deduplication

      -- after deduplication, we get all-private-column table
      -- since at least one column (or the boolean condition) is private in secure computation
      -- and we even do not know which elements are duplicated
      -- TODO we can sort the private columns "piecewise within each public group" for best efficiency, keeping public columns public
      -- this may be a bit tricky to describe in a compact way in SecreC
      let resUnArgTypes = map (\(z,i) -> scColPrivateTypeI i (z ^. annotation)) setY in
      let resUnDomain = SCShared3p in
      let resUnTableType = SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([resUnDomain],  resUnArgTypes)) in
      let resUnTableName = nameResUn j in
      let something (Var ztype z,i) = VarInit (variable SCPublic (scColPrivateTypeI i ztype) (pack z)) (SCVarName (nameTableArg resUnTableName i))
          -- HACK that does something to cover the Hole case
          something (Hole _, i)     = VarInit (variable SCPublic (scColPrivateTypeI i empty) . pack $ "HOLE_" ++ show i) (SCVarName (nameTableArg resUnTableName i)) in

      map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColTypeI i xtype) (nameArg i)) (funConstCol [SCVarName (pack x)])) setX ++
            map (\(z,i)           -> VarInit (variable SCPublic ((scColTypeI i . (view annotation)) z) (nameArg i)) (funConstCol [scConstType z])) setC ++

            -- create an input data structure that corresponds to particular goal
            [ VarDecl $ variable SCPublic argTableType argTableName
            , VarAsgn (nameTableBB argTableName) (funTrueCol [])
            ] ++
            map (\(_,i) -> VarAsgn (nameTableArg argTableName i) (SCVarName (nameArg i))) setX ++
            map (\(_,i) -> VarAsgn (nameTableArg argTableName i) (SCVarName (nameArg i))) setC ++

            -- call the goal, read updated valuation of free variables
            [ VarInit (variable SCPublic resTableType resTableName) (SCFunCall (nameGetTableStruct p) [SCVarName ds, SCVarName argTableName])] ++

            if isSetSemantics then
                -- remove duplicate solutions
                [ VarInit (variable SCPublic resUnTableType resUnTableName) (SCFunCall (nameDedup p) [SCVarName resTableName])]

                -- assign the output variables
                -- everything becomes private after deduplication
                ++ map something setY
                ++ [VarInit (variable resUnDomain (SCArray 1 SCBool) (nameB j)) (SCVarName (nameTableBB resUnTableName))]
           else
                -- assign the output variables
                -- the types come from type derivation
                map (\(Var ztype z,i) -> VarInit (variable SCPublic (scColTypeI i ztype) (pack z)) (SCVarName (nameTableArg resTableName i))) setY
                ++ [VarInit (variable resDomain (SCArray 1 SCBool) (nameB j)) (SCVarName (nameTableBB resTableName))]



-- TODO this index conversion is temporary here, we need something better
ind :: Int -> Int -> Int
ind i1 i2 = i1 * 1000 + i2

---------------------------------------------------------------------
-- construct SecreC statements computing an aggregation over predicate

-- aggregation creates an intensional predicate table that will not be used anywhere else,
-- so we do not need to increase the crros product table
aggrToSC :: Text -> Expr -> Int -> [Statement]
aggrToSC ds (Aggr _ f pr@(Pred ptype p zs) e1 e2) j =

                          let is = [0..length zs-1] in

                          --extract aggregated variable name
                          -- TODO this can be generalized by defining a mapping from variables of x to indices of result table
                          let x = case e1 of
                                      Var _ x -> x
                                      _       -> error $ "aggregation over complex expressions is not supported yet"
                          in
                          -- extract the type of x
                          let aggrAnns = filter (\z -> case z of {Var _ zn -> zn == x; _ -> False}) zs in
                          let ann = if length aggrAnns > 0 then (head aggrAnns) ^. annotation
                                    else error $ "aggregation variable not found among goal arguments" in

                          let (_, dtype) = scVarType ann in

                          --extract aggregation result variable name
                          -- TODO this can be generalized by adding a subcall of formulaToSC
                          let y = case e2 of
                                      Var _ y -> y
                                      _       -> error $ "comparing aggregation result to a complex expression is not supported yet"
                          in

                          -- prepare intentional predicate table
                          -- ignore updates of internal variables of pr since they are out of scope
                          -- remove duplicates before applying aggregation
                          let j1 = ind j 0 in
                          let intPredStmts = intPredToSC True ds pr j1 in

                          -- extract the index of aggregation input variable
                          let aggrInputIndices = filter snd $ zipWith (\z i -> (i, case z of {Var _ zn -> zn == x; _ -> False})) zs is in
                          let xi = if length aggrInputIndices > 0 then fst $ head aggrInputIndices
                                   else error $ "aggregation variable " ++ show x ++ " not found in aggregation predicate " ++ show pr
                          in

                          -- TODO here we assume that y is always a fresh variable, generalize it later
                          intPredStmts ++
                          [VarInit (variable SCPublic (scColPrivateType ann) (pack y)) $ funAggr (show f) [ SCVarName (nameTableArg (nameResUn j1) xi)
                                                                                            , SCVarName (nameTableBB (nameResUn j1))
                                                                                            , funSize [SCVarName (nameTableBB (nameResUn j1))]]

                          -- although Bj has already been declared by the internal predicate, let us re-declare it
                          , VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) (funTrueCol [funColSize [SCVarName (pack y)]])]

prepareGoal :: Text -> (S.Set String) -> Expr -> Int -> [Statement]
prepareGoal ds dv goalPred j =
    case goalPred of
        -- we remove duplicates before publishing final results
        Pred{} -> intPredToSC True ds goalPred j
        Aggr{} -> aggrToSC ds goalPred j
        _      -> error $ "only a single predicate or an aggregation is supported in the goal"

--------------------------------------------------
-- convert a formula to SecreC (transformation S^F)
formulaToSC :: Text -> Expr -> Int -> [Statement]
formulaToSC ds q j =
  [SCEmpty, Comment ("q" ++ show j)] ++ formulaToSC_case q
  where
    formulaToSC_case q' = case q' of

        ConstBool ann b -> let dom = scDomainFromAnn ann in
                           [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (funReshape [SCConstBool b, SCVarName nameMM])]

        Pred ann p zs    -> let dom = scDomainFromAnn ann in

                            --add database attributes to the set of declared variables
                            let predArgNames = S.fromList $ map (unpack . nameTableArg (nameTable j)) [0..length zs - 1] in

                            -- TODO if we extract all comparisons into one expression, we can get a nicer indexation
                            --let stmts = concat $ zipWith (\z i -> formulaToSC ds (Un ann z (Var (z ^. annotation) (unpack $ nameTableArg (nameTable j) i))) (ind j i)) zs [0..] in
                            --let bb = VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ SCAnds comps in
                            --stmts ++ [bb]


                            -- TODO it would be nice to have access to data types of extensional predicates here
                            let stmts = zipWith (\z i -> formulaToSC ds (Un ann z (Var (z ^. annotation) (unpack $ nameTableArg (nameTable j) i))) (ind j i)) zs [0..] in
                            let (comps', asgns') = partition snd $ zipWith (\s z -> (s, z ^. annotation ^. annBound)) stmts zs in
                            let comps = map ((\(VarInit _ bexpr) -> bexpr) . last . fst) comps' in
                            let asgns = concat $ map (init . fst) asgns' in

                            let bb = VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ if length comps > 0 then SCAnds comps else funTrueCol [SCVarName nameMM] in
                            asgns ++ [bb]

        Not ann (Pred _ p zs) ->
            let dom = scDomainFromAnn ann in
            [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName (nameTableArg (nameTable j) i))) zs [0..]))]

        Not ann e ->
            let dom = scDomainFromAnn ann in
            let sc = exprToSC e in
            [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ SCNot sc]

        Or ann e1 e2 ->
            let dom = scDomainFromAnn ann in
            let sc1 = exprToSC e1 in
            let sc2 = exprToSC e2 in
            [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ SCOr sc1 sc2]

        And ann e1 e2 ->
            let dom = scDomainFromAnn ann in
            let sc1 = exprToSC e1 in
            let sc2 = exprToSC e2 in
            [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ SCAnd sc1 sc2]

        Lt  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')]
        Le  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')]
        Gt  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')]
        Ge  ann _ _ -> let dom = scDomainFromAnn ann in [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')]

        -- TODO: so far, we use Eq also in place of unification, so put back the first variant after updating the preprocessing
        -- Eq can only be a comparison
        -- Eq  ann _ _ -> let dom = scDomainFromAnn ann in (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')])
        Eq ann e1 e2 -> formulaToSC_case (Is ann e1 e2)

        -- Is may be a comparison as well as initialization
        -- TODO we should become more strict about e1 being an expression
        Is ann e1 e2 -> ex
                        where
                          dom   = scDomainFromAnn ann
                          bcomp = VarInit (variable dom      (SCArray 1 SCBool) (nameB j)) $ exprToSC (Eq        ann e1 e2)
                          btrue = VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) $ funTrueCol [SCVarName nameMM]

                          -- if x is a fresh variable, init x
                          ex = case e1 of
                                   (Var annx x) -> if not (e1 ^. annotation ^. annBound) then
                                                       [VarInit (variable SCPublic (scColType annx) (pack x)) (funCopyCol [exprToSC e2]), btrue]
                                                   else ey
                                   _            -> ey
                          -- if y is a fresh variable, init y
                          ey = case e2 of
                                   (Var anny y) -> if not (e2 ^. annotation ^. annBound) then
                                                       [VarInit (variable SCPublic (scColType anny) (pack y)) (funCopyCol [exprToSC e1]), btrue]
                                                   else ez
                                   _            -> ez
                          -- if both x and y are not fresh, then compare
                          ez = [bcomp]

        -- unification may be a comparison as well as initialization
        -- most likely, there will be no Un constructions due to previous optimizations
        Un ann e1 e2 -> formulaToSC_case (Is ann e1 e2)

        Aggr _ _ _ _ _ -> aggrToSC ds q' j

        _ -> error $ "Unexpected boolean expression: " ++ show q'


-- convert an expression to SecreC (transformation S^F)
exprToSC :: Expr -> SCExpr
exprToSC e =
  case e of 
    ConstInt   _ c -> funConstIntCol [SCConstInt c, SCVarName nameMM]
    ConstFloat _ c -> funConstFloatCol [SCConstFloat c, SCVarName nameMM]
    ConstStr   _ c -> funConstStrCol [SCConstStr c, SCVarName nameMM]
    ConstBool  _ c -> funConstBoolCol [SCConstBool c, SCVarName nameMM]

    -- should we distinguish between DB and Free variables? it seems that not.
    Var   _ x -> SCVarName $ pack x

    Not  _ e0 -> funBoolOp [SCConstStr "not", exprToSC e0]
    Neg  _ e0 -> SCFunCall "neg" [exprToSC e0]
    Inv  _ e0 -> SCFunCall "inv" [exprToSC e0]
    Sqrt _ e0 -> SCFunCall "apply_sqrt" [exprToSC e0]

    FDiv _ e1 e2 -> binArith "/" e1 e2
    Div  _ e1 e2 -> binArith "div" e1 e2
    Mod  _ e1 e2 -> binArith "%" e1 e2
    Sub  _ e1 e2 -> binArith "-" e1 e2
    Lt   _ e1 e2 -> funBoolOp [SCConstStr "<", exprToSC e1, exprToSC e2]
    Le   _ e1 e2 -> funBoolOp [SCConstStr "<=", exprToSC e1, exprToSC e2]
    Eq   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2]
    Un   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2]
    Gt   _ e1 e2 -> funBoolOp [SCConstStr ">", exprToSC e1, exprToSC e2]
    Ge   _ e1 e2 -> funBoolOp [SCConstStr ">=", exprToSC e1, exprToSC e2]
    Mul  _ e1 e2 -> binArith "*" e1 e2
    Add  _ e1 e2 -> binArith "+" e1 e2
    Pow  _ e1 e2 -> binArith "pow" e1 e2
    And  _ e1 e2 -> funBoolOp [SCConstStr "and", exprToSC e1, exprToSC e2]
    Or   _ e1 e2 -> funBoolOp [SCConstStr "or", exprToSC e1, exprToSC e2]

    Pred _ _ _ -> error $ "High order predicates are not supported"
    _          -> error $ "Unexpected expression: " ++ show (pretty e)
  where
    binArith s x y = funArithOp [SCConstStr s, cast x y x, cast x y y]
    -- Cast arguments to float if either one is already float
    cast :: Expr -> Expr -> Expr -> SCExpr
    cast x y z 
      | (typeof x == PPFloat || typeof y == PPFloat) && typeof z /= PPFloat
        = SCFunCall "cast_float32" [exprToSC z]
      | otherwise = exprToSC z
    typeof = view $ annotation . annType

--------------------------------------------------
-- create a script that writes data from .csv file to Sharemind database
-- WARNING! this should never be used with actual private data, as it is insecure and is meant for testing purposes only

csvImportCode :: DP.DatalogProgram -> IO (SCProgram)
csvImportCode dp = do
  let ds = "ds"
  let extPreds = dp ^.. DP.dpDBClauses
  tableData <- mapM (getTableData . name) extPreds
  return $ program $ header
                     ++ [Funct $ mainFun $
                                     [ VarInit (variable SCPublic SCString ds) strDataset
                                     , funTdbOpenConnection [SCVarName ds]]
                                     ++ concat (zipWith (tableGenerationCode ds) extPreds tableData) ++
                                     [funTdbCloseConnection [SCVarName ds]]]

tableGenerationCode :: Text -> DBClause -> [[String]] -> [Statement]
tableGenerationCode ds dbc (tableHeader:tableRows) =

  [ funCreateTable  [ SCVarName ds, SCConstStr p, SCConstArr (map  SCConstInt types), SCConstArr (map  SCConstInt domains)
                    , SCConstStr headerStr, SCConstArr (map SCConstInt hlengths)]
  , funWriteToTable [ SCVarName ds, SCConstStr p, SCConstArr (map  SCConstInt types), SCConstArr (map  SCConstInt domains)
                    , SCConstArr (map SCConstAny boolData)
                    , SCConstArr (map SCConstAny intData)
                    , SCConstArr (map SCConstAny floatData)
                    , SCConstStr strData
                    , SCConstArr (map SCConstInt vlengths)]
  ]

  where
        p  = name dbc
        xs = vars dbc
        is = [0..length xs - 1]
        types = map (\x -> let dtype  = x ^. annotation ^. annType in
                        case dtype of
                            PPBool  -> 0
                            PPInt   -> 1
                            PPFloat -> 2
                            PPStr   -> 3
                            _       -> error $ "Can only create a table for bool, int, float, string datatypes."
                ) xs

        domains = map (\x -> let dom  = x ^. annotation ^. domain in
                        case dom of
                            Public  -> 0
                            Private -> 1
                            _       -> error $ "Can only create a table for known privacy domain."
                ) xs


        hlengths  = map length tableHeader
        headerStr = concat $ tableHeader

        tableData = concat $ map (zip types) tableRows

        boolData  = map snd $ filter (\x -> fst x == 0) tableData
        intData   = map snd $ filter (\x -> fst x == 1) tableData
        floatData = map snd $ filter (\x -> fst x == 2) tableData
        strData'  = map snd $ filter (\x -> fst x == 3) tableData

        vlengths = map length strData'
        strData  = concat $ strData'


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.SecreC
  ( secrecCode
  , csvImportCode
  ) where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Relude

import Control.Lens hiding(Empty)

import qualified Data.List as L
import Data.List.Split
import qualified Data.Set as S

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Debug.Trace

import Annotation
import qualified DatalogProgram as DP
import Expr
import ExprPretty
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
  | SCText
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
  pretty SCText    = "string"

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
  pretty (SCTemplateDecl (Just (ds,xs))) = line <> "template" <> angled (pds <> pxs)
      where pds = map ("domain" <+>) $ pretty <$> ds
            pxs = map ("type" <+>)   $ pretty <$> xs
  pretty (SCTemplateUse  (Just (ds,xs))) = angled (pds <> pxs)
      where pds = pretty <$> ds
            pxs = pretty <$> xs

-- since SecreC syntax is different from Datalog's, let us define SecreC's own expression type
-- we will only need to define pretty printing for it
data SCExpr
  = SCConstInt   Int
  | SCConstFloat Float
  | SCConstStr   Text
  | SCConstBool  Bool
  | SCConstArr   [SCExpr]
  | SCConstAny   Text
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
  | Import Text
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
  = Comment Text
  -- Variable declaration
  | VarDecl SCVar
  -- Variable assignment
  | VarAsgn Text SCExpr
  -- Variable initialization
  | VarInit SCVar SCExpr
  -- Function call
  | FunCall Text [SCExpr]
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
nameInTableStruct p    = "in_" <> p
nameOutTableStruct p   = "out_" <> p
nameGetTableStruct p = "getTable_" <> p
nameDedup p          = "deduplicate_" <> p
nameTableCat p       = "cat_" <> p
nameTablePermute p   = "permute_" <> p
nameTableExt p       = "extend_" <> p
nameGoalComp p l     = "goal_" <> p <> "_" <> show l

-- fixed names that are used globally in different components
nameArg  i = "arg" <> show i
nameMM     = "m"
-- private component of the filter
nameBB     = "b"
-- public component of the filter
nameBP     = "bp"

nameTableBB  t   = t <> "." <> nameBB
nameTableArg t i = t <> "." <> (nameArg i)
nameIndex t i    = t <> show i

nameM    i = "m" <> show i
nameN    i = "n" <> show i
nameB    i = "b" <> show i
nameB0   i = "b0" <> show i
nameArgs i = "args" <> show i
nameRes  i = "res" <> show i
nameResUn i = "resUnique" <> show i
nameTheta i = "theta" <> show i
nameTable i = "table" <> show i

-- some functions that are already defined in SecreC
funExtCol :: [SCExpr] -> SCExpr
funExtCol   = SCFunCall "extendColumn"
funGetDBCol :: [SCExpr] -> SCExpr
funGetDBCol = SCFunCall "getDBColumn"
funCat :: [SCExpr] -> SCExpr
funCat      = SCFunCall "myCat"
funPermute :: [SCExpr] -> SCExpr
funPermute  = SCFunCall "applyPermutation"
funReshape :: [SCExpr] -> SCExpr
funReshape  = SCFunCall "reshape"
funConstIntCol :: [SCExpr] -> SCExpr
funConstIntCol   = SCFunCall "constIntColumn"
funConstFloatCol :: [SCExpr] -> SCExpr
funConstFloatCol = SCFunCall "constFloatColumn"
funConstBoolCol :: [SCExpr] -> SCExpr
funConstBoolCol  = SCFunCall "constBoolColumn"
funConstStrCol :: [SCExpr] -> SCExpr
funConstStrCol   = SCFunCall "constStrColumn"
funConstCol :: [SCExpr] -> SCExpr
funConstCol      = SCFunCall "constColumn"
funTrueCol :: [SCExpr] -> SCExpr
funTrueCol  = SCFunCall "trueColumn"
funColSize :: [SCExpr] -> SCExpr
funColSize  = SCFunCall "colSize"
funSize :: [SCExpr] -> SCExpr
funSize     = SCFunCall "size"
funSum :: [SCExpr] -> SCExpr
funSum      = SCFunCall "sum"
funGetArg :: [SCExpr] -> SCExpr
funGetArg   = SCFunCall "argument"
funShuffle :: [SCExpr] -> SCExpr
funShuffle  = SCFunCall "lpShuffle"
funArithOp :: [SCExpr] -> SCExpr
funArithOp  = SCFunCall "aop"
funBoolOp :: [SCExpr] -> SCExpr
funBoolOp   = SCFunCall "bop"
funFindRep :: [SCExpr] -> SCExpr
funFindRep  = SCFunCall "findRepeating"
funCopyCol :: [SCExpr] -> SCExpr
funCopyCol  = SCFunCall "copyColumn"
funCountSort :: [SCExpr] -> SCExpr
funCountSort = SCFunCall "countSortPermutation"
funQuickSort :: [SCExpr] -> SCExpr
funQuickSort = SCFunCall "quickSortPermutation"

funAggr :: Text -> [SCExpr] -> SCExpr
funAggr s         = SCFunCall $ s <> "_filter"
funFilterTrue :: [SCExpr] -> SCExpr
funFilterTrue     = SCFunCall "filterTrue"
funDeclassify :: [SCExpr] -> SCExpr
funDeclassify     = SCFunCall "declassifyIfNeed"
funTdbGetRowCount :: [SCExpr] -> SCExpr
funTdbGetRowCount = SCFunCall "tdbGetRowCount"

funPublishCol :: [SCExpr] -> Statement
funPublishCol         = FunCall "publishCol"
funCreateTable :: [SCExpr] -> Statement
funCreateTable        = FunCall "createTable"
funWriteToTable :: [SCExpr] -> Statement
funWriteToTable       = FunCall "writePublicToTable"
funTdbOpenConnection :: [SCExpr] -> Statement
funTdbOpenConnection  = FunCall "tdbOpenConnection"
funTdbCloseConnection :: [SCExpr] -> Statement
funTdbCloseConnection = FunCall "tdbCloseConnection"

-- use this Sharemind dataset by default
strDataset :: SCExpr
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
          (PPInt32,  _) -> SCInt32
          (PPStr,  Private) -> SCArray 1 SCXorUInt8
          (PPStr,  Public)  -> SCText
          (PPStr,  Unknown) -> error $ "cannot determine data type for a string of unknown domain"
          (PPFloat32, _)      -> SCFloat32
          _                 -> SCDynamicT Nothing
  in (scDom, sctype)

scStructType :: (SCDomain -> SCType -> SCType -> SCType) -> Maybe Int -> Ann -> SCType
scStructType f i ann =
  let dom    = ann ^. domain in
  let dtype  = ann ^. annType in
  case (dtype, dom) of
      (PPBool, _)       -> f (scDomain i dom) SCBool  SCBool
      (PPInt32,  _)       -> f (scDomain i dom) SCInt32 SCInt32
      (PPStr,  Private) -> f (scDomain i dom) SCXorUInt32 SCXorUInt8
      (PPStr,  Public)  -> f (scDomain i dom) SCUInt32    SCUInt8
      (PPStr,  Unknown) -> error $ "cannot determine data type for a string of unknown domain"
      (PPFloat32, _)      -> f (scDomain i dom) SCFloat32 SCFloat32
      _                 -> f (scDomain i dom) (SCDynamicT i) (SCDynamicS i)

scStructPrivateType :: (SCDomain -> SCType -> SCType -> SCType) -> Maybe Int -> Ann -> SCType
scStructPrivateType f i ann =
  let dtype  = ann ^. annType in
  case dtype of
      PPBool -> f SCShared3p SCBool  SCBool
      PPInt32 -> f SCShared3p SCInt32 SCInt32
      PPStr  -> f SCShared3p SCXorUInt32 SCXorUInt8
      PPFloat32 -> f SCShared3p SCFloat32 SCFloat32
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
scConstType e                 = error $ "Expecting a constant, not " <> show e

dynamicColT :: Int -> SCType
dynamicColT i = SCDynamicT (Just i)

---------------------------------

-- all bounded variables in predicate head are inputs
-- all free variables in predicate head are outputs
partitionInputsOutputs :: [Expr] -> ([(Expr,Int)], [(Expr,Int)])
partitionInputsOutputs zs =
    let is = [0..length zs-1] in
    L.partition (\(z,_) -> z ^. annotation ^. annBound) $ zip zs is

--------------------------------------------------
-- convert a program to SecreC (transformation S^P)
secrecCode :: DP.DatalogProgram -> SCProgram
secrecCode dp = program $

  header
  <> map (Struct . extPredDecl) extPreds
  <> map (Funct . extPredGet) extPreds
  <> map Struct (zipWith intPredInDecl intPredPs intPredXss)
  <> map Struct (zipWith intPredOutDecl intPredPs intPredYss)
  <> map Funct (zipWith intPredExt intPredPs intPredXss)
  <> map Funct (zipWith ruleToSC rules [0..])
  <> map Funct (zipWith intPredCat intPredPs intPredYss)
  <> map Funct (zipWith intPredPermute intPredPs intPredYss)
  <> map Funct (L.zipWith3 intPredGet lss intPredPs intPredYss)
  <> map Funct (zipWith intPredDedup intPredPs intPredYss)
  <> [Funct goal]
 where
   rules = dp ^. DP.dpRules
   goal  = concreteGoal dp
   extPreds = dp ^.. DP.dpDBClauses
   (intPredPs, intPredXss, intPredYss) = unzip3 $ L.nub 
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

extPredDecl :: Expr -> StructDecl
extPredDecl dbc = struct (SCTemplateDecl Nothing) (nameOutTableStruct p) (y:ys)
  where
    p  = view predName dbc
    xs = view predArgs dbc
    y  = variable SCPublic (SCArray 1 SCBool) nameBB
    is = [0..length xs - 1]
    ys = zipWith (\x i -> case x of {(Attribute pptype _) -> variable SCPublic (scColTypeI i pptype) (nameArg i) ; _ -> error $ "Expected an attribute, but got " <> show x}) xs is

extPredGet :: Expr -> FunctionDecl
extPredGet dbc = function (SCTemplateDecl Nothing) returnType fname fargs fbody
  where
    result = "result"
    ds = "ds"
    m  = "m"
    mi = "mi"
    ni = "ni"
    p = view predName dbc
    n = length $ view predArgs dbc
    is = [0..n-1]
    returnType = Just $ SCStruct (nameOutTableStruct p) (SCTemplateUse Nothing)
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCText ds, variable SCPublic SCUInt m, variable SCPublic SCUInt mi, variable SCPublic SCUInt ni]
    fbody = [ VarDecl $ variable SCPublic (SCStruct (nameOutTableStruct p) (SCTemplateUse Nothing)) result
            , VarAsgn (nameTableBB result) (funTrueCol [SCVarName m])
            ]
            <> map (\i -> VarAsgn (nameTableArg result i) (funGetDBCol [SCVarName ds, SCConstStr p, SCConstInt i, SCVarName m, SCVarName mi, SCVarName ni])) is
            <> [Return (SCVarName result)]

intPredInDecl :: Text -> [Int] -> StructDecl
intPredInDecl p is = struct template (nameInTableStruct p) (y:ys)
  where
    template = SCTemplateDecl $ Just ([SCDynamic Nothing], map dynamicColT is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) nameBB
    ys = map (\i -> variable SCPublic (SCDynamicT (Just i)) (nameArg i)) is

intPredOutDecl :: Text -> [Int] -> StructDecl
intPredOutDecl p is = struct template (nameOutTableStruct p) (y:ys)
  where
    template = SCTemplateDecl $ Just ([SCDynamic Nothing], map dynamicColT is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) nameBB
    ys = map (\i -> variable SCPublic (SCDynamicT (Just i)) (nameArg i)) is


intPredExt :: Text -> [Int] -> FunctionDecl
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
            <> map (\i -> VarAsgn (nameTableArg result i) (funExtCol [SCVarName (nameTableArg result i), SCVarName m, SCVarName mi, SCVarName ni])) is
            <> [Return (SCVarName result)]


intPredCat :: Text -> [Int] -> FunctionDecl
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
            <> map (\i -> VarAsgn (nameTableArg result i) (funCat [SCVarName (nameTableArg input1 i), SCVarName (nameTableArg input2 i)])) is
            <> [Return (SCVarName result)]

intPredGet :: [(Int, (SCDomain, [SCType]))] -> Text -> [Int] -> FunctionDecl
intPredGet ls0 p is = function template returnType fname fargs fbody
  where
    ds = "ds"
    input  = "args"
    result = "result"

    --we only need to put output columns into the template
    ls1 = map (\(l,(bt,at1)) -> (l, (bt, map fst $ filter (\(_, i) -> elem i is) $ zip at1 [0..length at1-1]))) ls0
    ls  = map (\(l,(bt,at1)) -> (l, SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([bt], at1)))) ls1

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCText ds, variable SCPublic (dynamicColT 1) input]
    fbody = [ VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)]
            <> map (\(l,lt) -> VarInit (variable SCPublic lt (nameIndex result l)) (SCFunCall (nameGoalComp p l) [SCVarName ds, SCVarName input])) ls
            <> map (\(l,_)  -> VarAsgn result $ SCFunCall (nameTableCat p) [SCVarName result, SCVarName (nameIndex result l)]) ls
            <> [Return (SCVarName result)]

intPredPermute :: Text -> [Int] -> FunctionDecl
intPredPermute p is = function template returnType fname fargs fbody
  where
    table  = "t"
    result = "result"
    pi_     = "pi"

    template = SCTemplateDecl $ Just ([SCDynamic Nothing], [SCDynamicT Nothing, SCDynamicT (Just 0), SCDynamicT (Just 1)])
    returnType = Just $ SCDynamicT (Just 0)
    fname = nameTablePermute p
    fargs = [variable SCPublic (SCDynamicT (Just 1)) table, variable (SCDynamic Nothing) (SCArray 1 (SCDynamicT Nothing)) pi_]
    fbody = [ VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)
            , VarAsgn (nameTableBB result) (funPermute [SCVarName (nameTableBB table), SCVarName pi_])]
            <> map (\i -> VarAsgn (nameTableArg result i) (funPermute [SCVarName (nameTableArg table i), SCVarName pi_])) is
            <> [Return (SCVarName result)]

-- TODO this works correctly as far as we pass a single one choice of inputs (which is the case so far)
-- we would need piecewise deduplication otherwise
intPredDedup :: Text -> [Int] -> FunctionDecl
intPredDedup p is = function template returnType fname fargs fbody
  where
    pi_ = "pi"
    table  = "t"
    result = "result"

    template = SCTemplateDecl $ Just ([], [SCDynamicT (Just 0), SCDynamicT (Just 1)])
    returnType = Just $ SCDynamicT (Just 0)
    fname = nameDedup p
    fargs = [variable SCPublic (SCDynamicT (Just 1)) table]

    fbody = [ VarDecl (variable SCShared3p (SCArray 1 SCUInt32) pi_)
            , VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)
            , VarAsgn (nameTableBB result) (SCVarName (nameTableBB table))]
            <> map (\i -> VarAsgn (nameTableArg result i) (funCopyCol [SCVarName (nameTableArg table i)])) is <>
            [VarAsgn pi_ (funCountSort [SCVarName (nameTableBB result)])
            , VarAsgn result (SCFunCall (nameTablePermute p) [SCVarName result, SCVarName pi_])]

            <> concat (map (\i -> [ VarAsgn pi_ (funQuickSort [SCVarName (nameTableArg result i)])
                                   , VarAsgn result (SCFunCall (nameTablePermute p) [SCVarName result, SCVarName pi_])]
                      ) is)
            <>
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
  map (\(Var xtype x) -> let (xdom,xsctype) = scVarType xtype in VarInit (variable xdom xsctype x) (funGetArg [SCConstStr x])) xs <>

  -- establish database connection
  [ VarInit (variable SCPublic SCText ds) strDataset
  , funTdbOpenConnection [SCVarName ds]]

  -- construct a table that contains results for goalPred

  <> (prepareGoal ds xnames fullGoal j) <>

  -- close connection
  [funTdbCloseConnection [SCVarName ds]

  -- shuffle the results and leave only those whose truth bit is 1
  , VarInit (variable SCPublic SCUInt32 n) (funDeclassify [funSum [SCTypeCast SCUInt32 (SCVarName (nameB j))]])
  , VarInit (variable SCShared3p (SCArray 1 SCUInt32) pi_) (funShuffle [SCVarName (nameB j)])
  ] <>

  zipWith (\y i -> funPublishCol
                       [ SCConstInt i
                       , SCConstStr y
                       , (funFilterTrue [SCVarName pi_, SCVarName n, SCVarName y])
                       ]

  -- TODO this is for testing aggregations, remove after we implement parsing aggregations
  -- ) ["Y"] [0]
  ) ynames [0..length ynames-1]

  where
    xs       = dp ^.. DP.inputs
    ys       = dp ^.. DP.outputs
    fullGoal = dp ^.  DP.dpFullGoal
    ds       = "ds"
    pi_       = "pi"
    n        = "n"

    -- a dummy index (not important if we have one statement in the goal)
    j = 1

    xnames = S.fromList $ map (\(Var _ x) -> x) xs
    ynames =              map (\(Var _ y) -> y) ys


--------------------------------------------------
-- convert a rule (a Horn Clause) to SecreC function (transformation S^C)
ruleToSC :: Rule -> Int -> FunctionDecl
ruleToSC r j = function template resTableType fname fargs fbody
  where
    ds    = "ds"
    input = "args"
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
    -- template = SCTemplateDecl $ Just (map (SCDynamic . Just) js, (SCDynamicT Nothing) : (map (SCDynamicT . Just) js <> map (SCDynamicS . Just) js))
    template = SCTemplateDecl Nothing
    --

    fname      = nameGoalComp p j
    fargs      = [SCVar SCPublic SCText ds, SCVar SCPublic argTableType input]
    fbody      = ruleBodyToSC ann argTableType ds input p xs ys rtail

--------------------------------------------------
-- convert body of a rule (a Horn Clause) to SecreC function (a subtransformation of S^C)
ruleBodyToSC :: Ann -> SCType -> Text -> Text -> Text -> [(Expr,Int)] -> [(Expr,Int)] -> Expr -> [Statement]
ruleBodyToSC ann argTableType ds input p xs ys q =
  [ SCEmpty, Comment "compute the number of solutions in used predicates"
  , VarInit (variable SCPublic SCUInt (nameM 0)) (funSize [SCVarName (nameTableBB input)])
  ] <> getRowCounts <>
  [ VarInit (variable SCPublic SCUInt nameMM) $ SCProd (SCVarName (nameM 0) : (map (\(_,i) -> SCVarName (nameM i)) ts))
  ] <> getNs <>
  [ SCEmpty, Comment "extend the initial args to appropriate size"
  , VarInit (variable SCPublic argTableType inputTable) (SCFunCall (nameTableExt p) [SCVarName input, SCVarName nameMM, SCVarName (nameM 0), SCVarName (nameN 0)])
  ] <>
  [SCEmpty, Comment "evaluate all underlying predicates"] <> getTables <>
  [SCEmpty, Comment "assign input variables"] <> asgnInputArgs <>
  [SCEmpty, Comment "initialize filter"] <> initFilter <>
  [SCEmpty, Comment "evaluate the clause body"] <> evalBody <>

  [ SCEmpty, Comment "output the updated predicate arguments"

  --, VarDecl (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn [0..n-1]))) result)
  , VarDecl (variable SCPublic (SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], map (\(y,i) -> scColTypeI i (y ^. annotation)) ys))) result)
  --

  , VarAsgn (nameTableBB result) (SCAnd (SCVarName nameBB) (SCVarName nameBP))
  ] <> asgnOutputArgs <>
  [Return (SCVarName result)]

  where
    inputTable = "table0"
    result   = "result"

    asgnInputArgs  = map (\((Var xtype x),i) -> VarInit (variable SCPublic (scColType xtype) x) (SCVarName $ nameTableArg inputTable i)) xs
    asgnOutputArgs = map (\(z,i) -> VarAsgn (nameTableArg result i) (exprToSC z)) ys

    initFilter = [ VarInit (variable (scDomainFromAnn ann) (SCArray 1 SCBool) nameBB) (SCVarName (nameTableBB inputTable))
                 , VarInit (variable SCPublic (SCArray 1 SCBool) nameBP) (funReshape [SCConstBool True, SCVarName nameMM])
                 ]

    qs' = andsToList q
    qs  = mergeChoose qs'

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
      let (setZ',setC) = L.partition (\(zi,_) -> case zi of {Var _ _ -> True; Attribute _ _ -> True; Hole _ -> True; _ -> False}) (zip zs is) in
      let setZ = map (\(z,i) -> case z of {Attribute zann zval -> (Var zann zval,i); _ -> (z,i)}) setZ' in

      -- all bounded variables will be inputs
      -- all free variables will be assigned in this execution
      let (setX,setY) = L.partition (\(z,i) -> case z of
                                                 Var _ _ -> fromMaybe undefined $ dv !!? i
                                                 _       -> False) $ setZ
      in

      -- the input table
      let argTypes = map (\(z,i) -> scColTypeI i (z ^. annotation)) setX <> map (\(z,i) -> scColTypeI i (z ^. annotation)) setC in
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
      let something (Var ztype z,i) = VarInit (variable SCPublic (scColPrivateTypeI i ztype) z) (SCVarName (nameTableArg resUnTableName i)) in

      map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColTypeI i xtype) (nameArg i)) (funConstCol [SCVarName x])) setX <>
            map (\(z,i)           -> VarInit (variable SCPublic ((scColTypeI i . (view annotation)) z) (nameArg i)) (funConstCol [scConstType z])) setC <>

            -- create an input data structure that corresponds to particular goal
            [ VarDecl $ variable SCPublic argTableType argTableName
            , VarAsgn (nameTableBB argTableName) (funTrueCol [])
            ] <>
            map (\(_,i) -> VarAsgn (nameTableArg argTableName i) (SCVarName (nameArg i))) setX <>
            map (\(_,i) -> VarAsgn (nameTableArg argTableName i) (SCVarName (nameArg i))) setC <>

            -- call the goal, read updated valuation of free variables
            [ VarInit (variable SCPublic resTableType resTableName) (SCFunCall (nameGetTableStruct p) [SCVarName ds, SCVarName argTableName])] <>

            if isSetSemantics then
                -- remove duplicate solutions
                [ VarInit (variable SCPublic resUnTableType resUnTableName) (SCFunCall (nameDedup p) [SCVarName resTableName])]

                -- assign the output variables
                -- everything becomes private after deduplication
                <> map something setY
                <> [VarInit (variable resUnDomain (SCArray 1 SCBool) (nameB j)) (SCVarName (nameTableBB resUnTableName))]
           else
                -- assign the output variables
                -- the types come from type derivation
                map (\(Var ztype z,i) -> VarInit (variable SCPublic (scColTypeI i ztype) z) (SCVarName (nameTableArg resTableName i))) setY
                <> [VarInit (variable resDomain (SCArray 1 SCBool) (nameB j)) (SCVarName (nameTableBB resTableName))]



-- TODO this index conversion is temporary here, we need something better
ind :: Int -> Int -> Int
ind i1 i2 = i1 * 1000 + i2

---------------------------------------------------------------------
-- construct SecreC statements computing an aggregation over predicate

-- aggregation creates an intensional predicate table that will not be used anywhere else,
-- so we do not need to increase the crros product table
aggrToSC :: Text -> Expr -> Int -> [Statement]
aggrToSC ds (Aggr _ f pr@(Pred _ _ zs) e1 e2) j =

                          let is = [0..length zs-1] in

                          --extract aggregated variable name
                          -- TODO this can be generalized by defining a mapping from variables of x to indices of result table
                          let x = case e1 of
                                      Var _ x' -> x'
                                      _        -> error $ "aggregation over complex expressions is not supported yet"
                          in
                          -- extract the type of x
                          let aggrAnns = filter (\z -> case z of {Var _ zn -> zn == x; _ -> False}) zs in
                          let ann = if length aggrAnns > 0 then (head . fromMaybe undefined $ nonEmpty aggrAnns) ^. annotation
                                    else error $ "aggregation variable not found among goal arguments" in

                          --extract aggregation result variable name
                          -- TODO this can be generalized by adding a subcall of 'formula to SC' transformation
                          let y = case e2 of
                                      Var _ y' -> y'
                                      _       -> error $ "comparing aggregation result to a complex expression is not supported yet"
                          in

                          -- prepare intentional predicate table
                          -- ignore updates of internal variables of pr since they are out of scope
                          -- remove duplicates before applying aggregation
                          let j1 = ind j 0 in
                          let intPredStmts = intPredToSC True ds pr j1 in

                          -- extract the index of aggregation input variable
                          let aggrInputIndices = filter snd $ zipWith (\z i -> (i, case z of {Var _ zn -> zn == x; _ -> False})) zs is in
                          let xi = if length aggrInputIndices > 0 then fst $ head . fromMaybe undefined $ nonEmpty aggrInputIndices
                                   else error $ "aggregation variable " <> show x <> " not found in aggregation predicate " <> show pr
                          in

                          -- TODO here we assume that y is always a fresh variable, generalize it later
                          intPredStmts <>
                          [VarInit (variable SCPublic (scColPrivateType ann) y) $ funAggr (show f) [ SCVarName (nameTableArg (nameResUn j1) xi)
                                                                                            , SCVarName (nameTableBB (nameResUn j1))
                                                                                            , funSize [SCVarName (nameTableBB (nameResUn j1))]]

                          -- although Bj has already been declared by the internal predicate, let us re-declare it
                          , VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) (funTrueCol [funColSize [SCVarName y]])]

prepareGoal :: Text -> (S.Set Text) -> Expr -> Int -> [Statement]
prepareGoal ds _ goalPred j =
    case goalPred of
        -- we remove duplicates before publishing final results
        Pred{} -> intPredToSC True ds goalPred j
        Aggr{} -> aggrToSC ds goalPred j
        _      -> error $ "only a single predicate or an aggregation is supported in the goal"

--------------------------------------------------
-- convert a formula to SecreC (transformation S^F)
formulaToSC :: Text -> Expr -> Int -> [Statement]
formulaToSC ds q j =
  [SCEmpty, Comment ("q" <> show j)] <> formulaToSC_case q
  where
    ann = q ^. annotation
    dom = scDomainFromAnn ann
    addB   = case dom of
                 SCPublic -> VarAsgn nameBP . SCAnd (SCVarName nameBP)
                 _        -> VarAsgn nameBB . SCAnd (SCVarName nameBB)
    bj     = SCVarName (nameB j)
    initBj = VarInit (variable dom (SCArray 1 SCBool) (nameB j))
    formulaToSC_case q' = case q' of
        ConstBool _ b -> [addB (funReshape [SCConstBool b, SCVarName nameMM])]

        Pred _ p zs   -> --add database attributes to the set of declared variables
                            let predArgNames = S.fromList $ map (nameTableArg (nameTable j)) [0..length zs - 1] in

                            -- TODO it would be nice to have access to data types of extensional predicates here
                            let stmts = zipWith (\z i -> formulaToSC ds (Un ann z (Var (z ^. annotation) (nameTableArg (nameTable j) i))) (ind j i)) zs [0..] in
                            let (comps', asgns') = L.partition snd $ zipWith (\s z -> (s, z ^. annotation ^. annBound)) stmts zs in
                            let comps = concat $ map (map (\(VarInit _ bexpr) -> bexpr) . fst) comps' in
                            let asgns = concat $ map fst asgns' in

                            let bb = if length comps > 0 then [addB $ SCAnds comps] else [] in
                            asgns <> bb

        Not _ (Pred _ p zs) ->
            [initBj $ SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName (nameTableArg (nameTable j) i))) zs [0..]), addB bj]

        Not _ e ->
            let b1 = nameB (ind j 1) in
            let s1 = formulaToSC ds e (ind j 1) in
            s1 <> [initBj $ SCNot (SCVarName b1), addB bj]

        Or _ e1 e2 ->
            let b1 = nameB (ind j 1) in
            let b2 = nameB (ind j 2) in
            let s1 = formulaToSC ds e1 (ind j 1) in
            let s2 = formulaToSC ds e2 (ind j 2) in
            s1 <> s2 <> [initBj $ SCOr (SCVarName b1) (SCVarName b2), addB bj]

        And _ e1 e2 ->
            let b1 = nameB (ind j 1) in
            let b2 = nameB (ind j 2) in
            let s1 = formulaToSC ds e1 (ind j 1) in
            let s2 = formulaToSC ds e2 (ind j 2) in
            s1 <> s2 <> [initBj $ SCAnd (SCVarName b1) (SCVarName b2), addB bj]

        Lt _ _ _ -> [initBj $ exprToSC q', addB bj]
        Le _ _ _ -> [initBj $ exprToSC q', addB bj]
        Gt _ _ _ -> [initBj $ exprToSC q', addB bj]
        Ge _ _ _ -> [initBj $ exprToSC q', addB bj]
        Eq _ _ _ -> [initBj $ exprToSC q', addB bj]

        -- TODO this is a workaround for choose construction
        Un _ (Expr.List _ xs) (Choose _ (Expr.List _ zs) (Expr.List _ bs)) ->
            let n = length bs in
            let zss = chunksOf n zs in
            [ VarAsgn nameBB $ L.foldr1 (\x y -> funCat [x,y]) $ map (SCAnd (SCVarName nameBB) . bexprToSC) bs
            , VarAsgn nameBP $ L.foldr1 (\x y -> funCat [x,y]) $ map (const $ SCVarName nameBP) bs] <>
            zipWith (\(Var annx x) zs -> VarInit (variable SCPublic (scColType annx) x) $ L.foldr1 (\x y -> funCat [x,y]) $ map exprToSC zs) xs zss

        Un _ e1 e2@(Choose _ _ _) -> formulaToSC_case $ Un ann (eList [e1]) e2

        -- unification may be a comparison as well as initialization (for strings)
        Un _ e1 e2 ->  ex
                        where

                          -- if x is a fresh variable, init x
                          ex = case e1 of
                                   (Var annx x) -> if not (e1 ^. annotation ^. annBound) then
                                                       [VarInit (variable SCPublic (scColType annx) x) (funCopyCol [exprToSC e2])]
                                                   else ey
                                   _            -> ey
                          -- if y is a fresh variable, init y
                          ey = case e2 of
                                   (Var anny y) -> if not (e2 ^. annotation ^. annBound) then
                                                       [VarInit (variable SCPublic (scColType anny) y) (funCopyCol [exprToSC e1])]
                                                   else ez
                                   _            -> ez
                          -- if both x and y are not fresh, then compare
                          ez = [initBj $ exprToSC (Eq ann e1 e2), addB bj]

        Is _ e1 e2 -> formulaToSC_case (Un ann e1 e2)

        Aggr _ _ _ _ _ -> aggrToSC ds q' j

        _ -> error $ "Unexpected boolean expression: " <> show q'

-- convert an expression to SecreC (transformation S^F)
exprToSC :: Expr -> SCExpr
exprToSC e =
  case e of 
    ConstInt   _ c -> funConstIntCol [SCConstInt c, SCVarName nameMM]
    ConstFloat _ c -> funConstFloatCol [SCConstFloat c, SCVarName nameMM]
    ConstStr   _ c -> funConstStrCol [SCConstStr c, SCVarName nameMM]
    ConstBool  _ c -> funConstBoolCol [SCConstBool c, SCVarName nameMM]

    -- should we distinguish between DB and Free variables? it seems that not.
    Var   _ x -> SCVarName $ x

    Not  _ e0 -> funBoolOp [SCConstStr "not", exprToSC e0, SCVarName nameBP]
    Neg  _ e0 -> SCFunCall "neg" [exprToSC e0]
    Inv  _ e0 -> SCFunCall "inv" [exprToSC e0]
    Sqrt _ e0 -> SCFunCall "apply_sqrt" [exprToSC e0]

    FDiv _ e1 e2 -> binArith "/" e1 e2
    Div  _ e1 e2 -> binArith "div" e1 e2
    Mod  _ e1 e2 -> binArith "%" e1 e2
    Sub  _ e1 e2 -> binArith "-" e1 e2
    Lt   _ e1 e2 -> funBoolOp [SCConstStr "<", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Le   _ e1 e2 -> funBoolOp [SCConstStr "<=", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Eq   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Un   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Gt   _ e1 e2 -> funBoolOp [SCConstStr ">", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Ge   _ e1 e2 -> funBoolOp [SCConstStr ">=", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Mul  _ e1 e2 -> binArith "*" e1 e2
    Add  _ e1 e2 -> binArith "+" e1 e2
    Pow  _ e1 e2 -> binArith "pow" e1 e2
    And  _ e1 e2 -> funBoolOp [SCConstStr "and", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Or   _ e1 e2 -> funBoolOp [SCConstStr "or", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Pred _ _ _ -> error $ "High order predicates are not supported"
    _          -> error $ "Unexpected expression: " <> show (prettyMinimal e)
  where
    binArith s x y = funArithOp [SCConstStr s, cast x y x, cast x y y]
    -- Cast arguments to float if either one is already float
    cast :: Expr -> Expr -> Expr -> SCExpr
    cast x y z 
      | (typeof x == PPFloat32 || typeof y == PPFloat32) && typeof z /= PPFloat32
        = SCFunCall "cast_float32" [exprToSC z]
      | otherwise = exprToSC z
    typeof = view $ annotation . annType

-- convert a pure boolean expression to SecreC (used only for the condition bits)
bexprToSC :: Expr -> SCExpr
bexprToSC e =
  case e of 

    ConstBool _ b -> funReshape [SCConstBool b, SCVarName nameMM]
    Var   _ x -> SCVarName x
    Not  _ e0 -> SCNot $ bexprToSC e0

    Lt   _ e1 e2 -> funBoolOp [SCConstStr "<", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Le   _ e1 e2 -> funBoolOp [SCConstStr "<=", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Eq   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Un   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Is   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Gt   _ e1 e2 -> funBoolOp [SCConstStr ">", exprToSC e1, exprToSC e2, SCVarName nameBP]
    Ge   _ e1 e2 -> funBoolOp [SCConstStr ">=", exprToSC e1, exprToSC e2, SCVarName nameBP]

    And  _ e1 e2 -> SCAnd (bexprToSC e1) (bexprToSC e2)
    Or   _ e1 e2 -> SCOr  (bexprToSC e1) (bexprToSC e2)
    _          -> error $ "Unexpected pure boolean expression: " <> show (prettyMinimal e)

--------------------------------------------------
-- create a script that writes data from .csv file to Sharemind database
-- WARNING! this should never be used with actual private data, as it is insecure and is meant for testing purposes only

csvImportCode :: DP.DatalogProgram -> IO (SCProgram)
csvImportCode dp = do
  let ds = "ds"
  let extPreds = dp ^.. DP.dpDBClauses
  tableData <- mapM (getTableData . view predName) extPreds
  return $ program $ header
                     <> [Funct $ mainFun $
                                     [ VarInit (variable SCPublic SCText ds) strDataset
                                     , funTdbOpenConnection [SCVarName ds]]
                                     <> concat (zipWith (tableGenerationCode ds) extPreds tableData) <>
                                     [funTdbCloseConnection [SCVarName ds]]]

tableGenerationCode :: Text -> Expr -> [[Text]] -> [Statement]
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
        p  = dbc ^. predName
        xs = dbc ^. predArgs
        types = map (\x -> let dtype  = x ^. annotation ^. annType in
                        case dtype of
                            PPBool  -> 0
                            PPInt32   -> 1
                            PPFloat32 -> 2
                            PPStr   -> 3
                            _       -> error $ "Can only create a table for bool, int, float, string datatypes."
                ) xs

        domains = map (\x -> let dom  = x ^. annotation ^. domain in
                        case dom of
                            Public  -> 0
                            Private -> 1
                            _       -> error $ "Can only create a table for known privacy domain."
                ) xs


        hlengths  = map T.length tableHeader
        headerStr = mconcat $ tableHeader

        tableData = concat $ map (zip types) tableRows

        boolData  = map snd $ filter (\x -> fst x == 0) tableData
        intData   = map snd $ filter (\x -> fst x == 1) tableData
        floatData = map snd $ filter (\x -> fst x == 2) tableData
        strData'  = map snd $ filter (\x -> fst x == 3) tableData

        vlengths = map T.length strData'
        strData  = mconcat $ strData'

mergeChoose :: [Expr] -> [Expr]
mergeChoose qs =
    let (qs0',qs1) = L.partition (\q -> case q of {Is _ _ Choose{} -> True; _ -> False }) qs in
    let qs0 = map (\(Is annx x ch) -> (Is annx (eList [x]) ch)) qs0' in
    if length qs0 > 0 then
        let q' = L.foldr1 (\(Is _ (Expr.List _ [x]) (Choose _ (Expr.List _ zs) _)) (Is ann (Expr.List _ xs) (Choose _ (Expr.List _ zss) bs)) -> Is ann (eList (x:xs)) (eChoose (eList (zs <> zss)) bs) ) qs0 in
        qs1 <> [q']
    else
        qs



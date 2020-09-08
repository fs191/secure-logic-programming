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
nameTableStruct p    = pack $ "table_" ++ p
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
nameTheta i = pack $ "theta" ++ show i
nameTable i = pack $ "table" ++ show i

-- some functions that are already defined in SecreC
funExtCol   = SCFunCall "extendColumn"
funGetDBCol = SCFunCall "getDBColumn"
funCat      = SCFunCall "myCat"
funPermute  = SCFunCall "applyPermutation"
funReshape  = SCFunCall "reshape"
funConstCol = SCFunCall "constColumn"
funTrueCol  = SCFunCall "trueColumn"
funFreeVCol = SCFunCall "freeVarColumn"
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

funFilterTrue     = SCFunCall "filterTrue"
funDeclassify     = SCFunCall "declassifyIfNeed"
funTdbGetRowCount = SCFunCall "tdbGetRowCount"

funPublishArg         = FunCall "publishArg"
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
          (PPAuto,     _)   -> SCDynamicT Nothing
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
      (PPAuto, _)       -> f (scDomain i dom) (SCDynamicT i) (SCDynamicS i)

scStructPrivateType :: (SCDomain -> SCType -> SCType -> SCType) -> Maybe Int -> Ann -> SCType
scStructPrivateType f i ann =
  let dtype  = ann ^. annType in
  case dtype of
      PPBool -> f SCShared3p SCBool  SCBool
      PPInt  -> f SCShared3p SCInt32 SCInt32
      PPStr  -> f SCShared3p SCXorUInt32 SCXorUInt8
      PPAuto -> f SCShared3p (SCDynamicT i) (SCDynamicS i)

scColTypeI :: Int -> Ann -> SCType
scColTypeI i = scStructType SCColumn (Just i)

scColPrivateTypeI :: Int -> Ann -> SCType
scColPrivateTypeI i = scStructPrivateType SCColumn (Just i)

scColType :: Ann -> SCType
scColType = scStructType SCColumn Nothing

scConstType :: Expr -> SCExpr
scConstType (ConstInt   _ c) = SCConstInt c
scConstType (ConstFloat _ c) = SCConstFloat c
scConstType (ConstBool  _ c) = SCConstBool c
scConstType (ConstStr   _ c) = SCConstStr c
scConstType (Attribute  _ c) = SCConstStr c
scConstType e                 = error $ "Expecting a constant, not " ++ show e

scSubstType :: Int -> Ann -> SCType
scSubstType i ann = SCSubst (scDomain (Just i) (ann ^. domain)) (scColTypeI i ann)

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

--------------------------------------------------
-- convert a program to SecreC (transformation S^P)
secrecCode :: DP.DatalogProgram -> SCProgram
secrecCode dp = program $

  header
  ++ map (Struct . extPredDecl) extPreds
  ++ map (Funct . extPredGet) extPreds
  ++ map Struct (zipWith intPredDecl intPredPs intPredNs)
  ++ map Funct (zipWith intPredExt intPredPs intPredNs)
  ++ map Funct (zipWith (ruleToSC xis argTableType) rules [0..])
  ++ map Funct (zipWith intPredCat intPredPs intPredNs)
  ++ map Funct (zipWith (intPredPermute yis) intPredPs intPredNs)
  ++ map Funct (zipWith3 intPredGet lss intPredPs intPredNs)
  ++ map Funct (zipWith (intPredDedup yis) intPredPs intPredNs)
  ++ [Funct goal]
 where
   rules = dp ^. DP.dpRules
   (xis,yis,argTableType,goal) = concreteGoal rules (dp ^.. DP.inputs) (dp ^.. DP.outputs) (dp ^. DP.dpGoal)
   extPreds = dp ^.. DP.dpDBClauses
   (intPredPs, intPredNs) = unzip $ nub $ map (\p -> (p ^. predName, predicateArity p)) $ map (\r -> r ^. ruleHead) rules

   lss = [ls | r <- rules
               , let ls' = zipWith (\r' l -> let p = ruleName r' in
                                             let boolType = scDomainFromAnn (ruleAnn r') in
                                             let argTypes = map scColType (ruleSchema r') in
                                             (if ruleName r == p then l else -1,
                                              SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([boolType], argTypes)))
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
extPredDecl dbc = struct (SCTemplateDecl Nothing) (nameTableStruct p) (y:ys)
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

intPredGet :: [(Int, SCType)] -> String -> Int -> FunctionDecl
intPredGet ls p n = function template returnType fname fargs fbody
  where
    ds = "ds"
    input  = "args"
    result = "result"

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameGetTableStruct p
    fargs = [variable SCPublic SCString ds, variable SCPublic (dynamicColT 1) input]
    fbody = [ VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)]
            ++ map (\(l,lt) -> VarInit (variable SCPublic lt (nameIndex result l)) (SCFunCall (nameGoalComp p l) [SCVarName ds, SCVarName input])) ls
            ++ map (\(l,lt) -> VarAsgn result $ SCFunCall (nameTableCat p) [SCVarName result, SCVarName (nameIndex result l)]) ls
            ++ [Return (SCVarName result)]

intPredPermute :: [Int] -> String -> Int -> FunctionDecl
intPredPermute is p n = function template returnType fname fargs fbody
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

intPredDedup :: [Int] -> String -> Int -> FunctionDecl
intPredDedup is@(i':is') p n = function template returnType fname fargs fbody
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
concreteGoal :: [Rule] -> [Expr] -> [Expr] -> Expr -> ([Int], [Int], SCType, FunctionDecl)
concreteGoal rules xs ys (Pred ptype p zs) = (xis, yis, argTableType, mainFun $
  -- get the arguments of a goal
  map (\(Var xtype x,i) -> let (xdom,xsctype) = scVarType xtype in VarInit (variable xdom xsctype (pack x)) (funGetArg [SCConstStr x])) setX ++
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColTypeI i xtype)        (nameArg i)) (funConstCol [SCVarName (pack x)])) setX ++
  map (\(z,i)           -> VarInit (variable SCPublic ((scColTypeI i . (view annotation)) z) (nameArg i)) (funConstCol [scConstType z])) setC ++
  map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColTypeI i xtype)        (nameArg i)) (funFreeVCol [])) setF ++

  -- create an input data structure that corresponds to particular goal
  [ VarDecl $ variable SCPublic argTableType input
  , VarAsgn (nameTableBB input) (funTrueCol [])
  ] ++
  map (\i -> VarAsgn (nameTableArg input i) (SCVarName (nameArg i))) is ++

  -- establish database connection
  [ VarInit (variable SCPublic SCString ds) strDataset
  , funTdbOpenConnection [SCVarName ds]

  -- call the goal, read updated valuation of free variables
  , VarInit (variable SCPublic res0TableType result0) (SCFunCall (nameGetTableStruct p) [SCVarName ds, SCVarName input])

  -- close connection
  , funTdbCloseConnection [SCVarName ds]

  -- remove duplicate solutions
  , VarInit (variable SCPublic resTableType result) (SCFunCall (nameDedup p) [SCVarName result0])

  -- shuffle the results and leave only those whose truth bit is 1
  , VarInit (variable SCPublic SCUInt32 n) (funDeclassify [funSum [SCTypeCast SCUInt32 (SCVarName (nameTableBB result))]])
  , VarInit (variable outDomain (SCArray 1 SCUInt32) pi) (funShuffle [SCVarName (nameTableBB result)])
  ] ++
  map (\(Var _ zi,i) -> funPublishArg
               [ SCConstInt i
               , SCConstStr zi
               , (funFilterTrue [SCVarName pi, SCVarName n, SCVarName (nameTableArg result i)])
               ]
  ) setY)

  where
    ds     = "ds"
    input  = "args"
    result0 = "result0"
    result = "result"
    pi     = "pi"
    n      = "n"

    is = [0..length zs - 1]
    (setZ',setC) = partition (\(zi,_) -> case zi of {Var _ _ -> True; Attribute _ _ -> True; _ -> False}) (zip zs is)
    setZ = map (\(z,i) -> case z of {Attribute zann zval -> (Var zann zval,i); _ -> (z,i)}) setZ'
    setX = [(zi,i) | (zi@(Var _ z),i) <- setZ, (Var _ x) <- xs, z == x]
    setY = [(zi,i) | (zi@(Var _ z),i) <- setZ, (Var _ y) <- ys, z == y]
    --we also have free variables that are neither in X nor in Y
    setF = filter (\zi -> not (elem zi setX)) setZ

    xis = map snd $ setX ++ setC
    yis = map snd $ setY

    -- which types the args of p have according to the goal (inputs/outputs)
    goalTypes = zipWith (\z i -> scColTypeI i (z ^. annotation)) zs is
    rs = filter (\r -> ruleName r == p) rules

    -- which types the args of p have according to rules for relation p
    ruleTypes = map (\r -> zipWith scColTypeI is (ruleSchema r)) rs

    -- we eventually take the strongest of these two to avoid privacy leakage of both DB and the user's inputs
    argTypes        = foldr (zipWith joinType) (map dynamicColumn is) (goalTypes : ruleTypes)

    -- after deduplication, we get all-private-column table
    -- since at least one column (or the boolean condition) is private in secure computation
    privateArgTypes = zipWith (\z i -> scColPrivateTypeI i (z ^. annotation)) zs is

    argTableType  = SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCPublic],   argTypes))
    res0TableType = SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([outDomain],  argTypes))
    resTableType  = SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCShared3p], privateArgTypes))

    -- if the truthness condition of at least one rule has private type, then so has the final answer
    doms = map (\r -> let q = r ^. ruleTail in ((q ^. annotation) ^.) domain) rules
    outDomain = scDomainFromAnn ptype


--------------------------------------------------
-- convert a rule (a Horn Clause) to SecreC (transformation S^C)
ruleToSC :: [Int] -> SCType -> Rule -> Int -> FunctionDecl
ruleToSC xis' argTableType r j = function template returnType fname fargs fbody
  where
    ds    = "ds"
    input = "args"
    rhead = r ^. ruleHead
    rtail = r ^. ruleTail
    p     = ruleName r
    ann   = ruleAnn r
    zs    = args r
    n     = length zs

    is    = [0..n-1]
    xis   = take n xis'

    -- template   = SCTemplateDecl $ Just ((SCDynamic Nothing) :  map (SCDynamic . Just) is, (SCDynamicT Nothing) : (map (SCDynamicT . Just) is ++ map (SCDynamicS . Just) is))
    --returnType = Just $ SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn is))
    js = map fst $ filter snd $ zipWith (\z i -> (i, case scDomainFromAnn (z ^. annotation) of {SCDynamic _ -> True; _ -> False})) zs is
    -- TODO template may still be needed if we have unknown types
    --template = SCTemplateDecl $ Just (map (SCDynamic . Just) js, (SCDynamicT Nothing) : (map (SCDynamicT . Just) js ++ map (SCDynamicS . Just) js))
    template = SCTemplateDecl Nothing
    returnType = Just $ SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], zipWith (\z i -> scColTypeI i (z ^. annotation)) zs is))
    --

    fname      = nameGoalComp p j
    fargs      = [SCVar SCPublic SCString ds, SCVar SCPublic argTableType input]
    fbody      = ruleBodyToSC xis ann argTableType ds input p zs rtail

ruleBodyToSC :: [Int] -> Ann -> SCType -> Text -> Text -> String -> [Expr] -> Expr -> [Statement]
ruleBodyToSC xis ann argTableType ds input p zs q =

  [ SCEmpty, Comment "compute the number of solutions in used predicates"
  , VarInit (variable SCPublic SCUInt (nameM 0)) (funSize [SCVarName (nameTableBB input)])
  ] ++ getRowCounts ++
  [ VarInit (variable SCPublic SCUInt nameMM) $ SCProd (SCVarName (nameM 0) : (map (\(_,i) -> SCVarName (nameM i)) ts))
  ] ++ getNs ++
  [ SCEmpty, Comment "extend the initial args to appropriate size"
  , VarInit (variable SCPublic argTableType inputTable) (SCFunCall (nameTableExt p) [SCVarName input, SCVarName nameMM, SCVarName (nameM 0), SCVarName (nameN 0)])
  ] ++
  [SCEmpty, Comment "evaluate all underlying predicates"] ++ getTables ++ evalBody ++

  [ SCEmpty, Comment "output the updated predicate arguments"
  , VarInit (variable (scDomainFromAnn ann) (SCArray 1 SCBool) result_b) (SCAnd (SCVarName (nameTableBB inputTable)) (SCAnds (map (\i -> SCVarName (nameB i)) [1..length qs])))

  --, VarDecl (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn [0..n-1]))) result)
  , VarDecl (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], zipWith (\z i -> scColTypeI i (z ^. annotation)) zs [0..n-1]))) result)
  --

  , VarAsgn (nameTableBB result) (SCVarName result_b)
  ] ++ asgnOutputArgs ++
  [Return (SCVarName result)]

  where
    inputTable = "table0"
    result   = "result"
    result_b = "b"
    n  = length zs
    is = [0..n-1]

    (inputArgs', outputArgs') = partition snd $ zipWith (\z i -> ((z,i), elem i xis)) zs is

    asgnInputArgs  = map (\((z,i),_) -> Eq (z ^. annotation) z (Var (z ^. annotation) (unpack $ nameTableArg inputTable i))) inputArgs'
    asgnOutputArgs = map (\((z,i),_) -> VarAsgn (nameTableArg result i) (exprToSC z)) outputArgs'

    qs = asgnInputArgs ++ andsToList q
    ts = map (\(qj,j) -> (qj ^. predName, j)) $ filter (\(qj,j) -> case qj of {Pred _ _ _ -> True; _ -> False}) $ zip qs [1..]
    ks = 0 : (map snd ts)

    getRowCounts = map (\(tk,k) -> VarInit (variable SCPublic SCUInt (nameM k)) (funTdbGetRowCount [SCVarName ds, SCConstStr tk])) ts
    getNs        = map (\k      -> let js = (filter (k >=) ks) in
                                   VarInit (variable SCPublic SCUInt (nameN k)) (SCDiv (SCVarName nameMM) (SCProd (map (\j -> SCVarName (nameM j)) js))) ) ks
    getTables    = map (\(tk,k) -> VarInit (variable SCPublic (SCStruct (nameTableStruct tk) (SCTemplateUse Nothing)) (nameTable k)) (SCFunCall (nameGetTableStruct tk) [SCVarName ds, SCVarName nameMM, SCVarName (nameM k), SCVarName (nameN k)])) ts

    -- initially, the input args are declared variables
    declaredVars = S.fromList $ map (unpack . nameTableArg inputTable) [0..length zs - 1]

    -- TODO it is better to use state monad here, or even put the "first time used" bit into annotation
    (_,evalBody)   = foldl (\(dv, stmts) (qj,j) -> formulaToSC dv stmts qj j) (declaredVars,[]) (zip qs [1..])


--------------------------------------------------
-- convert a formula to SecreC (transformation S^F)
formulaToSC :: (S.Set String) -> [Statement] -> Expr -> Int -> (S.Set String, [Statement])
formulaToSC dv stmts q j =
  (dvUpdated, stmts ++ [SCEmpty, Comment ("q" ++ show j)] ++ predCode)
  where
    (dvUpdated, predCode) = formulaToSC_case q
    formulaToSC_case q'   = case q' of

        ConstBool ann b -> let dom = scDomainFromAnn ann in
                           (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (funReshape [SCConstBool b, SCVarName nameMM])])

        Pred ann p zs    -> let dom = scDomainFromAnn ann in
                            let bb = VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ SCAnds $ map (\i -> SCVarName $ nameB (ind j i)) [0..length zs - 1] in
                            --add database attributes to the set of declared variables
                            let predArgNames = S.fromList $ map (unpack . nameTableArg (nameTable j)) [0..length zs - 1] in
                            -- TODO extract all comparisons into one line, this makes indexation nicer
                            let (dv',stmts') = foldl (\(dv0, stmts0) (z,i) -> formulaToSC dv0 stmts0 (Eq ann z (Var ann (unpack $ nameTableArg (nameTable j) i))) (ind j i)) (S.union dv predArgNames,[]) (zip zs [0..]) in
                            (dv', stmts' ++ [bb])

        Not ann (Pred _ p zs) ->
            let dom = scDomainFromAnn ann in
            (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName (nameTableArg (nameTable j) i))) zs [0..]))])

        Lt  ann _ _ -> let dom = scDomainFromAnn ann in (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')])
        Le  ann _ _ -> let dom = scDomainFromAnn ann in (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')])
        Gt  ann _ _ -> let dom = scDomainFromAnn ann in (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')])
        Ge  ann _ _ -> let dom = scDomainFromAnn ann in (dv, [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (exprToSC q')])

        -- an equality may be a comparison as well as initialization
        Eq ann e1 e2 -> ex
                        where
                          dom   = scDomainFromAnn ann
                          bcomp = VarInit (variable dom      (SCArray 1 SCBool) (nameB j)) $ exprToSC (Eq        ann e1 e2)
                          btrue = VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) $ exprToSC (ConstBool ann True)

                          -- if x is a fresh variable, init x
                          ex = case e1 of
                                   (Var annx x) -> if not (S.member x dv) then
                                                     (S.insert x dv, [VarInit (variable SCPublic (scColType annx) (pack x)) (exprToSC e2), btrue])
                                                   else ey
                                   _            -> ey
                          -- if y is a fresh variable, init y
                          ey = case e2 of
                                   (Var anny y) -> if not (S.member y dv) then
                                                     (S.insert y dv, [VarInit (variable SCPublic (scColType anny) (pack y)) (exprToSC e1), btrue])
                                                   else ez
                                   _            -> ez
                          -- if both x and y are not fresh, then compare
                          ez = (dv, [bcomp])

        Is ann e1 e2 -> formulaToSC_case (Eq ann e1 e2)

        _ -> error $ "Unexpected boolean expression: " ++ show q'
        where
            -- TODO index conversion is temporary here, we need something better
            ind i1 i2 = i1 * 1000 + i2


-- convert an expression to SecreC (transformation S^F)
exprToSC :: Expr -> SCExpr
exprToSC e =
  case e of 
    ConstInt   _ c -> funConstCol [SCConstInt c, SCVarName nameMM]
    ConstFloat _ c -> funConstCol [SCConstFloat c, SCVarName nameMM]
    ConstStr   _ c -> funConstCol [SCConstStr c, SCVarName nameMM]
    ConstBool  _ c -> funConstCol [SCConstBool c, SCVarName nameMM]

    -- should we distinguish between DB and Free variables? it seems that not.
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
    And  _ e1 e2 -> funBoolOp [SCConstStr "and", exprToSC e1, exprToSC e2]
    Or   _ e1 e2 -> funBoolOp [SCConstStr "or", exprToSC e1, exprToSC e2]

    Pred _ _ _ -> error $ "High order predicates are not supported"


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
                                     ++ zipWith (tableGenerationCode ds) extPreds tableData ++
                                     [funTdbCloseConnection [SCVarName ds]]]

tableGenerationCode :: Text -> DBClause -> [[String]] -> Statement
tableGenerationCode ds dbc (tableHeader:tableRows) =

  funWriteToTable [ SCVarName ds, SCConstStr p, SCConstArr (map  SCConstBool isInt)
                  , SCConstStr headerStr, SCConstArr (map SCConstInt hlengths)
                  , SCConstStr valuesStr, SCConstArr (map SCConstInt vlengths)
                  , SCConstArr (map SCConstAny intData)]
  where
        p  = name dbc
        xs = vars dbc
        is = [0..length xs - 1]
        isInt = map (\x -> let dtype  = x ^. annotation ^. annType in
                        case dtype of
                            PPBool -> False
                            PPInt  -> True
                            PPStr  -> False
                            PPAuto -> error $ "Cannot create a table without knowing column data type."
                ) xs

        hlengths  = map length tableHeader
        headerStr = concat $ tableHeader

        (_intData, _strData) = unzip $ map (\vs -> (partition snd) (zip vs isInt)) tableRows
        intData = map fst . concat $ _intData
        strData = map fst . concat $ _strData
        vlengths = map length strData
        valuesStr = concat $ strData

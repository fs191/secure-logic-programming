{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC
--  ( secrecCode
--  ) where
where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc

import Expr

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
  | SCStruct String [SCType]
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
  pretty (SCColumn pt dt st) = "relColumn" <> "<" <> tupled [pretty pt, pretty dt, pretty st] <> ">"
  pretty (SCStruct s ts)    = pretty s <> "<" <> vsep (pretty <$> ts) <> ">"
  pretty (SCArray n sctype) = pretty sctype <+> "[[" <+> pretty n <+> "]]"

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
  { _fdTemplate   :: Maybe ([SCDomain], [SCType])
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
      template = case _fdTemplate fd of
        Just (ds,xs)  -> "template<" <> tupled (pds ++ pxs) <> ">"
                         where pds = map ("domain" <+>) $ pretty <$> ds
                               pxs = map ("type" <+>)   $ pretty <$> xs
        Nothing -> ""
      n    = pretty $ _fdName fd
      pars = tupled $ pretty <$> _fdParams fd
      body = vsep $ pretty <$> _fdBody fd
      rt   = case _fdReturnType fd of
        Just x  -> pretty x
        Nothing -> "void"

data StructDecl = StructDecl
  { _sdTemplate   :: Maybe ([SCDomain], [SCType])
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
      template = case _sdTemplate sd of
        Just (ds,xs)  -> "template<" <> tupled (pds ++ pxs) <> ">"
                         where pds = map ("domain" <+>) $ pretty <$> ds
                               pxs = map ("type" <+>)   $ pretty <$> xs
        Nothing -> ""
      n  = pretty $ _sdName sd
      ms = punctuate semi $ pretty <$> _sdMembers sd

secrecCode :: SCProgram
secrecCode = program
    header
--  , extPredDecl
--  , intPredDecl
--  , ruleFuns
--  , intPredComp
--  , goal
-- ]

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

dynamicColD i = SCDynamic  (Just i)
dynamicColT i = SCDynamicT (Just i)
dynamicColS i = SCDynamicS (Just i)
dynamicColumn i = SCColumn (dynamicColD i) (dynamicColT i) (dynamicColS i)

extPredDecl :: Expr -> StructDecl
extPredDecl (Pred _ p xs) = struct Nothing (pack p) (y:ys)
  where
    y  = variable SCPublic (SCArray 1 SCBool) "b"
    ys = map (\(Var pptype x) -> variable SCPublic (scColType pptype) (pack x)) xs

intPredDecl :: Expr -> StructDecl
intPredDecl (Pred _ p xs) = struct template (pack p) (y:ys)
  where
    is = [0..length xs - 1]
    template = Just ((SCDynamic Nothing) : (map dynamicColD is), map dynamicColT is ++ map dynamicColS is)
    y  = variable (SCDynamic Nothing) (SCArray 1 SCBool) "b"
    ys = zipWith (\(Var pptype _) i -> variable SCPublic (dynamicColumn i) (pack ("arg" ++ show i))) xs is

intPredExtend :: Expr -> FunctionDecl
intPredExtend (Pred _ p xs) = function template returnType fname fargs fcode
  where
    is = [0..length xs - 1]
    template = Just ([], [dynamicColT 0, dynamicColT 1])
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
    template = Just ([], [dynamicColT 0, dynamicColT 1, dynamicColT 2])
    returnType = Just $ dynamicColT 0
    fname = pack ("cat_" ++ show p)
    fargs = [variable SCPublic (dynamicColT 1) "t1", variable SCPublic (dynamicColT 2) "t2"]
    fcode = [VarDecl $ variable SCPublic (SCDynamicT (Just 0)) "t0",
             VarAsgn "t0.b" (SCFunCall "myCat" [SCVarName "t1.b", SCVarName "t2.b"])]
            ++ map (\i -> VarAsgn ("t0.arg" ++ show i) (SCFunCall "myCat" [SCVarName ("t1.arg" ++ show i), SCVarName ("t2.arg" ++ show i)])) is
            ++ [Return (SCVarName "t0")]

intPredGet :: Expr -> [Int] -> FunctionDecl
intPredGet (Pred _ p xs) ls = function template returnType fname fargs fcode
  where
    template = Just ([], [dynamicColT 0, dynamicColT 1])
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

function :: Maybe ([SCDomain], [SCType]) -> Maybe SCType -> Text -> [SCVar] -> [Statement] -> FunctionDecl
function = FunctionDecl

mainFun :: [Statement] -> FunctionDecl
mainFun = function Nothing Nothing "main" []

struct :: Maybe ([SCDomain], [SCType]) -> Text -> [SCVar] -> StructDecl
struct = StructDecl

variable :: SCDomain -> SCType -> Text -> SCVar
variable = SCVar

program :: [TopStatement] -> SCProgram
program = SCProgram


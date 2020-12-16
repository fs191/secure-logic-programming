{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.SecreC
  ( secrecCode
  ) where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Relude

import Control.Lens hiding(Empty)

import qualified Data.List as L
import Data.List.Split
import qualified Data.Set as S

import Annotation
import qualified DatalogProgram as DP
import Expr
import ExprPretty
import Rule

import Language.SecreC.SCProgram
import Language.SecreC.SCExpr
import qualified Language.SecreC.Builtin as BI

data SecreCState = SecreCState
  { _scsVarEnv :: Set Text
  }
makeLenses ''SecreCState

getFreshVarName 
  :: (MonadState SecreCState m) 
  => Text -> m Text
getFreshVarName name =
  do
    env <- use scsVarEnv
    if name `S.member` env
      then do
        let err = error "impossible"
        let checkFree i = (name <> show i) `S.notMember` env
        let idx = fromMaybe err $ find checkFree [(0 :: Int)..]
        let freeName = name <> show idx
        scsVarEnv <>= S.singleton freeName
        return freeName
      else do
        scsVarEnv <>= S.singleton name
        return name

mainFun :: [Statement] -> FunctionDecl
mainFun = FunctionDecl (SCTemplateDecl Nothing) Nothing "main" []

struct :: SCTemplate -> Text -> [SCVar] -> StructDecl
struct = StructDecl

variable :: SCDomain -> SCType -> Text -> SCVar
variable = SCVar

program :: [TopStatement] -> SCProgram
program = SCProgram

-- some variable/function names that are used multiple times
nameInTableStruct :: (Semigroup a, IsString a) => a -> a
nameInTableStruct p    = "in_" <> p
nameOutTableStruct :: (Semigroup a, IsString a) => a -> a
nameOutTableStruct p   = "out_" <> p
nameGetTableStruct :: (Semigroup a, IsString a) => a -> a
nameGetTableStruct p = "getTable_" <> p
nameDedup :: (Semigroup a, IsString a) => a -> a
nameDedup p          = "deduplicate_" <> p
nameTableCat :: (Semigroup a, IsString a) => a -> a
nameTableCat p       = "cat_" <> p
nameTablePermute :: (Semigroup a, IsString a) => a -> a
nameTablePermute p   = "permute_" <> p
nameTableExt :: (Semigroup a, IsString a) => a -> a
nameTableExt p       = "extend_" <> p
nameGoalComp :: (Semigroup a1, IsString a1, Show a2) => a1 -> a2 -> a1
nameGoalComp p l     = "goal_" <> p <> "_" <> show l

-- fixed names that are used globally in different components
nameArg :: (Semigroup a1, IsString a1, Show a2) => a2 -> a1
nameArg  i = "arg" <> show i
nameMM :: Text
nameMM     = "m"
-- private component of the filter
nameBB :: Text
nameBB     = "b"
-- public component of the filter
nameBP :: Text
nameBP     = "bp"

nameTableBB :: Text -> Text
nameTableBB  t   = t <> "." <> nameBB
nameTableArg :: (Semigroup a, IsString a) => a -> Int -> a
nameTableArg t i = t <> "." <> nameArg i
nameIndex :: (Semigroup a1, IsString a1) => a1 -> Int -> a1
nameIndex t i    = t <> show i

nameM :: (Semigroup a1, IsString a1) => Int -> a1
nameM    i = "m" <> show i
nameN :: (Semigroup a1, IsString a1) => Int -> a1
nameN    i = "n" <> show i
nameB :: (Semigroup a1, IsString a1) => Int -> a1
nameB    i = "b" <> show i
nameArgs :: (Semigroup a1, IsString a1) => Int -> a1
nameArgs i = "args" <> show i
nameRes :: (Semigroup a1, IsString a1) => Int -> a1
nameRes  i = "res" <> show i
nameResUn :: (Semigroup a1, IsString a1) => Int -> a1
nameResUn i = "resUnique" <> show i
nameTable :: (Semigroup a1, IsString a1) => Int -> a1
nameTable i = "table" <> show i


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
          (PPStr,  Unknown) -> error "cannot determine data type for a string of unknown domain"
          (PPFloat32, _)      -> SCFloat32
          _                 -> SCDynamicT Nothing
  in (scDom, sctype)

scStructType :: (SCDomain -> SCType -> SCType -> SCType) -> Maybe Int -> Ann -> SCType
scStructType f i ann =
  let dom    = ann ^. domain in
  let dtype  = ann ^. annType in
  case (dtype, dom) of
      (PPBool, _)       -> f (scDomain i dom) SCBool  SCBool
      (PPInt32,  _)     -> f (scDomain i dom) SCInt32 SCInt32
      (PPStr,  Private) -> f (scDomain i dom) SCXorUInt32 SCXorUInt8
      (PPStr,  Public)  -> f (scDomain i dom) SCUInt32    SCUInt8
      (PPStr,  Unknown) -> error "cannot determine data type for a string of unknown domain"
      (PPFloat32, _)    -> f (scDomain i dom) SCFloat32 SCFloat32
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
    L.partition (\(z,_) -> z ^. annotation . annBound) $ zip zs is

--------------------------------------------------
-- convert a program to SecreC (transformation S^P)
secrecCode :: (MonadState SecreCState m) => DP.DatalogProgram -> m SCProgram
secrecCode dp = 
  do
    goal <- concreteGoal dp
    rulesConv <- traverse ruleToSC rules
    return . program $
      header
      <> map (Struct . extPredDecl) extPreds
      <> map (Funct . extPredGet) extPreds
      <> map Struct (zipWith intPredInDecl intPredPs intPredXss)
      <> map Struct (zipWith intPredOutDecl intPredPs intPredYss)
      <> map Funct (zipWith intPredExt intPredPs intPredXss)
      <> rulesConv
      <> map Funct (zipWith intPredCat intPredPs intPredYss)
      <> map Funct (zipWith intPredPermute intPredPs intPredYss)
      <> map Funct (L.zipWith3 intPredGet lss intPredPs intPredYss)
      <> map Funct (zipWith intPredDedup intPredPs intPredYss)
      <> [Funct goal]
 where
   rules = dp ^. DP.dpRules
   extPreds = dp ^.. DP.dpDBClauses
   (intPredPs, intPredXss, intPredYss) = unzip3 $ L.nub 
                                         $ map (\r -> let p = r ^. ruleHead in
                                                      let zs = predicateVars p in
                                                      let (xs',ys') = partitionInputsOutputs zs in
                                                      let xs = map snd xs' in
                                                      let ys = map snd ys' in
                                                      (p ^. predName, xs, ys)) rules

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
extPredGet dbc = FunctionDecl (SCTemplateDecl Nothing) returnType fname fargs fbody
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
            , VarAsgn (nameTableBB result) (BI.trueColumn (SCVarName m))
            ]
            <> map (\i -> VarAsgn (nameTableArg result i) (BI.getDBColumn (SCVarName ds) (SCConstStr p) (SCConstInt i) (SCVarName m) (SCVarName mi) (SCVarName ni))) is
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
intPredExt p is = FunctionDecl template returnType fname fargs fbody
  where
    result = "result"
    m  = "m"
    mi = "mi"
    ni = "ni"

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1])
    returnType = Just $ dynamicColT 0
    fname = nameTableExt p
    fargs = [variable SCPublic (dynamicColT 1) result, variable SCPublic SCUInt m, variable SCPublic SCUInt mi, variable SCPublic SCUInt ni]
    fbody = [VarAsgn (nameTableBB result) (BI.extendColumn (SCVarName $ nameTableBB result) (SCVarName m) (SCVarName mi) (SCVarName ni))]
            <> map (\i -> VarAsgn (nameTableArg result i) (BI.extendColumn (SCVarName (nameTableArg result i)) (SCVarName m) (SCVarName mi) (SCVarName ni))) is
            <> [Return (SCVarName result)]


intPredCat :: Text -> [Int] -> FunctionDecl
intPredCat p is = FunctionDecl template returnType fname fargs fbody
  where
    input1 = "t1"
    input2 = "t2"
    result = "t0"

    template = SCTemplateDecl $ Just ([], [dynamicColT 0, dynamicColT 1, dynamicColT 2])
    returnType = Just $ dynamicColT 0
    fname = nameTableCat p
    fargs = [variable SCPublic (dynamicColT 1) input1, variable SCPublic (dynamicColT 2) input2]
    fbody = [VarDecl $ variable SCPublic (SCDynamicT (Just 0)) result,
             VarAsgn (nameTableBB result) (BI.myCat (SCVarName (nameTableBB input1)) (SCVarName (nameTableBB input2)))]
            <> map (\i -> VarAsgn (nameTableArg result i) (BI.myCat (SCVarName (nameTableArg input1 i)) (SCVarName (nameTableArg input2 i)))) is
            <> [Return (SCVarName result)]

intPredGet :: [(Int, (SCDomain, [SCType]))] -> Text -> [Int] -> FunctionDecl
intPredGet ls0 p is = FunctionDecl template returnType fname fargs fbody
  where
    ds = "ds"
    input  = "args"
    result = "result"

    --we only need to put output columns into the template
    ls1 = map (\(l,(bt,at1)) -> (l, (bt, map fst $ filter (\(_, i) -> i `elem` is) $ zip at1 [0..length at1-1]))) ls0
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
intPredPermute p is = FunctionDecl template returnType fname fargs fbody
  where
    table  = "t"
    result = "result"
    pi_     = "pi"

    template = SCTemplateDecl $ Just ([SCDynamic Nothing], [SCDynamicT Nothing, SCDynamicT (Just 0), SCDynamicT (Just 1)])
    returnType = Just $ SCDynamicT (Just 0)
    fname = nameTablePermute p
    fargs = [variable SCPublic (SCDynamicT (Just 1)) table, variable (SCDynamic Nothing) (SCArray 1 (SCDynamicT Nothing)) pi_]
    fbody = [ VarDecl (variable SCPublic (SCDynamicT (Just 0)) result)
            , VarAsgn (nameTableBB result) (BI.applyPermutation (SCVarName (nameTableBB table)) ( SCVarName pi_))]
            <> map (\i -> VarAsgn (nameTableArg result i) (BI.applyPermutation (SCVarName (nameTableArg table i)) ( SCVarName pi_))) is
            <> [Return (SCVarName result)]

-- TODO this works correctly as far as we pass a single one choice of inputs (which is the case so far)
-- we would need piecewise deduplication otherwise
intPredDedup :: Text -> [Int] -> FunctionDecl
intPredDedup p is = FunctionDecl template returnType fname fargs fbody
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
            <> map (\i -> VarAsgn (nameTableArg result i) (BI.copyColumn (SCVarName (nameTableArg table i)))) is <>
            [VarAsgn pi_ (BI.countSortPermutation (SCVarName (nameTableBB result)))
            , VarAsgn result (SCFunCall (nameTablePermute p) [SCVarName result, SCVarName pi_])]

            <> concatMap (\i -> [ VarAsgn pi_ (BI.quickSortPermutation (SCVarName (nameTableArg result i)))
                                   , VarAsgn result (SCFunCall (nameTablePermute p) [SCVarName result, SCVarName pi_])]
                      ) is
            <>
            if not $ null is then
                let (r:rs) = map (BI.findRepeating . SCVarName . nameTableArg result) is in
                [ VarAsgn (nameTableBB result) (SCAnd (SCVarName (nameTableBB result)) (SCNot (foldr SCAnd r rs)))
                , Return (SCVarName result)]
            else
                [Return (SCVarName result)]

--------------------------------------------------
-- convert a predicate to SecreC (transformation S^G)
concreteGoal :: (MonadState SecreCState m) => DP.DatalogProgram -> m FunctionDecl
concreteGoal dp = 
  do
    resTable <- prepareGoal ds xnames fullGoal
    return . mainFun $
      -- get user inputs
      map (\(Var xtype x) -> let (xdom,xsctype) = scVarType xtype in VarInit (variable xdom xsctype x) (BI.argument (SCConstStr x))) xs <>

      -- establish database connection
      [ VarInit (variable SCPublic SCText ds) strDataset
      , BI.tdbOpenConnection (SCVarName ds)]

      -- construct a table that contains results for goalPred

      <> resTable <>

      -- close connection
      [BI.tdbCloseConnection (SCVarName ds)

      -- shuffle the results and leave only those whose truth bit is 1
      , VarInit (variable SCPublic SCUInt32 n) (BI.declassifyIfNeed (BI.sum (SCTypeCast SCUInt32 (SCVarName (nameB j)))))
      , VarInit (variable SCShared3p (SCArray 1 SCUInt32) pi_) (BI.lpShuffle (SCVarName (nameB j)))
      ] <>

      zipWith (\y i -> BI.publishColumn
                           (SCConstInt i)
                           (SCConstStr y)
                           (BI.filterTrue (SCVarName pi_) (SCVarName n) (SCVarName y))

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
ruleToSC :: (MonadState SecreCState m) => Rule -> m TopStatement
ruleToSC r = 
  do
    let ds    = "ds"
    let input = "args"
    let rtail = r ^. ruleTail
    let p     = ruleName r
    let ann   = ruleAnn r
    let zs    = args r

    -- we assume that all bounded variables are inputs, and free variables are outputs
    let (xs,ys) = partitionInputsOutputs zs

    let argTypes = map (\(x,i) -> scColTypeI i (x ^. annotation)) xs
    let resTypes = map (\(y,i) -> scColTypeI i (y ^. annotation)) ys

    let argTableType = SCStruct (nameInTableStruct p) (SCTemplateUse $ Just ([SCPublic],   argTypes))
    let resTableType = Just $ SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], resTypes))

    -- TODO template may still be needed if we allow unknown types
    -- js = map fst $ filter snd $ map (\(z,i) -> (i, case scDomainFromAnn (z ^. annotation) of {SCDynamic _ -> True; _ -> False})) xs
    -- template = SCTemplateDecl $ Just (map (SCDynamic . Just) js, (SCDynamicT Nothing) : (map (SCDynamicT . Just) js <> map (SCDynamicS . Just) js))
    let template = SCTemplateDecl Nothing

    fname <- getFreshVarName $ "goal" <> p
    let fargs      = [SCVar SCPublic SCText ds, SCVar SCPublic argTableType input]
    let inputTable = "table0"
    let result     = "result"

    let asgnInputArgs  = map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColType xtype) x) (SCVarName $ nameTableArg inputTable i)) xs
    let asgnOutputArgs = map (\(z,i) -> VarAsgn (nameTableArg result i) (exprToSC z)) ys

    let initFilter = [ VarInit (variable (scDomainFromAnn ann) (SCArray 1 SCBool) nameBB) (SCVarName (nameTableBB inputTable))
                 , VarInit (variable SCPublic (SCArray 1 SCBool) nameBP) (BI.reshape (SCConstBool True) [SCVarName nameMM])
                 ]

    let qs' = andsToList rtail
    let qs  = mergeChoose qs'

    let ts = map (\(qj,j) -> (qj ^. predName, j)) $ filter (has _Pred . fst) $ zip qs [1..]
    let ks = 0 : map snd ts

    let getRowCounts = map (\(tk,k) -> VarInit (variable SCPublic SCUInt (nameM k)) (BI.tdbGetRowCount (SCVarName ds) (SCConstStr tk))) ts
    let getNs        = map (\k      -> let js = filter (k >=) ks in
                                   VarInit (variable SCPublic SCUInt (nameN k)) (SCDiv (SCVarName nameMM) (SCProd (map (SCVarName . nameM) js))) ) ks
    let getTables    = map (\(tk,k) -> VarInit (variable SCPublic (SCStruct (nameOutTableStruct tk) (SCTemplateUse Nothing)) (nameTable k)) (SCFunCall (nameGetTableStruct tk) [SCVarName ds, SCVarName nameMM, SCVarName (nameM k), SCVarName (nameN k)])) ts

    evalBody <- concat <$> traverse (formulaToSC ds) qs
    let fbody = 
          [ SCEmpty, Comment "compute the number of solutions in used predicates"
          , VarInit (variable SCPublic SCUInt (nameM 0)) (BI.size (SCVarName (nameTableBB input)))
          ] <> getRowCounts <>
          [ VarInit (variable SCPublic SCUInt nameMM) $ SCProd (SCVarName (nameM 0) : map (\(_,i) -> SCVarName (nameM i)) ts)
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
    return . Funct $ FunctionDecl template resTableType fname fargs fbody

---------------------------------------------------------------------
-- construct SecreC statements computing all valuations of predicate

-- TODO this works correctly as far as we pass a single one choice of inputs (which is the case so far)
-- we would need to take into account the entire table for deduplication before aggregation
intPredToSC :: (MonadState SecreCState m) => Bool -> Text -> Expr -> m [Statement]
intPredToSC isSetSemantics ds (Pred ptype p zs) =
  do
    -- declared variables are the bounded arguments
    let dv = map (view $ annotation . annBound) zs

    -- link inputs, outputs, and constants to indices of zs
    let is = [0..length zs - 1]

    -- separate constants and variables
    let (setZ',setC) = L.partition (\(zi,_) -> case zi of {Var _ _ -> True; Attribute _ _ -> True; Hole _ -> True; _ -> False}) (zip zs is)
    let setZ = map (\(z,i) -> case z of {Attribute zann zval -> (Var zann zval,i); _ -> (z,i)}) setZ'

    -- all bounded variables will be inputs
    -- all free variables will be assigned in this execution
    let (setX,setY) = L.partition (\(z,i) -> case z of
                                               Var _ _ -> dv L.!! i
                                               _       -> False) setZ

    -- the input table
    let argTypes = map (\(z,i) -> scColTypeI i (z ^. annotation)) setX <> map (\(z,i) -> scColTypeI i (z ^. annotation)) setC
    let argTableType = SCStruct (nameInTableStruct p) (SCTemplateUse $ Just ([SCPublic], argTypes))
    argTableName <- getFreshVarName "nameArgs"

    -- the output table before deduplication
    let resTypes = map (\(z,i) -> scColTypeI i (z ^. annotation)) setY
    let resDomain = scDomainFromAnn ptype
    let resTableType = SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([resDomain],  resTypes))
    resTableName <- getFreshVarName "nameRes"

    -- the output table after deduplication

    -- after deduplication, we get all-private-column table
    -- since at least one column (or the boolean condition) is private in secure computation
    -- and we even do not know which elements are duplicated
    -- TODO we can sort the private columns "piecewise within each public group" for best efficiency, keeping public columns public
    -- this may be a bit tricky to describe in a compact way in SecreC
    let resUnArgTypes = map (\(z,i) -> scColPrivateTypeI i (z ^. annotation)) setY
    let resUnDomain = SCShared3p
    let resUnTableType = SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([resUnDomain],  resUnArgTypes))
    resUnTableName <- getFreshVarName "nameResUn"
    let something (Var ztype z,i) = VarInit (variable SCPublic (scColPrivateTypeI i ztype) z) (SCVarName (nameTableArg resUnTableName i))
        something _ = undefined

    return $ map (\(Var xtype x,i) -> VarInit (variable SCPublic (scColTypeI i xtype) (nameArg i)) (BI.constColumn (SCVarName x))) setX <>
          map (\(z,i)           -> VarInit (variable SCPublic ((scColTypeI i . view annotation) z) (nameArg i)) (BI.constColumn (scConstType z))) setC <>

          -- create an input data structure that corresponds to particular goal
          [ VarDecl $ variable SCPublic argTableType argTableName
          , VarAsgn (nameTableBB argTableName) (BI.trueColumn (SCTypeCast SCUInt $ SCConstInt 1))
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
              <> [VarInit (variable resUnDomain (SCArray 1 SCBool) (nameB (undefined "j"))) (SCVarName (nameTableBB resUnTableName))]
         else
              -- assign the output variables
              -- the types come from type derivation
              map (\(Var ztype z,i) -> VarInit (variable SCPublic (scColTypeI i ztype) z) (SCVarName (nameTableArg resTableName i))) setY
              <> [VarInit (variable resDomain (SCArray 1 SCBool) (nameB (undefined "j"))) (SCVarName (nameTableBB resTableName))]
intPredToSC _ _ _ = undefined

---------------------------------------------------------------------
-- construct SecreC statements computing an aggregation over predicate

-- aggregation creates an intensional predicate table that will not be used anywhere else,
-- so we do not need to increase the crros product table
aggrToSC :: (MonadState SecreCState m) => Text -> Expr -> m [Statement]
aggrToSC ds (Aggr _ f pr@(Pred _ _ zs) e1 e2) =
  do
    let is = [0..length zs-1]
    --extract aggregated variable name
    -- TODO this can be generalized by defining a mapping from variables of x to indices of result table
    let x = case e1 of
            Var _ x' -> x'
            _        -> error "aggregation over complex expressions is not supported yet"
    -- extract the type of x
    let aggrAnns = filter (\case {Var _ zn -> zn == x; _ -> False}) zs
    let ann = case aggrAnns of
                x:_ -> x ^. annotation
                _   -> error "aggregation variable not found among goal arguments"

    --extract aggregation result variable name
    -- TODO this can be generalized by adding a subcall of 'formula to SC' transformation
    let y = case e2 of
              Var _ y' -> y'
              _       -> error "comparing aggregation result to a complex expression is not supported yet"
    -- prepare intentional predicate table
    -- ignore updates of internal variables of pr since they are out of scope
    -- remove duplicates before applying aggregation
    let j1 = undefined -- ind j 0
    intPredStmts <- intPredToSC True ds pr
    -- extract the index of aggregation input variable
    let aggrInputIndices = filter snd $ zipWith (\z i -> (i, case z of {Var _ zn -> zn == x; _ -> False})) zs is
    let xi = if not $ null aggrInputIndices then fst $ L.head aggrInputIndices
         else error $ "aggregation variable " <> show x <> " not found in aggregation predicate " <> show pr
    let funAggr = 
          case f of
            Expr.Sum -> BI.sumFilter
            Expr.Min -> BI.minFilter
            Expr.Max -> BI.maxFilter
            Expr.Count -> BI.countFilter
            Expr.Prod -> BI.timesFilter
            Expr.Average -> BI.avgFilter
    -- TODO here we assume that y is always a fresh variable, generalize it later
    return $ intPredStmts <> 
      [VarInit (variable SCPublic (scColPrivateType ann) y) $ funAggr (SCVarName (nameTableArg (nameResUn j1) xi))
                                                                    (SCVarName (nameTableBB (nameResUn j1)))
                                                                    (BI.size (SCVarName (nameTableBB (nameResUn j1))))

      -- although Bj has already been declared by the internal predicate, let us re-declare it
      , VarInit (variable SCPublic (SCArray 1 SCBool) (nameB (undefined "j"))) (BI.trueColumn (BI.colSize (SCVarName y)))]
aggrToSC _ _ = undefined

prepareGoal :: (MonadState SecreCState m) => Text -> S.Set Text -> Expr -> m [Statement]
prepareGoal ds _ goalPred =
    case goalPred of
        -- we remove duplicates before publishing final results
        Pred{} -> intPredToSC True ds goalPred
        Aggr{} -> aggrToSC ds goalPred
        _      -> error "only a single predicate or an aggregation is supported in the goal"

--------------------------------------------------
-- convert a formula to SecreC (transformation S^F)
formulaToSC :: (MonadState SecreCState m) => Text -> Expr -> m [Statement]
formulaToSC ds q = 
  do
    res <- formulaToSC_case q
    return $ [SCEmpty, Comment "q" {-<> show j-}] <> res
  where
    ann = q ^. annotation
    dom = scDomainFromAnn ann
    addB   = case dom of
                 SCPublic -> VarAsgn nameBP . SCAnd (SCVarName nameBP)
                 _        -> VarAsgn nameBB . SCAnd (SCVarName nameBB)
    bj     = SCVarName (nameB (undefined "j"))
    initBj = VarInit (variable dom (SCArray 1 SCBool) (nameB (undefined "j")))
    formulaToSC_case :: (MonadState SecreCState m) => Expr -> m [Statement]
    formulaToSC_case q' = case q' of
        ConstBool _ b -> return [addB (BI.reshape (SCConstBool b) [SCVarName nameMM])]

        Pred _ n zs   -> 
          do
            --add database attributes to the set of declared variables
            -- TODO it would be nice to have access to data types of extensional predicates here
            stmts <- traverse (\(z, i) -> formulaToSC ds (Un ann z (Var (z ^. annotation) $ nameTableArg n i))) (zs `zip` [0..])
            --let (comps', asgns') = L.partition snd $ zipWith (\s z -> (s, z ^. annotation . annBound)) stmts zs
            let comps = undefined
            let asgns = undefined
            --let comps = concatMap (map (\(VarInit _ bexpr) -> bexpr) . fst) comps'
            --let asgns = concatMap fst asgns'

            let bb = [addB $ SCAnds comps | not $ null comps]
            return $ asgns <> bb

        Not _ (Pred _ _ zs) -> return
            [initBj $ SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName (nameTableArg (nameTable (undefined "j")) (undefined "i")))) zs [0..]), addB bj]

        Not _ e ->
          do
            let b1 = nameB (undefined "ind j 1")
            let s1 = formulaToSC ds e (undefined "ind j 1")
            return $ s1 <> [initBj $ SCNot (SCVarName b1), addB bj]

        Or _ e1 e2 ->
          do
            let b1 = nameB (undefined "ind j 1")
            let b2 = nameB (undefined "ind j 2")
            let s1 = formulaToSC ds e1 (undefined "ind j 1")
            let s2 = formulaToSC ds e2 (undefined "ind j 2")
            return $ s1 <> s2 <> [initBj $ SCOr (SCVarName b1) (SCVarName b2), addB bj]

        And _ e1 e2 ->
          do
            let b1 = nameB (undefined "ind j 1")
            let b2 = nameB (undefined "ind j 2")
            let s1 = formulaToSC ds e1 (undefined "ind j 1")
            let s2 = formulaToSC ds e2 (undefined "ind j 2")
            return $ s1 <> s2 <> [initBj $ SCAnd (SCVarName b1) (SCVarName b2), addB bj]

        Lt{} -> return [initBj $ exprToSC q', addB bj]
        Le{} -> return [initBj $ exprToSC q', addB bj]
        Gt{} -> return [initBj $ exprToSC q', addB bj]
        Ge{} -> return [initBj $ exprToSC q', addB bj]
        Eq{} -> return [initBj $ exprToSC q', addB bj]

        -- TODO this is a workaround for choose construction
        Is _ (Expr.List _ xs) (Choose _ (Expr.List _ zs) (Expr.List _ bs)) ->
          do
            let n = length bs
            let zss = chunksOf n zs
            return $
              [ VarAsgn nameBB $ L.foldr1 BI.myCat $ map (SCAnd (SCVarName nameBB) . bexprToSC) bs
              , VarAsgn nameBP $ L.foldr1 BI.myCat $ map (const $ SCVarName nameBP) bs] <>
              zipWith (\(Var annx x) zs -> VarInit (variable SCPublic (scColType annx) x) $ L.foldr1 BI.myCat $ map exprToSC zs) xs zss

        -- unification may be a comparison as well as initialization (for strings)
        Un _ e1 e2 -> return ex
                        where

                          -- if x is a fresh variable, init x
                          ex = case e1 of
                                   (Var annx x) -> if not (e1 ^. annotation . annBound) then
                                                       [VarInit (variable SCPublic (scColType annx) x) (BI.copyColumn (exprToSC e2))]
                                                   else ey
                                   _            -> ey
                          -- if y is a fresh variable, init y
                          ey = case e2 of
                                   (Var anny y) -> if not (e2 ^. annotation . annBound) then
                                                       [VarInit (variable SCPublic (scColType anny) y) (BI.copyColumn (exprToSC e1))]
                                                   else ez
                                   _            -> ez
                          -- if both x and y are not fresh, then compare
                          ez = [initBj $ exprToSC (Eq ann e1 e2), addB bj]

        -- TODO actually, we only have the initialization case due to previos processing
        Is _ e1 e2 -> formulaToSC_case (Un ann e1 e2)

        Aggr{} -> return . aggrToSC ds q' $ undefined "j"

        _ -> error $ "Unexpected boolean expression: " <> show q'

-- convert an expression to SecreC (transformation S^F)
exprToSC :: Expr -> SCExpr
exprToSC e =
  case e of 
    ConstInt   _ c -> BI.constIntColumn (SCConstInt c) (SCVarName nameMM)
    ConstFloat _ c -> BI.constFloatColumn (SCConstFloat c) (SCVarName nameMM)
    ConstStr   _ c -> BI.constStrColumn (SCConstStr c) (SCVarName nameMM)
    ConstBool  _ c -> BI.constBoolColumn (SCConstBool c) (SCVarName nameMM)

    -- should we distinguish between DB and Free variables? it seems that not.
    Var   _ x -> SCVarName x

    Not  _ e0 -> SCFunCall "not" [exprToSC e0]
    Neg  _ e0 -> SCFunCall "neg" [exprToSC e0]
    Inv  _ e0 -> SCFunCall "inv" [exprToSC e0]
    Sqrt _ e0 -> SCFunCall "apply_sqrt" [exprToSC e0]

    FDiv _ e1 e2 -> BI.aop (SCConstStr "/") (exprToSC e1) (exprToSC e2)
    Div  _ e1 e2 -> BI.aop (SCConstStr "div") (exprToSC e1) (exprToSC e2)
    Mod  _ e1 e2 -> BI.aop (SCConstStr "%") (exprToSC e1) (exprToSC e2)
    Sub  _ e1 e2 -> BI.aop (SCConstStr "-") (exprToSC e1) (exprToSC e2)
    Mul  _ e1 e2 -> BI.aop (SCConstStr "*") (exprToSC e1) (exprToSC e2)
    Add  _ e1 e2 -> BI.aop (SCConstStr "+") (exprToSC e1) (exprToSC e2)
    Pow  _ e1 e2 -> BI.aop (SCConstStr "pow") (exprToSC e1) (exprToSC e2)
    Lt   _ e1 e2 -> BI.bop (SCConstStr "<") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Le   _ e1 e2 -> BI.bop (SCConstStr "<=") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Eq   _ e1 e2 -> BI.bop (SCConstStr "==") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Un   _ e1 e2 -> BI.bop (SCConstStr "==") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Gt   _ e1 e2 -> BI.bop (SCConstStr ">") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Ge   _ e1 e2 -> BI.bop (SCConstStr ">=") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    And  _ e1 e2 -> BI.bop (SCConstStr "and") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Or   _ e1 e2 -> BI.bop (SCConstStr "or") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Cast a x     -> case a ^. annType of
                      PPFloat32 -> SCFunCall "cast_float32" [exprToSC x]
                      t         -> error $ "Casting not supported for type " <> show t
    Pred{}       -> error "High order predicates are not supported"
    _            -> error $ "Unexpected expression: " <> show (prettyMinimal e)

-- convert a pure boolean expression to SecreC (used only for the condition bits)
bexprToSC :: Expr -> SCExpr
bexprToSC e =
  case e of 

    ConstBool _ b -> BI.reshape (SCConstBool b) [SCVarName nameMM]
    Var   _ x -> SCVarName x
    Not  _ e0 -> SCNot $ bexprToSC e0

    Lt   _ e1 e2 -> BI.bop (SCConstStr "<") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Le   _ e1 e2 -> BI.bop (SCConstStr "<=") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Eq   _ e1 e2 -> BI.bop (SCConstStr "==") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Un   _ e1 e2 -> BI.bop (SCConstStr "==") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Is   _ e1 e2 -> BI.bop (SCConstStr "==") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Gt   _ e1 e2 -> BI.bop (SCConstStr ">") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)
    Ge   _ e1 e2 -> BI.bop (SCConstStr ">=") ( exprToSC e1) ( exprToSC e2) (SCVarName nameBP)

    And  _ e1 e2 -> SCAnd (bexprToSC e1) (bexprToSC e2)
    Or   _ e1 e2 -> SCOr  (bexprToSC e1) (bexprToSC e2)
    _          -> error $ "Unexpected pure boolean expression: " <> show (prettyMinimal e)


mergeChoose :: [Expr] -> [Expr]
mergeChoose qs =
    let (qs0',qs1) = L.partition (\case {Is _ _ Choose{} -> True; _ -> False }) qs in
    let qs0 = map (\(Is annx x ch) -> Is annx (eList [x]) ch) qs0' in
    if not $ null qs0 then
        let q' = L.foldr1 (\(Is _ (Expr.List _ [x]) (Choose _ (Expr.List _ zs) _)) (Is ann (Expr.List _ xs) (Choose _ (Expr.List _ zss) bs)) -> Is ann (eList (x:xs)) (eChoose (eList (zs <> zss)) bs) ) qs0 in
        qs1 <> [q']
    else
        qs


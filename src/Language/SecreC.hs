{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.SecreC
  ( secrecCode
  , csvImportCode
  ) where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Control.Lens hiding(Empty)
import Control.Monad.State.Strict

import Data.List
import Data.Maybe
import qualified Data.Set as S

import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc

import Annotation
import qualified DatalogProgram as DP
import DBClause
import Expr
import Rule
import Table(getTableData)

import Language.SecreC.SCProgram
import Language.SecreC.SCExpr

data SCState = SCState
  { _scsVarIdx :: Int
  , _scsDomIdx :: Int
  }
makeLenses ''SCState

type SCM = State SCState



-- Instantiate a fresh variable
variable :: SCDomain -> SCType -> Text -> SCM SCVar
variable d t n = 
  do
    i <- use scsVarIdx
    scsVarIdx += 1
    return . SCVar d t $ n <> "_" <> (pack $ show i)

freshDynDom :: SCM SCDomain
freshDynDom = 
  do
    i <- use scsDomIdx
    scsDomIdx += 1
    return . SCDynamic . pack $ "D" <> show i

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
nameMM     = pack $ "m"

nameTableBB  t   = pack $ unpack t ++ "." ++ unpack nameBB
nameTableArg t i = pack $ unpack t ++ "." ++ unpack i
nameIndex t i    = pack $ unpack t ++ show i

-- some functions that are already defined in SecreC
funExtCol   = SCFunCall "extendColumn"
funGetDBCol = SCFunCall "getDBColumn"
funCat      = SCFunCall "myCat"
funPermute  = SCFunCall "applyPermutation"
funReshape  = SCFunCall "reshape"
funConstCol = SCFunCall "constColumn"
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

funPublishCol         = ExprStmt . SCFunCall "publishCol"
funPublishVal         = ExprStmt . SCFunCall "publishVal"
funWriteToTable       = ExprStmt . SCFunCall "writePublicToTable"
funTdbOpenConnection  = ExprStmt . SCFunCall "tdbOpenConnection"
funTdbCloseConnection = ExprStmt . SCFunCall "tdbCloseConnection"

-- use this Sharemind dataset by default
strDataset = SCConstStr "DS1"

-- type rewrite function
scDomain :: PPDomain -> SCM SCDomain
scDomain Private = return SCShared3p
scDomain Public  = return SCPublic
scDomain Unknown = freshDynDom

scDomainFromAnn :: Ann -> SCM SCDomain
scDomainFromAnn ann = scDomain $ ann ^. domain

scVarType :: Ann -> SCM (SCDomain, SCType)
scVarType ann =
  do
    let dom    = ann ^. domain 
    let dtype  = ann ^. annType 

    scDom <- scDomain dom 
    let sctype = case (dtype, dom) of
            (PPBool, _) -> SCBool
            (PPInt,  _) -> SCInt32
            (PPStr,  Private) -> SCArray 1 SCXorUInt8
            (PPStr,  Public)  -> SCString
            (PPStr,  Unknown) -> error $ "cannot determine data type for a string of unknown domain"
            _                 -> SCDynamicT "T"
    return (scDom, sctype)

scStructType :: Ann -> SCM SCType
scStructType ann =
  do
    let dom    = ann ^. domain
    let dtype  = ann ^. annType
    scDom <- scDomain dom
    let f x y = return $ SCColumn scDom x y
    case (dtype, dom) of
        (PPBool, _)       -> f SCBool  SCBool
        (PPInt,  _)       -> f SCInt32 SCInt32
        (PPStr,  Private) -> f SCXorUInt32 SCXorUInt8
        (PPStr,  Public)  -> f SCUInt32    SCUInt8
        (PPStr,  Unknown) -> error $ "cannot determine data type for a string of unknown domain"
        _                 -> dynamicCol

dynamicCol :: SCM SCType
dynamicCol = 
  do
    i <- view scsDomIdx
    scsDomIdx += 1
    let dom = SCDynamic $ "D" <> (pack $ show i)
        dynT = SCDynamicT $ "T" <> (pack $ show i)
        dynS = SCDynamicS $ "S" <> (pack $ show i)
    return $ SCColumn dom dynT dynS

scStructPrivateType :: Ann -> SCM SCType
scStructPrivateType ann =
  do
    let dtype  = ann ^. annType 
    let f x y = return $ SCColumn SCShared3p x y
    case dtype of
        PPBool -> f SCBool  SCBool
        PPInt  -> f SCInt32 SCInt32
        PPStr  -> f SCXorUInt32 SCXorUInt8
        _      -> dynamicCol

scConstType :: Expr -> SCExpr
scConstType (ConstInt   _ c) = SCConstInt c
scConstType (ConstFloat _ c) = SCConstFloat c
scConstType (ConstBool  _ c) = SCConstBool c
scConstType (ConstStr   _ c) = SCConstStr c
scConstType (Attribute  _ c) = SCConstStr c
scConstType e                = error $ "Expecting a constant, not " ++ show e

scSubstType :: Ann -> SCM SCType
scSubstType ann = 
  do
    scDom <- scDomain (ann ^. domain)
    scCol <- scStructType ann
    return $ SCSubst scDom scCol

---------------------------------

-- all bounded variables in predicate head are inputs
-- all free variables in predicate head are outputs
partitionInputsOutputs :: [Expr] -> ([Expr], [Expr])
partitionInputsOutputs zs = partition (^. annotation . annBound) zs

--------------------------------------------------
-- convert a program to SecreC (transformation S^P)
secrecCode :: DP.DatalogProgram -> SCM SCProgram
secrecCode dp = 
  do
    extPredStructs <- mapM extPredDecl extPreds
    extPredFuncts  <- mapM extPredGet extPreds
    return . SCProgram $ mconcat
      [ header
      , extPredStructs
      , extPredFuncts
      , map Struct (zipWith intPredInDecl intPredPs intPredXss)
      , map Struct (zipWith intPredOutDecl intPredPs intPredYss)
      , map Funct (zipWith intPredExt intPredPs intPredXss)
      , map Funct (zipWith ruleToSC rules [0..])
      , map Funct (zipWith intPredCat intPredPs intPredYss)
      , map Funct (zipWith intPredPermute intPredPs intPredYss)
      , map Funct (zipWith3 intPredGet undefined intPredPs intPredYss)
      , map Funct (zipWith intPredDedup intPredPs intPredYss)
      , [Funct goal]
      ]
  where
    rules = dp ^. DP.dpRules
    goal  = concreteGoal rules (dp ^.. DP.inputs) (dp ^.. DP.outputs) (dp ^. DP.dpGoal)
    extPreds = dp ^.. DP.dpDBClauses
    mapper p = (p ^. predName, xs, ys)
      where
        (xs,ys) = partitionInputsOutputs zs
        zs = predicateVars p

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

extPredDecl :: DBClause -> SCM TopStatement
extPredDecl dbc = 
  do
    y <- variable SCPublic (SCArray 1 SCBool) nameBB
    ys <- traverse attrToSCVar xs
    return . Struct $ StructDecl Nothing (nameOutTableStruct p) (y:ys)
  where
    p  = name dbc
    xs = vars dbc

attrToSCVar :: Expr -> SCM SCVar
attrToSCVar (Attribute pptype _) = 
  do
    stType <- scStructType pptype
    variable SCPublic stType "arg"
attrToSCVar x = error $ "Expected an attribute, but got " ++ show x

extPredGet :: DBClause -> SCM TopStatement
extPredGet dbc = 
  do
    ds <- variable SCPublic SCString "ds"
    m  <- variable SCPublic SCUInt "m"
    mi <- variable SCPublic SCUInt "mi"
    ni <- variable SCPublic SCUInt "ni"
    let p = name dbc
      
    result <- variable SCPublic (SCStruct (nameOutTableStruct p) Nothing) "result"
    let resultE = varToExpr result
    let mapper :: Int -> Statement
        mapper i = ExprStmt $ SCAsgn (SCFieldAccess resultE . pack $ "arg" <> show i) 
                   (funGetDBCol [varToExpr ds, SCConstStr p, SCConstInt i, varToExpr m, varToExpr mi, varToExpr ni])
        n = length $ vars dbc
        is = [0..n-1]
        returnType = Just $ SCStruct (nameOutTableStruct p) Nothing
        fname = nameGetTableStruct p
    let fbody = 
          [ VarDecl result . Just $ funTrueCol [varToExpr m]
          ]
          ++ map mapper is
          ++ [Return $ varToExpr result]
    return . Funct $ FunctionDecl Nothing returnType fname [ds, m, mi, ni] fbody

intPredDecl :: Rule -> SCM [StructDecl]
intPredDecl r = 
  do
    let ruleArgs = r ^. ruleHead . predArgs
    let (outVars, inVars) = partition (^. annotation . annBound) ruleArgs
    d    <- freshDynDom
    b    <- variable d (SCArray 1 SCBool) "b"
    ins  <- traverse generateFactStruct inVars
    outs <- traverse generateFactStruct outVars

    return $ StructDecl (Just template) (nameInTableStruct p) ([b]<>ins<>outs)
  where
    template = SCTemplateDecl $ Just ([SCDynamic Nothing], map dynamicColT is)

generateFactStruct :: Expr -> SCM SCVar
generateFactStruct e = 
  do
    d <- freshDynDom
    let colVar n = variable SCPublic (dynDomainToType d) n
        err = error "expected identifier"
    colVar . pack . fromMaybe err $ identifier e

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
concreteGoal :: [Rule] -> [Expr] -> [Expr] -> Expr -> FunctionDecl
concreteGoal rules xs ys goalPred = mainFun $

  -- get user inputs
  map (\(Var xtype x) -> let (xdom,xsctype) = scVarType xtype in VarInit (variable xdom xsctype (pack x)) (funGetArg [SCConstStr x])) xs ++

  -- establish database connection
  [ VarInit (variable SCPublic SCString ds) strDataset
  , funTdbOpenConnection [SCVarName ds]]

  -- construct a table that contains results for goalPred

  -- TODO this is for testing aggregations, remove after we implement parsing aggregations
  -- ++ (prepareGoal ds xnames (Aggr ((last ys) ^. annotation) "max" goalPred (last ys) (Var ((last ys) ^. annotation) "Y")) j) ++
  ++ (prepareGoal ds xnames goalPred j) ++

  -- close connection
  [funTdbCloseConnection [SCVarName ds]

  -- shuffle the results and leave only those whose truth bit is 1
  , VarInit (variable SCPublic SCUInt32 n) (funDeclassify [funSum [SCTypeCast SCUInt32 (SCVarName (nameB j))]])
  , VarInit (variable outDomain (SCArray 1 SCUInt32) pi) (funShuffle [SCVarName (nameB j)])
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
    ds     = "ds"
    pi     = "pi"
    n      = "n"

    -- a dummy index (not important if we have one statement in the goal)
    j = 1

    xnames = S.fromList $ map (\(Var _ x) -> x) xs
    ynames =              map (\(Var _ y) -> y) ys
    outDomain = scDomainFromAnn (goalPred ^. annotation)


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
ruleBodyToSC :: Ann -> SCType -> Text -> Text -> String -> [(Expr,Int)] -> [(Expr,Int)] -> Expr -> SCM [Statement]
ruleBodyToSC ann argTableType ds input p xs ys q =
  do
    var0 <- variable SCPublic SCUInt "m"
    var1 <- variable SCPublic SCUInt "mm"
    return
      [ SCEmpty, Comment "compute the number of solutions in used predicates"
      , VarInit var0 $ funSize [SCVarName (nameTableBB input)]
      ]

  -- [ SCEmpty, Comment "compute the number of solutions in used predicates"
  -- , VarInit (variable SCPublic SCUInt "m") (funSize [SCVarName (nameTableBB input)])
  -- ] ++ getRowCounts ++
  -- [ VarInit (variable SCPublic SCUInt "mm") $ SCProd (SCVarName (nameM 0) : (map (\(_,i) -> SCVarName (nameM i)) ts))
  -- ] ++ getNs ++
  -- [ SCEmpty, Comment "extend the initial args to appropriate size"
  -- , VarInit (variable SCPublic argTableType inputTable) (SCFunCall (nameTableExt p) [SCVarName input, SCVarName nameMM, SCVarName (nameM 0), SCVarName (nameN 0)])
  -- ] ++
  -- [SCEmpty, Comment "evaluate all underlying predicates"] ++ getTables ++
  -- [SCEmpty, Comment "assign input variables"] ++ asgnInputArgs ++
  -- [SCEmpty, Comment "evaluate the clause body"] ++ evalBody ++

  -- [ SCEmpty, Comment "output the updated predicate arguments"
  -- , VarInit (variable (scDomainFromAnn ann) (SCArray 1 SCBool) result_b) (SCAnd (SCVarName (nameTableBB inputTable)) (SCAnds (map (\i -> SCVarName (nameB i)) [1..length qs])))

  -- --, VarDecl (variable SCPublic (SCStruct (nameTableStruct p) (SCTemplateUse $ Just ([SCDynamic Nothing], map dynamicColumn [0..n-1]))) result)
  -- , VarDecl (variable SCPublic (SCStruct (nameOutTableStruct p) (SCTemplateUse $ Just ([scDomainFromAnn ann], map (\(y,i) -> scColTypeI i (y ^. annotation)) ys))) result)
  -- --

  -- , VarAsgn (nameTableBB result) (SCVarName result_b)
  -- ] ++ asgnOutputArgs ++
  -- [Return (SCVarName result)]

  --where
  --  inputTable = "table0"
  --  result   = "result"
  --  result_b = "b"

  --  --asgnInputArgs  = map (\((Var xtype x),i) -> VarInit (variable SCPublic (scColType xtype) (pack x)) (SCVarName $ nameTableArg inputTable i)) xs
  --  asgnOutputArgs = map (\(z,i) -> VarAsgn (nameTableArg result i) (exprToSC z)) ys

  --  qs = andsToList q
  --  ts = map (\(qj,j) -> (qj ^. predName, j)) $ filter (\(qj,j) -> case qj of {Pred _ _ _ -> True; _ -> False}) $ zip qs [1..]
  --  ks = 0 : (map snd ts)

  --  --getRowCounts = map (\(tk,k) -> VarInit (variable SCPublic SCUInt (nameM k)) (funTdbGetRowCount [SCVarName ds, SCConstStr tk])) ts
  --  getNs        = map (\k      -> let js = (filter (k >=) ks) in
  --                                 VarInit (variable SCPublic SCUInt (nameN k)) (SCDiv (SCVarName nameMM) (SCProd (map (\j -> SCVarName (nameM j)) js))) ) ks
  --  getTables    = map (\(tk,k) -> VarInit (variable SCPublic (SCStruct (nameOutTableStruct tk) (SCTemplateUse Nothing)) (nameTable k)) (SCFunCall (nameGetTableStruct tk) [SCVarName ds, SCVarName nameMM, SCVarName (nameM k), SCVarName (nameN k)])) ts

  --  evalBody = concat $ zipWith (\qj j -> formulaToSC ds qj j) qs [1..]


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
aggrToSC ds (Aggr ann f pr@(Pred ptype p zs) e1 e2) j =

                          let is = [0..length zs-1] in
                          let (dom, dtype) = scVarType ann in

                          --extract aggregated variable name
                          -- TODO this can be generalized by defining a mapping from variables of x to indices of result table
                          let x = case e1 of
                                      Var _ x -> x
                                      _       -> error $ "aggregation over complex expressions is not supported yet"
                          in

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
                          [VarInit (variable SCPublic (scColType ann) (pack y)) $ funAggr f [ SCVarName (nameTableArg (nameResUn j1) xi)
                                                                                            , SCVarName (nameTableBB (nameResUn j1))
                                                                                            , funSize [SCVarName (nameTableBB (nameResUn j1))]]

                          -- although Bj has already been declared by the internal predicate, let us re-declare it
                          , VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) (funTrueCol [funColSize [SCVarName (pack y)]])]

prepareGoal :: Text -> (S.Set String) -> Expr -> Int -> [Statement]
prepareGoal ds dv goalPred j =
    case goalPred of
        -- we remove duplicates before publishing final results
        Pred _ _ _     -> intPredToSC True ds goalPred j
        Aggr _ _ _ _ _ -> aggrToSC ds goalPred j
        _              -> error $ "only a single predicate or an aggregation is supported in the goal"

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
                            let bb = VarInit (variable dom (SCArray 1 SCBool) (nameB j)) $ SCAnds $ map (\i -> SCVarName $ nameB (ind j i)) [0..length zs - 1] in
                            --add database attributes to the set of declared variables
                            let predArgNames = S.fromList $ map (unpack . nameTableArg (nameTable j)) [0..length zs - 1] in

                            -- TODO if we extract all comparisons into one expression, we can get a nicer indexation
                            let stmts' = concat $ zipWith (\z i -> formulaToSC ds (Un ann z (Var (z ^. annotation) (unpack $ nameTableArg (nameTable j) i))) (ind j i)) zs [0..] in

                            stmts' ++ [bb]

        Not ann (Pred _ p zs) ->
            let dom = scDomainFromAnn ann in
            [VarInit (variable dom (SCArray 1 SCBool) (nameB j)) (SCNot (SCAnds $ zipWith (\z i -> SCEq (exprToSC z) (SCVarName (nameTableArg (nameTable j) i))) zs [0..]))]

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
                          btrue = VarInit (variable SCPublic (SCArray 1 SCBool) (nameB j)) $ exprToSC (ConstBool ann True)

                          -- if x is a fresh variable, init x
                          ex = case e1 of
                                   (Var annx x) -> if not (e1 ^. annotation ^. annBound) then
                                                       [VarInit (variable SCPublic (scColType annx) (pack x)) (exprToSC e2), btrue]
                                                   else ey
                                   _            -> ey
                          -- if y is a fresh variable, init y
                          ey = case e2 of
                                   (Var anny y) -> if not (e2 ^. annotation ^. annBound) then
                                                       [VarInit (variable SCPublic (scColType anny) (pack y)) (exprToSC e1), btrue]
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
    ConstInt   _ c -> funConstCol [SCConstInt c, SCVarName nameMM]
    ConstFloat _ c -> funConstCol [SCConstFloat c, SCVarName nameMM]
    ConstStr   _ c -> funConstCol [SCConstStr c, SCVarName nameMM]
    ConstBool  _ c -> funConstCol [SCConstBool c, SCVarName nameMM]

    -- should we distinguish between DB and Free variables? it seems that not.
    Var   _ x -> SCVarName $ pack x

    Not  _ e0 -> funBoolOp [SCConstStr "not", exprToSC e0]
    Neg  _ e0 -> funArithOp [SCConstStr "neg", exprToSC e0]
    Inv  _ e0 -> funArithOp [SCConstStr "inv", exprToSC e0]
    Sqrt _ e0 -> funArithOp [SCConstStr "sqrt", exprToSC e0]

    FDiv _ e1 e2 -> funArithOp [SCConstStr "/", exprToSC e1, exprToSC e2]
    Div  _ e1 e2 -> funArithOp [SCConstStr "div", exprToSC e1, exprToSC e2]
    Mod  _ e1 e2 -> funArithOp [SCConstStr "%", exprToSC e1, exprToSC e2]
    Sub  _ e1 e2 -> funArithOp [SCConstStr "-", exprToSC e1, exprToSC e2]
    Lt   _ e1 e2 -> funBoolOp [SCConstStr "<", exprToSC e1, exprToSC e2]
    Le   _ e1 e2 -> funBoolOp [SCConstStr "<=", exprToSC e1, exprToSC e2]
    Eq   _ e1 e2 -> funBoolOp [SCConstStr "==", exprToSC e1, exprToSC e2]
    Gt   _ e1 e2 -> funBoolOp [SCConstStr ">", exprToSC e1, exprToSC e2]
    Ge   _ e1 e2 -> funBoolOp [SCConstStr ">=", exprToSC e1, exprToSC e2]
    Mul  _ e1 e2 -> funArithOp [SCConstStr "*", exprToSC e1, exprToSC e2]
    Add  _ e1 e2 -> funArithOp [SCConstStr "+", exprToSC e1, exprToSC e2]
    Pow  _ e1 e2 -> funArithOp [SCConstStr "pow", SCTypeCast SCFloat32 (exprToSC e1), SCTypeCast SCFloat32 (exprToSC e2)]
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


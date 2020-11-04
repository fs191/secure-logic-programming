module Solve (checkSat, extractSatSolution) where

import Relude

import qualified SimpleSMT as SMT
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Generics.Uniplate.Data

--import Data.Text.Prettyprint.Doc

import Expr

checkSat :: [Expr] -> IO Bool
checkSat es = do

  s <- SMT.newSolver "z3" ["-in"] Nothing
  void $ declareVars s (vars es)
  let sExprs = map fromJust $ filter (\x -> case x of Nothing -> False; _ -> True) $ exprToSExpr <$> es
  void $ traverse (SMT.assert s) sExprs
  result <- SMT.check s
  void $ SMT.stop s

  return $ case result of
               SMT.Unsat -> False
               _         -> True

extractSatSolution :: [Expr] -> [Expr] -> IO [Expr]
extractSatSolution _    [] = do return []
extractSatSolution args es = do

  s <- SMT.newSolver "z3" ["-in"] Nothing
  let vnames = vars es
  let vargs  = vars args

  declareVars s vnames

  -- if we could not handle at least one expression, then the SAT model is overapproximated
  -- in Prolog, we assume that all "suitable" expressions are ground and can be pushed into the end
  -- in Datalog, the order is not important anyway (although it affects the efficiency)
  let (goodZ3, badZ3) = partition (\(e,x) -> case x of Nothing -> False; _ -> True) $ zip es (exprToSExpr <$> es)
  let sExprs = map (fromJust . snd) goodZ3
  let rest   = map fst badZ3

  --putStrLn $ "====="
  --putStrLn $ show (pretty es)
  --putStrLn $ show sExprs
  --putStrLn $ show vnames
  --putStrLn $ show vargs

  void $ traverse (SMT.assert s) sExprs
  result <- SMT.check s

  res <- case result of
               SMT.Unsat   -> do return [constBool False]
               SMT.Unknown -> do return es
               SMT.Sat     -> do
                   let exprs = map (SMT.Atom . toString) vargs
                   values <- mapM (SMT.getExpr s) exprs
                   --putStrLn $ show values

                   -- try to negate the solution and see if there exist more solutions
                   let neg = if values == [] then [] else [foldr1 SMT.or $ map (\(x,val) -> Solve.neq x (SMT.value val)) (zip exprs values)]
                   void $ traverse (SMT.assert s) neg

                   result <- SMT.check s

                   let result2 = case result of
                                     SMT.Unsat -> rest ++ map (\(x,val) -> eIs (var x) (smtValueToExpr val)) (zip vargs values)
                                     _         -> es
                   return result2
  void $ SMT.stop s
  return res

smtValueToExpr :: SMT.Value -> Expr
smtValueToExpr (SMT.Bool c)   = constBool c
smtValueToExpr (SMT.Int  c)   = constInt $ fromIntegral c
smtValueToExpr (SMT.Real c)   = constFloat $ fromRational c
smtValueToExpr (SMT.Bits _ _) = error $ "Unsupported output in Z3 solver: a bit vector"
smtValueToExpr (SMT.Other c)  = error $ "Unknown output arrived from Z3 solver: " <> show c

exprToSExpr :: Expr -> Maybe SMT.SExpr
exprToSExpr (ConstInt _ x) = return $ SMT.Atom (show x)
exprToSExpr (Var _ x)      = return $ SMT.Atom (toString x)
exprToSExpr (Not _ x)   = SMT.not <$> exprToSExpr x
exprToSExpr (Neg _ x)   = SMT.neg <$> exprToSExpr x
exprToSExpr (Inv _ x)   = Solve.inv <$> exprToSExpr x
exprToSExpr (Div _ x y) = SMT.div <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Sub _ x y) = SMT.sub <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Lt _ x y)  = SMT.lt <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Le _ x y)  = SMT.leq <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Eq _ x y)  = SMT.eq <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Neq _ x y) = Solve.neq <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Un _ x y)  = SMT.eq <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Is _ x y)  = SMT.eq <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Gt _ x y)  = SMT.gt <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Ge _ x y)  = SMT.geq <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Mul _ x y) = SMT.mul <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Add _ x y) = SMT.add <$> exprToSExpr x <*> exprToSExpr y
--exprToSExpr (Min _ x y) = Solve.min <$> exprToSExpr x <*> exprToSExpr y
--exprToSExpr (Max _ x y) = Solve.max <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (And _ x y) = SMT.and <$> exprToSExpr x <*> exprToSExpr y
exprToSExpr (Or _ x y)  = SMT.or <$> exprToSExpr x <*> exprToSExpr y
-- TODO: Should probably print a warning if expression type is not supported
exprToSExpr _           = Nothing

--min :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
--min x y = SMT.ite (SMT.lt y x) y x

--max :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
--max x y = SMT.ite (SMT.lt x y) y x

neq :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
neq x y = SMT.not $ SMT.eq x y

inv :: SMT.SExpr -> SMT.SExpr
inv = SMT.div (SMT.Atom "1")

vars :: [Expr] -> [Text]
vars (e:es) = union (nub [v | Var _ v <- universe e]) (vars es)
vars [] = []

declareVars :: SMT.Solver -> [Text] -> IO SMT.Solver
declareVars s (v:vs) = do
  void $ SMT.declare s (toString v) (SMT.Atom "Int")
  declareVars s vs
declareVars s [] = return s


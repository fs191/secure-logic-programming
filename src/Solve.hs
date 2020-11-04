module Solve (checkSat) where

import Relude

import qualified SimpleSMT as SMT
import Data.List
import Data.Maybe
import Data.Generics.Uniplate.Data

import Expr

checkSat :: [Expr] -> IO Bool
checkSat es = do
  s <- SMT.newSolver "z3" ["-in"] Nothing
  void $ declareVars s (vars es)
  let sExprs = map fromJust $ filter (\x -> case x of Nothing -> False; _ -> True) $ exprToSExpr <$> es
  void $ traverse (SMT.assert s) sExprs
  result <- SMT.check s
  return $ case result of
               SMT.Unsat -> False
               _         -> True

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


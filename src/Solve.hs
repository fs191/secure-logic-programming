module Solve (checkSat) where

import Data.Maybe
import qualified Data.Traversable as T
import qualified SimpleSMT as SMT
import qualified Data.Map as M
import Control.Monad.State
import Data.List
import Data.Generics.Uniplate.Data

import Expr

checkSat :: [Expr] -> IO Bool
checkSat es = do
  s <- SMT.newSolver "z3" ["-in"] Nothing
  declareVars s (vars es)
  asserts s es
  result <- SMT.check s
  let res = case result of
               SMT.Unsat -> False
               _         -> True

  return $ res

asserts :: SMT.Solver -> [Expr] -> IO SMT.Solver
asserts s (Pred {}:es) = asserts s es
asserts s (e:es) = do
  SMT.assert s (exprToSExpr e)
  asserts s es
asserts s [] = return s

exprToSExpr :: Expr -> SMT.SExpr
exprToSExpr (ConstInt _ x) = SMT.Atom (show x)
exprToSExpr (Var _ x)      = SMT.Atom x
exprToSExpr (Not _ x)   = SMT.not $ exprToSExpr x
exprToSExpr (Neg _ x)   = SMT.neg $ exprToSExpr x
exprToSExpr (Inv _ x)   = Solve.inv $ exprToSExpr x
exprToSExpr (Div _ x y) = SMT.div (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Sub _ x y) = SMT.sub (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Lt _ x y)  = SMT.lt (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Le _ x y)  = SMT.leq (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Eq _ x y)  = SMT.eq (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Neq _ x y) = Solve.neq (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Un _ x y)  = SMT.eq (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Is _ x y)  = SMT.eq (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Gt _ x y)  = SMT.gt (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Ge _ x y)  = SMT.geq (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Mul _ x y) = SMT.mul (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Add _ x y) = SMT.add (exprToSExpr x) (exprToSExpr y)
--exprToSExpr (Min _ x y) = Solve.min (exprToSExpr x) (exprToSExpr y)
--exprToSExpr (Max _ x y) = Solve.max (exprToSExpr x) (exprToSExpr y)
exprToSExpr (And _ x y) = SMT.and (exprToSExpr x) (exprToSExpr y)
exprToSExpr (Or _ x y)  = SMT.or (exprToSExpr x) (exprToSExpr y)
exprToSExpr _ = error "conversion not yet implemented"

--min :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
--min x y = SMT.ite (SMT.lt y x) y x

--max :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
--max x y = SMT.ite (SMT.lt x y) y x

neq :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
neq x y = SMT.not $ SMT.eq x y

inv :: SMT.SExpr -> SMT.SExpr
inv = SMT.div (SMT.Atom "1")

vars :: [Expr] -> [String]
vars (e:es) = union (nub [v | Var _ v <- universe e]) (vars es)
vars [] = []

declareVars :: SMT.Solver -> [String] -> IO SMT.Solver
declareVars s (v:vs) = do
  SMT.declare s v (SMT.Atom "Int")
  declareVars s vs
declareVars s [] = return s


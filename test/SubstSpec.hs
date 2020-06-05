module SubstSpec where

import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.List

import Data.Generics.Uniplate.Operations

import Test.Hspec

import Expr
import Substitution

x :: Expr
x = var "x"

y :: Expr
y = var "y"

xs :: Expr
xs = var "x"

ys :: Expr
ys = var "y"

zs :: Expr
zs = var "z"

c1 :: Expr
c1 = constInt 1

c2 :: Expr
c2 = constInt 2

f :: [Expr] -> Expr
f = predicate "f"

g :: [Expr] -> Expr
g = predicate "g"

spec :: Spec
spec = 
  do
    describe "Substitution.unify" $ do
      unifies c1 c1
      unifies x x
      unifies x y
      unifies x c1
      unifies (f [x]) (f [c1])
      unifies (f [x, x]) (f [y, c1])
      unifies (f [x, x]) (f [c1, c1])
      unifies (f [x, x]) (f [y, y])
      unifien't c1 c2
      unifien't (f [c1]) (f [c2])
      unifien't (f [x, x]) (f [c1, c2])
      unifien't (f [c1]) (f [c1, c2])
      unifien't (g [c1]) (f [c1])
    describe "Substitution.refreshVarNames" $ do
      refreshes c1
      refreshes x
      refreshes $ f [x]
      refreshes $ f [x, y]
      refreshes $ f [x, c1]
    describe "Substitution.compress" $ do
      compresses ["x" |-> ys, "y" |-> c1] ["x" |-> c1, "y" |-> c1]
      compresses ["x" |-> c2, "y" |-> c1] ["x" |-> c2, "y" |-> c1]
      compresses ["x" |-> ys, "y" |-> zs] ["x" |-> zs, "y" |-> zs, "z" |-> zs]
      

unifies :: Expr -> Expr -> Spec
unifies a b = it desc $
  (isJust $ a `unify` b) `shouldBe` True
  where desc = "Can unify " <> (show $ pretty a) <> " and " <> (show $ pretty b)

unifien't :: Expr -> Expr -> Spec
unifien't a b = it desc $
  (isJust $ a `unify` b) `shouldBe` False
  where desc = "Does not unify " <> (show $ pretty a) <> " and " <> (show $ pretty b)

refreshes :: Expr -> Spec
refreshes e = it desc $
  refreshAndApply "X_" e `shouldSatisfy` (\s -> and [isPrefixOf "X_" v | (Var _ v) <- universe s])
  where desc = "Refreshes all variables in " <> (show $ pretty e)

compresses :: [Subst] -> [Subst] -> Spec
compresses input output = it desc $
  compress (mconcat input) `shouldBe` mconcat output
  where desc = "Compresses " <> (show $ pretty input)


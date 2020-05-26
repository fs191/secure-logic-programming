module SubstSpec where

import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.List

import Data.Generics.Uniplate.Operations

import Test.Hspec

import Expr
import DBClause
import Substitution

x :: Expr DBVar
x = Var $ Free "x"

y :: Expr DBVar
y = Var $ Free "y"

c1 :: Expr a
c1 = ConstNum 1

c2 :: Expr a
c2 = ConstNum 2

f :: [Expr a] -> Expr a
f = Pred "f"

g :: [Expr a] -> Expr a
g = Pred "g"

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
      

unifies :: Expr DBVar -> Expr DBVar -> Spec
unifies a b = it ("Can unify " <> (show $ pretty a) <> " and " <> (show $ pretty b))
  $ (isJust $ a `unify` b) `shouldBe` True

unifien't :: Expr DBVar -> Expr DBVar -> Spec
unifien't a b = it ("Does not unify " <> (show $ pretty a) <> " and " <> (show $ pretty b))
  $ (isJust $ a `unify` b) `shouldBe` False

refreshes :: Expr DBVar -> Spec
refreshes e = it ("Refreshes all variables in " <> (show $ pretty e))
  $ refreshAndApply "X_" e `shouldSatisfy` (\s -> and [isPrefixOf "X_" $ name v | (Var v) <- universe s])


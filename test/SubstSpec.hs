module SubstSpec where

import Data.Maybe
import Data.Text.Prettyprint.Doc

import Test.Hspec

import Expr
import Substitution

x :: Expr
x = var "x"

y :: Expr
y = var "y"

z :: Expr
z = var "z"

x0 :: Expr
x0 = var "X_0"

x1 :: Expr
x1 = var "X_1"

x2 :: Expr
x2 = var "X_2"

c1 :: Expr
c1 = constInt 1

c2 :: Expr
c2 = constInt 2

f :: [Expr] -> Expr
f = predicate "f"

g :: [Expr] -> Expr
g = predicate "g"

spec :: Spec
spec = parallel $
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
      refreshes c1 c1
      refreshes x x0
      refreshes (f [x]) $ f [x0]
      refreshes (f [x, y]) $ f [x0, x1]
      refreshes (f [x, c1]) $ f [x0, c1]
      refreshes (f [x1, x0]) $ f [x0, x1]
      refreshes (f [x1, x1]) $ f [x0, x0]
      refreshes (f [x1, x1, x0]) $ f [x0, x0, x1]
      refreshes (f [x2, x1, x0]) $ f [x0, x1, x2]
    describe "Substitution.compress" $ do
      compresses [x |->! y, y |->! c1] [x |->! c1, y |->! c1]
      compresses [x |->! c2, y |->! c1] [x |->! c2, y |->! c1]
      compresses [x |->! y, y |->! z] [x |->! z, y |->! z, z |->! z]
      

unifies :: Expr -> Expr -> Spec
unifies a b = it desc $
  (isJust $ a `unify` b) `shouldBe` True
  where desc = "Can unify " <> (show $ pretty a) <> " and " <> (show $ pretty b)

unifien't :: Expr -> Expr -> Spec
unifien't a b = it desc $
  (isJust $ a `unify` b) `shouldBe` False
  where desc = "Does not unify " <> (show $ pretty a) <> " and " <> (show $ pretty b)

refreshes :: Expr -> Expr -> Spec
refreshes e e' = it desc $
  refreshAndApply "X_" e `shouldBe` e'
  where desc = "Refreshes " <> (show $ pretty e) <> " to " <> (show $ pretty e')

compresses :: [Subst] -> [Subst] -> Spec
compresses input output = it desc $
  compress (mconcat input) `shouldBe` mconcat output
  where desc = "Compresses " <> (show $ pretty input)

(|->!) :: Expr -> Expr -> Subst
a |->! b = fromJust $ a |-> b

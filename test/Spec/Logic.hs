{-# LANGUAGE OverloadedStrings #-}
module Spec.Logic
  ( logicSpecs
  ) where

import Prelude hiding (lookup)

import Base
import Formula
import Logic
import Universe
import Util (forceKey)

import Test.Hspec

import Control.Monad.State (runState)

runU :: State Universe a -> (a, Universe)
runU = flip runState empty

check' :: Universe -> Int64 -> Int64 -> Maybe TValueId
check' u s p = lookup (forceKey s) (forceKey p) u

insertTheorem' :: Int64 -> Implication -> State Universe ()
insertTheorem' = insertTheorem . forceKey

logicSpecs :: Spec
logicSpecs = do
  it "allows direct assertions" $ do
    let (_,u) = runU $ do
          assertTrait $ (1,1) ~. True

    check' u 1 1 `shouldBe` Just true

  it "can deduce when traits are added" $ do
    let (proofs,u) = runU $ do
          insertTheorem' 1 $ (1 ==. True) =>. (2 ==. True)
          assertTrait $ (1,1) ~. True

    check' u 1 2 `shouldBe` Just true
    length proofs `shouldBe` 1

  it "can chain deductions" $ do
    let (proofs,u) = runU $ do
          insertTheorem' 1 $ (1 ==. True) =>. (2 ==. True)
          insertTheorem' 2 $ (2 ==. True) =>. (3 ==. True)
          assertTrait $ (1,1) ~. True

    check' u 1 3 `shouldBe` Just true
    length proofs `shouldBe` 2

  it "can assert conjunctions" $ do
    let f = (1 ==. True) =>. ((2 ==. True) &&. (3 ==. True))
        (_,u) = runU $ do
          insertTheorem' 1 f
          assertTrait $ (1,1) ~. True

    check' u 1 2 `shouldBe` Just true
    check' u 1 3 `shouldBe` Just true

  it "can assert disjunctions (if most parts are known)" $ do
    let f = (1 ==. True) =>. ((2 ==. True) ||. (3 ==. True))
        (_,u) = runU $ do
          insertTheorem' 1 f
          _ <- assertTrait $ (1,2) ~. False
          assertTrait $ (1,1) ~. True

    check' u 1 3 `shouldBe` Just true

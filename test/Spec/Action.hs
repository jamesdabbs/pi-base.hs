{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Action
  ( actionSpecs
  ) where

import Prelude hiding (lookup)

import Base
import Actions (getUniverse, assertTrait, assertTheorem)
import Formula
import Models (runDB)
import Universe
import Util (toSqlKey)

import Database.Persist (insert, getBy)
import Test.Hspec
import qualified Test.Hspec as H

space :: Action SpaceId
space = runDB . insert $ Space "" "" Nothing

property :: Action PropertyId
property = runDB . insert $ Property "" "" "" (toSqlKey 1)

t,f :: PropertyId -> Formula PropertyId
t p = Atom p True
f p = Atom p False

lookupU :: SpaceId -> PropertyId -> Action (Maybe TValueId)
lookupU s p = do
  u <- getUniverse
  return $ lookup s p u

lookupDB :: SpaceId -> PropertyId -> Action (Maybe TValueId)
lookupDB s p = do
  mt <- runDB . getBy $ TraitSP s p
  return $ case mt of
    Nothing -> Nothing
    Just (Entity _ tr) -> Just $ traitValueId tr

actionSpecs :: Config -> H.Spec
actionSpecs c = do
  let
    run a = do
      er <- runEitherT $ runReaderT (runAction a) c
      case er of
        Left err -> error $ show err
        Right v  -> return v

  it "can persist to the database" $ do
    rs <- run $ do
      p <- property
      q <- property
      r <- property

      s <- space

      _ <- assertTheorem $ t p =>. t q
      _ <- assertTheorem $ t q =>. f r
      _ <- assertTrait   $ s |= p

      -- TODO: figure out how to lift assertions to here
      r1 <- lookupU  s p
      r2 <- lookupDB s p
      r3 <- lookupU  s r
      r4 <- lookupDB s r
      return (r1, r2, r3, r4)
    rs `shouldBe` (Just true, Just true, Just false, Just false)

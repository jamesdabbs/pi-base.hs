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

run :: Action a -> Config -> IO a
run a c = do
  er <- runEitherT $ runReaderT (runAction a) c
  case er of
    Left err -> error $ show err
    Right v  -> return v

yields :: (Show a, Eq a) => Action a -> a -> Action ()
yields action result = do
  real <- action
  liftIO $ shouldBe real result

actionSpecs :: H.SpecWith Config
actionSpecs = do
  it "can persist to the database" $ run $ do
    p <- property
    q <- property
    r <- property

    s <- space

    _ <- assertTheorem $ t p =>. t q
    _ <- assertTheorem $ t q =>. f r
    _ <- assertTrait   $ s |= p

    lookupU  s p `yields` Just true
    lookupDB s p `yields` Just true
    lookupU  s r `yields` Just false
    lookupDB s r `yields` Just false

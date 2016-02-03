{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.App
  ( appSpecs
  ) where

import Base

import Control.Monad.IO.Class      (MonadIO)
import Data.Aeson                  hiding (json)
import Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Network.HTTP.Types          (methodPost, status201)
import Network.Wai                 (Application)
import Network.Wai.Test            (SResponse(..))
import Test.Hspec                  (it)
import qualified Test.Hspec as H
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified Data.HashMap.Strict as HM

shouldBe :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
shouldBe a b = liftIO $ H.shouldBe a b

postJSON :: ByteString -> AuthToken -> L.ByteString -> WaiSession SResponse
postJSON path token body = request methodPost path headers body
  where
    headers =
      [ ("Authorization", token)
      , ("Content-Type", "application/json")
      , ("Accept", "application/json")
      ]

toObject :: L.ByteString -> Object
toObject s = case eitherDecode s of
  (Left         err) -> error err
  (Right (Object o)) -> o
  _ -> error "Couldn't decode as object"

shouldIncludeJson :: L.ByteString -> L.ByteString -> WaiSession ()
shouldIncludeJson haystack needle = intersection `shouldBe` needle'
    where
      needle' = toObject needle
      intersection = HM.intersection (toObject haystack) needle'

appSpecs :: H.SpecWith Application
appSpecs = do
  it "can create new spaces" $ do
    let fields = [json|{name:"New Space", description:"Desc"}|]
    (SResponse status _ body) <- postJSON "/spaces" "test@example.com" fields
    status `shouldBe` status201
    body `shouldIncludeJson` fields


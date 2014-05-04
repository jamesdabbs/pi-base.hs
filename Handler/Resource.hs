module Handler.Resource
( page
, getJson
) where

import Import

import Data.Text (unpack)


intParam :: Text -> Int -> Handler Int
intParam name def = do
  val <- lookupGetParam name
  case val of
    Nothing   -> return def
    Just text -> return . read . unpack $ text

page from size = do
  _from <- intParam "from" from
  _size <- intParam "size" size
  objects <- runDB $ selectList [] [OffsetBy _from, LimitTo _size]
  return objects

getJson _id = do
  obj <- runDB $ get404 _id
  returnJson $ obj

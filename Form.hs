module Form
( propertyField
, valueField
, formulaField
, fs
, fLayout
, save
) where

import Import
import Model.Space (spaceUnknownProperties)
import Util (encodeText, decodeText)

import Yesod.Form.Bootstrap3


propertyField :: SpaceId -> Field Handler PropertyId
propertyField sid = selectField $ do
  available <- spaceUnknownProperties sid
  optionsPairs $ map (\(Entity _id p) -> (propertyName p, _id)) available

valueField :: Field Handler TValueId
valueField = selectField $ do
  entities <- runDB $ selectList [] [Asc TValueId]
  optionsPairs $ map (\(Entity _id v) -> (tValueName v, _id)) entities

parseFormula :: Text -> Either FormMessage (Formula Int64)
parseFormula t = case decodeText t of
  Nothing -> Left $ MsgInvalidEntry "Could not parse formula"
  Just f  -> Right f

-- Note: this only updates the class if one is already present
addClass :: Text -> [(Text,Text)] -> [(Text,Text)]
addClass klass attrs = map add attrs
  where
    add ("class", a) = ("class", klass <> " " <> a)
    add b = b

formulaField :: Field Handler (Formula Int64)
formulaField = Field
    { fieldParse = parseHelper $ parseFormula
    , fieldView  = \_id name attrs val isReq ->
        let attrs' = addClass "formula" attrs
        in toWidget [hamlet|
$newline never
<input id="#{_id}" name="#{name}" *{attrs'} :isReq:required="" value="#{showVal val}">
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id encodeText

fs :: Text -> FieldSettings App
fs = bfs

fLayout :: BootstrapFormLayout
fLayout = BootstrapHorizontalForm {
  bflLabelOffset = ColMd 0,
  bflLabelSize   = ColMd 4,
  bflInputOffset = ColMd 0,
  bflInputSize   = ColMd 8
}

save :: MonadHandler m => AForm m ()
save = bootstrapSubmit ("Save" :: BootstrapSubmit Text)

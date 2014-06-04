module Form
( spaceField
, propertyField
, valueField
, formulaField
, fs
, fLayout
, save
) where

import Import
import Util (encodeText, decodeText)

import Yesod.Form.Bootstrap3


choices :: (PersistEntity val, YesodPersist site, RenderMessage site msg,
  PersistQuery (YesodPersistBackend site (HandlerT site IO)),
  PersistEntityBackend val
  ~ PersistMonadBackend
    (YesodPersistBackend site (HandlerT site IO))) =>
  EntityField val typ
  -> (val -> msg) -> HandlerT site IO (OptionList (Key val))
choices order display = do
  entities <- runDB $ selectList [] [Asc order]
  optionsPairs $ map (\s -> (display $ entityVal s, entityKey s)) entities

spaceField :: Field Handler SpaceId
spaceField = selectField (choices SpaceName spaceName)

propertyField :: Field Handler PropertyId
propertyField = selectField (choices PropertyName propertyName)

valueField :: Field Handler TValueId
valueField = selectField (choices TValueId tValueName)

parseFormula :: Text -> Either FormMessage (Formula Int64)
parseFormula t = case decodeText t of
  Nothing -> Left $ MsgInvalidEntry "Could not parse formula"
  Just f  -> Right f

formulaField :: Field Handler (Formula Int64)
formulaField = Field
    { fieldParse = parseHelper $ parseFormula
    , fieldView  = \_id name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{_id}" name="#{name}" *{attrs} :isReq:required="" value="#{showVal val}">
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

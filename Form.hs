module Form
( spaceField
, propertyField
, valueField
, fs
) where

import Import

import Yesod.Form.Bootstrap3 (bfs)


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

fs :: Text -> FieldSettings App
fs = bfs

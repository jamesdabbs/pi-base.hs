module Handler.Cors where

import Import


allowed :: Handler Value
allowed = returnJson $ object []

optionsSpacesR, optionsPropertiesR, optionsTheoremsR, optionsTraitsR :: Handler Value
optionsSpacesR     = allowed
optionsPropertiesR = allowed
optionsTheoremsR   = allowed
optionsTraitsR     = allowed

optionsSpaceR, optionsPropertyR, optionsTheoremR, optionsTraitR :: a -> Handler Value
optionsSpaceR    _ = allowed
optionsPropertyR _ = allowed
optionsTheoremR  _ = allowed
optionsTraitR    _ = allowed

optionsExploreR, optionsResetR, optionsTestResetR :: Handler Value
optionsExploreR    = allowed
optionsResetR      = allowed
optionsTestResetR  = allowed

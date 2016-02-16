module Api.Base
  ( module Api.Types
  , module Base
  , module Api.Base
  , pageJSON
  ) where

import Api.Types
import Base
import Pager (pageJSON)

import Servant as Api.Base hiding (serve)

module Base.Aeson (module X, fromJSONValue) where

import Data.Aeson as X
import Data.Aeson.Types as X
import Data.Aeson.KeyMap as X
import Data.Aeson.Key as X
import Data.Aeson.Optics as X

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON
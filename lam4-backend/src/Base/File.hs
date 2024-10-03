module Base.File (module X, readFileUtf8, writeFileUtf8) where

import Control.Monad.IO.Class
import Data.Text (Text)
-- deliberately not importing readFileLBS
import Relude.File as X (readFileBS, writeFileLBS, writeFileBS)
import Relude.String.Conversion

-- | https://www.snoyman.com/blog/2016/12/beware-of-readfile/
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 = fmap decodeUtf8 . readFileBS

writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp = X.writeFileLBS fp . encodeUtf8
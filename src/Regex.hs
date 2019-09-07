{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Regex where

import Control.Lens.Regex
import Text.RawString.QQ
import Data.ByteString
import Control.Lens
import Text.Regex.PCRE.Light (multiline)
import Data.Aeson.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Bifunctor

article :: ByteString
article = [r|
{"id": "my-title"}
# My Title

{this will be unaffected}

{"colored": true, "id": "code-block"}
```haskell
myCode
```
|]


jsonAnnotation :: Regex
jsonAnnotation = compile "^\\{.*\\}$" [multiline]

test :: Int
test = ()


capitalizedTitles = article &  regexBS jsonAnnotation . match . key "id" . _String %~ T.toUpper

-- allTitles = article ^.. regexBS jsonAnnotation . match . key "id" . _String
-- ["my-title","code-block"]

-- thing :: ByteString -> ByteString
-- thing =  %~

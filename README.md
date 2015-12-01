# snap-language
Language handling for Snap.

Support for determining the client's prefered language using
the Accept-Language header or using suffixes to the requested URI.

Try this example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Simple where

import Snap.Http.Server
import Snap.Core
import Data.Map
import Control.Applicative

import Snap.Language

data Lang = SV | EN deriving Eq

table :: RangeMapping Lang
table = fromList [("sv-SE",SV),("en-GB",EN)]

getLanguage :: Snap Lang
getLanguage = 
  getSuffixLanguage table <|> 
  getAcceptLanguage table <|> 
  return EN

test :: IO ()
test = quickHttpServe $ do
  lang <- getLanguage
  dir "hello" $ handler lang

handler :: Lang -> Snap ()
handler EN = writeBS "hello"
handler SV = writeBS "hej"
```

You can now access `/hello` and you will get an answer depending on your Accept-Language header.

Or you can access `/hello.en-GB` or `/hello.sv-SE` directly.
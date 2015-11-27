{-# LANGUAGE OverloadedStrings #-}
-- |
-- Language handling for Snap

module Snap.Language 
  ( Language(..)
  , acceptLanguage
  , getLanguage
  , setLanguageToCookie
  , readLanguageCookie
  ) where

import Data.Attoparsec.ByteString.Char8(parseOnly,
                                        string,
                                        double,
                                        Parser,
                                        letter_ascii,
                                        many1,
                                        many',
                                        char,
                                        option,
                                        eitherP,
                                        sepBy,
                                        skipSpace,
                                        endOfLine)
import Data.ByteString                 (ByteString)
import Data.ByteString.Char8           (pack,unpack)
import Data.Char                       (toLower)
import Data.List                       (intersperse,isPrefixOf,find)
import Control.Applicative             ((*>),(<$>),(<*>),(<|>))
import Snap.Core                       (getsRequest,
                                        getHeader,
                                        MonadSnap,
                                        Cookie(..),
                                        addResponseCookie,
                                        modifyResponse,
                                        getCookie,
                                        setHeader,
                                        pass)
import Data.Map                        (Map,
                                        toList)
import Data.Tuple                      (swap)

range :: Parser String
range = (++) <$> mletters <*> (fmap concat $ many' $ (:) <$> (char '-') <*> mletters)
  where mletters = many1 letter_ascii

rangeval :: Parser (Maybe String, Double)
rangeval = 
  do
    r <- eitherP (char '*') range
    q <- option 1 $ string ";q=" *> double
    return (either (const Nothing) Just r,q)

acceptLanguageParser :: Parser [(Maybe String, Double)]
acceptLanguageParser = skipSpace *> (sepBy rangeval $ skipSpace *> char ',' <* skipSpace)

matches :: String 
        -> Maybe String 
        -> Bool
matches _ Nothing = True
matches provided (Just requested) = 
  (map toLower requested) `isPrefixOf` (map toLower provided)

candidates :: Map String a
           -> [(Maybe String, Double)]
           -> [(a,Double)]
candidates provided requested = concatMap go $ toList provided
  where go (range,x) = map (\(a,b) -> (x,b)) $ filter (matches range . fst) requested

pickLanguage' :: Map String a
              -> [(Maybe String,Double)]
              -> Maybe a
pickLanguage' provided requested = fmap fst $ foldr go Nothing $ candidates provided requested
  where go r'           Nothing                      = return r'
        go r'@(val',q') (Just r@(val,q)) | q' > q    = return r'
                                         | otherwise = return r 

pickLanguage :: Map String a
             -> ByteString
             -> Maybe a
pickLanguage provided headerString = 
  either (const Nothing) (pickLanguage' provided) $ parseOnly acceptLanguageParser headerString

-- | Attempt to find a suitable language according to the Accept-Language
-- header of the request. This handler will call pass if it cannot find a
-- suitable language.
acceptLanguage :: (Language a, MonadSnap m)
               => m a
acceptLanguage =
  do
    al <- getsRequest $ getHeader "Accept-Language"
    maybe pass return $ al >>= pickLanguage rangeMapping

-- | Your own internal representation of a language should have
-- Eq, Read and Show instances. Also the following should hold true:
--
-- > (read $ show x) == x
-- 
-- The basic idea is to use a sum type and just derive all the instances.
class (Eq a,Read a,Show a) => Language a
  where rangeMapping :: Map String a -- ^ A Mapping from language ranges as defined in rfc2616 to languages in your own representation.

-- | Set the language to a cookie. If this cookie is set it will override 
-- anything in Accept-Language.
-- The idea is that you use this when the user makes a choice in your
-- application to use a specific language.
setLanguageToCookie :: (MonadSnap m, Language a)
                    => a -- ^ the language to set in the cookie.
                    -> m ()
setLanguageToCookie val =
  modifyResponse $ addResponseCookie $ Cookie "snapLanguage" (pack $ show val) Nothing Nothing Nothing False False

-- | Attempt to read the language from a cookie. Uses Read to read the cookie.
readLanguageCookie :: (MonadSnap m, Language a)
                   => m a
readLanguageCookie =
  do
    c <- getCookie "snapLanguage"
    case fmap (reads . unpack . cookieValue) c of
      Just [(val,_)] -> return val
      _              -> pass

setContentLanguage :: (MonadSnap m, Language a)
                   => a
                   -> m ()
setContentLanguage val =
 maybe (return ()) go $ lookup val $ map swap $ toList rangeMapping
   where go = modifyResponse . setHeader "Content-Language" . pack

-- | Get the language to use. getLanguage will first look for the language
-- cookie, otherwise it will look into the Accept-Language header.
-- Will also try to set Content-Language in the Repsonse.
getLanguage :: (MonadSnap m, Language a)
            => a              -- ^ a default language.
            -> m a
getLanguage def =
  do
    lang <- readLanguageCookie <|> acceptLanguage <|> return def
    setContentLanguage lang 
    return lang
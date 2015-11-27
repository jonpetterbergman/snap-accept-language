{-# LANGUAGE OverloadedStrings #-}
-- |
-- Language handling for Snap

module Snap.Language 
  ( Language
  , RangeMapping
  , getLanguage
  , setLanguageToCookie
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

candidates :: RangeMapping a
           -> [(Maybe String, Double)]
           -> [(a,Double)]
candidates provided requested = concatMap go $ toList provided
  where go (range,x) = map (\(a,b) -> (x,b)) $ filter (matches range . fst) requested

pickLanguage' :: RangeMapping a
              -> [(Maybe String,Double)]
              -> Maybe a
pickLanguage' provided requested = fmap fst $ foldr go Nothing $ candidates provided requested
  where go r'           Nothing                      = return r'
        go r'@(val',q') (Just r@(val,q)) | q' > q    = return r'
                                         | otherwise = return r 

pickLanguage :: RangeMapping a
             -> ByteString
             -> Maybe a
pickLanguage provided headerString = 
  either (const Nothing) (pickLanguage' provided) $ parseOnly acceptLanguageParser headerString

snapLanguage :: MonadSnap m
             => RangeMapping a
             -> m a
snapLanguage provided =
  do
    al <- getsRequest $ getHeader "Accept-Language"
    maybe pass return $ al >>= pickLanguage provided

-- | Your own internal representation of a language should have
-- Eq, Read and Show instances. Also the following should hold true:
--
-- > (read $ show x) == x
-- 
-- The basic idea is to use a sum type and just derive all the instances.
class (Eq a,Read a,Show a) => Language a

-- | A Mapping from language ranges as defined in rfc2616 to languages
-- in your own representation.
type RangeMapping a = Map String a

-- | Set the language to a cookie. If this cookie is set it will override 
-- anything in Accept-Language.
-- The idea is that you use this when the user makes a choice in your
-- application to use a specific language.
setLanguageToCookie :: (MonadSnap m, Language a)
                    => a -- ^ the language to set in the cookie.
                    -> m ()
setLanguageToCookie val =
  modifyResponse $ addResponseCookie $ Cookie "snapLanguage" (pack $ show val) Nothing Nothing Nothing False False

readLanguageCookie :: (MonadSnap m, Read a)
                   => m a
readLanguageCookie =
  do
    c <- getCookie "snapLanguage"
    case fmap (reads . unpack . cookieValue) c of
      Just [(val,_)] -> return val
      _              -> pass

setContentLanguage :: (MonadSnap m, Language a)
                   => a
                   -> RangeMapping a
                   -> m ()
setContentLanguage val provided =
 maybe (return ()) go $ lookup val $ map swap $ toList provided
   where go = modifyResponse . setHeader "Content-Language" . pack

-- | Get the language to use. getLanguage will first look for the language
-- cookie, otherwise it will look into the Accept-Language header.
-- Will also try to set Content-Language in the Repsonse.
getLanguage :: (MonadSnap m, Language a)
            => a              -- ^ a default language.
            -> RangeMapping a 
            -> m a
getLanguage def provided =
  do
    lang <- readLanguageCookie <|> snapLanguage provided <|> return def
    maybe (return ()) (modifyResponse . setHeader "Content-Language" . pack . fst) $ find ((==) lang . snd) $ toList provided
    return lang
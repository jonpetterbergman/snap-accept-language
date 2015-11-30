{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Language handling for Snap

module Snap.Language 
  ( Language(..)
  , getAcceptLanguage
  , getSuffixLanguage
  , setContentLanguage
  ) where

import           Data.Attoparsec.ByteString.Char8(parseOnly,
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
import           Data.ByteString                 (ByteString,
                                                  isSuffixOf)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Char                       (toLower)
import           Data.List                       (intersperse,isPrefixOf,find)
import           Control.Applicative             ((*>),(<$>),(<*>),(<|>))
import           Snap.Core                       (getsRequest,
                                                  getRequest,
                                                  putRequest,
                                                  getHeader,
                                                  MonadSnap,
                                                  Cookie(..),
                                                  addResponseCookie,
                                                  modifyResponse,
                                                  getCookie,
                                                  setHeader,
                                                  pass,
                                                  rqPathInfo)
import           Data.Maybe                      (mapMaybe,
                                                  listToMaybe)
import           Data.Map                        (Map,
                                                  toList)
import           Data.Tuple                      (swap)

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
getAcceptLanguage :: (Language a, MonadSnap m)
               => m a
getAcceptLanguage =
  do
    al <- getsRequest $ getHeader "Accept-Language"
    maybe pass return $ al >>= pickLanguage rangeMapping

-- | Your own internal representation of a language.
class Eq a => Language a
  where rangeMapping :: Map String a -- ^ A Mapping from language ranges as defined in rfc2616 to languages in your own representation.

removeSuffix :: ByteString
             -> ByteString
             -> Maybe ByteString
removeSuffix suf x | suf `B.isSuffixOf` x = Just $ B.take ((B.length x) - (B.length suf)) x
                   | otherwise            = Nothing

suffixes :: Language a
         => [(ByteString,a)]
suffixes = map go $ toList rangeMapping 
  where go (str,val) = (BC.pack $ '.':str,val)

matchSuffix :: ByteString
            -> [(ByteString,a)]
            -> Maybe (ByteString,a)
matchSuffix str sfxs = listToMaybe $ mapMaybe go sfxs
  where go (sfx,val) = fmap (,val) $ removeSuffix sfx str

-- | Attempt to find a suitable language according to a suffix URI.
-- Will call pass if it cannot find a suitable language.
-- If a match is found, the suffix will be removed from the URI in the request.
getSuffixLanguage :: (Language a, MonadSnap m)
                  => m a
getSuffixLanguage = 
  do
    r <- getRequest
    case matchSuffix (rqPathInfo r) suffixes of
      Nothing -> pass
      Just (rqPathInfo',val) -> 
        do
          putRequest $ r { rqPathInfo = rqPathInfo' }
          return val


switchSuffixLanguage :: Language a
                     => ByteString
		     -> Maybe a
                     -> ByteString
switchSuffixLanguage uri (lang :: Maybe a) = maybe (addSuffix lang path) (addSuffix lang . fst) $ matchSuffix path (suffixes :: [(ByteString,a)])
  where (path,params)    = BC.break ((==) '?') uri
        addSuffix lang p = B.concat [p,findSfx lang,params]
        findSfx Nothing  = B.empty
        findSfx (Just l) = maybe B.empty id $ lookup l $ map swap suffixes

-- | Set the Content-Language header in the response.
setContentLanguage :: (Language a, MonadSnap m)
                   => a
                   -> m ()
setContentLanguage val =
 maybe (return ()) go $ lookup val $ map swap $ toList rangeMapping
   where go = modifyResponse . setHeader "Content-Language" . BC.pack


{-# LANGUAGE OverloadedStrings #-}

module Snap.AcceptLanguage 
  ( setLanguageToCookie
  , getLanguage 
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
                                        Snap,Cookie(..),
                                        addResponseCookie,
                                        modifyResponse,
                                        getCookie,
                                        setHeader)

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

candidates :: [(String,a)]
           -> [(Maybe String, Double)]
           -> [(a,Double)]
candidates provided requested = concatMap go provided
  where go (range,x) = map (\(a,b) -> (x,b)) $ filter (matches range . fst) requested

pickLanguage' :: a
             -> [(String,a)]
             -> [(Maybe String,Double)]
             -> a
pickLanguage' def provided requested = fst $ foldr go (def,0) $ candidates provided requested
  where go r'@(val',q') r@(val,q) | q' > q    = r'
                                  | otherwise = r 

pickLanguage :: a
             -> [(String,a)]
             -> ByteString
             -> a
pickLanguage def provided headerString = 
  either (const def) (pickLanguage' def provided) $ parseOnly acceptLanguageParser headerString

snapLanguage :: a
             -> [(String,a)]
             -> Snap a
snapLanguage def provided =
  do
    al <- getsRequest $ getHeader "Accept-Language"
    return $ maybe def (pickLanguage def provided) al

setLanguageToCookie :: Show a
                    => a
                    -> Snap ()
setLanguageToCookie val =
  modifyResponse $ addResponseCookie $ Cookie "snapLanguage" (pack $ show val) Nothing Nothing Nothing False False

readLanguageCookie :: Read a
                   => Snap (Maybe a)
readLanguageCookie =
  do
    c <- getCookie "snapLanguage"
    case fmap (reads . unpack . cookieValue) c of
      Just [(val,_)] -> return $ Just val
      _              -> return Nothing

getLanguage' :: Read a
                 => a
                 -> [(String,a)]
                 -> Snap a
getLanguage' def provided =
  readLanguageCookie >>=
  maybe (snapLanguage def provided) return

getLanguage :: (Read a,Eq a)
                => a
                -> [(String,a)]
                -> Snap a
getLanguage def provided =
  do
    lang <- getLanguage' def provided
    maybe (return ()) (modifyResponse . setHeader "ContentLanguage" . pack . fst) $ find ((==) lang . snd) provided
    return lang
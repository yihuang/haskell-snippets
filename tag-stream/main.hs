{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
import Data.List hiding (take)
import Prelude hiding (take)
import Control.Monad
import Control.Applicative hiding (many)
import Data.Monoid
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

if' :: a -> a -> Bool -> a
if' a1 a2 cond = if cond then a1 else a2

splitLast :: [a] -> ([a], a)
splitLast = loop []
  where
    loop acc [] = error "split empty list"
    loop acc (x:[]) = (reverse acc, x)
    loop acc (x:xs) = loop (x:acc) xs

data Token' s = TagOpen s
             | TagClose s
             | PartialToken s
             | Text s
    deriving (Show)

type Token = Token' ByteString

tag :: Parser Token
tag = do
    string "<"
    s <- takeTill (=='>')
    atEnd >>= if'
        (return $ PartialToken $ "<" `mappend` s)
        (do take 1
            return $ case S.uncons s of
                Just ('/', s') -> TagClose s'
                Nothing -> TagOpen S.empty
                _ -> TagOpen s
        )

text :: Parser Token
text = do
    s <- takeTill (=='<')
    if S.null s
        then empty
        else return $ Text s

token :: Parser Token
token = tag <|> text

html :: Parser [Token]
html = many token

accumParse :: ByteString -> ByteString -> Either String (ByteString, [Token])
accumParse acc input = splitPartial <$> parseOnly html (acc `mappend` input)
  where
    splitPartial :: [Token] -> (ByteString, [Token])
    splitPartial [] = (S.empty, [])
    splitPartial (splitLast -> (init, PartialToken s)) = (s, init)
    splitPartial tokens = (S.empty, tokens)

main = do
    let toTokens = EL.concatMapAccumM accumParse S.empty
        enum = E.enumList 2 ["  <a href=", "\"x", "x\">x", "x</a>"]
    print $ E.run_ $ (enum E.$= toTokens) E.$$ EL.consume

{-# LANGUAGE OverloadedStrings #-}
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

-- takeTill' :: (Char -> Bool) -> Parser ByteString
-- takeTill' p = do
--     (h, t) <- span p <$> get
--     put t
--     return h

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
    end <- atEnd
    if end
      then return $ PartialToken $ "<" `mappend` s
      else do
        take 1
        return $ case S.uncons s of
            Just ('/', s') -> TagClose s'
            Nothing -> TagOpen S.empty
            _ -> TagOpen s

text :: Parser Token
text = do
    s <- takeTill (=='<')
    if S.null s
      then empty
      else return $ Text s

token = tag <|> text

html :: Parser [Token]
html = many token

accum :: ByteString -> ByteString -> Either String (ByteString, [Token])
accum acc input = handlePartial <$> (parseOnly html $ acc `mappend` input)
  where
    handlePartial tokens =
      if (null tokens)
        then (S.empty, [])
        else case last tokens of
          PartialToken s
            -> (s, init tokens)
          _ -> (S.empty, tokens)

main = do
    let enum = E.enumList 1 ["  <a href=", "\"x", "x\">x", "x</a>"] E.$= EL.concatMapAccumM accum S.empty
    print $ (E.run_ $ enum E.$$ EL.consume)

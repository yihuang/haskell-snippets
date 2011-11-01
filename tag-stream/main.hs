{-# LANGUAGE OverloadedStrings, ViewPatterns, FlexibleInstances, TupleSections #-}
import System.Environment
import Data.List hiding (take)
import Prelude hiding (take, takeWhile)
import Control.Monad
import Control.Applicative hiding (many)
import Data.Monoid
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as E
import qualified Data.Enumerator.List as EL

if' :: a -> a -> Bool -> a
if' a1 a2 cond = if cond then a1 else a2

splitLast :: [a] -> ([a], a)
splitLast = loop []
  where
    loop acc [] = error "split empty list"
    loop acc (x:[]) = (reverse acc, x)
    loop acc (x:xs) = loop (x:acc) xs

type Attr' s = (s, s)
type Attr = Attr' ByteString
data Token' s = TagOpen s [Attr' s]
             | TagClose s
             | PartialToken s
             | Text s
    --deriving (Show)

type Token = Token' ByteString

instance Show (Token' ByteString) where
    show (TagOpen name attrs) = "TagOpen "++S.unpack name++show attrs
    show (TagClose name) = "TagClose "++S.unpack name
    show (PartialToken s) = "Partial "++S.unpack s
    show (Text s) = "Text"++show (S.length s)

attr :: Parser Attr
attr = do
    name <- takeTill (inClass ">= ")
    skipSpace
    option (name, S.empty) $ do
        string "="
        (name,) <$> takeTill (inClass "> ")

takeTill' p = do
    s <- takeTill p
    e <- atEnd
    if e
      then return (s, Nothing)
      else do
        s' <- take 1
        return (s, Just $ s' `S.index` 0)

attrs :: Parser [Attr]
attrs = many attr

-- attrs = loop []
--   where
--     loop acc = atEnd >>= if' (return acc) (do
--         skipSpace
--         (name, mc) <- takeTill' (inClass ">= ")
--         case mc of
--             Nothing -> loop $ (name, S.empty) : acc
--             Just '>' -> return $ if S.null name then acc else (name, S.empty):acc
--             Just ' ' -> do
--                 skipSpace
--                 <- take 1
--             Just _  -> do
--                 skipSpace
--                 (value, mc') <- takeTill' (inClass " >")
--                 case mc' of
--                     Just '>' -> return $ (name, value) : acc
--                     _ -> loop $ (name, value) : acc
--         )

tag :: Parser Token
tag = do
    string "<"
    (name, mc) <- takeTill' (\s -> s=='>' || s==' ')
    case mc of
        Nothing -> return $ PartialToken $ "<" `mappend` name
        Just '>' -> case S.uncons name of
                      Just ('/', name') -> return $ TagClose name'
                      _ -> return $ TagOpen name []
        _ -> do a <- attrs
                return $ TagOpen name a

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

accumParse :: Monad m => ByteString -> ByteString -> m (ByteString, [Token])
accumParse acc input = liftM splitPartial $
                         either fail return $
                           parseOnly html (acc `mappend` input)
  where
    splitPartial :: [Token] -> (ByteString, [Token])
    splitPartial [] = (S.empty, [])
    splitPartial (splitLast -> (init, PartialToken s)) = (s, init)
    splitPartial tokens = (S.empty, tokens)

main = do
    [filename] <- getArgs
    let toTokens = EL.concatMapAccumM accumParse S.empty
        enum = E.enumFile filename
    tokens <- E.run_ $ (enum E.$= toTokens) E.$$ EL.consume
    print tokens

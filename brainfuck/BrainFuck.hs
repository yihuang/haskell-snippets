{-# LANGUAGE DeriveDataTypeable #-}
import Prelude hiding (catch)
import Data.Data
import Data.Char
import System.IO
import System.Environment
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import ListZipper

data BFException = BFFinish | BFException String
    deriving (Show, Typeable)
instance Exception BFException

instance HasDefault Char where
    defaultValue = '\0'
instance HasDefault Int where
    defaultValue = 0

type CodeP = Zipper Char

data BFState = BFS {
    dp :: Pointer Int       -- data pointer
  , cp :: CodeP             -- code pointer
  , jumpStack :: [CodeP]    -- jump location stack
} deriving (Show)

fromCode :: String -> Maybe BFState
fromCode code = do
    zipper <- fromList code
    return BFS {
        dp = newP
      , cp = zipper
      , jumpStack = []
    }

type BF a = StateT BFState IO a

-- primitives

showState :: BF ()
showState = do
    let showMemory = show . concatMap show
    st <- get
    let (Pointer rs a xs) = dp st
    liftIO $ putStrLn $ "memory: " ++ (showMemory . reverse) rs ++ " " ++ showMemory [a] ++ " " ++ showMemory xs
    -- let (Zipper rs a xs) = cp st
    -- liftIO $ putStrLn $ "code: " ++ reverse rs ++ " " ++ [a] ++ " " ++ xs 

jumpTo :: CodeP -> BF ()
jumpTo p = modify $ \st -> st { cp = p }

pushJump :: BF ()
pushJump = modify $ \st -> st { jumpStack = cp st : jumpStack st }

popJump :: BF (Maybe CodeP)
popJump = do
    st <- get
    case jumpStack st of
        []     -> return Nothing
        (p:ps) -> put st { jumpStack = ps } >> return (Just p)

-- | read from current data pointer
readDp :: BF Int
readDp = fmap (readP . dp) get

-- | modify current data pointer
modifyDp :: (Pointer Int -> Pointer Int) -> BF ()
modifyDp f = modify $ \st -> st { dp = f (dp st) }

-- | write c into current data pointer
writeDp :: Int -> BF ()
writeDp c = do
    modifyDp $ writeP c
    -- showState

-- | move to next byte code
nextCp :: BF ()
nextCp = do
    st <- get
    case next (cp st) of
        Nothing -> liftIO $ throwIO BFFinish
        Just p  -> put st { cp = p }

-- | [
jmpCmd :: BF ()
jmpCmd = do
    c <- readDp
    if c == defaultValue
        then do
            cp' <- fmap cp get
            case nextUntil (==']') cp' of
                Nothing -> liftIO $ throwIO $ BFException "no matching ]"
                Just p  -> jumpTo p
        else do
            pushJump
            nextCp

-- | ]
jmpBackCmd :: BF ()
jmpBackCmd = do
    c <- readDp
    if c /= defaultValue
        then do
            mjump <- popJump
            case mjump of
                Nothing -> liftIO $ throwIO $ BFException "no matching ["
                Just p  -> jumpTo p
        else nextCp

-- | main eval loop
eval :: BF ()
eval = forever $ do
    c <- fmap (current . cp) get
    -- liftIO $ putStr [c]
    case c of
        '>' -> modifyDp nextP >> nextCp
        '<' -> modifyDp prevP >> nextCp
        '+' -> modifyDp (modifyP succ) >> nextCp
        '-' -> modifyDp (modifyP pred) >> nextCp
        '.' -> do
                 c <- readDp
                 liftIO $ putStr [toEnum c]
                 nextCp
        ',' -> do
                 c <- liftIO getChar
                 writeDp $ fromEnum c
                 nextCp
        '[' -> jmpCmd
        ']' -> jmpBackCmd
        _   -> nextCp

evalCode :: String -> IO ()
evalCode code = case fromCode code of
    Just st -> (runStateT eval st >> return ())
    Nothing -> throwIO $ BFException "invalid input"

errorHandle :: BFException -> IO ()
errorHandle e = case e of
    BFFinish -> hFlush stdout
    otherwise -> print e

evalFile :: FilePath -> IO ()
evalFile file = withFile file ReadMode $ \fp ->
    (hGetContents fp >>= evalCode)
    `catch` errorHandle

repl = forever $ do
    (getLine >>= evalCode)
    `catch` errorHandle

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> evalFile file
        [] -> repl

{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import System.IO
import Control.Monad
import Control.Applicative
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString as B
import Network

type Identity = String

data Cmd = Id Identity
         | Msg Identity String
     deriving (Show, Read)

data State = State {
    users :: M.Map Identity Handle
}

getHandle :: IORef State -> Identity -> IO (Maybe Handle)
getHandle state u = readIORef state >>= return . M.lookup u . users

regUser :: IORef State -> Identity -> Handle -> IO Bool
regUser state u h = atomicModifyIORef state $ \st ->
        case M.lookup u (users st) of
            Just _ -> (st, False)
            Nothing -> ( st {users = M.insert u h (users st)}
                       , True
                       )

chat :: IORef State -> Handle -> IO ()
chat state h = do
    s <- hGetLine h
    case read s of
        Id u -> regUser state u h >>= hPutStrLn h . show
        Msg u msg -> do
            mh <- getHandle state u
            case mh of
                Just th -> hPutStrLn th msg
                Nothing -> hPutStrLn h "unknown id"

deriving instance Show PortID

tcpServer :: HostName -> PortID -> (IORef State -> Handle -> IO ()) -> IO ()
tcpServer host port handler = withSocketsDo $ do
    sock <- listenOn port
    state <- newIORef $ State M.empty
    putStrLn $ "Listening on "++show port
    forever $ do
        (handle, _, _) <- accept sock
        hSetBuffering handle NoBuffering
        forkIO $ (forever $ handler state handle)
            `catch` ( \e -> print (e::SomeException) )
            `finally` hClose handle

main = tcpServer "localhost" (PortNumber $ fromInteger 8000) chat

-- |
-- split a big file into small files
-- split inputfile outputdir outputfilesize

import System.IO
import System.FilePath ((</>))
import System.Environment
import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

bufferSize = 1024 :: Integer

data Status = End | Continue

chunk :: Monad m => E.Iteratee a m (Maybe [a])
chunk = E.continue loop where
    loop (E.Chunks []) = chunk
    loop (E.Chunks xs) = E.yield (Just xs) (E.Chunks [])
    loop E.EOF = E.yield Nothing E.EOF

splitter :: Integer -> FilePath -> E.Iteratee B.ByteString IO ()
splitter size outdir = loop 0
  where
    loop idx = do
        let filename = outdir </> (show idx)
        h <- liftIO $ openFile filename WriteMode
        res <- fill h 0 `E.catchError` \e -> liftIO (putStrLn ("error:"++show e)) >> return End
        liftIO $ hClose h
        case res of
            End      -> return ()
            Continue -> loop $ idx + 1

    fill :: Handle -> Integer -> E.Iteratee B.ByteString IO Status
    fill h l = 
        if l > size
            then return Continue
            else do
                ss <- chunk
                -- E.tryIO $ throw $ AssertionFailed "test"
                case ss of
                    Nothing -> return End
                    Just xs  -> do
                        let s = B.concat xs
                        E.tryIO $ B.hPut h s
                        fill h $ l + toInteger (B.length s)

main = do
    [input, outdir, arg_size] <- getArgs
    let size = read arg_size :: Integer
    E.run_ $ EB.enumFile input E.$$ (splitter size outdir)

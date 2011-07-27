-- |
-- split big files into small files
-- split inputfile outputdir outputfilesize

import System.IO
import System.FilePath ((</>))
import System.Environment (getArgs)
import Control.Monad (mapM_)
import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

data Status = Error | FileEnd | SizeExceed

iterFileSized :: FilePath
              -> Integer
              -> E.Iteratee B.ByteString IO Status
iterFileSized path size = do
    h <- E.tryIO $ openBinaryFile path WriteMode
    status <- iterHandleSized h size `E.catchError` \e -> E.tryIO (print e) >> return Error
    E.tryIO $ hClose h
    return status

iterHandleSized :: MonadIO m => Handle
                             -> Integer
                             -> E.Iteratee B.ByteString m Status
iterHandleSized h size = loop h 0
  where
    loop h accum
        | accum > size = return SizeExceed
        | otherwise    = E.continue step
      where
        step E.EOF = E.yield FileEnd E.EOF
        step (E.Chunks []) = loop h accum
        step (E.Chunks xs) = do
            E.tryIO $ mapM_ (B.hPut h) xs
            loop h $ accum + (toInteger . sum . map B.length $ xs)

splitter :: Integer -> FilePath -> E.Iteratee B.ByteString IO ()
splitter size outdir = loop 0
  where
    loop idx = do
        let path = outdir </> show idx
        res <- iterFileSized path size
        case res of
            SizeExceed -> loop $ idx + 1
            _          -> return ()

main = do
    [input, outdir, arg_size] <- getArgs
    let size = read arg_size :: Integer
    E.run_ $ EB.enumFile input E.$$ splitter size outdir

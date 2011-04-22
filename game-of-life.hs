{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Data.Array.IArray

type Size = (Int, Int)
type WorldArray = Array (Int, Int) Life
data Life = Alive | Dead
  deriving (Eq)
newtype World = World { arr :: WorldArray }
instance Show World where
    show = showWorld
instance Read World where
    readList = readWorld

readWorld :: String -> [([World], String)]
readWorld s = [([w], "")]
  where
    w = World $ array ((0,0), (100,100)) [((0,0), Alive)]

splitAll :: Int -> [a] -> [[a]] -> [[a]]
splitAll n l accu = splitAll n rest (item:accu)
  where
    (item, rest) = splitAt n l

showWorld :: World -> String
showWorld (World arr) = ""

--intercalate "\n" [row r|]
--  where
--    (w, h) = bounds arr
--    all = elems arr
--    rows = 

judge :: Life -> Int -> Life
judge life n
    | n==3 = Alive
    | n<2 || n>3 = Dead
    | otherwise = life

neighbours :: WorldArray -> (Int, Int) -> Int
neighbours arr pos@(i, j) = length $ filter (==Alive) $ map (arr !) indexes
  where
    (_, (w, h)) = bounds arr
    indexes = [(i   `mod` w,       j-1 `mod` h)
              ,(i   `mod` w,       j+1 `mod` h)
              ,(i-1 `mod` w,       j-1 `mod` h)
              ,(i-1 `mod` w,       j   `mod` h)
              ,(i-1 `mod` w,       j+1 `mod` h)
              ,(i+1 `mod` w,       j-1 `mod` h)
              ,(i+1 `mod` w,       j   `mod` h)
              ,(i+1 `mod` w,       j+1 `mod` h)]

step :: World -> World
step (World arr) = World narr
  where
    bounds_@(_, (w, h)) = bounds arr
    narr = array bounds_ $ do
        i <- [0..w-1]
        j <- [0..h-1]
        let
            life = arr ! (i, j)
            n = neighbours arr (i, j)
        return ((i, j), judge life n)

interactive :: World -> IO ()
interactive w = do
    print w
    c <- getChar
    if c == 'q' then
        return ()
        else
        interactive $ step w

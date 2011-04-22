{-# LANGUAGE FlexibleContexts #-}
import Data.List
import Data.Array.IArray

type Size = (Int, Int)
type WorldArray = Array (Int, Int) Life
data Life = Alive | Dead
  deriving (Eq)
instance Show Life where
    show Alive = "O"
    show Dead = "*"
newtype World = World { arr :: WorldArray }
instance Show World where
    show = showWorld
instance Read World where
    readList = readWorld

newWorld :: (Int, Int) -> [((Int, Int), Life)] -> World
newWorld (w,h) lifes = World $ empty // lifes
  where
    empty = array
              ((0,0), (w-1,h-1))
              [((i,j),Dead) | i<-[0..w-1], j<-[0..h-1]]

readWorld :: String -> [([World], String)]
readWorld s = [([w], "")]
  where
    w = World $ array ((0,0), (100,100)) [((0,0), Alive)]

splitAll :: Int -> [a] -> [[a]]
splitAll n l = reverse $ doSplitAll n l []
  where 
    doSplitAll _ [] accu = accu
    doSplitAll n l accu = doSplitAll n rest (item:accu)
      where (item, rest) = splitAt n l

showWorld :: World -> String
showWorld (World arr) = intercalate "\n" rows
  where
    (_, (w, h)) = bounds arr
    all = elems arr
    rows = do
        row <- splitAll (w+1) all
        return $ intercalate "" $ map show row

judge :: Life -> Int -> Life
judge life n
    | n==3 = Alive
    | n<2 || n>3 = Dead
    | otherwise = life

neighbours :: WorldArray -> (Int, Int) -> Int
neighbours arr pos@(i, j) = length $ filter (==Alive) $ map (arr !) indexes
  where
    (_, (w, h)) = bounds arr
    indexes = [((i  ) `mod` w,       (j-1) `mod` h)
              ,((i  ) `mod` w,       (j+1) `mod` h)
              ,((i-1) `mod` w,       (j-1) `mod` h)
              ,((i-1) `mod` w,       (j  ) `mod` h)
              ,((i-1) `mod` w,       (j+1) `mod` h)
              ,((i+1) `mod` w,       (j-1) `mod` h)
              ,((i+1) `mod` w,       (j  ) `mod` h)
              ,((i+1) `mod` w,       (j+1) `mod` h)]

step :: World -> World
step (World arr) = World narr
  where
    bounds_@(_, (w, h)) = bounds arr
    narr = array bounds_ $ do
        i <- [0..w]
        j <- [0..h]
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

main = interactive $ newWorld (50, 50) [
		((31,31), Alive),
		((30,32), Alive),
		((28,32), Alive),
		((29,28), Alive),
		((29,32), Alive),
		((30,32), Alive),
		((30,32), Alive),
		((32,29), Alive),
		((31,33), Alive)]

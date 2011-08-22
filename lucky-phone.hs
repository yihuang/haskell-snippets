import Control.Monad
import System.Environment

type Number = (Int, Int, Int, Int, Int, Int, Int)

mkTuple7 :: [a] -> (a,a,a,a,a,a,a)
mkTuple7 (a:b:c:d:e:f:g:xs) = (a,b,c,d,e,f,g)
mkTuple7 _ = error "mkTuple7: not enough numbers"

allNumbers :: [Number]
-- allNumbers = fmap mkTuple7 $ sequence (replicate 7 [0..9])
allNumbers = do
    a <- [0..9]
    b <- [0..9]
    c <- [0..9]
    d <- [0..9]
    e <- [0..9]
    f <- [0..9]
    g <- [0..9]
    return (a,b,c,d,e,f,g)

-- 检查一串数字是否连续 （相邻数字之差都相等且差的绝对值为1）
seqNum :: [Int] -> Bool
seqNum l@(a:b:xs) =
  let
    step = a - b
    sameStep = all (==step) (zipWith (-) l (tail l))
  in
    abs step == 1 && sameStep

seqNum _ = True

rule1 :: Number -> Bool
rule1 (a,b,c,d,e,f,g) = seqNum [b, c, d]
                     && e==b
                     && f==c
                     && g==d

main = do
    [c'] <- getArgs
    let c = read c'
    print . take c . filter rule1 $ allNumbers

tris :: [Int]
tris = 1 : 3 : zipWith (\a b -> b + b - a + 1) tris (tail tris)

divisors :: Int -> Int
divisors n = (*) 2 $ (+) 1 $ length . filter (\x -> n `rem` x == 0) $ [2 .. (floor . sqrt . fromIntegral $ n)]

main = print $ head . filter ((>500) . divisors) $ tris

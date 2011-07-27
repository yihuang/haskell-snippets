import Debug.Trace

f :: Int -> Int
f a = trace "evaluated" $ a * a

main = print $ length $ map f [1..10]

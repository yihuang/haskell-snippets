import Debug.Trace

data Stream a = Chunks [a]
              | EOF

data Step a b = Yield b (Stream a)
              | Error
              | Continue (Iteratee a b)

type Iteratee a b = Stream a -> Step a b
type Enumerator a b = Iteratee a b -> Iteratee a b

consume :: Show a => Int -> Iteratee a [a]
consume n =
    loop [] 0
  where
    loop acc len
        | len >= n  = let (a, b) = splitAt n acc
                      in const $ Yield a (Chunks b)
        | otherwise = \s -> case s of
            Chunks xs -> trace (show xs) $ Continue $ loop (acc ++ xs) (len + length xs)
            EOF       -> Yield acc EOF

enumList :: Int -> [a] -> Iteratee a b -> Iteratee a b
enumList n xs it =
    loop xs it
  where
    loop [] it = it
    loop xs it =
        let (a, b) = splitAt n xs 
            it1 = it (Chunks a)
        in  case it1 of
                Continue it2 -> loop b it2
                _            -> const it1

run :: Iteratee a b -> b
run it = case it EOF of
    Yield b _ -> b
    _         -> error "non exausted iteratee"

main = print $ run $ enumList 4 [1..100] $ consume 30

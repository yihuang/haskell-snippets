import Prelude hiding (head, drop, takeWhile, map, take, lines, foldl)
import qualified Data.List as L

data Stream a = Chunks [a]
              | EOF

data Iteratee a b = Yield b (Stream a)
                  | Error String
                  | Continue (Stream a -> Iteratee a b)

type Enumerator a b     = Iteratee a b -> Iteratee a b
type Enumeratee ao ai b = Iteratee ai b -> Iteratee ao (Iteratee ai b)

instance Functor (Iteratee a) where
    fmap f it = case it of
        Yield b ext  -> Yield (f b) ext
        Error err    -> Error err
        Continue step -> Continue $ \s -> fmap f (step s)

instance Monad (Iteratee a) where
    return b = Yield b (Chunks [])
    i1 >>= f = case i1 of
        Continue k  -> Continue (\s -> k s >>= f)
        Yield b ext -> case f b of
                           Continue k' -> k' ext
                           Yield b2 _  -> Yield b2 ext
                           Error err   -> Error err
        Error err   -> Error err

peak :: Iteratee a (Maybe a)
peak = Continue $ \s -> case s of
    Chunks (x:xs) -> Yield (Just x) s
    Chunks [] -> peak
    EOF -> Yield Nothing EOF

head :: Iteratee a (Maybe a)
head = do c <- peak
          case c of
              Just _ -> drop 1
              _      -> return ()
          return c

take :: Integer -> Iteratee a [a]
take n = loop [] n where
    loop acc 0 = return $ reverse acc
    loop acc n = do
        ma <- head
        case ma of
            Just a -> loop (a:acc) (n-1)
            Nothing -> return acc

takeWhile :: (a -> Bool) -> Iteratee a [a]
takeWhile f = loop [] where
    loop acc = Continue $ \s -> case s of
        Chunks xs -> let (a, b) = L.break f xs
                     in  if null b
                             then loop (acc++a)
                             else Yield (acc++a) (Chunks b)
        EOF       -> Yield acc EOF 

drop :: Integer -> Iteratee a ()
drop n = Continue $ \s -> case s of
    Chunks xs -> let (a, b) = L.genericSplitAt n xs
                     d      = n - L.genericLength a
                 in  if d > 0
                     then drop d
                     else Yield () (Chunks b)
    EOF       -> Yield () EOF

foldl :: (b -> a -> b) -> b -> Iteratee a b
foldl f b = loop b where
    loop b = Continue $ \s -> case s of
        Chunks [] -> loop b
        Chunks xs -> loop (L.foldl f b xs)
        EOF       -> Yield b EOF

isEOF :: Iteratee a Bool
isEOF = Continue $ \s -> case s of
    EOF -> Yield True EOF
    _   -> Yield False s

line :: Iteratee Char String
line = do
    l <- takeWhile (\s -> s == '\r' || s == '\n')
    mh <- head
    case mh of
        Just '\r' -> do c <- peak
                        if c == Just '\n'
                            then drop 1
                            else return ()
        _         -> return ()
    return l

pair :: Iteratee a b1 -> Iteratee a b2 -> Iteratee a (b1, b2)
pair it1 it2 = case (it1, it2) of
    (Yield b1 ext, Yield b2 _) -> Yield (b1, b2) ext
    (Error err, _) -> Error err
    (_, Error err) -> Error err
    _            -> Continue $ \s -> 
        let it1' = case it1 of
                       (Continue step) -> step s
                       _               -> it1
            it2' = case it2 of
                       (Continue step) -> step s
                       _               -> it2
        in  pair it1' it2'

lines :: Enumeratee Char [Char] b
lines ii = case ii of
    Continue step -> do
        l <- line
        let ii' = step (Chunks [l])
        eof <- isEOF
        if eof
            then return ii'
            else lines ii'
    _ -> return ii

map :: (ao -> ai) -> Enumeratee ao ai b
map f = loop where
    loop ii = case ii of
        Continue step ->
            Continue $ \s -> case s of
                EOF       -> Yield ii EOF
                Chunks xs -> let ii' = step . Chunks . fmap f $ xs
                             in  case ii' of
                                 Continue _ -> loop ii'
                                 Yield _ _  -> Yield ii' (Chunks [])
                                 Error err  -> Error err
        _             -> return ii

joinI :: Iteratee ao (Iteratee ai b) -> Iteratee ao b
joinI oi = oi >>= check where
    check ii = case run ii of
                   Left err -> Error err
                   Right b  -> return b

enumList :: Integer -> [a] -> Enumerator a b
enumList bufSize xs = loop xs where
    loop xs (Continue step) | not (null xs) =
        let (a, b) = L.genericSplitAt bufSize xs
        in  case step (Chunks a) of
                it -> loop b it
    loop _ it = it

enumEOF :: Enumerator a b
enumEOF (Yield x _) = Yield x EOF
enumEOF (Error err) = Error err
enumEOF (Continue k) = case k EOF of
    Continue _ -> error "enumEOF: divergent iteratee"
    s          -> enumEOF s

run :: Iteratee a b -> Either String b
run it = case enumEOF it of
    Yield b _  -> Right b
    Error err  -> Left err
    Continue _ -> error "run: divergent iteratee"

main = do
    let d = enumList 4 "line1\r\nline2\nline3\r\n\r\n\r\n"
        i = take 20
        fj = map (*2)
        sum = foldl (+) 0
        count = foldl (\n _ -> n+1) 0
        conc = foldl (flip (:)) []
    print $ run $ d $ joinI $ lines (pair (fmap reverse conc) count)

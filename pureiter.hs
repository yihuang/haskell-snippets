data Stream a = Chunks [a]
              | EOF

type Step a b = Stream a -> Iteratee a b

data Iteratee a b = Continue (Step a b)
                  | Yield b (Stream a)
                  | Error String

type Enumerator a b = Step a b -> Iteratee a b

type Enumeratee ao ai b = Iteratee ai b -> Iteratee ao (Iteratee ai b)

($$) :: 

enum :: a -> Enumerator a b
enum a = \step ->
    let
    stream = Chunks [a]
    in
    step stream

enumList :: [[a]] -> Enumerator a b
enumList ll = \step -> loop

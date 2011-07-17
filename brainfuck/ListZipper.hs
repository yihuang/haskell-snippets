module ListZipper where

data Zipper a = Zipper [a] a [a]
    deriving (Show)

fromList :: [a] -> Maybe (Zipper a)
fromList []     = Nothing
fromList (x:xs) = Just $ Zipper [] x xs

current (Zipper _ a _) = a

next :: (Zipper a) -> Maybe (Zipper a)
next (Zipper rs a (x:xs)) = Just $ Zipper (a:rs) x xs
next (Zipper _ _ [])     = Nothing

tryNext :: (Zipper a) -> (Zipper a)
tryNext z = case next z of
                Nothing -> z
                Just z' -> z'

prev :: (Zipper a) -> Maybe (Zipper a)
prev (Zipper (x:rs) a xs) = Just $ Zipper rs x (a:xs)
prev (Zipper [] _ _)     = Nothing

tryPrev :: (Zipper a) -> (Zipper a)
tryPrev z = case prev z of
                Nothing -> z
                Just z' -> z'

nextUntil :: (a -> Bool) -> (Zipper a) -> Maybe (Zipper a)
nextUntil f p
    | f (current p) = Just p
    | otherwise     = next p >>= nextUntil f

class HasDefault a where
    defaultValue :: a

data Pointer a = Pointer [a] a [a]
    deriving (Show)

newP :: HasDefault a => Pointer a
newP = Pointer [] defaultValue []

nextP :: HasDefault a => Pointer a -> Pointer a
nextP (Pointer rs a (x:xs)) =
      Pointer (a:rs) x xs
nextP (Pointer rs x [])     =
      Pointer (x:rs) defaultValue []

prevP :: HasDefault a => Pointer a -> Pointer a
prevP (Pointer (x:rs) a xs) =
      Pointer rs x (a:xs)
prevP (Pointer [] a xs) =
      Pointer [] defaultValue (a:xs)

readP :: Pointer a -> a
readP (Pointer _ a _) = a

writeP :: a -> Pointer a -> Pointer a
writeP a' (Pointer rs a xs) = (Pointer rs a' xs)

modifyP :: (a -> a) -> Pointer a -> Pointer a
modifyP f p = writeP (f $ readP p) p


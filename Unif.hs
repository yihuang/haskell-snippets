module Unif (unify, main) where

import Data.List (intersperse)

-- utils --

mapFst :: (a -> b) -> (a, c) -> (b,   c)
mapFst    f           (a, c) =  (f a, c)

-- types --

type Name = String

data Term = Var Name
          | App Name [Term]

type Sub = (Term, Name)

-- implementation --

compose :: [Sub] -> [Sub] -> [Sub]
compose []     s1   = s1
compose (s:ss) s1   = compose ss $ s : iter s s1
  where
    iter :: Sub -> [Sub] -> [Sub]
    iter s ss = map (mapFst (sub s)) ss

occurs :: Name -> Term -> Bool
occurs x t = case t of
  (Var y)       -> x==y
  (App _ ts)    -> foldr (&&) False . map (occurs x) $ ts

sub :: Sub -> Term -> Term
sub (t1, y) t@(Var a)
  | a==y = t1
  | otherwise = t
sub s (App f ts) = App f $ map (sub s) ts

subs :: [Sub] -> Term -> Term
subs ss t = foldl (flip sub) t ss

unify :: Term -> Term -> Maybe [Sub]
unify t1 t2 = case (t1, t2) of
  (Var x, Var y)    -> if x==y then Just [] else Just [(t1, y)]
  (Var x, App _ _)  -> if occurs x t2 then Nothing else Just [(t2, x)]
  (App _ _, Var x)  -> if occurs x t1 then Nothing else Just [(t1, x)]
  (App n1 ts1, App n2 ts2)
                    -> if n1/=n2 then Nothing else unify_args ts1 ts2
  where
    unify_args [] []    = Just []
    unify_args _ []     = Nothing
    unify_args [] _     = Nothing
    unify_args (t1:ts1) (t2:ts2) = do
      u <- unify t1 t2
      let
        update = map (subs u)
      u1 <- unify_args (update ts1) (update ts2)
      return (u1 `compose` u)

-- test cases --

instance Show Term where
    show (Var s) = s
    show (App name ts) = name++"("++(concat . intersperse "," $ (map show ts))++")"

showSub (t, s) = show t++"/"++s 

a = Var "a"
x = Var "x"
y = Var "y"
z = Var "z"
f = App "f"
g = App "g"
j = App "j"
t1 = j [x,y,z]
t2 = j [f [y,y], f [z,z], f [a,a]]
main = case unify t1 t2 of
         Nothing -> putStrLn "unify fail"
         Just u  -> putStrLn $ concat . intersperse "\n" $ map showSub u

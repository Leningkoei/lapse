module Comparator where

import Type ( LispValue(..), Atom(..) )

instance Eq LispValue where (==) = equal
equal :: LispValue -> LispValue -> Bool
equal (Atom (Symbol a)) (Atom (Symbol b)) = a == b
equal (Atom (Character a)) (Atom (Character b)) = a == b
equal (Atom (Number a)) (Atom (Number b)) = a == b
equal (List a) (List b) = equal a b
  where equal :: [LispValue] -> [LispValue] -> Bool
        equal [] [] = True
        equal [] _ = False
        equal _ [] = False
        equal (x:xs) (y:ys) = x == y && equal xs ys
equal _ _ = False

instance Ord LispValue where compare = Comparator.compare
compare :: LispValue -> LispValue -> Ordering
compare (Atom (Symbol a)) (Atom (Symbol b)) = Prelude.compare a b
compare (Atom (Number a)) (Atom (Number b)) = Prelude.compare a b
compare (Atom (Character a)) (Atom (Character b)) = Prelude.compare a b
compare (List a) (List b) = length a `Prelude.compare` length b

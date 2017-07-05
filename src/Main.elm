module Main exposing (..)

import List.Comonad as L

listAp : (a -> List a -> a) -> L.Comonad a -> a
listAp f (L.Comonad x xs) = f x xs

inc : a -> List a -> Int
inc _ xs = List.length xs

incNumber : L.Comonad Int
incNumber =
  (L.Comonad 0 [])
  |> L.extend (listAp inc)
  |> L.extend (listAp inc)
  |> L.extend (listAp inc)
  |> L.extend (listAp inc)

sum : Int -> List Int -> Int
sum = List.foldr (+)

sumNumbers : L.Comonad Int
sumNumbers =
  (L.Comonad 1 [])
  |> L.extend (listAp sum)
  |> L.extend (listAp sum)
  |> L.extend (listAp sum)
  |> L.extend (listAp sum)

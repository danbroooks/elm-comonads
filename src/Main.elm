module Main exposing (..)

import List.Comonad as L

plus : Int -> L.Comonad Int -> Int
plus n (L.Comonad x _) = x + n

adder : L.Comonad Int -> L.Comonad Int
adder co =
  L.extend (plus 1) co

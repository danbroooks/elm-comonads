module List.Comonad exposing (Comonad(..), extend, extract, laws)

import List exposing (map)

type Comonad a = Comonad a (List a)

extend : (Comonad a -> a) -> Comonad a -> Comonad a
extend f (Comonad _ xs as c) =
  Comonad (f c) (map f (map (\n -> Comonad n xs) xs))

extract : Comonad a -> a
extract (Comonad x _) = x

leftIdentity : Comonad a -> Bool
leftIdentity co =
  (extend extract co) == co

rightIdentity : (Comonad a -> a) -> Comonad a -> Bool
rightIdentity f co =
  (extract <| (extend f) <| co) == f co

laws : Comonad a -> Bool
laws co =
  leftIdentity co && rightIdentity extract co

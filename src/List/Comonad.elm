module List.Comonad exposing (Comonad(..), extend, extract)

import List

type Comonad a = Comonad a (List a)

extend : (Comonad a -> a) -> Comonad a -> Comonad a
extend f (Comonad _ xs as c) = Comonad (f c) (f c :: xs)

extract : Comonad a -> a
extract (Comonad x _) = x

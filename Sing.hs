{-# LANGUAGE KindSignatures, PolyKinds, TypeFamilies #-}
module Sing where

data family Sing (a :: k)

{-# LANGUAGE GADTs, TypeOperators, RankNTypes,
             KindSignatures, DataKinds, TypeFamilies, PolyKinds #-}
module Test where

import Prelude hiding (head)

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: * -> Nat -> * where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

type family   (m :: Nat) :< (n :: Nat) :: Bool
type instance m          :< 'Zero      = 'False
type instance 'Zero      :< ('Succ n)  = 'True
type instance ('Succ m)  :< ('Succ n)  = m :< n

data SNat :: Nat -> * where
  SZero :: SNat 'Zero
  SSucc :: forall (n :: Nat). SNat n -> SNat ('Succ n)

head :: Vec a ('Succ n) -> a
head (VCons h _) = h

plus :: Nat -> Nat -> Nat
plus Zero     m = m
plus (Succ n) m = Succ (plus n m)

type family   Plus (n :: Nat) (m :: Nat) :: Nat
type instance Plus 'Zero m     = m
type instance Plus ('Succ n) m = 'Succ (Plus n m)

append :: Vec a n -> Vec a m -> Vec a (Plus n m)
append VNil v2        = v2
append (VCons h t) v2 = VCons h (append t v2)


data Fin :: Nat -> * where
  FZero :: Fin ('Succ n)
  FSucc :: Fin n -> Fin ('Succ n)

data Eql :: k -> k -> * where
  Refl :: Eql a a


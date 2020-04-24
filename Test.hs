{-# LANGUAGE GADTs, TypeOperators, RankNTypes,
             KindSignatures, DataKinds, TypeFamilies #-}
module Test where

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

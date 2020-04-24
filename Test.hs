{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
module Test where

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: * -> Nat -> * where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

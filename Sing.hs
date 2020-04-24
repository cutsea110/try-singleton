{-# LANGUAGE KindSignatures, PolyKinds, TypeFamilies, GADTs, DataKinds, RankNTypes #-}
module Sing where

data family Sing (a :: k)

data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data instance Sing (a :: Nat) where
  SZero :: Sing 'Zero
  SSucc :: Sing n -> Sing ('Succ n)

type SNat (a :: Nat)   = Sing a
type SBool (a :: Bool) = Sing a

data instance Sing (m :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust    :: forall (a :: k). Sing a -> Sing ('Just a)

data Vec :: * -> Nat -> * where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

data instance Sing (v :: Vec a n) where
  SVNil :: Sing 'VNil
  SVCons :: Sing a -> Sing v -> Sing ('VCons a v)

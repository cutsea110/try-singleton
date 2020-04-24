{-# LANGUAGE GADTs, TypeOperators, RankNTypes,
             KindSignatures, DataKinds, TypeFamilies, PolyKinds, UndecidableInstances #-}
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

data SBool :: Bool -> * where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

isEven :: Nat -> Bool
isEven Zero = True
isEven (Succ Zero) = False
isEven (Succ (Succ n)) = isEven n

type family IsEven (n :: Nat) :: Bool
type instance IsEven 'Zero             = 'True
type instance IsEven ('Succ 'Zero)     = 'False
type instance IsEven ('Succ ('Succ n)) = IsEven n

sIsEven :: SNat n -> SBool (IsEven n)
sIsEven SZero             = STrue
sIsEven (SSucc SZero)     = SFalse
sIsEven (SSucc (SSucc n)) = sIsEven n

nextEven :: Nat -> Nat
nextEven n = if isEven n then n else Succ n

type family If (b :: Bool) (t :: k) (e :: k) :: k
type instance If 'True  (t :: k) (e :: k) = t
type instance If 'False (t :: k) (e :: k) = e

sIf :: forall (b :: Bool) (n :: k) (m :: k) (f :: k -> *). SBool b -> f n -> f m -> f (If b n m)
sIf STrue  x y = x
sIf SFalse x y = y

type family NextEven (n :: Nat) :: Nat
type instance NextEven n = If (IsEven n) n ('Succ n)

sNextEven :: SNat n -> SNat (NextEven n)
sNextEven n = sIf (sIsEven n) n (SSucc n)

data Fin :: Nat -> * where
  FZero :: Fin ('Succ n)
  FSucc :: Fin n -> Fin ('Succ n)

data Eql :: k -> k -> * where
  Refl :: Eql a a

makeEven :: SNat n -> Vec a n -> Vec a (NextEven n)
makeEven n vec = case sIsEven n of
  STrue  -> vec
  SFalse -> case vec of
    VCons h t -> VCons h (VCons h t)

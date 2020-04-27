{-# LANGUAGE KindSignatures, PolyKinds, TypeFamilies, GADTs, DataKinds, RankNTypes, ScopedTypeVariables #-}
module Sing where

data family Sing (a :: k)
class SingI (a :: k) where
  sing :: Sing a

data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

type SBool (a :: Bool) = Sing a
instance SingI 'False where
  sing = SFalse
instance SingI 'True where
  sing = STrue

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data instance Sing (a :: Nat) where
  SZero :: Sing 'Zero
  SSucc :: SingI n => Sing n -> Sing ('Succ n)

type SNat (a :: Nat)   = Sing a
instance SingI 'Zero where
  sing = SZero
instance SingI n => SingI ('Succ n) where
  sing = SSucc sing

data instance Sing (m :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust    :: forall (a :: k). Sing a -> Sing ('Just a)
instance SingI 'Nothing where
  sing = SNothing
instance SingI a => SingI ('Just a) where
  sing = SJust sing

data Vec :: * -> Nat -> * where
  VNil  :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

data instance Sing (v :: Vec a n) where
  SVNil :: Sing 'VNil
  SVCons :: Sing a -> Sing v -> Sing ('VCons a v)
type SVec a n = Sing a
instance SingI 'VNil where
  sing = SVNil
instance (SingI a, SingI v) => SingI ('VCons a v) where
  sing = SVCons sing sing

replicate2 :: forall a n. SingI n => a -> Vec a n
replicate2 a = case (sing :: Sing n) of
  SZero   -> VNil
  SSucc _ -> VCons a (replicate2 a)

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.MessagePack.Types.Generic () where

import           Control.Monad.Trans.State.Strict   (StateT, evalStateT, get,
                                                     put)
import           Control.Monad.Validate             (MonadValidate, refute)
import           Data.Bits                          (shiftR)
import           Data.Kind                          (Type)
import           Data.Word                          (Word64)
import           GHC.Generics

import           Data.MessagePack.Types.Class
import           Data.MessagePack.Types.DecodeError (DecodeError)
import           Data.MessagePack.Types.Object      (Object (..))

instance GMessagePack V1 where
    gToObject = undefined
    gFromObject _ _ = refute "can't instantiate void type"

instance GMessagePack U1 where
    gToObject _ U1 = ObjectNil
    gFromObject _ ObjectNil = return U1
    gFromObject _ _         = refute "invalid encoding for custom unit type"

instance GProdPack a => GMessagePack a where
    gToObject cfg = toObject cfg . prodToObject cfg
    gFromObject cfg o = do
        list <- fromObjectWith cfg o
        evalStateT (prodFromObject cfg) list

instance (GSumPack a, GSumPack b, SumSize a, SumSize b) => GMessagePack (a :+: b) where
    gToObject cfg = sumToObject cfg 0 size
        where size = unTagged (sumSize :: Tagged (a :+: b) Word64)

    gFromObject cfg = \case
        ObjectWord code -> checkSumFromObject0 cfg size (fromIntegral code)
        o               -> fromObjectWith cfg o >>= uncurry (checkSumFromObject cfg size)
        where size = unTagged (sumSize :: Tagged (a :+: b) Word64)

instance GMessagePack a => GMessagePack (M1 t c a) where
    gToObject cfg (M1 x) = gToObject cfg x
    gFromObject cfg x = M1 <$> gFromObject cfg x

instance MessagePack a => GMessagePack (K1 i a) where
    gToObject cfg (K1 x) = toObject cfg x
    gFromObject cfg o = K1 <$> fromObjectWith cfg o


-- Product type packing.

class GProdPack f where
    prodToObject :: Config -> f a -> [Object]
    prodFromObject
        :: ( Applicative m
           , Monad m
           , MonadValidate DecodeError m
           )
        => Config -> StateT [Object] m (f a)


instance (GProdPack a, GProdPack b) => GProdPack (a :*: b) where
    prodToObject cfg (a :*: b) = prodToObject cfg a ++ prodToObject cfg b
    prodFromObject cfg = do
        f <- prodFromObject cfg
        g <- prodFromObject cfg
        pure $ f :*: g

instance GMessagePack a => GProdPack (M1 t c a) where
    prodToObject cfg (M1 x) = [gToObject cfg x]
    prodFromObject cfg = do
        objs <- get
        case objs of
            (x:xs) -> do
                put xs
                M1 <$> gFromObject cfg x
            _      -> refute "invalid encoding for product type"


-- Sum type packing.

checkSumFromObject0
    :: ( Applicative m
       , Monad m
       , MonadValidate DecodeError m
       )
    => (GSumPack f) => Config -> Word64 -> Word64 -> m (f a)
checkSumFromObject0 cfg size code
  | code < size = sumFromObject cfg code size ObjectNil
  | otherwise = refute "invalid encoding for sum type"


checkSumFromObject
    :: ( Applicative m
       , Monad m
       , MonadValidate DecodeError m
       )
    => (GSumPack f) => Config -> Word64 -> Word64 -> Object -> m (f a)
checkSumFromObject cfg size code x
  | code < size = sumFromObject cfg code size x
  | otherwise   = refute "invalid encoding for sum type"


class GSumPack f where
    sumToObject :: Config -> Word64 -> Word64 -> f a -> Object
    sumFromObject
        :: ( Applicative m
           , Monad m
           , MonadValidate DecodeError m
           )
        => Config
        -> Word64
        -> Word64
        -> Object
        -> m (f a)


instance (GSumPack a, GSumPack b) => GSumPack (a :+: b) where
    sumToObject cfg code size = \case
        L1 x -> sumToObject cfg code sizeL x
        R1 x -> sumToObject cfg (code + sizeL) sizeR x
      where
        sizeL = size `shiftR` 1
        sizeR = size - sizeL

    sumFromObject cfg code size x
        | code < sizeL = L1 <$> sumFromObject cfg code sizeL x
        | otherwise    = R1 <$> sumFromObject cfg (code - sizeL) sizeR x
      where
        sizeL = size `shiftR` 1
        sizeR = size - sizeL


instance GSumPack (C1 c U1) where
    sumToObject cfg code _ _ = toObject cfg code
    sumFromObject cfg _ _ = gFromObject cfg


instance GMessagePack a => GSumPack (C1 c a) where
    sumToObject cfg code _ x = toObject cfg (code, gToObject cfg x)
    sumFromObject cfg _ _ = gFromObject cfg


-- Sum size.

class SumSize f where
    sumSize :: Tagged f Word64

newtype Tagged (s :: Type -> Type) b = Tagged { unTagged :: b }

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a Word64) + unTagged
        (sumSize :: Tagged b Word64)

instance SumSize (C1 c a) where
    sumSize = Tagged 1

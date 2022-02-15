{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE ViewPatterns        #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Object
-- Copyright : (c) Hideyuki Tanaka, 2009-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack object definition
--
--------------------------------------------------------------------

module Data.MessagePack.Types.Class
    ( MessagePack (..)
    , GMessagePack (..)
    , Config
    , defaultConfig
    ) where

import           Control.Arrow                      ((***))
import           Control.Monad.Validate             (MonadValidate (..))
import qualified Data.ByteString                    as S
import qualified Data.ByteString.Lazy               as L
import qualified Data.HashMap.Strict                as HashMap
import           Data.Hashable                      (Hashable)
import           Data.Int                           (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict                 as IntMap
import qualified Data.Map                           as Map
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as LT
import qualified Data.Vector                        as V
import qualified Data.Vector.Storable               as VS
import qualified Data.Vector.Unboxed                as VU
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)
import           GHC.Generics                       (Generic, Rep, from, to)

import           Data.MessagePack.Types.Assoc       (Assoc (..))
import           Data.MessagePack.Types.DecodeError (DecodeError)
import           Data.MessagePack.Types.Object      (Object (..))


data Config = Config

defaultConfig :: Config
defaultConfig = Config


-- Generic serialisation.

class GMessagePack f where
    gToObject :: Config -> f a -> Object
    gFromObject
        :: ( Applicative m
           , Monad m
           , MonadValidate DecodeError m
           )
        => Config
        -> Object
        -> m (f a)


class MessagePack a where
    toObject :: Config -> a -> Object
    fromObjectWith
        :: ( Applicative m
           , Monad m
           , MonadValidate DecodeError m
           )
        => Config
        -> Object
        -> m a

    default toObject :: (Generic a, GMessagePack (Rep a)) => Config -> a -> Object
    toObject = genericToObject
    default fromObjectWith
        :: ( Applicative m
           , Monad m
           , MonadValidate DecodeError m
           , Generic a, GMessagePack (Rep a))
        => Config
        -> Object
        -> m a
    fromObjectWith = genericFromObject


genericToObject :: (Generic a, GMessagePack (Rep a)) => Config -> a -> Object
genericToObject cfg = gToObject cfg . from

genericFromObject
    :: ( Applicative m
       , Monad m
       , MonadValidate DecodeError m
       , Generic a
       , GMessagePack (Rep a)
       )
    => Config
    -> Object
    -> m a
genericFromObject cfg x = to <$> gFromObject cfg x


-- Instances for integral types (Int etc.).

toInt :: Integral a => a -> Int64
toInt = fromIntegral

fromInt :: Integral a => Int64 -> a
fromInt = fromIntegral

toWord :: Integral a => a -> Word64
toWord = fromIntegral

fromWord :: Integral a => Word64 -> a
fromWord = fromIntegral

instance MessagePack Int64 where
    toObject _ i
      | i < 0     = ObjectInt i
      | otherwise = ObjectWord $ toWord i
    fromObjectWith _ = \case
        ObjectInt  n -> return n
        ObjectWord n -> return $ toInt n
        _            -> refute "invalid encoding for integer type"

instance MessagePack Word64 where
    toObject _ = ObjectWord
    fromObjectWith _ = \case
        ObjectWord n -> return n
        _            -> refute "invalid encoding for integer type"

instance MessagePack Int    where { toObject cfg = toObject cfg . toInt; fromObjectWith cfg o = fromInt <$> fromObjectWith cfg o }
instance MessagePack Int8   where { toObject cfg = toObject cfg . toInt; fromObjectWith cfg o = fromInt <$> fromObjectWith cfg o }
instance MessagePack Int16  where { toObject cfg = toObject cfg . toInt; fromObjectWith cfg o = fromInt <$> fromObjectWith cfg o }
instance MessagePack Int32  where { toObject cfg = toObject cfg . toInt; fromObjectWith cfg o = fromInt <$> fromObjectWith cfg o }

instance MessagePack Word   where { toObject cfg = toObject cfg . toWord; fromObjectWith cfg o = fromWord <$> fromObjectWith cfg o }
instance MessagePack Word8  where { toObject cfg = toObject cfg . toWord; fromObjectWith cfg o = fromWord <$> fromObjectWith cfg o }
instance MessagePack Word16 where { toObject cfg = toObject cfg . toWord; fromObjectWith cfg o = fromWord <$> fromObjectWith cfg o }
instance MessagePack Word32 where { toObject cfg = toObject cfg . toWord; fromObjectWith cfg o = fromWord <$> fromObjectWith cfg o }


-- Core instances.

instance MessagePack Object where
    toObject _ = id
    fromObjectWith _ = return

instance MessagePack () where
    toObject _ _ = ObjectNil
    fromObjectWith _ = \case
        ObjectNil                    -> return ()
        ObjectArray (V.toList -> []) -> return ()
        _                            -> refute "invalid encoding for ()"

instance MessagePack Bool where
    toObject _ = ObjectBool
    fromObjectWith _ = \case
        ObjectBool b -> return b
        _            -> refute "invalid encoding for Bool"

instance MessagePack Float where
    toObject _ = ObjectFloat
    fromObjectWith _ = \case
        ObjectInt    n -> return $ fromIntegral n
        ObjectWord   n -> return $ fromIntegral n
        ObjectFloat  f -> return f
        ObjectDouble d -> return $ realToFrac d
        _              -> refute "invalid encoding for Float"

instance MessagePack Double where
    toObject _ = ObjectDouble
    fromObjectWith _ = \case
        ObjectInt    n -> return $ fromIntegral n
        ObjectWord   n -> return $ fromIntegral n
        ObjectFloat  f -> return $ realToFrac f
        ObjectDouble d -> return d
        _              -> refute "invalid encoding for Double"

-- Because of overlapping instance, this must be above [a].
-- IncoherentInstances and TypeSynonymInstances are required for this to work.
instance MessagePack String where
    toObject cfg = toObject cfg . T.pack
    fromObjectWith cfg obj = T.unpack <$> fromObjectWith cfg obj


-- Instances for binary and UTF-8 encoded string.

instance MessagePack S.ByteString where
    toObject _ = ObjectBin
    fromObjectWith _ = \case
        ObjectBin r -> return r
        _           -> refute "invalid encoding for ByteString"

instance MessagePack L.ByteString where
    toObject _ = ObjectBin . L.toStrict
    fromObjectWith cfg obj = L.fromStrict <$> fromObjectWith cfg obj

instance MessagePack T.Text where
    toObject _ = ObjectStr
    fromObjectWith _ = \case
        ObjectStr s -> return s
        _           -> refute "invalid encoding for Text"

instance MessagePack LT.Text where
    toObject cfg = toObject cfg . LT.toStrict
    fromObjectWith cfg obj = LT.fromStrict <$> fromObjectWith cfg obj


-- Instances for array-like data structures.

instance MessagePack a => MessagePack [a] where
    toObject cfg = ObjectArray . V.fromList . map (toObject cfg)
    fromObjectWith cfg = \case
        ObjectArray o -> mapM (fromObjectWith cfg) (V.toList o)
        _             -> refute "invalid encoding for list"

instance MessagePack a => MessagePack (V.Vector a) where
    toObject cfg = ObjectArray . V.map (toObject cfg)
    fromObjectWith cfg = \case
        ObjectArray o -> V.fromList <$> mapM (fromObjectWith cfg) (V.toList o)
        _             -> refute "invalid encoding for Vector"

instance (MessagePack a, VU.Unbox a) => MessagePack (VU.Vector a) where
    toObject cfg = ObjectArray . V.map (toObject cfg) . V.fromList . VU.toList
    fromObjectWith cfg = \case
        ObjectArray o -> VU.fromList . V.toList <$> V.mapM (fromObjectWith cfg) o
        _             -> refute "invalid encoding for Unboxed Vector"

instance (MessagePack a, VS.Storable a) => MessagePack (VS.Vector a) where
    toObject cfg = ObjectArray . V.map (toObject cfg) . V.fromList . VS.toList
    fromObjectWith cfg = \case
        ObjectArray o -> VS.fromList . V.toList <$> V.mapM (fromObjectWith cfg) o
        _             -> refute "invalid encoding for Storable Vector"

-- Instances for map-like data structures.

instance (MessagePack a, MessagePack b) => MessagePack (Assoc [(a, b)]) where
    toObject cfg (Assoc xs) = ObjectMap . V.fromList $ map (toObject cfg *** toObject cfg) xs
    fromObjectWith cfg = \case
        ObjectMap xs ->
            Assoc <$> mapM (\(k, v) -> (,) <$> fromObjectWith cfg k <*> fromObjectWith cfg v) (V.toList xs)
        _ -> refute "invalid encoding for Assoc"

instance (MessagePack k, MessagePack v, Ord k) => MessagePack (Map.Map k v) where
    toObject cfg = toObject cfg . Assoc . Map.toList
    fromObjectWith cfg obj = Map.fromList . unAssoc <$> fromObjectWith cfg obj

instance MessagePack v => MessagePack (IntMap.IntMap v) where
    toObject cfg = toObject cfg . Assoc . IntMap.toList
    fromObjectWith cfg obj = IntMap.fromList . unAssoc <$> fromObjectWith cfg obj

instance (MessagePack k, MessagePack v, Hashable k, Eq k) => MessagePack (HashMap.HashMap k v) where
    toObject cfg = toObject cfg . Assoc . HashMap.toList
    fromObjectWith cfg obj = HashMap.fromList . unAssoc <$> fromObjectWith cfg obj


-- Instances for various tuple arities.

instance (MessagePack a1, MessagePack a2) => MessagePack (a1, a2) where
    toObject cfg (a1, a2) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2])) = (,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2
    fromObjectWith _ _                        = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3) => MessagePack (a1, a2, a3) where
    toObject cfg (a1, a2, a3) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3])) = (,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3
    fromObjectWith _ _ = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4) => MessagePack (a1, a2, a3, a4) where
    toObject cfg (a1, a2, a3, a4) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3, toObject cfg a4]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3, a4])) = (,,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3 <*> fromObjectWith cfg a4
    fromObjectWith _ _ = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5) => MessagePack (a1, a2, a3, a4, a5) where
    toObject cfg (a1, a2, a3, a4, a5) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3, toObject cfg a4, toObject cfg a5]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3, a4, a5])) = (,,,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3 <*> fromObjectWith cfg a4 <*> fromObjectWith cfg a5
    fromObjectWith _ _ = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6) => MessagePack (a1, a2, a3, a4, a5, a6) where
    toObject cfg (a1, a2, a3, a4, a5, a6) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3, toObject cfg a4, toObject cfg a5, toObject cfg a6]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3, a4, a5, a6])) = (,,,,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3 <*> fromObjectWith cfg a4 <*> fromObjectWith cfg a5 <*> fromObjectWith cfg a6
    fromObjectWith _ _ = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7) => MessagePack (a1, a2, a3, a4, a5, a6, a7) where
    toObject cfg (a1, a2, a3, a4, a5, a6, a7) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3, toObject cfg a4, toObject cfg a5, toObject cfg a6, toObject cfg a7]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3, a4, a5, a6, a7])) = (,,,,,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3 <*> fromObjectWith cfg a4 <*> fromObjectWith cfg a5 <*> fromObjectWith cfg a6 <*> fromObjectWith cfg a7
    fromObjectWith _ _ = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8) where
    toObject cfg (a1, a2, a3, a4, a5, a6, a7, a8) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3, toObject cfg a4, toObject cfg a5, toObject cfg a6, toObject cfg a7, toObject cfg a8]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3, a4, a5, a6, a7, a8])) = (,,,,,,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3 <*> fromObjectWith cfg a4 <*> fromObjectWith cfg a5 <*> fromObjectWith cfg a6 <*> fromObjectWith cfg a7 <*> fromObjectWith cfg a8
    fromObjectWith _ _ = refute "invalid encoding for tuple"

instance (MessagePack a1, MessagePack a2, MessagePack a3, MessagePack a4, MessagePack a5, MessagePack a6, MessagePack a7, MessagePack a8, MessagePack a9) => MessagePack (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    toObject cfg (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ObjectArray $ V.fromList [toObject cfg a1, toObject cfg a2, toObject cfg a3, toObject cfg a4, toObject cfg a5, toObject cfg a6, toObject cfg a7, toObject cfg a8, toObject cfg a9]
    fromObjectWith cfg (ObjectArray (V.toList -> [a1, a2, a3, a4, a5, a6, a7, a8, a9])) = (,,,,,,,,) <$> fromObjectWith cfg a1 <*> fromObjectWith cfg a2 <*> fromObjectWith cfg a3 <*> fromObjectWith cfg a4 <*> fromObjectWith cfg a5 <*> fromObjectWith cfg a6 <*> fromObjectWith cfg a7 <*> fromObjectWith cfg a8 <*> fromObjectWith cfg a9
    fromObjectWith _ _ = refute "invalid encoding for tuple"

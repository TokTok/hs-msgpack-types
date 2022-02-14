{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Data.MessagePack.Arbitrary () where

import qualified Data.ByteString           as S
import           Data.MessagePack.Types    (Object (..))
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen       as Gen


instance Arbitrary Object where
    arbitrary = Gen.sized $ \n -> Gen.oneof
        [ pure ObjectNil
        , ObjectBool   <$> arbitrary
        , ObjectInt    <$> negatives
        , ObjectWord   <$> arbitrary
        , ObjectFloat  <$> arbitrary
        , ObjectDouble <$> arbitrary
        , ObjectStr    <$> (T.pack <$> arbitrary)
        , ObjectBin    <$> (S.pack <$> arbitrary)
        , ObjectArray  <$> (V.fromList <$> Gen.resize (n `div` 2) arbitrary)
        , ObjectMap    <$> (V.fromList <$> Gen.resize (n `div` 4) arbitrary)
        , ObjectExt    <$> arbitrary <*> (S.pack <$> arbitrary)
        ]
        where negatives = Gen.choose (minBound, -1)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData  #-}
{-# LANGUAGE Trustworthy #-}
module Test.QuickCheck.Instances.MessagePack () where

import qualified Data.ByteString                      as S
import           Data.MessagePack.Types               (Assoc (..), Object (..))
import qualified Data.Text                            as T
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen                  as Gen
import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Vector     ()


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
        , ObjectArray  <$> Gen.resize (n `div` 2) arbitrary
        , ObjectMap    <$> Gen.resize (n `div` 4) arbitrary
        , ObjectExt    <$> arbitrary <*> arbitrary
        ]
        where negatives = Gen.choose (minBound, -1)


instance Arbitrary a => Arbitrary (Assoc a) where
    arbitrary = Assoc <$> arbitrary

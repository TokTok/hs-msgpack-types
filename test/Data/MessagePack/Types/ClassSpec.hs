{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE Trustworthy         #-}
module Data.MessagePack.Types.ClassSpec where

import           Control.Applicative               (empty, pure, (<$>), (<*>),
                                                    (<|>))
import           Control.Monad                     (mplus, mzero)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.HashMap.Strict               as HashMap
import           Data.Hashable                     (Hashable)
import           Data.Int                          (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.Map                          as Map
import           Data.MessagePack.Types            (Assoc (..),
                                                    MessagePack (..),
                                                    Object (..))
import qualified Data.Text                         as Text
import qualified Data.Text.Lazy                    as LText
import qualified Data.Vector                       as V
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Unboxed               as VU
import           Data.Word                         (Word, Word16, Word32,
                                                    Word64, Word8)
import           GHC.Generics                      (Generic)
import           Test.Hspec                        (Spec, describe, it,
                                                    shouldBe, shouldSatisfy)
import           Test.QuickCheck                   (Arbitrary (..),
                                                    genericShrink, property,
                                                    withMaxSuccess)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

data MyType
    = SequenceTyCon Int String
    | EnumTyCon
    | RecordTyCon { intValue :: Int }
    | F01 Int8
    | F02 Int16
    | F03 Int32
    | F04 Int64
    | F05 Word
    | F06 Word8
    | F07 Word16
    | F08 Word32
    | F09 Word64
    | F10 ()
    | F11 Bool
    | F12 Float
    | F13 Double
    | F14 BS.ByteString
    | F15 LBS.ByteString
    | F16 Text.Text
    | F17 LText.Text
    | F18 (V.Vector Int)
    | F19 (VS.Vector Int)
    | F20 (VU.Vector Int)
    | F21 (Assoc [(Int, String)])
    | F22 (Map.Map Int Int)
    | F23 (IntMap.IntMap Int)
    | F24 (HashMap.HashMap Int Int)
    | F25 (Int, Int)
    | F26 (Int, Int, Int)
    | F27 (Int, Int, Int, Int)
    | F28 (Int, Int, Int, Int, Int)
    | F29 (Int, Int, Int, Int, Int, Int)
    | F30 (Int, Int, Int, Int, Int, Int, Int)
    | F31 (Int, Int, Int, Int, Int, Int, Int, Int)
    | F32 (Int, Int, Int, Int, Int, Int, Int, Int, Int)
    deriving (Show, Eq, Generic)

instance MessagePack MyType
instance Arbitrary MyType where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack . take 10 <$> arbitrary
instance Arbitrary LBS.ByteString where
    arbitrary = LBS.pack . take 10 <$> arbitrary
instance Arbitrary Text.Text where
    arbitrary = Text.pack . take 10 <$> arbitrary
instance Arbitrary LText.Text where
    arbitrary = LText.pack . take 10 <$> arbitrary
instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary
instance (Arbitrary a, VS.Storable a) => Arbitrary (VS.Vector a) where
    arbitrary = VS.fromList <$> arbitrary
instance (Arbitrary a, VU.Unbox a) => Arbitrary (VU.Vector a) where
    arbitrary = VU.fromList <$> arbitrary
instance (Arbitrary a, Hashable a, Eq a, Arbitrary b) => Arbitrary (HashMap.HashMap a b) where
    arbitrary = HashMap.fromList <$> arbitrary


spec :: Spec
spec = do
    describe "GMessagePack" $ do
        it "is a reversible operation"
            $ withMaxSuccess 10000
            $ property
            $ \(x :: MyType) -> fromObject (toObject x) `shouldBe` Just x

        it "handles arbitrary values"
            $ withMaxSuccess 10000
            $ property
            $ \ob -> fromObject ob `shouldSatisfy` \case
                  Just EnumTyCon -> True
                  Just _         -> True
                  Nothing        -> True

        it "produces msgpack values as expected" $ do
            toObject (SequenceTyCon 111 "hello")
                `shouldBe` ObjectArray
                               [ ObjectWord 0
                               , ObjectArray [ObjectWord 111, ObjectStr "hello"]
                               ]
            toObject EnumTyCon `shouldBe` ObjectWord 1
            toObject (RecordTyCon 222)
                `shouldBe` ObjectArray [ObjectWord 2, ObjectWord 222]

    describe "MessagePack" $ do
        it "handles wrong encodings correctly" $ do
            (fromObject $ ObjectArray [ObjectWord 1, ObjectWord 222] :: Maybe MyType)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe MyType)
                `shouldBe` Nothing
            (fromObject $ ObjectArray [ObjectWord 99999] :: Maybe MyType)
                `shouldBe` Nothing
            (fromObject $ ObjectArray [] :: Maybe MyType)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe Int64)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe BS.ByteString)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe LBS.ByteString)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe Text.Text)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe LText.Text)
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (V.Vector Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (VU.Vector Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (VS.Vector Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Assoc [(Int, Int)]))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Map.Map Int Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (IntMap.IntMap Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (HashMap.HashMap Int Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe Word64)
                `shouldBe` Nothing
            (fromObject (ObjectArray [ObjectWord 0]) :: Maybe ())
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int, Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int, Int, Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int, Int, Int, Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int, Int, Int, Int, Int, Int))
                `shouldBe` Nothing
            (fromObject ObjectNil :: Maybe (Int, Int, Int, Int, Int, Int, Int, Int, Int))
                `shouldBe` Nothing

        it "has a working Read/Show implementation" $ property $ \(x :: Object) ->
            read (show x) `shouldBe` x

        it "can parse both nil and [] as ()" $ do
            (fromObject ObjectNil :: Maybe ())
                `shouldBe` Just ()
            (fromObject (ObjectArray []) :: Maybe ())
                `shouldBe` Just ()

        it "can parse ints and doubles as floats" $ do
            (fromObject (ObjectDouble 123) :: Maybe Float)
                `shouldBe` Just 123
            (fromObject (ObjectWord 123) :: Maybe Float)
                `shouldBe` Just 123
            (fromObject (ObjectInt (-123)) :: Maybe Float)
                `shouldBe` Just (-123)

        it "can parse ints and floats as doubles" $ do
            (fromObject (ObjectFloat 123) :: Maybe Double)
                `shouldBe` Just 123
            (fromObject (ObjectWord 123) :: Maybe Double)
                `shouldBe` Just 123
            (fromObject (ObjectInt (-123)) :: Maybe Double)
                `shouldBe` Just (-123)

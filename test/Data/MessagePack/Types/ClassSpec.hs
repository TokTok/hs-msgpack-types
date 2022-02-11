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
import           Control.Monad.Validate            (MonadValidate (..),
                                                    runValidate)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.HashMap.Strict               as HashMap
import           Data.Hashable                     (Hashable)
import           Data.Int                          (Int16, Int32, Int64, Int8)
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.Map                          as Map
import           Data.MessagePack.Types            (Assoc (..),
                                                    MessagePack (..),
                                                    Object (..), errorMessages)
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


type Result a = Either [String] a

decode :: MessagePack a => Object -> Result a
decode = mapLeft errorMessages . runValidate . fromObject
  where
    mapLeft f (Left a)  = Left (f a)
    mapLeft _ (Right b) = Right b


spec :: Spec
spec = do
    describe "GMessagePack" $ do
        it "is a reversible operation"
            $ withMaxSuccess 10000
            $ property
            $ \(x :: MyType) -> decode (toObject x) `shouldBe` Right x

        it "handles arbitrary values"
            $ withMaxSuccess 10000
            $ property
            $ \ob -> decode ob `shouldSatisfy` \case
                  Right EnumTyCon -> True
                  Right _         -> True
                  Left _          -> True

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
            (decode $ ObjectArray [ObjectWord 1, ObjectWord 222] :: Result MyType)
                `shouldBe` Left ["invalid encoding for custom unit type"]
            (decode ObjectNil :: Result MyType)
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode $ ObjectArray [ObjectWord 99999] :: Result MyType)
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode $ ObjectArray [] :: Result MyType)
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result Int64)
                `shouldBe` Left ["invalid encoding for integer type"]
            (decode ObjectNil :: Result BS.ByteString)
                `shouldBe` Left ["invalid encoding for ByteString"]
            (decode ObjectNil :: Result LBS.ByteString)
                `shouldBe` Left ["invalid encoding for ByteString"]
            (decode ObjectNil :: Result Text.Text)
                `shouldBe` Left ["invalid encoding for Text"]
            (decode ObjectNil :: Result LText.Text)
                `shouldBe` Left ["invalid encoding for Text"]
            (decode ObjectNil :: Result (V.Vector Int))
                `shouldBe` Left ["invalid encoding for Vector"]
            (decode ObjectNil :: Result (VU.Vector Int))
                `shouldBe` Left ["invalid encoding for Unboxed Vector"]
            (decode ObjectNil :: Result (VS.Vector Int))
                `shouldBe` Left ["invalid encoding for Storable Vector"]
            (decode ObjectNil :: Result (Assoc [(Int, Int)]))
                `shouldBe` Left ["invalid encoding for Assoc"]
            (decode ObjectNil :: Result (Map.Map Int Int))
                `shouldBe` Left ["invalid encoding for Assoc"]
            (decode ObjectNil :: Result (IntMap.IntMap Int))
                `shouldBe` Left ["invalid encoding for Assoc"]
            (decode ObjectNil :: Result (HashMap.HashMap Int Int))
                `shouldBe` Left ["invalid encoding for Assoc"]
            (decode ObjectNil :: Result Word64)
                `shouldBe` Left ["invalid encoding for integer type"]
            (decode (ObjectArray [ObjectWord 0]) :: Result ())
                `shouldBe` Left ["invalid encoding for ()"]
            (decode ObjectNil :: Result (Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result (Int, Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result (Int, Int, Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result (Int, Int, Int, Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result (Int, Int, Int, Int, Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result (Int, Int, Int, Int, Int, Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode ObjectNil :: Result (Int, Int, Int, Int, Int, Int, Int, Int))
                `shouldBe` Left ["invalid encoding for tuple"]
            (decode $ ObjectArray [ObjectNil, ObjectNil] :: Result (Int, String))
                `shouldBe` Left ["invalid encoding for integer type", "invalid encoding for Text"]

        it "has a working Read/Show implementation" $ property $ \(x :: Object) ->
            read (show x) `shouldBe` x

        it "can parse both nil and [] as ()" $ do
            (decode ObjectNil :: Result ())
                `shouldBe` Right ()
            (decode (ObjectArray []) :: Result ())
                `shouldBe` Right ()

        it "can parse ints and doubles as floats" $ do
            (decode (ObjectDouble 123) :: Result Float)
                `shouldBe` Right 123
            (decode (ObjectWord 123) :: Result Float)
                `shouldBe` Right 123
            (decode (ObjectInt (-123)) :: Result Float)
                `shouldBe` Right (-123)

        it "can parse ints and floats as doubles" $ do
            (decode (ObjectFloat 123) :: Result Double)
                `shouldBe` Right 123
            (decode (ObjectWord 123) :: Result Double)
                `shouldBe` Right 123
            (decode (ObjectInt (-123)) :: Result Double)
                `shouldBe` Right (-123)

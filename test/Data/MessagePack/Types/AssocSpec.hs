{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Data.MessagePack.Types.AssocSpec where

import           Data.MessagePack.Types                (Assoc (..))
import           Test.Hspec                            (Spec, describe, it,
                                                        shouldBe)
import           Test.QuickCheck                       (property)
import           Test.QuickCheck.Instances.MessagePack ()

spec :: Spec
spec =
    describe "Assoc" $
        it "has a working Read/Show implementation"
            $ property
            $ \(x :: Assoc [(Int, Int)]) -> read (show x) `shouldBe` x

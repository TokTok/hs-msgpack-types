{-# LANGUAGE LambdaCase #-}
module Data.MessagePack.TagsSpec where

import           Data.MessagePack.Tags
import           Test.Hspec            (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec =
    describe "Assoc" $
        it "has a working Read/Show implementation" $ do
            (TAG_nil :: Int) `shouldSatisfy` \case
                TAG_nil -> True
                _       -> False
            (TAG_nil :: Integer) `shouldSatisfy` \case
                TAG_nil -> True
                _       -> False

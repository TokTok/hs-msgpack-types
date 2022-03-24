{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}
module Data.MessagePack.Types.DecodeError
    ( DecodeError
    , decodeError
    , errorMessages
    ) where

import           Control.Monad.Validate          (MonadValidate (..))
import           Data.String                     (IsString (..))
import           Text.ParserCombinators.ReadPrec (ReadPrec)

data DecodeError = DecodeError
    { errorMessages :: [String]
    }
    deriving (Show, Eq)

decodeError :: String -> DecodeError
decodeError = DecodeError . (:[])

instance IsString DecodeError where
    fromString str = DecodeError [str]

instance Semigroup DecodeError where
    DecodeError a <> DecodeError b = DecodeError (a <> b)

instance MonadValidate DecodeError ReadPrec where
    refute = fail . show
    tolerate m = m >> return Nothing

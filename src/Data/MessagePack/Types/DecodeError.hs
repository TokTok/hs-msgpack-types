module Data.MessagePack.Types.DecodeError
    ( DecodeError
    , decodeError
    , errorMessages
    ) where

import           Data.String (IsString (..))

data DecodeError = DecodeError
    { errorMessages :: [String]
    }
    deriving (Show)

decodeError :: String -> DecodeError
decodeError = DecodeError . (:[])

instance IsString DecodeError where
    fromString str = DecodeError [str]

instance Semigroup DecodeError where
    DecodeError a <> DecodeError b = DecodeError (a <> b)

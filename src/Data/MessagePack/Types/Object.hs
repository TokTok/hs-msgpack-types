{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StrictData         #-}
module Data.MessagePack.Types.Object
    ( Object (..)
    ) where

import           Control.DeepSeq (NFData (..))
import qualified Data.ByteString as S
import           Data.Int        (Int64)
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)
import qualified Data.Vector     as V
import           Data.Word       (Word64, Word8)
import           GHC.Generics    (Generic)


-- | Object Representation of MessagePack data.
data Object
    = ObjectNil
      -- ^ represents nil
    | ObjectBool                  Bool
      -- ^ represents true or false
    | ObjectInt    {-# UNPACK #-} Int64
      -- ^ represents a negative integer
    | ObjectWord   {-# UNPACK #-} Word64
      -- ^ represents a positive integer
    | ObjectFloat  {-# UNPACK #-} Float
      -- ^ represents a floating point number
    | ObjectDouble {-# UNPACK #-} Double
      -- ^ represents a floating point number
    | ObjectStr                   T.Text
      -- ^ extending Raw type represents a UTF-8 string
    | ObjectBin                   S.ByteString
      -- ^ extending Raw type represents a byte array
    | ObjectArray                 (V.Vector Object)
      -- ^ represents a sequence of objects
    | ObjectMap                   (V.Vector (Object, Object))
      -- ^ represents key-value pairs of objects
    | ObjectExt    {-# UNPACK #-} Word8 S.ByteString
      -- ^ represents a tuple of an integer and a byte array where
      -- the integer represents type information and the byte array represents data.
    deriving (Read, Show, Eq, Ord, Typeable, Generic)

instance NFData Object

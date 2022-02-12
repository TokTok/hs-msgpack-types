{-# LANGUAGE Trustworthy #-}
module Data.MessagePack.Types
    ( Assoc (..)
    , Object (..)
    , MessagePack (..)
    , Config, defaultConfig
    , DecodeError, decodeError, errorMessages
    , fromObject
    ) where

import           Control.Monad.Validate             (runValidate)
import           Data.MessagePack.Types.Assoc       (Assoc (..))
import           Data.MessagePack.Types.Class       (Config, MessagePack (..),
                                                     defaultConfig)
import           Data.MessagePack.Types.DecodeError (DecodeError, decodeError,
                                                     errorMessages)
import           Data.MessagePack.Types.Generic     ()
import           Data.MessagePack.Types.Object      (Object (..))


-- | Similar to 'fromObjectWith' 'defaultConfig' but returns the result in
-- a 'MonadFail'.
--
-- Useful when running in another 'MonadFail' like 'Maybe'.
fromObject :: (MonadFail m, MessagePack a) => Object -> m a
fromObject obj =
    case runValidate (fromObjectWith defaultConfig obj) of
        Left err -> fail $ show err
        Right ok -> return ok

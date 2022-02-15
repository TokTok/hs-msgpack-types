{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Trustworthy                #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.MessagePack.Types.Assoc
-- Copyright : (c) Daiki Handa, 2010-2011
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- MessagePack map labeling type
--
--------------------------------------------------------------------

module Data.MessagePack.Types.Assoc
    ( Assoc (..)
    ) where

import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)

-- not defined for general Functor for performance reason.
-- (ie. you would want to write custom instances for each type using
-- specialized mapM-like functions)
newtype Assoc a
    = Assoc { unAssoc :: a }
    deriving (Show, Read, Eq, Ord, Typeable, NFData)

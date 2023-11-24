-- Generated file - DO NOT EDIT
-- To regenerate, run:
--   tools/gen-tags > src/Data/MessagePack/Tags.hs
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures                 #-}
module Data.MessagePack.Tags where

pattern TAG_nil       = 0xc0  -- 11000000
pattern TAG_false     = 0xc2  -- 11000010
pattern TAG_true      = 0xc3  -- 11000011
pattern TAG_bin_8     = 0xc4  -- 11000100
pattern TAG_bin_16    = 0xc5  -- 11000101
pattern TAG_bin_32    = 0xc6  -- 11000110
pattern TAG_ext_8     = 0xc7  -- 11000111
pattern TAG_ext_16    = 0xc8  -- 11001000
pattern TAG_ext_32    = 0xc9  -- 11001001
pattern TAG_float_32  = 0xca  -- 11001010
pattern TAG_float_64  = 0xcb  -- 11001011
pattern TAG_uint_8    = 0xcc  -- 11001100
pattern TAG_uint_16   = 0xcd  -- 11001101
pattern TAG_uint_32   = 0xce  -- 11001110
pattern TAG_uint_64   = 0xcf  -- 11001111
pattern TAG_int_8     = 0xd0  -- 11010000
pattern TAG_int_16    = 0xd1  -- 11010001
pattern TAG_int_32    = 0xd2  -- 11010010
pattern TAG_int_64    = 0xd3  -- 11010011
pattern TAG_fixext_1  = 0xd4  -- 11010100
pattern TAG_fixext_2  = 0xd5  -- 11010101
pattern TAG_fixext_4  = 0xd6  -- 11010110
pattern TAG_fixext_8  = 0xd7  -- 11010111
pattern TAG_fixext_16 = 0xd8  -- 11011000
pattern TAG_str_8     = 0xd9  -- 11011001
pattern TAG_str_16    = 0xda  -- 11011010
pattern TAG_str_32    = 0xdb  -- 11011011
pattern TAG_array_16  = 0xdc  -- 11011100
pattern TAG_array_32  = 0xdd  -- 11011101
pattern TAG_map_16    = 0xde  -- 11011110
pattern TAG_map_32    = 0xdf  -- 11011111

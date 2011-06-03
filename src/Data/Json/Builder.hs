-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Json.Builder
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  Leon P Smith <leon@melding-monads.com>
--
-- Data structure agnostic JSON serialization
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Json.Builder
     ( Key  (..)
     , Value(..)
     , Object
     , row
     , Array
     , element
     , Escaped(..)
     ) where

import           Blaze.ByteString.Builder as Blaze
                   ( Write
                   , Builder
                   , copyByteString
                   , fromByteString
                   , fromLazyByteString
                   , writeByteString
                   , fromWrite
                   , fromWriteList
                   , writeWord8         )
import           Blaze.ByteString.Builder.Char.Utf8
                   ( fromChar, writeChar, fromText, fromLazyText )
import           Blaze.Text (float, double, integral)

import           Data.Bits ( Bits((.&.), shiftR) )
import qualified Data.Map               as Map
import           Data.Monoid ( Monoid (mempty, mappend, mconcat) )
import           Data.Int    ( Int8, Int16, Int32, Int64)
import           Data.Word   ( Word, Word8, Word16, Word32, Word64 )

import qualified Data.Char              as Char

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.ByteString.Internal ( c2w )

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL

---- The "core" of json-builder

class Value a => Key a where
  escape           :: a -> Escaped

class Value a where
  toBuilder        :: a -> Blaze.Builder

newtype Escaped = Escaped Blaze.Builder deriving (Monoid)

instance Key Escaped where
  escape = id

instance Value Escaped where
  toBuilder (Escaped str) = fromChar '"' `mappend` str `mappend` fromChar '"'

type CommaTracker = (Bool -> Blaze.Builder) -> Bool -> Blaze.Builder

comma :: Blaze.Builder -> CommaTracker
comma b f True  =                        b `mappend` f False
comma b f False = fromChar ',' `mappend` b `mappend` f False
{-# INLINE comma #-}

newtype Object = Object CommaTracker

instance Value Object where
  toBuilder (Object f) = fromChar '{' `mappend` f (\_ -> fromChar '}') True

instance Monoid Object where
  mempty = Object id
  mappend (Object f) (Object g) = Object (f . g)

row :: (Key k, Value a) => k -> a -> Object
row k a = Object syntax
  where
    syntax = comma (mconcat [ toBuilder k, fromChar ':',  toBuilder a ])


newtype Array = Array CommaTracker

instance Value Array where
  toBuilder (Array f) = fromChar '[' `mappend` f (\_ -> fromChar ']') True

instance Monoid Array where
  mempty = Array id
  mappend (Array f) (Array g) = Array (f . g)

element :: Value a => a -> Array
element a = Array $ comma (toBuilder a)

-- Primitive instances for json-builder

instance Value () where
  toBuilder _ = copyByteString "null"

instance Value Int     where
  toBuilder = integral

instance Value Int8    where
  toBuilder = integral

instance Value Int16   where
  toBuilder = integral

instance Value Int32   where
  toBuilder = integral

instance Value Int64   where
  toBuilder = integral

instance Value Integer where
  toBuilder = integral

instance Value Word    where
  toBuilder = integral

instance Value Word8   where
  toBuilder = integral

instance Value Word16  where
  toBuilder = integral

instance Value Word32  where
  toBuilder = integral

instance Value Word64  where
  toBuilder = integral

instance Value Double where
  toBuilder = double

instance Value Float where
  toBuilder = float

instance Value Bool where
  toBuilder True  = copyByteString "true"
  toBuilder False = copyByteString "false"

instance Key BS.ByteString where
  escape x = Escaped (loop (splitQ x))
    where
      splitQ = BU.break quoteNeeded

      loop (a,b)
        = fromByteString a `mappend`
            case BU.decode b of
              Nothing     ->  mempty
              Just (c,n)  ->  fromWrite (quoteChar c) `mappend`
                                loop (splitQ (BS.drop n b))

instance Value BS.ByteString where
  toBuilder = toBuilder . escape

instance Key BL.ByteString where
  escape x = Escaped (loop (splitQ x))
    where
      splitQ = BLU.break quoteNeeded

      loop (a,b)
        = fromLazyByteString a `mappend`
            case BLU.decode b of
              Nothing     ->  mempty
              Just (c,n)  ->  fromWrite (quoteChar c) `mappend`
                                loop (splitQ (BL.drop n b))

instance Value BL.ByteString where
  toBuilder = toBuilder . escape

instance Key T.Text where
  escape x = Escaped (loop (splitQ x))
    where
      splitQ = T.break quoteNeeded

      loop (a,b)
        = fromText a `mappend`
            case T.uncons b of
              Nothing      ->  mempty
              Just (c,b')  ->  fromWrite (quoteChar c) `mappend`
                                 loop (splitQ b')

instance Value T.Text where
  toBuilder = toBuilder . escape

instance Key TL.Text where
  escape x = Escaped (loop (splitQ x))
    where
      splitQ = TL.break quoteNeeded

      loop (a,b)
        = fromLazyText a `mappend`
            case TL.uncons b of
              Nothing      ->  mempty
              Just (c,b')  ->  fromWrite (quoteChar c) `mappend`
                                 loop (splitQ b')

instance Value TL.Text where
  toBuilder = toBuilder . escape

instance Key [Char] where
  escape str = Escaped (fromWriteList writeEscapedChar str)
    where
      writeEscapedChar c | quoteNeeded c = quoteChar c
                         | otherwise     = writeChar c

instance Value [Char] where
  toBuilder = toBuilder . escape

instance Value a => Value [a] where
  toBuilder = toBuilder . mconcat . map element

instance (Key k, Value a) => Value (Map.Map k a) where
  toBuilder = toBuilder
            . Map.foldrWithKey (\k a b -> row k a `mappend` b) mempty

------------------------------------------------------------------------------

quoteNeeded :: Char -> Bool
quoteNeeded c = c == '\\' || c == '"' || Char.ord c < 0x20
{-# INLINE quoteNeeded #-}

quoteChar :: Char -> Write
quoteChar c = case c of
                '\\'  ->  writeByteString "\\\\"
                '"'   ->  writeByteString "\\\""
                '\b'  ->  writeByteString "\\b"
                '\f'  ->  writeByteString "\\f"
                '\n'  ->  writeByteString "\\n"
                '\r'  ->  writeByteString "\\r"
                '\t'  ->  writeByteString "\\t"
                _     ->  hexEscape c

hexEscape  :: Char -> Write
hexEscape  (c2w -> c)
  = writeByteString "\\u00"
    `mappend` writeWord8 (char ((c `shiftR` 4) .&. 0xF))
    `mappend` writeWord8 (char ( c             .&. 0xF))

char :: Word8 -> Word8
char i | i < 10    = i + 48
       | otherwise = i + 87
{-# INLINE char #-}

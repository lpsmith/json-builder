-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Json.Builder.Implementation
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  Leon P Smith <leon@melding-monads.com>
--
-- This module contains all definitions for the library.  Different
-- subsets are exported by Data.Json.Builder and Data.Json.Builder.Internal.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Json.Builder.Implementation where

import           Prelude hiding ((++))

import           Blaze.ByteString.Builder as Blaze
                   ( Write
                   , Builder
                   , copyByteString
                   , fromByteString
                   , fromLazyByteString
                   , writeByteString
                   , fromWrite
                   , fromWriteList
                   , writeWord8
                   , toByteString
                   , toLazyByteString    )
import           Blaze.ByteString.Builder.Char.Utf8
                   ( fromChar, writeChar, fromText, fromLazyText )
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder ( fromString )

import           Blaze.Text (float, double, integral)

import           Data.Bits ( Bits((.&.), shiftR) )
import qualified Data.Map               as Map
import           Data.Monoid ( Monoid (mempty, mappend, mconcat) )
import           Data.Int    ( Int8, Int16, Int32, Int64)
import           Data.Word   ( Word, Word8, Word16, Word32, Word64 )

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.ByteString.Internal ( c2w )

import           Data.String  ( fromString )

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL

import qualified Data.HashMap.Lazy      as HashMap

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

-- | The 'Value' typeclass represents types that can be rendered
-- into valid json syntax.

class Value a where
  toJson :: a -> Json

-- | The 'Json' type represents valid json syntax.  It cannot be directly
-- analyzed, however it can be turned into a 'Builder' via 'toBuilder',
-- a (lazy) 'BS.ByteString' via 'toJsonBS' or 'toJsonLBS',  or used as a
-- component of a json 'Array' or json 'Object' using 'element' or 'row'.

newtype Json = Json Builder

instance Value Json where
  toJson = id


-- | The 'Escaped' type represents json string syntax.  The purpose of this
-- type is so that json strings can be efficiently constructed from multiple
-- Haskell strings without superfluous conversions or concatinations.
--
-- Internally, it is just a 'Builder' value which must produce a UTF-8 encoded
-- bytestring with backslashes,  quotes,  and control characters appropriately
-- escaped.   It also must not render the opening or closing quote,  which
-- are instead rendered by 'toJson'.

newtype Escaped = Escaped Builder deriving (Monoid)

instance Value    Escaped where
  toJson (Escaped str) = Json (fromChar '"' ++ str ++ fromChar '"')

-- | The 'JsString' typeclass represents types that can be render into json
-- string syntax.  They are special because only strings can appear as field
-- names of json objects.

class Value a => JsString a where
  escape :: a -> Escaped

instance JsString Escaped where
  escape = id

-- | The 'Object' type represents syntax for a json object.  It has a singleton
-- constructor 'row', and an instance of 'Monoid', so that 'mempty' represents 
-- the empty object and 'mappend' concatinates two objects.  Arbitrary objects
-- can be constructed using these operators.
--
-- Note that duplicate field names will appear in the output, so it is up
-- to the user of this interface to avoid duplicate field names.
newtype Object = Object CommaMonoid deriving (Monoid)

instance Value Object where
  toJson (Object xs) = case xs of
                         Empty    -> Json (copyByteString "{}")
                         Comma ys -> Json (fromChar '{' ++ ys ++ fromChar '}')

class JsObject a where
  toObject :: a -> Object

instance JsObject Object where
  toObject = id

-- |  The 'row' function constructs a json object consisting of exactly
-- one field.  These objects can be concatinated using 'mappend'.
row :: (JsString k, Value a) => k -> a -> Object
row   k a = Object (Comma (toBuilder k ++ fromChar ':' ++ toBuilder a))

-- |  The 'Array' type represents syntax for a json array.  It has been given
-- a singleton constructor 'element' and an instance of 'Monoid',  so that
-- 'mempty' represents the empty array and 'mappend' concatinates two arrays.
-- Arbitrary arrays can be constructed using these operators.

newtype Array = Array CommaMonoid deriving (Monoid)

instance Value Array where
  toJson (Array xs) = case xs of
                        Empty     -> Json (copyByteString "[]")
                        Comma  ys -> Json (fromChar '[' ++ ys ++ fromChar ']')

class JsArray a where
  toArray  :: a -> Array

instance JsArray Array where
  toArray = id

-- |  The 'element' function constructs a json array consisting of exactly
-- one value.  These arrays can be concatinated using 'mappend'.
element :: Value a => a -> Array
element a = Array  (Comma (toBuilder a))

-- | A 'CommaMonoid' inserts commas between builders.  In order to
-- satisify the 'Monoid' identity laws,  'Empty' must be distinguished
-- from @'Comma' 'mempty'@.  To demonstrate the difference:
--
-- @
-- mconcat [\"foo\", \"\"    , \"bar\"]  ==  \"foo,,bar\"
-- mconcat [\"foo\", Empty , \"bar\"]  ==  \"foo,bar\"
-- @
--
-- The strings in this example denote @CommaMonoids@ via
-- @'fromString' = Comma . 'Builder.fromString'@.  Thus @\"\"@ is equivalent
-- to @Comma mempty@.

data CommaMonoid
   = Empty
   | Comma !Builder

instance Monoid CommaMonoid where
  mempty = Empty
  mappend Empty     x = x
  mappend (Comma a) x
        = Comma (a ++ case x of
                        Empty   -> mempty
                        Comma b -> fromChar ',' ++ b)

toBuilder :: Value a => a -> Builder
toBuilder x = case toJson x of
                Json y -> y
{-# SPECIALIZE toBuilder :: Json -> Builder #-}
{-# INLINE toBuilder #-}

toJsonBS  :: Value a => a -> BS.ByteString
toJsonBS  = toByteString     . toBuilder

toJsonLBS :: Value a => a -> BL.ByteString
toJsonLBS = toLazyByteString . toBuilder


-- Primitive instances for json-builder

-- | renders as @null@
instance Value () where
  toJson _ = Json (copyByteString "null")

instance Value Int     where
  toJson = Json . integral

instance Value Int8    where
  toJson = Json . integral

instance Value Int16   where
  toJson = Json . integral

instance Value Int32   where
  toJson = Json . integral

instance Value Int64   where
  toJson = Json . integral

instance Value Integer where
  toJson = Json . integral

instance Value Word    where
  toJson = Json . integral

instance Value Word8   where
  toJson = Json . integral

instance Value Word16  where
  toJson = Json . integral

instance Value Word32  where
  toJson = Json . integral

instance Value Word64  where
  toJson = Json . integral

instance Value Double where
  toJson = Json . double

instance Value Float where
  toJson = Json . float

-- | renders as @true@ or @false@

instance Value Bool where
  toJson x = Json (fromByteString $! if x then "true" else "false")

-- | must be UTF-8 encoded

instance JsString BS.ByteString where
  escape x = Escaped (loop x)
    where
      loop (BU.break quoteNeeded -> (a,b))
        = fromByteString a ++
            case BU.decode b of
              Nothing     ->  mempty
              Just (c,n)  ->  quoteChar c ++ loop (BS.drop n b)

instance Value BS.ByteString where
  toJson = toJson . escape

-- | must be UTF-8 encoded

instance JsString BL.ByteString where
  escape x = Escaped (loop x)
    where
      loop (BLU.break quoteNeeded -> (a,b))
        = fromLazyByteString a ++
            case BLU.decode b of
              Nothing     ->  mempty
              Just (c,n)  ->  quoteChar c ++ loop (BL.drop n b)

instance Value BL.ByteString where
  toJson = toJson . escape

instance JsString T.Text where
  escape x = Escaped (loop x)
    where
      loop (T.break quoteNeeded -> (a,b))
        = fromText a ++
            case T.uncons b of
              Nothing      ->  mempty
              Just (c,b')  ->  quoteChar c ++ loop b'

instance Value T.Text where
  toJson = toJson . escape

instance JsString TL.Text where
  escape x = Escaped (loop x)
    where
      loop (TL.break quoteNeeded -> (a,b))
        = fromLazyText a ++
            case TL.uncons b of
              Nothing      ->  mempty
              Just (c,b')  ->  quoteChar c ++ loop b'

instance Value TL.Text where
  toJson = toJson . escape

instance JsString [Char] where
  escape str = Escaped (fromWriteList writeEscapedChar str)
    where
      writeEscapedChar c | quoteNeeded c = quoteCharW c
                         | otherwise     = writeChar  c

instance Value [Char] where
  toJson = toJson . escape

-- | renders as an 'Array'
instance Value a => Value [a] where
  toJson = toJson . toArray

instance Value a => JsArray [a] where
  toArray = foldr (\a as -> element a ++ as) mempty

-- | renders as an 'Object'
instance (JsString k, Value a) => Value (Map.Map k a) where
  toJson = toJson . toObject

instance (JsString k, Value a) => JsObject (Map.Map k a) where
  toObject = Map.foldrWithKey     (\k a b -> row k a ++ b) mempty

-- | renders as an 'Object'
instance (JsString k, Value a) => Value (HashMap.HashMap k a) where
  toJson = toJson . toObject

instance (JsString k, Value a) => JsObject (HashMap.HashMap k a) where
  toObject = HashMap.foldrWithKey (\k a b -> row k a ++ b) mempty

------------------------------------------------------------------------------

quoteNeeded :: Char -> Bool
quoteNeeded c = c == '\\' || c == '"' || c < '\x20'
{-# INLINE quoteNeeded #-}

quoteChar :: Char -> Builder
quoteChar c = case c of
                 '\\'  ->  copyByteString "\\\\"
                 '"'   ->  copyByteString "\\\""
                 '\b'  ->  copyByteString "\\b"
                 '\f'  ->  copyByteString "\\f"
                 '\n'  ->  copyByteString "\\n"
                 '\r'  ->  copyByteString "\\r"
                 '\t'  ->  copyByteString "\\t"
                 _     ->  fromWrite (hexEscape c)

quoteCharW :: Char -> Write
quoteCharW c = case c of
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
    ++ writeWord8 (char ((c `shiftR` 4) .&. 0xF))
    ++ writeWord8 (char ( c             .&. 0xF))
{-# INLINE hexEscape #-}

char :: Word8 -> Word8
char i | i < 10    = i + 48
       | otherwise = i + 87
{-# INLINE char #-}

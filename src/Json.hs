-----------------------------------------------------------------------------
-- |
-- Module      :  Json
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
{-# LANGUAGE UndecidableInstances #-}

module Json
     ( Key  (..)
     , Value(..)
     , Object
     , row
     , Array
     , element
     ) where

import           Blaze.ByteString.Builder as Blaze
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Char8 (fromChar)
import           Blaze.ByteString.Builder.Char.Utf8 (fromText, fromLazyText)
import           Blaze.Text (float, double, integral)

import           Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.Map               as Map
import           Data.Monoid
import           Data.Word (Word16, Word8)

import qualified Data.Char              as Char

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.ByteString.Char8()
import           Data.ByteString.Internal (w2c, c2w)

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL

---- The "core" of json-builder

class Value a where
  toBuilder        :: a -> Blaze.Builder

class Value a => Key a

data Pair = Pair !Blaze.Builder !Bool

newtype Object = Object (Bool -> Pair)

instance Value Object where
  toBuilder (Object f)
    = case f True of
        Pair fb _ -> mconcat [fromChar '{', fb, fromChar '}']

instance Monoid Object where
  mempty = Object $ \x -> Pair mempty x
  mappend (Object f) (Object g)
    = Object $ \x -> case f x of
                      Pair fb x' ->
                           case g x' of
                            Pair gb x'' ->
                                 Pair (fb `mappend` gb) x''

row :: (Key k, Value a) => k -> a -> Object
row k a = Object syntax
  where
    syntax = comma (mconcat [ toBuilder k, fromChar ':',  toBuilder a ])
    comma b True  = Pair b False
    comma b False = Pair (fromChar ',' `mappend` b) False


newtype Array = Array (Bool -> Pair)

instance Value Array where
  toBuilder (Array f)
    = case f True of
        Pair fb _ -> mconcat [fromChar '[', fb, fromChar ']']

instance Monoid Array where
  mempty = Array $ \x -> Pair mempty x
  mappend (Array f) (Array g)
    = Array $ \x -> case f x of
                     Pair fb x' ->
                          case g x' of
                           Pair gb x'' ->
                                Pair (fb `mappend` gb) x''

element :: Value a => a -> Array
element a = Array $ comma (toBuilder a)
  where
    comma b True  = Pair b False
    comma b False = Pair (fromChar ',' `mappend` b) False


-- Primitive instances for json-builder

instance Value () where
  toBuilder _ = copyByteString "null"

instance Integral a => Value a where
  toBuilder = integral

instance Value Double where
  toBuilder = double

instance Value Float where
  toBuilder = float

instance Value Bool where
  toBuilder True  = copyByteString "true"
  toBuilder False = copyByteString "false"

instance Value BS.ByteString where
  toBuilder x = fromChar '"' `mappend` loop (splitQ x)
    where
      splitQ = BU.break quoteNeeded

      loop (a,b)
        = fromByteString a `mappend`
            case BU.decode b of
              Nothing     ->  fromChar '"'
              Just (c,n)  ->  quoteChar c `mappend`
                                loop (splitQ (BS.drop n b))

instance Key BS.ByteString

instance Value BL.ByteString where
  toBuilder x = fromChar '"' `mappend` loop (splitQ x)
    where
      splitQ = BLU.break quoteNeeded

      loop (a,b)
        = fromLazyByteString a `mappend`
            case BLU.decode b of
              Nothing     ->  fromChar '"'
              Just (c,n)  ->  quoteChar c `mappend`
                                loop (splitQ (BL.drop n b))

instance Key BL.ByteString

instance Value T.Text where
  toBuilder x = fromChar '"' `mappend` loop (splitQ x)
    where
      splitQ = T.break quoteNeeded

      loop (a,b)
        = fromText a `mappend`
            case T.uncons b of
              Nothing      ->  fromChar '"'
              Just (c,b')  ->  quoteChar c `mappend` loop (splitQ b')

instance Key T.Text

instance Value TL.Text where
  toBuilder x = fromChar '"' `mappend` loop (splitQ x)
    where
      splitQ = TL.break quoteNeeded

      loop (a,b)
        = fromLazyText a `mappend`
            case TL.uncons b of
              Nothing      ->  fromChar '"'
              Just (c,b')  ->  quoteChar c `mappend` loop (splitQ b')

instance Key TL.Text

-- FIXME: rewrite/optimize the quoting routines for Strings

instance Value [Char] where
  toBuilder = toBuilder . BU.fromString

instance Key [Char]

-- Convenient (?) instances for json-builder

instance Value a => Value [a] where
  toBuilder = toBuilder . mconcat . map element

instance (Key k, Value a) => Value (Map.Map k a) where
  toBuilder = toBuilder
            . Map.foldrWithKey (\k a b -> row k a `mappend` b) mempty


------------------------------------------------------------------------------

quoteNeeded :: Char -> Bool
quoteNeeded c = c == '\\' || c == '"' || Char.ord c < 0x20

quoteChar :: Char -> Builder
quoteChar c = case c of
                '\\'  ->  copyByteString "\\\\"
                '"'   ->  copyByteString "\\\""
                '\b'  ->  copyByteString "\\b"
                '\f'  ->  copyByteString "\\f"
                '\n'  ->  copyByteString "\\n"
                '\r'  ->  copyByteString "\\r"
                '\t'  ->  copyByteString "\\t"
                _     ->  hexEscape c

hexEscape  :: Char -> Builder
hexEscape  (c2w -> c)
  = fromWrite (writeByteString "\\u00"
               `mappend` writeWord8 (char ((c `shiftR` 4) .&. 0xF))
               `mappend` writeWord8 (char ( c             .&. 0xF)))

char :: Word8 -> Word8
char i | i < 10    = i + 48
       | otherwise = i + 87
{-# INLINE char #-}

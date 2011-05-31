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
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Json
     ( Value(..)
     , Object
     , row
     , Array
     , element
     ) where

import           Blaze.ByteString.Builder as Blaze
import           Data.Monoid
import           Blaze.ByteString.Builder.ByteString
import           Blaze.ByteString.Builder.Char8
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BU
import           Data.ByteString.Char8()
import           Data.ByteString.Internal (w2c, c2w)
import qualified Data.Char              as Char
import qualified Data.Map               as Map
import           Data.Word (Word16, Word8)
import           Data.Bits (shiftL, shiftR)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding (encodeUtf8)
import           Blaze.Text (float, double, integral)


---- The "core" of json-builder

class Value a where
  toBuilder        :: a -> Blaze.Builder
  toByteString     :: a -> BS.ByteString
  toLazyByteString :: a -> BL.ByteString
  toByteString     = Blaze.toByteString     . toBuilder
  toLazyByteString = Blaze.toLazyByteString . toBuilder

class Value a => Key a

data Pair = Pair !Blaze.Builder !Bool

newtype Object = Object (Bool -> Pair)

instance Value Object where
  toBuilder (Object f) = case f True of
                           Pair fb _ -> mconcat [fromChar '{', fb, fromChar '}']

instance Monoid Object where
  mempty = Object $ \x -> Pair mempty x
  mappend (Object f) (Object g)
      = Object $ \x -> case f x of
                         Pair fb x' -> case g x' of
                                         Pair gb x'' -> Pair (fb `mappend` gb) x''

row :: (Key a, Value b) => a -> b -> Object
row str a = Object $ comma (mconcat [ toBuilder str, fromChar ':',  toBuilder a ])
  where
    comma b True  = Pair b False
    comma b False = Pair (fromChar ',' `mappend` b) False


newtype Array = Array (Bool -> Pair)

instance Value Array where
  toBuilder (Array f) = case f True of
                          Pair fb _ -> mconcat [fromChar '[', fb, fromChar ']']

instance Monoid Array where
  mempty = Array $ \x -> Pair mempty x
  mappend (Array f) (Array g)
      = Array $ \x -> case f x of
                         Pair fb x' -> case g x' of
                                         Pair gb x'' -> Pair (fb `mappend` gb) x''

element :: Value a => a -> Array
element a = Array $ comma (toBuilder a)
  where
    comma b True  = Pair b False
    comma b False = Pair (fromChar ',' `mappend` b) False


-- Primitive instances for json-builder

instance Value () where
  toBuilder _ = fromByteString "null"

instance Integral a => Value a where
  toBuilder = integral

instance Value Double where
  toBuilder = double

instance Value Float where
  toBuilder = float

instance Value Bool where
  toBuilder True  = fromByteString "true"
  toBuilder False = fromByteString "false"

instance Value BS.ByteString where
  toBuilder x = fromWrite (writeChar '"' `mappend` loop (splitQ x))
    where
      loop (a,b)
        = writeByteString a `mappend`
            case BU.decode b of
              Nothing     ->  writeChar '"'
              Just (c,n)  ->  writeByteString (quoteChar c) `mappend`
                                loop (splitQ (BS.drop n b))

      splitQ = BU.break quoteNeeded

      quoteNeeded :: Char -> Bool
      quoteNeeded c = c == '\\' || c == '"' || Char.ord c < 0x20

      quoteChar :: Char -> BS.ByteString
      quoteChar c = case c of
                     '\\'  ->  "\\\\"
                     '"'   ->  "\\\""
                     '\b'  ->  "\\b"
                     '\f'  ->  "\\f"
                     '\n'  ->  "\\n"
                     '\r'  ->  "\\r"
                     '\t'  ->  "\\t"
                     _     ->  hexEscape c

      hexEscape :: Char -> BS.ByteString
      hexEscape c = s
        where
          !n = fromIntegral (Char.ord c) :: Word16

          !s = BS.append "\\u" $ fst $ BS.unfoldrN 4 f n

          f n = Just (char (fromIntegral (n `shiftR` 12)), n `shiftL` 4)

          char i
            | i < 10    = (c2w '0' -  0) + i
            | otherwise = (c2w 'a' - 10) + i

instance Key BS.ByteString

-- FIXME: rewrite/optimize the quoting routines for ByteString, Text, String
--        Should we support direct quoting of lazy strings?

instance Value Text where
  toBuilder = toBuilder . encodeUtf8

instance Key Text

instance Value [Char] where
  toBuilder = toBuilder . BU.fromString

instance Key [Char]

-- Convenient (?) instances for json-builder

instance Value a => Value (Maybe a) where
  toBuilder Nothing  = fromByteString "null"
  toBuilder (Just a) = toBuilder a

instance Value a => Value [a] where
  toBuilder = toBuilder . mconcat . map element

instance Value a => Value (Map.Map BS.ByteString a) where
  toBuilder = toBuilder . Map.foldrWithKey (\k a b -> row k a `mappend` b) mempty

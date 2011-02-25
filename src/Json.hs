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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

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
import           Data.Word (Word16, Word8)
import           Data.Bits (shiftL, shiftR)


---- The "core" of json-builder

class Value a where
  toBuilder        :: a -> Blaze.Builder
  toByteString     :: a -> BS.ByteString
  toLazyByteString :: a -> BL.ByteString
  toByteString     = Blaze.toByteString     . toBuilder
  toLazyByteString = Blaze.toLazyByteString . toBuilder

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

row :: Value a => BS.ByteString -> a -> Object
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

instance Value Integer where
  -- FIXME:  Should this be more efficient?
  toBuilder x = fromString (show x)

instance Value Double where
  -- FIXME:  Do we emit the correct syntax?
  -- FIXME:  Should this be more efficient?
  toBuilder x = fromString (show x)

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
      quoteNeeded c = case c of
                         '\\' -> True
                         '"'  -> True
                         _    -> Char.isControl c

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


-- Convenient (?) instances for json-builder

instance Value a => Value (Maybe a) where
  toBuilder Nothing  = fromByteString "null"
  toBuilder (Just a) = toBuilder a

instance Value a => Value [a] where
  toBuilder = toBuilder . mconcat . map element

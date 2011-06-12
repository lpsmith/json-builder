-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Json.Builder.Internal
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  Leon P Smith <leon@melding-monads.com>
--
-- Internal bits.   By using the constructors provided in this module,
-- you can break the abstraction that json-builder provides and emit invalid
-- JSON syntax.   Also, this module is not as stable as the public interface
-- and can change at any time.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Json.Builder.Internal
     ( Json    (..)
     , Object  (..)
     , Array   (..)
     , Value   (..)
     , Escaped (..)
     , JsString(..)
     , CommaMonoid(..)
     ) where

import Prelude hiding ((++))

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8  ( fromChar )
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze  ( fromString )
import Data.String  ( fromString )
import Data.ByteString  ( ByteString )
import Data.ByteString.Char8()
import Data.Monoid

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

-- | The 'Value' typeclass represents types that can be rendered
-- into valid json syntax.

class Value a where
  toJson :: a -> Json

-- | The 'Json' type represents valid json syntax.  It cannot be directly
-- analyzed, however it can be turned into a 'Builder' via 'toBuilder',
-- a (lazy) 'ByteString' via 'toJsonBS' or 'toJsonLBS',  or used as a component
-- of a json 'Array' or json 'Object' using 'element' or 'row'.

newtype Json = Json Builder

instance Value Json where
  toJson = id

-- | The 'JsString' typeclass represents types that can be render into json string
-- syntax.  They are special because only strings can appear as field names
-- of json objects.

class Value a => JsString a where
  escape :: a -> Escaped

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

instance JsString Escaped where
  escape = id

-- |  The 'Object' type represents syntax for a json object.  It has a singleton
-- constructor 'row', and an instance of 'Monoid', so that 'mempty' represents the
-- empty object and 'mappend' concatinates two objects.  Arbitrary objects can
-- be constructed using these operators.
--
-- Note that duplicate field names will appear in the output, so it is up
-- to the user of this interface to avoid duplicate field names.
newtype Object = Object CommaMonoid deriving (Monoid)

instance Value Object where
  toJson (Object xs) = case xs of
                         Empty    -> Json (copyByteString "{}")
                         Comma ys -> Json (fromChar '{' ++ ys ++ fromChar '}')

-- |  The 'Array' type represents syntax for a json array.  It has been given
-- a singleton constructor 'element' and an instance of 'Monoid',  so that
-- 'mempty' represents the empty array and 'mappend' concatinates two arrays.
-- Arbitrary arrays can be constructed using these operators.

newtype Array = Array CommaMonoid deriving (Monoid)

instance Value Array where
  toJson (Array xs) = case xs of
                        Empty     -> Json (copyByteString "[]")
                        Comma  ys -> Json (fromChar '[' ++ ys ++ fromChar ']')

-- | A 'CommaMonoid' inserts commas between builders.  In order to
-- satisify the 'Monoid' identity laws,  'Empty' must be distinguished
-- from @'Comma' 'mempty'@.  To demonstrate the difference:
--
-- @
-- mconcat [\"foo\", Comma mempty, \"bar\"]  ==  \"foo,,bar\"
-- mconcat [\"foo\", Empty       , \"bar\"]  ==  \"foo,bar\"
-- @
--
-- Note that this example abuses notation somewhat, treating Builders as
-- strings using the injection @'fromString' = Comma . 'Blaze.fromString'@

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

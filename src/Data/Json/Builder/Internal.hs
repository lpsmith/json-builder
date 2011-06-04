-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Json.Builder.Internal
-- Copyright   :  (c) 2011 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  Leon P Smith <leon@melding-monads.com>
--
-- Internal bits.   You can break this library's abstraction and emit
-- invalid Json syntax the constructors provided in this module.
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Json.Builder.Internal
     ( Value   (..)
     , Json    (..)
     , JsString(..)
     , Escaped (..)
     ) where

import Prelude hiding ((++))

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8 ( fromChar )
import Data.ByteString(ByteString)
import Data.Monoid

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

-- | The 'Value' typeclass represents types that can be rendered
-- into valid json syntax.

class Value a where
  toJson :: a -> Json

-- | The 'Json' type represents valid json syntax.  It cannot be directly
-- analyzed, however it can be rendered into a 'ByteString' and used to
-- as a component of an array or an object to build a bigger json value.

newtype Json = Json Builder

instance Value Json where
  toJson = id

-- | The 'String' typeclass represents types that render into json string
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


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

module Data.Json.Builder.Internal
     ( Json    (..)
     , Object  (..)
     , Array   (..)
     , Escaped (..)
     , CommaMonoid(..)
     ) where

import Data.Json.Builder.Implementation

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Json.Builder
-- Copyright   :  (c) 2011-2012 Leon P Smith
-- License     :  BSD3
--
-- Maintainer  :  Leon P Smith <leon@melding-monads.com>
--
-- Data structure agnostic JSON serialization
--
-----------------------------------------------------------------------------

module Data.Json.Builder
     ( -- * Json Values
       Json
     , toBuilder
     , toJsonBS
     , toJsonLBS
     , Value(toJson)
       -- * Json Arrays @[\"foobar\",true,42]@
     , Array
     , element
     , JsArray(toArray)
       -- * Json Objects @{\"x\":3.14,\"y\":-2.7}@
     , Object
     , row
     , JsObject(toObject)
       -- * Json Strings
     , Escaped
     , JsString(escape)
       -- * Monoid (from @Data.Monoid@)
     , Monoid(mempty, mappend, mconcat)
     ) where

import Data.Monoid
import Data.Json.Builder.Implementation

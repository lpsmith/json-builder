import Prelude hiding ((++))
import Data.Aeson as Aeson
import Data.Json.Builder as Json
import qualified Data.Json.Builder.Internal as Json
import qualified Data.Vector as Vector
import Data.Attoparsec.Number
import Data.Monoid
import Criterion.Main
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Control.DeepSeq

import Blaze.ByteString.Builder.Char8

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

f x xs = fromChar ',' ++ toBuilder x ++ xs
{-# INLINE f #-}

array x = Json.Array (Json.Comma x)
{-# INLINE array #-}

fastList :: Json.Value a => [a] -> Json.Array
fastList xs
  = case xs of
      [] -> mempty
      (x:xs) -> array (toBuilder x ++ foldr f mempty xs)

fastVector :: Json.Value a => Vector.Vector a -> Json.Array
fastVector xs
  | Vector.null xs = mempty
  | otherwise      = array (  toBuilder (Vector.head xs)
                           ++ Vector.foldr f mempty (Vector.tail xs))

instance NFData L.ByteString where
    rnf = go
      where go (L.Chunk _ cs) = go cs
            go L.Empty        = ()
    {-# INLINE rnf #-}

instance Json.Value Number where
  toJson (I x) = toJson x
  toJson (D x) = toJson x

instance Json.Value Aeson.Value where
  toJson (Object obj) = toJson obj
  toJson (Array  arr) = toJson (Vector.foldr (\x xs -> element x ++ xs) mempty arr)
  toJson (String txt) = toJson txt
  toJson (Number num) = toJson num
  toJson (Bool   b  ) = toJson b
  toJson (Null      ) = toJson ()

fastToJson (Array arr) = toJson (fastVector arr)
fastToJson x           = toJson x

input1 :: Aeson.Value
input1 = toJSON (fst (B.unfoldrN 0x20 f 0))
   where f n = Just (n, n+1)

type Zs = [Integer]

input2 :: Aeson.Value
input2 = toJSON ([1..100] :: Zs)

input3 :: Aeson.Value
input3 = toJSON (take 100 (cycle [True,False]))

main = defaultMain
  [-- bench "aeson1"  $ nf Aeson.encode input1
--  , bench "build1"  $ nf toJsonLBS input1
--  , bench "aeson2"  $ nf Aeson.encode input2
--  , bench "build2a" $ nf toJsonLBS input2
--  , bench "build2b" $ nf (toJsonLBS . fastToJson) input2
--  , bench "aeson3"  $ nf (\n -> Aeson.encode (toJSON ([1..n] :: Zs))) 100
--  , bench "build3a" $ nf (\n -> toJsonLBS ([1..n] :: Zs)) 100
--  , bench "build3b" $ nf (\n -> toJsonLBS (fastList ([1..n] :: Zs))) 100
    bench "build4"  $ nf toJsonLBS input3
  , bench "aeson4"  $ nf Aeson.encode input3
  , bench "build4"  $ nf toJsonLBS input3
  ]

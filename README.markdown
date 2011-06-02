`json-builder` is a data structure agnostic json serialization library.  Most existing serializers operate on a specified data structure that corresponds to a json value.   Other pieces of data must then marshaled into that specific structure before they can be serialized to json.   

`json-builder` avoids this marshaling,  and thus is potentially more efficient when you want to turn an arbitrary data structure into a json string.  Moreover,  I've found that writing a  custom serializer for this API to be somewhat simpler than writing a custom marshaller into the data structures that say, the [json](http://hackage.haskell.org/package/json)  or  [aeson](http://hackage.haskell.org/package/aeson) packages use.

Unfortunately, `json-builder` cannot yet read or process json data.  Moreover, it's not clear to me how to pull a similar kind of trick as above,  and avoid unnecessary data structures. 

This library revolves around a single typeclass `Json.Value` that represents pieces of data that can be serialized to the Json format.   It's member is `toBuilder`, which returns a [builder](http://hackage.haskell.org/packages/archive/blaze-builder/0.3.0.1/doc/html/Blaze-ByteString-Builder.html) that represents the concrete syntax of that value.

    class Json.Value a where
      toBuilder        :: a -> Blaze.ByteString.Builder

If a user-supplied instance of `Json.Value` uses functions from [blaze-builder](http://hackage.haskell.org/package/blaze-builder), then it is possible to emit invalid JSON strings.   This is one disadvantage relative to specifying a data structure.  However valid JSON is guaranteed if these instances use only valid `toBuilder` methods to create builders.

Arrays are represented by an abstract type with a singleton constructor `element` and a [monoid](http://www.haskell.org/ghc/docs/7.0-latest/html/libraries/base-4.3.1.0/Data-Monoid.html#t:Monoid) instance.  Thus arbitrary sequences can be serialized using `mempty` and `mappend`.

    element :: Value a => a -> Array
    instance Monoid Array

Objects are similar.  There is a typeclass "Key" used to distinguish Strings from other Json values which cannot be keys,  a singleton constructor,  and a monoid instance. Note that rows with duplicate keys will appear in the output; it is up to the user of this interface to ensure that keys are unique.

    class Value a => Key a

    row :: (Key a, Value b) => a -> b -> Object
    instance Monoid Object

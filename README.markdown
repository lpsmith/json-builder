`json-builder` is a data structure agnostic json serialization library.  Most existing serializers operate on a specific data structure that corresponds to a json value.   Other pieces of data must then marshaled into that data structure before they can be serialized to json.

`json-builder` avoids this marshaling,  and thus is potentially more efficient when you want to serialize arbitrary data structures.  Moreover,  I've found that writing a serializer using this API to be somewhat simpler than writing a marshaller into the data structures used by the [json](http://hackage.haskell.org/package/json) or [aeson](http://hackage.haskell.org/package/aeson) packages.

Unfortunately, `json-builder` cannot yet read or process json data.  Moreover, it's not clear to me how to pull a similar kind of trick above,  and avoid unnecessary data structures.

This library revolves around a single typeclass `Value` that represents pieces of data that can be serialized to the Json format. It's member is `toJson`, which returns a newtyped [Builder](http://hackage.haskell.org/packages/archive/blaze-builder/0.3.0.1/doc/html/Blaze-ByteString-Builder.html) that represents the concrete syntax of that value.

    class Value a where
      toJson :: a -> Json

Arrays are represented by an abstract type with a singleton constructor `element` and a [monoid](http://www.haskell.org/ghc/docs/7.0-latest/html/libraries/base-4.3.1.0/Data-Monoid.html#t:Monoid) instance.  Thus arbitrary sequences can be serialized using `mempty` and `mappend`.

    element :: Value a => a -> Array
    instance Monoid Array

Objects are similar.  There is a typeclass `JsString` that distinguishes strings from other Json values,  a singleton constructor,  and a monoid instance. Note that rows with duplicate keys will appear in the output; it is up to the user of this interface to ensure that keys are unique.

    class Value a => JsString a

    row :: (JsString a, Value b) => a -> b -> Object
    instance Monoid Object

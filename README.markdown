`json-builder` is a data structure agnostic json serialization library.  Most existing serializers operate on a specified data structure that corresponds to a json value.   Other pieces of data must then marshaled into that specific structure before they can be serialized to json.   

`json-builder` avoids this marshaling,  and thus is potentially more efficient when you want to turn an arbitrary data structure into a json string.  Moreover,  I've found that writing a  custom serializer for this API to be somewhat simpler than writing a custom marshaller into the data structures that say, the [json](http://hackage.haskell.org/package/json)  or  [aeson](http://hackage.haskell.org/package/aeson) packages use.

Unfortunately, `json-builder` cannot yet read or process json data.  Moreover, it's not clear to me how to pull a similar kind of trick as above,  and avoid unnecessary data structures. 

This library revolves around a single typeclass `Json.Value` that represents pieces of data that can be serialized to the Json format.   The most important member is `toBuilder`, which returns a blaze-builder that represents the concrete syntax of that value.   The other members of the typeclass are supplied for convenience and need not be explicitly defined in any instance.  

    class Json.Value a where
      toBuilder        :: a -> Blaze.ByteString.Builder
      toByteString     :: a -> Data.ByteString.ByteString
      toLazyByteString :: a -> Data.ByteString.Lazy.ByteString

Arrays are represented by an abstract type that is a monoid and has one constructor to write a single element;  Objects are similar.

One disadvantage this approach compared to the specified data structure is that the user-supplied instances of the `Json` typeclass can emit invalid JSON strings.  This shouldn't be a significant problem in practice,  because instances of `Json` should typically occur less often than uses of `toBuilder`, and valid JSON is guaranteed if these instances use only valid `toBuilder` definitions and does not create it's own builders through the Blaze.Builder.* modules.

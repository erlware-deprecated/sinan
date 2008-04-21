"""
The stream oriented parser produces a series of events that describe the structure currently being parsed. These events are then consumed by the calling application in some manner. The actual mechanism of consumption will vary from language type to language type though a few different types of APIs are discussed in the appendix of this document.

JSON is composed of two types of data structures primitive and complex types. The events for the two types of structures remain the same, changing only in the description of the structure being described.

The events around primitive types are relatively simple and should be implemented as simply as possible. Individual API definers will choose how they want to implement it using the examples in the appendix as a guide.


Primitive types in JSON are strings, numbers, booleans and null. These are described within the event itself


      string = STRING_DATA(value)
      number = NUMBER_DATA(value)
      boolean = BOOLEAN_DATA(value)
      null = NULL_DATA(value)



Complex types in JSON are objects and arrays. These are represented by a series of events that describe the object. Because they are complex types their representation is much more complex then that of primitive types. However, it allows the object to be consumed as it occurred in the stream.


       object =  OBJECT_BEGIN
                     KEY(string_value)
                     VALUE_BEGIN
                      ... # recursive type description
                     VALUE_END
                      ... # arbitrary number of additional key/value pairs
                  OBJECT_END

       array =  ARRAY_BEGIN
                     VALUE_BEGIN
                       ... # recursive type description
                     VALUE_END
                ARRAY_END
"""


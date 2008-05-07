import sys
import libsinan.sinexceptions
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
class TestHandler(object):
    indent = ""

    def object_begin(self):
        print self.indent + "OBJECT_BEGIN"
        self.indent += "  "

    def key(self, value):
        print self.indent + "KEY(" + value + ")"

    def value_begin(self):
        print self.indent + "VALUE_BEGIN"
        self.indent += "  "

    def string(self, value):
        print self.indent + "STRING(" + value + ")"

    def number(self, value):
        print self.indent + "NUMBER(" + str(value) + ")"

    def true(self):
        print self.indent + "TRUE"

    def false(self):
        print self.indent + "FALSE"

    def null(self):
        print self.indent + "NULL"

    def array_begin(self):
        print self.indent + "ARRAY_BEGIN"
        self.indent += "  "

    def array_end(self):
        self.indent = self.indent[:-2]
        print self.indent + "ARRAY_END"

    def object_end(self):
        self.indent = self.indent[:-2]
        print self.indent + "OBJECT_END"

    def value_end(self):
        self.indent = self.indent[:-2]
        print self.indent + "VALUE_END"

class ReaderWithPushback(object):
    """ Provides a very simple reader with peek and pushback
    capability. Helps enormously with simple parsing """

    def __init__(self, somefile):
        self._read = somefile.read
        self.held = ''
        self.sawEOF = False

    def read(self, length=1):
        """ Read some number of characters from the internal stream,
        reading from the pushback buffer if required"""
        assert length > 0
        while len(self.held) < length and not self.sawEOF:
            chunk = self._read(length - len(self.held))
            if chunk:
                self.held += chunk
            else:
                self.sawEOF = True
        if len(self.held) > length:
            self.held, result = (self.held[length :],
                                 self.held[: length])
        else:
            self.held, result = '', self.held
            return result

    def pushback(self, somestuff):
        """ Make a character readable again """
        self.held = somestuff + self.held

    def peek(self, length=1):
        """ Allow the calling code to peek at an item on the list"""
        data = self.read(length)
        self.pushback(data)
        return data

def parse(stream, handler):
    """ Parse a stream, eventing into handler """
    new_stream = ReaderWithPushback(stream)
    while parse_value(new_stream, handler): continue

def parse_value(stream, handler):
    """ Parse a json value. In reality, figure out what
    is being parsed and use the dedicated parsing function """

    discard_whitespace(stream)
    char = stream.peek()
    handler.value_begin()
    if char == '{':
        parse_object(stream, handler)
    elif char == '[':
        parse_array(stream, handler)
    elif char == '"':
        parse_string(stream,  handler)
    elif char == '-':
        parse_number(stream, handler)
    elif (char == '0' or char == '1' or char == '2' or
          char == '3' or char == '4' or char == '5' or
          char == '6' or char == '7' or char == '8' or
          char == '9'):
        parse_number(stream, handler)
    elif char == 't':
        parse_true(stream, handler)
    elif char == 'f':
        parse_false(stream, handler)
    elif char == 'n':
        parse_null(stream, handler)
    elif char == '':
        return False
    else:
        raise libsinan.sinexceptions.ParseError, "unexpected value " + char
    return handler.value_end()

def discard_whitespace(stream):
    """ Read ahead to the first non-whitespace character, return
    when found """
    char = stream.read()
    while char:
        if char == ' ' or char == '\t' or char == '\n' or char == '\l':
            char = stream.read()
            continue
        else:
            stream.pushback(char)
            break

def parse_true(stream, handler):
    """ Parse a true atom """
    value = stream.read(4)
    if value == 'true':
        return handler.true()
    else:
        stream.pushback(value)
        raise (libsinan.sinexceptions.ParseError,
               value + " is not true ")

def parse_false(stream, handler):
    """ Parse a false value """
    value = stream.read(5)
    if value == 'false':
        return handler.false()
    else:
        stream.pushback(value)
        raise (libsinan.sinexceptions.ParseError,
               value + " is not false ")

def parse_null(stream, handler):
    """ Parse a null atom """
    value = stream.read(4)
    if value == 'null':
        return handler.null()
    else:
        stream.pushback(value)
        raise (libsinan.sinexceptions.ParseError,
               value + " is not null ")

def parse_number(stream, handler):
    """ Parse a full number """
    char = stream.read()
    buf = []

    if char == '-':
        buf.append(char)
        char = stream.read()

    while char:
        if (char == '0' or char == '1' or char == '2' or
            char == '3' or char == '4' or char == '5' or
            char == '6' or char == '7' or char == '8' or
            char == '9'):
            buf.append(char)
        elif char == '.':
            buf.append(char)
            parse_number_part(stream, buf)
            break
        else:
            stream.pushback(char)
            break
        char = stream.read()

    number = ''.join(buf)
    try:
        try:
            value = int(number)
            return handler.number(value)
        except ValueError:
            value = float(number)
            return handler.number(value)
    except ValueError:
        raise libsinan.sinexceptions.ParseError, number + " is not a number"

def parse_number_part(stream, buf):
    """ Parse the decimal part of a number """
    char = stream.read()
    while char:
        if (char == '0' or char == '1' or char == '2' or
            char == '3' or char == '4' or char == '5' or
            char == '6' or char == '7' or char == '8' or
            char == '9'):
            buf.append(char)
        elif char == 'e' or char == 'E':
            buf.append(char)
            parse_exponent(stream, buf)
            break
        else:
            stream.pushback(char)
            break
        char = stream.read()

def parse_exponent(stream, buf):
    """ Parse the exponent part of a number """
    char = stream.read()
    if char == '+' or char == '-':
        buf.append(char)
        char = stream.read()

    while char:
        if (char == '0' or char == '1' or char == '2' or
            char == '3' or char == '4' or char == '5' or
            char == '6' or char == '7' or char == '8' or
            char == '9'):
            buf.append(char)
        else:
            stream.pushback(char)
            break
        char = stream.read()

def parse_string(stream, handler = None):
    """ Parse a string """
    buf = []
    char = stream.read()
    START = 1
    BODY = 2
    ESC = 3
    state = START
    while char:
        if char == '"' and state == START:
            state = BODY
        elif char == '\\' and state == BODY:
            state = ESC
        elif char == '"' and state == BODY:
            if handler :
                return handler.string(''.join(buf))
            else:
                return ''.join(buf)
        elif state == ESC:
            if char == 'b':
                buf.append('\b')
            elif char == 'f':
                buf.append('\f')
            elif char == 'n':
                buf.append('\n')
            elif char == 'r':
                buf.append('\r')
            elif char == 't':
                buf.append('\t')
            elif char == 'u':
                tbuf = [stream.read(),
                        stream.read(),
                        stream.read(),
                        stream.read()]
                buf.append(chr(int(''.join(tbuf), 16)))
            else:
                buf.append(char)
            state = BODY
        else:
            buf.append(char)

        char = stream.read()

def parse_key(stream, handler):
    """ Parse the key of an object """
    key = parse_string(stream)
    return handler.key(key)

def parse_object(stream, handler):
    char = 1
    KEY_SEP = 1
    VALUE_SEP = 2
    START = 3
    state = START
    while char:
        discard_whitespace(stream)
        char = stream.read()
        if char == '{' and state == START:
            handler.object_begin()
            discard_whitespace(stream)
            parse_key(stream, handler)
            state = KEY_SEP
        elif char == ':' and state == KEY_SEP:
            parse_value(stream, handler)
            state = VALUE_SEP
        elif char == ',' and state == VALUE_SEP:
            discard_whitespace(stream)
            parse_key(stream, handler)
            state = KEY_SEP
        elif char == '}' and state == VALUE_SEP:
            return handler.object_end()
            break
        else:
            raise libsinan.sinexceptions.ParseError, "unexpected value " + char

def parse_array(stream, handler):
    char = 1
    VALUE_SEP = 2
    START = 3
    state = START
    while char:
        discard_whitespace(stream)
        char = stream.read()
        if char == '[' and state == START:
            handler.array_begin()
            discard_whitespace(stream)
            parse_value(stream, handler)
            state = VALUE_SEP
        elif char == ',' and state == VALUE_SEP:
            discard_whitespace(stream)
            parse_value(stream, handler)
            state = VALUE_SEP
        elif char == ']' and state == VALUE_SEP:
            handler.array_end()
            break
        else:
            raise (libsinan.sinexceptions.ParseError,
                   "unexpected value " + char)

if __name__ == "__main__":
    ftest = "example.txt"
    if len(sys.argv) >= 2:
        ftest = sys.argv[1]

    f = open(ftest, 'r')
    parse(f, TestHandler())
    f.close()


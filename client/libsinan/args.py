import re
from sinexceptions import SinanError

class ParseError(SinanError):
    def __init__(self, value):
        self.value = value


class ParsedArgs:
    "A simple class designed to handle parsed args, and arg parsing"
    KEY = 100
    SPECIAL_VALUE = 150
    VALUE = 200
    START = 300

    VALUE_CHECKER = re.compile("\w+")

    def __init__(self, argv, default_task, special_opts = {}, default_opts = {}):
        self.argv = argv
        self.task = None
        self.default_task = default_task
        self.opt_root = default_opts
        self.current_key = None
        self.current_opt = self.opt_root
        self.special_opts = special_opts

    def parse(self):
        """Parses the argv associated with this task into
        consumable objects """

        self.parse_state = ParsedArgs.START

        for arg in self.argv:
            self.parse_arg(arg)

        if self.parse_state == ParsedArgs.SPECIAL_VALUE:
            self.special_opts[self.special_key] = True

    def parse_task(self, arg):
        """ Just set the task if it doesn't already exist.
        Otherwise error out"""
        if self.task == None:
            self.task = arg
        else:
            raise ParseError("Only expected one task arg, got " + self.task +
                             " and " + arg)

    def parse_key_part(self, start, arg):
        pos = arg.find(":", start)

        if pos == -1:
            self.current_key = arg[start:]
            self.parse_state = ParsedArgs.VALUE
        else:
            key = arg[start:pos]
            Obj = {}
            try:
                Obj = self.current_opt[key]
            except KeyError:
                self.current_opt[key] = Obj

            self.current_opt = Obj
            self.parse_key_part(pos + 1, arg)



    def parse_key(self, arg):
        self.parse_key_part(0, arg)
        self.parse_state = ParsedArgs.VALUE


    def parse_special_key(self, arg):
        """ Parse special local keys, these are keys
        for the client not the server """
        self.special_key = arg
        self.parse_state = ParsedArgs.SPECIAL_VALUE

    def parse_possible_key(self, arg):
        if arg.startswith("--"):
            self.parse_key(arg[2:])
        elif arg.startswith("-"):
            self.parse_key(arg[1:])
        elif arg.startswith("+"):
            self.parse_special_key(arg[1:])
        else:
            self.parse_task(arg)

    def parse_special_value(self, arg):
        if arg.startswith("+") or arg.startswith("-"):
            self.special_opts[self.special_key] = True
            self.special_key = None
            self.parse_state = ParsedArgs.KEY
            self.parse_possible_key(arg)
        else:
            self.parse_state = ParsedArgs.KEY
            self.special_opts[self.special_key] = arg

    def parse_value(self, arg):
        """Parse the value. Really just make sure it doesn't look
        like a key. if it does error out"""
        if arg.startswith("-"):
            raise ParseError("Expecting value, got arg")
        if self.current_key == None:
            raise ParseError("No key specified")

        if ParsedArgs.VALUE_CHECKER.match(arg):
            self.current_opt[self.current_key] = arg
        else:
            self.current_opt[self.current_key] = simplejson.load(arg)

        self.current_opt = self.opt_root
        self.parse_state = ParsedArgs.KEY


    def parse_arg(self, arg):
        """ This is a simple parse state dispatcher,
        all the actual work is done in the various parse methods """

        {ParsedArgs.KEY: self.parse_possible_key,
         ParsedArgs.SPECIAL_VALUE: self.parse_special_value,
         ParsedArgs.VALUE: self.parse_value,
         ParsedArgs.START: self.parse_possible_key} [self.parse_state](arg)


def parse(argv, default_task, special_opts = {}, default_opts = {}):
    """ Init the parsed args and return an arg object representing them

    >>> parse(['--zu:za:zee', 'ahah'], 'ha').opt_root
    {'zu': {'za': {'zee': 'ahah'}}}

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah'], 'ha').opt_root
    {'zu': {'za': {'zook': 'muhahah', 'zee': 'ahah'}}}

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah', '+port', '3322'], 'ha').special_opts
    {'port': '3322'}

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah', '+help', '+port', '3322'], 'ha').special_opts
    {'port': '3322', 'help': True}
    """
    parser = ParsedArgs(argv, default_task, special_opts, default_opts)
    parser.parse()
    return parser



def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()

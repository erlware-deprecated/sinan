import re
from sinexceptions import SinanError

VALUE_CHECKER = re.compile("\w+")

class ParseError(SinanError):
    def __init__(self, value):
        self.value = value


def parse_task(argv, arg, args, index):
    """ Just set the task if it doesn't already exist.
    Otherwise error out"""
    try:
        args['task']
        raise ParseError("Only expected one task arg, got " + self.task +
                         " and " + arg)
    except KeyError:
        args['task'] = arg
        parse_possible_key(argv, args, index + 1)


def parse_special_key(argv, arg, args, index):
    """ Parse special local keys, these are keys
    for the client not the server """
    parse_special_value(argv, arg, args, index + 1)


def parse_special_value(argv, key, args, index):
    if (len(argv) <= index or
        argv[index].startswith("+") or
        argv[index].startswith("-")):
        args['special_opts'][key] = True
        parse_possible_key(argv, args, index)
    else:
        arg = argv[index]
        args['special_opts'][key] = arg
        parse_possible_key(argv, args, index + 1)


def parse_key(argv, arg, start, current_opt, args, index):
    pos = arg.find(":", start)

    if pos == -1:
        parse_value(argv, arg[start:], current_opt, args, index + 1)
    else:
        key = arg[start:pos]
        obj = {}
        try:
            obj = current_opt[key]
        except KeyError:
            current_opt[key] = obj

        parse_key(argv, arg, pos + 1, obj, args, index)


def parse_value(argv, key, opt, args, index):
    """Parse the value. Really just make sure it doesn't look
    like a key. if it does error out"""
    if len(argv) <= index:
        raise ParseError("Expected a value for " + key)

    arg = argv[index]

    if arg.startswith("-"):
        raise ParseError("Expecting value, got arg")

    if VALUE_CHECKER.match(arg):
        opt[key] = arg
    else:
        opt[key] = arg

    parse_possible_key(argv, args, index + 1)


def parse_possible_key(argv, args, index):
    if len(argv) <= index:
        return args

    arg = argv[index]
    if arg.startswith("--"):
        parse_key(argv, arg[2:], 0, args['opts'], args, index)
    elif arg.startswith("-"):
        parse_key(argv, arg[1:], 0, args['opts'], args, index)
    elif arg.startswith("+"):
        parse_special_key(argv, arg[1:], args, index)
    else:
        parse_task(argv, arg, args, index)



def parse(argv, default_task, special_opts = {}, default_opts = {}):
    """ Init the parsed args and return an arg object representing them

    >>> parse(['--zu:za:zee', 'ahah'], 'ha')['opts']
    {'zu': {'za': {'zee': 'ahah'}}}

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah'], 'ha')['opts']
    {'zu': {'za': {'zook': 'muhahah', 'zee': 'ahah'}}}

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah', '+port', '3322'], 'ha')['special_opts']
    {'port': '3322'}

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah', '+help', '+port', '3322'], 'ha')['special_opts']
    {'port': '3322', 'help': True}

    >>> parse(['hobo', '--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah', '+help', '+port', '3322'], 'ha')['task']
    'hobo'

    >>> parse(['--zu:za:zee', 'ahah', '--zu:za:zook', 'muhahah', '+help', '+port', '3322'], 'ha')['task']
    'ha'
    """
    args = {'special_opts': special_opts,
            'opts': default_opts,
            'default_task': default_task}
    parse_possible_key(argv, args, 0)

    try:
        args['task']
    except KeyError:
        args['task'] = default_task

    return args



def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()

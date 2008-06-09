import optparse


def parse_key_value(key, value, args):
    if ':' not in key:
        args[key] = value
        return

    key, rest = key.split(':', 1)
    args = args.setdefault(key, {})
    parse_key_value(rest, value, args)


def parse(argv, default_task, client_opts = {}, server_opts = {}):
    """Parse the argv vector and return an arg dict representing the arguments.

    >>> parse(['build', 'zu:za:zee', 'ahah'], 'ha')['server_opts']
    {'zu': {'za': {'zee': 'ahah'}}}

    >>> parse(['clean', 'zu:za:zee', 'ahah', 'zu:za:zook', 'muhahah'], 'ha')['server_opts']
    {'zu': {'za': {'zook': 'muhahah', 'zee': 'ahah'}}}

    >>> parse(['build', 'zu:za:zee', 'ahah', 'zu:za:zook', 'muhahah', '--url', 'myurl'], 'ha')['client_opts']
    {'url': 'myurl'}

    >>> parse(['--url', 'myurl'], 'ha')['task']
    'ha'
    """
    args = {'client_opts': client_opts.copy(),
            'server_opts': server_opts.copy(),
            'default_task': default_task,
            'task': default_task}

    usage = """usage: %prog [options] [task [server-variable server-value ...]]

Server arguments (pairs of variable and values) are complex. There are
always sane defaults so you shouldn't need them, but you may. To get
information about server arguments read the sinan documentation.
"""

    parser = optparse.OptionParser(usage)

    help = "the directory containing the directory containing the 'erl' binary"
    parser.add_option("-p", "--prefix", type="str", help=help)

    parser.add_option("-u", "--url", type="str",
                      help="the url for the sinan server")

    options, posargs = parser.parse_args(argv)

    for opt in ('prefix', 'url'):
        if getattr(options, opt, None):
            args['client_opts'][opt] = getattr(options, opt)

    if posargs:
        args['task'] = posargs.pop(0)

    while posargs:
        if len(posargs) < 2:
            parser.error('Missing value for %s' % posargs[0])

        key, value, posargs = posargs[0], posargs[1], posargs[2:]

        parse_key_value(key, value, args['server_opts'])

    return args



def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()

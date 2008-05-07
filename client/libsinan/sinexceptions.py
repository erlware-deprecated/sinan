class SinanError(Exception):
    """ A very simple exception class to use as a base
    exception class for the sinan client """

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)


class ParseError(SinanError):
    """ A very simple exception class to use as a base
    exception class for the sinan client """

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)


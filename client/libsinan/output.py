
def handle(task, conn):
    """ Handles output from the server. For the most part this just
    parses the default types of event layout and prints it to standard out
    in special cases it may do something else """
    json = simplejson.load(conn)
    print json

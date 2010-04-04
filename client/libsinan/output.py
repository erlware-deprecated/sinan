import sys
import libsinan.jsax

class SimpleTaskHandler(object):
    def __init__(self):
        self.event_type = None
        self.type = None
        self.desc = None
        self.task = None
        self.fault = None

    def set_event_type(self, value):
        self.event_type = value

    def set_type(self, value):
        self.type = value

    def set_desc(self, value):
        self.desc = value

    def set_task(self, value):
        self.task = value

    def object_begin(self):
        return True

    def key(self, value):
        if value == "event_type":
            self.next = self.set_event_type
        elif value == "type":
            self.next = self.set_type
        elif value == "desc":
            self.next = self.set_desc
        elif value == "task":
            self.next = self.set_task

        return True

    def value_begin(self):
        return True

    def string(self, value):
        self.next(value)
        return True

    def number(self, value):
        self.next(value)
        return True

    def true(self):
        self.next(True)
        return True

    def false(self):
        self.next(False)
        return True

    def null(self):
        self.next(None)
        return True

    def array_begin(self):
        self.array = []
        return True

    def array_end(self):
        self.next(self.array)
        self.array = None
        return True

    def object_end(self):
        status = 0
        """ We only get one object per right now so
        lets print it out when we get it """

        if (self.type == "task_event" and self.desc and
            (self.event_type == "io"
             or self.event_type == "wip")):
            sys.stdout.write(self.desc)
            sys.stdout.flush()
        elif self.type == "task_event" and self.desc:
            print self.desc
        elif self.type == "task_event":
            print "[" + self.task + "]", self.event_type
        elif self.type == "run_event" and self.event_type == "stop" and self.desc:
            print "stopping, " + self.desc
        elif self.type == "run_event" and self.event_type == "fault":
            self.fault = True
            if self.desc:
                print self.desc
            print "run complete with faults"
            status = 4
            sys.exit(4)

        self.event_type = None
        self.type = None
        self.desc = None
        self.task = None
        self.next = None
        return status

    def value_end(self):
        return True

def handle(task, conn):
    """ Handles output from the server. For the most part this just
    parses the default types of event layout and prints it to standard out
    in special cases it may do something else """
    if conn.status == 200:
        try:
            handler = SimpleTaskHandler()
            libsinan.jsax.parse(conn, SimpleTaskHandler())
            if handler.fault:
                return 1
            return 0
        except ValueError, msg:
            print "Got an error back from sinan. Check the logs at ~/.sinan/logs/kernel.log"
    else:
        print conn.read()
        return 1

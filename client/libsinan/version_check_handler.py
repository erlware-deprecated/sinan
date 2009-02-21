import libsinan

from libsinan import handler, output, jsax


class VersionCheckTaskHandler(output.SimpleTaskHandler):
    def __init__(self):
        output.SimpleTaskHandler.__init__(self)
        self.version = None

    def object_end(self):
        """ We only get one object per right now so
        lets print it out when we get it """

        if self.task == "version":
            if self.event_type == 'info':
                self.version = self.desc
            return True
        else:
            return output.SimpleTaskHandler.object_end(self)


class VersionCheckHandler(handler.Handler):

    def handles(self, task):
        return task == "version"

    def handle(self, largs):
        self.do_request(largs, handle)

version = None

def handle(task, conn):
    global version
    if conn.status == 200:
        try:
            task_handler = VersionCheckTaskHandler()
            jsax.parse(conn, task_handler)
            version = task_handler.version
            return 0
        except ValueError, msg:
            print "Got an error back from sinan. Check the logs at ~/.sinan/logs/kernel.log"
    else: 
        return 1

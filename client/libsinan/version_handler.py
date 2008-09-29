import libsinan

from libsinan import handler, output, jsax


class VersionTaskHandler(output.SimpleTaskHandler):

    def object_end(self):
        """ We only get one object per right now so
        lets print it out when we get it """

        if self.task == "version":
            if self.event_type == 'info':
                print self.desc
            return True
        else:
            return output.SimpleTaskHandler.object_end(self)


class VersionHandler(handler.Handler):

    def handles(self, task):
        return task == "version"

    def handle(self, largs):
        self.do_request(largs, handle)


def handle(task, conn):
    if conn.status == 200:
        try:
            jsax.parse(conn, VersionTaskHandler())
            return 0
        except ValueError, msg:
            print "Got an error back from sinan. Check the logs at ~/.sinan/logs/kernel.log"
    else:
        print conn.read()
        return 1

libsinan.add_task_handler(VersionHandler())

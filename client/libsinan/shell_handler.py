import os
import re
import libsinan.handler
import libsinan.output

shell_paths = []
shell_apps = []


class ShellTaskHandler (libsinan.output.SimpleTaskHandler):
    def object_end(self):
        """ We only get one object per right now so
        lets print it out when we get it """

        if self.task == "shell":
            if self.event_type == "app_name":
                shell_apps.append(self.desc)
            elif self.event_type == "app_path":
                shell_paths.append(self.desc)
            return True
        else:
            return libsinan.output.SimpleTaskHandler.object_end(self)



class ShellHandler(libsinan.handler.Handler):
    def handles(self, task):
        return task == "shell"

    def handle(self, largs):
        self.do_request(largs, handle)
        args = ["erl"]
        for path in shell_paths:
            args.append("-pa")
            args.append(path)

        prefix = ""
        try:
            prefix = largs['special_opts']['prefix']
            if not prefix[-1] == '/':
                prefix += '/'
        except KeyError:
            pass

        print "starting shell ..."
        os.execvp(prefix + "bin/erl", args)

def handle(task, conn):
    """ Handles output from the server. For the most part this just
    parses the default types of event layout and prints it to standard out
    in special cases it may do something else """
    if conn.status == 200:
        try:
            libsinan.jsax.parse(conn, libsinan.shell_handler.ShellTaskHandler())
            return 0
        except ValueError, msg:
            print "Got an error back from sinan. Check the logs at ~/.sinan/logs/kernel.log"
    else:
        print conn.read()
        return 1
libsinan.add_task_handler(ShellHandler())

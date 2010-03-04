import os

import libsinan

from libsinan import handler, output, jsax

shell_paths = []
shell_apps = []


class ShellTaskHandler(output.SimpleTaskHandler):
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
            return output.SimpleTaskHandler.object_end(self)



class ShellHandler(handler.Handler):
    def handles(self, task):
        return task == "shell"

    def handle(self, largs):
        self.do_request(largs, handle)
        args = ["erl"]
        for path in shell_paths:
            args.append("-pa")
            args.append(path)

        prefix = ""

        if 'prefix' in largs['client_opts']:
            prefix = largs['client_opts']['prefix']
            if not prefix[-1] == '/':
                prefix += '/'

        args.extend(largs['client_opts'].get('erl_args', []))

        full_path = prefix + "bin/erl"

        if not os.path.exists(full_path):
            print "The erl release needs to be installed to use the shell task"
            return

        print "starting shell ..."
        os.execvp(full_path, args)

def handle(task, conn):
    """ Handles output from the server. For the most part this just
    parses the default types of event layout and prints it to standard out
    in special cases it may do something else """
    if conn.status == 200:
        from libsinan import shell_handler
        try:
            jsax.parse(conn, shell_handler.ShellTaskHandler())
            return 0
        except ValueError, msg:
            print "Got an error back from sinan. Check the logs at ~/.sinan/logs/kernel.log"
    else:
        print conn.read()
        return 1

libsinan.add_task_handler(ShellHandler())

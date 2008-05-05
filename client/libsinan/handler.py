import libsinan.output
import libsinan.encoder
import re
import os
import urllib2

class Handler:
    DEFAULT_VALIDATOR = re.compile('\w+')


    def ask_user(self, prompt, default = None, regexp = DEFAULT_VALIDATOR):
        if default:
            prompt += ' [default ' + default + ']'

        prompt = prompt + "> "
        value = raw_input(prompt)

        if not value and default:
            return default

        while not regexp.match(value):
            print value + " wasn't valid!"
            value = raw_input(prompt)
        return value

    def add_start_dir(self, config):
        """ Add the argument required by many or most of
        the tasks in the system. """
        try:
            config['opts']['build']['start_dir'] = os.getcwd()
        except KeyError:
            config['opts']['build'] = {'start_dir' : os.getcwd()}
        return config

    def handles(self):
        return true


    def jsonify_opts(self, largs):
        """ The opts are already in config layout. All we need to
        do is jsonify them """
        try:
            return libsinan.encoder.dumps(largs['opts'])
        except KeyError:
            return None

    def do_request(self, largs, handle = libsinan.output.handle):
        """ Actually make the task request to the server """
        config = self.jsonify_opts(self.add_start_dir(largs))
        print config
        task = largs['task']
        url = largs['special_opts']['url']
        if url[-1:] == '/':
            url += 'do_task/' + task
        else:
            url += '/do_task/' + task

        conn = urllib2.urlopen(url, config)

        handle(task, conn)

    def handle(self, largs):
        self.do_request(largs)

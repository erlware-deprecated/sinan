import re
import os
import httplib

from libsinan import encoder, output


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
            config['server_opts']['build']['start_dir']
        except KeyError:
            try:
                config['server_opts']['build']['start_dir'] = os.getcwd()
            except KeyError:
                config['server_opts']['build'] = {'start_dir' : os.getcwd()}
        return config

    def handles(self, task):
        return True

    def jsonify_opts(self, largs):
        """ The opts are already in config layout. All we need to
        do is jsonify them """
        try:
            return encoder.dumps(largs['server_opts'])
        except KeyError:
            return None

    def do_request(self, largs, handle = output.handle):
        """ Actually make the task request to the server """
        config = self.jsonify_opts(self.add_start_dir(largs))
        task = largs['task']
        url = largs['client_opts']['url']
        query = '/do_task/' + task

        conn = httplib.HTTPConnection(url)
        headers = {"Content-type": "application/json"}
        conn.request("POST", query, config, headers)
        response = conn.getresponse()
        return handle(task, response)

    def handle(self, largs):
        return self.do_request(largs)

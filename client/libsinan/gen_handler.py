import re
import libsinan.handler

class GenHandler(libsinan.handler.Handler):
    PROJECT_VALIDATOR = re.compile(r'^[a-z][a-zA-Z0-9_]*$')
    APPS_VALIDATOR = re.compile(r'^\s*([a-z][a-zA-Z0-9_@]*)\s*$|^(\s*([a-z][a-zA-Z0-9_@]*)(\s+([a-z][a-zA-Z0-9_@]*))*\s*)$')

    def handles(self, task):
        return task == "gen"

    def valid_repo(self, repo):
        return not repo.strip() == "" and len(repo.split())==1

    def gather_user_info(self):
        print "Please specify your name"
        name = self.ask_user("your name")
        print "Please specify your email address"
        address = self.ask_user('your email')
        print "Please specify the copyright holder"
        copyholder = self.ask_user('copyright holder', name)

        if not copyholder:
            copyholder = name

        return {"username" : name,
                "email_address" : address,
                "copyright_holder" : copyholder}


    def get_application_names(self):
        print ("Please specify the name the OTP app" +
        " that you would like to create within this project.")
        value = self.ask_user('app', None, self.APPS_VALIDATOR)
        values = value.split()

        more = self.ask_user('would you like to enter more y/n', 'n').upper()
        if more == 'Y' or more == 'YES':
            while 1:
                value = self.ask_user('apps', None, self.APPS_VALIDATOR)
                values = values + value.split()
                more = self.ask_user('would you like to enter another y/n',
                                     'n').upper()
                if more == 'N' or more == 'NO':
                    break
        return values



    def get_new_project_info(self):
        print "Please specify name of your project"
        while 1:
            name = self.ask_user('project name', None, self.PROJECT_VALIDATOR)
            if len(name.split())==1:
                break
            print "Error: project name may NOT contain spaces"
        print "Please specify the version of your project"
        version = self.ask_user('project version', '0.1.0.0')

        return {"project_version" : version,
                "project_name" : name}


    def handle(self, largs):
        """ Query the user for information about the user and
        the project he wishes to create before passing that
        information to the user """
        try:
            largs['server_opts']['tasks']['gen']
        except KeyError:
            if not largs['server_opts'].has_key('tasks'):
                largs['server_opts']['tasks'] = {}

            shell_info =  {"user_info" : self.gather_user_info(),
                           "project_info" :
                           self.get_new_project_info(),
                           "apps" : self.get_application_names()}
            largs['server_opts']['tasks']['gen'] = shell_info
        self.do_request(largs)




libsinan.add_task_handler(GenHandler())

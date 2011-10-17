---
layout: default
title: Sinan Release Testing
---

Introduction
------------

The release testing system for sinan drives sinan from the command
line just like a user would. We do this so that we can get complete
end to end testing of all aspects of sinan. This is another level of
testing above UnitTesting and they should not be confused.

The ReleaseTesting system is built in python and based around python's
pexpect module. This is very similar to expect and used in a similar
way. The http://www.noah.org/wiki/pexpect[pexpect module] is well documented and
you should look to that documentation for details on using pexpect.

There are a few things you need to run the release testing
framework. You will need the following.

- http://www.noah.org/wiki/pexpect[pexpect]

To run the tests simple do the following in &lt;sinan-root&gt;.

    $> make smoketests

You may also run individual tests by running the following command.

    $> PYTHONPATH=$PYTHONPATH:./smoketests python smoketests/tests/<test-file>.py

This can be very helpful during development.


Why Python?
-----------

One of the big questions that comes up is why python and not Erlang
for the release testing. The answer to this is fairly simple and comes
from the unix axiom of the right tool for the right job. Python has
the pexpect module which is designed to drive command line
programs. Erlang does not have this module nor modules similar to
this. Not only that, but Python has tighter integration with the
system the Erlang does.

This doesn't make Python better then Erlang or Erlang better then
python, it simply means that for this particular path Python is a
better tool then Erlang. For many other tasks Erlang is a better
tool. It all depends very much on the task at hand.


Test Framework
--------------

The testing framework makes use of the
[Python unittest library](http://docs.python.org/library/unittest.html)
and the nose testing infrastructure, along with a convience module
called sin_testing provided here. Individual test files are expected
to go into the tests directory. Each file is also expected to be
completely self contained, that is one test file may not depend on
other test files. You may, of course, share code between tests, but
that shared code should go into the sin_testing library and then
called from the clients for that code.

sin_testing
-----------

The sin_testing library is designed specificially for supporting the
testing of sinan. To that end it provides many convienience functions
for setting up and driving sinan. Lets go through each one and how
they will help you write tests.

AppDesc Class
--------------

The app desc class is a project descriptor that is used to describe
things that need to be generated and built to the smoketest class. At
the moment it is just a container for values as you can see. The only
thing to be aware of is the app_names. App names is a list of
applications that you would like to be generated in the project.

    {% highlight python %}
    class AppDesc(object):
        def __init__(self,
                     user_name=None,
                     email=None,
                     copyright_holder=None,
                     project_name=None,
                     project_version=None,
                     app_names=None):
            self.user_name = user_name
            self.email = email
            self.copyright_holder = copyright_holder
            self.project_name = project_name
            self.project_version = project_version
            self.app_names = app_names
    {% endhighlight %}


SmokeTest Class
----------------

The smoketest class is a subclass of unittest.TestCase that provides a
few nice features for sinan tests. Setup function creates a new
temporary directory and then changes the current directory to that
directory. The teardown function does the reverse of that procedure.

This class also provides a few helper functions to ensure that files
that you expect to exist exist. These functions are assert_dirs_exist
and assert_files_exist.

The first argument to both of these functions is the base directory for the
checks. The rest of the arguments are directories or files to check for
in the context of the base directory.

    {% highlight python %}
        self.assert_dirs_exist(projdir,
                               "bin",
                               "config",
                               "lib")


        self.assert_files_exist(projdir,
                                ["config", "sys.config"],
                                "sinan.config")
    {% endhighlight %}

In the first example we are checking if a set of directories exist in
the project directory. In the second example we are checking to see if
the files exist. Notice the use of lists here. When these functions
see a list argument the automatically convert them to paths with the
correct seperator for the platform you are running on. This makes it
easy to check large numbers of files or directories. If the file or
directory doesn't exist then an error is asserted.

### do_&lt;task&gt; functions

There are several functions for running standard sinan tasks as
well. These are, do_clean, do_t, do_release, do_dist.

These run the task named at the end in the current directory, with the
exception of the do_t which runs the test task. The do_t is a hack to
get around test autodetection in unittest.


### do_run

The do run function is the thing you will use the most. It will take
an appdesc and run build, test, relaese, and dist. In all cases it
will test the output and make sure it ran correctly. You may then
customize the generated app to your wishes. A good example of this is
the release_test.py file.

### sinan decorator

The sinan decorator allows you to declare that your function runs a
specific sinan task. The decorator spawns the sinan currently in
development passes the spawned process to the function that is being
decorated. For example, the following code lets we want to execute a
test that needs to run sinan gen. We could simply do it as follows.

    {% highlight python %}
    --decorator should go here but maruku sucks--sin_testing.sinan("gen")
    def my_test_function(self, child):
          """ Here is where we would do stuff"""
          pass
    {% endhighlight %}

When the function is called the sinan decorator will run the gen
command, passing the pexpect child object to the function
decorated. The function can then do anything it would like with that
child using the pexpect api.

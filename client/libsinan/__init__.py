""" libsinan provides an interface to the sinan build system """
import libsinan.handler

__all__ = ["args", "output", "sinexceptions", "handler", "gen_handler", "version_handler", "shell_handler", "version_check_handler"]


__taskhandlers__ = []


def add_task_handler(handler):
    __taskhandlers__.append(handler)


def get_handler_for_task(task):
    """ go through the list of handlers. If/when you find a
    handler that will handle a task return it, otherwise just return
    the default """
    for handler in __taskhandlers__:
        if handler.handles(task):
            return handler

    return libsinan.handler.Handler()

from libsinan import shell_handler, gen_handler, version_handler

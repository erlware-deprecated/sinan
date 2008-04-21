""" libsinan provides an interface to the sinan build system """

__all__ = ["args", "output", "sinexceptions"]


__taskhandlers__ = []


def add_task_handler(handler):
    __taskhandlers__.push(handler)

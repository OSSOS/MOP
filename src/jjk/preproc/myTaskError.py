#^L
#+
########################################################################
#
#   Class:      TaskError
#
#   Purpose:
#       creates a subclass of EnvironmentError for TaskError loging.
#       taskrunner knows how to handle task errors
#
#   Arguments:
#       EnvironmentError  : SuperClass exception
#
########################################################################
#-

class TaskError(EnvironmentError):
    "Used to end excution and send taskRunner some info back stream"


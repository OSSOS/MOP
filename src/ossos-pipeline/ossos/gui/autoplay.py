__author__ = "David Rusk <drusk@uvic.ca>"

import threading

from ossos.gui import config


class AutoplayManager(object):
    def __init__(self, model):
        self.model = model

        self.interval = config.read("DISPLAY.AUTOPLAY_INTERVAL")
        self._task_thread = None
        self._running = False

    def start_autoplay(self):
        self._task_thread = AutoplayThread(self.model, self.interval)
        self._task_thread.start()
        self._running = True

    def stop_autoplay(self):
        if self._running:
            self._task_thread.stop()
            self._running = False

    def is_running(self):
        return self._running


class TaskThread(threading.Thread):
    """
    A thread that executes a task at specified intervals.
    """

    def __init__(self, interval):
        """
        Args:
          interval: float
            The interval between task executions in seconds.
        """
        super(TaskThread, self).__init__()

        self._finished = threading.Event()
        self._interval = interval

    def stop(self):
        self._finished.set()

    def run(self):
        while not self._finished.is_set():
            self.do_task()

            # Sleep for interval or until stopped
            self._finished.wait(self._interval)

    def do_task(self):
        """
        The task done by this thread.  Provide functionality by overriding
        in subclasses.
        """
        pass


class AutoplayThread(TaskThread):
    def __init__(self, model, interval):
        super(AutoplayThread, self).__init__(interval)
        self.model = model

    def do_task(self):
        self.model.next_obs()


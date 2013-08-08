__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.fitsviewer.exceptions import MPLViewerError


class InteractionContext(object):
    """
    Very useful reference for matplotlib event handling:
    http://matplotlib.org/users/event_handling.html
    """

    MOUSE_BUTTON_LEFT = 1
    MOUSE_BUTTON_RIGHT = 3

    def __init__(self, displayable):
        self.displayable = displayable
        self._register_event_handlers()

        self.state = CreateCircleState(self)

    def _register_event_handlers(self):
        """
        Connect to start listening for the relevant events.
        """
        self.cidpress = self.displayable.register_mpl_event_handler(
            "button_press_event", self.on_press)
        self.cidrelease = self.displayable.register_mpl_event_handler(
            "button_release_event", self.on_release)
        self.cidmotion = self.displayable.register_mpl_event_handler(
            "motion_notify_event", self.on_motion)

    def on_press(self, event):
        if not self.displayable.is_event_in_axes(event):
            return

        if event.button == InteractionContext.MOUSE_BUTTON_LEFT:
            self.state = self._choose_left_click_state(event)
        elif event.button == InteractionContext.MOUSE_BUTTON_RIGHT:
            self.state = AdjustColormapState(self)
        else:
            # Ignore any other button such as middle click.
            return

        self.state.on_press(event)

    def _choose_left_click_state(self, event):
        circle = self.displayable.circle

        if circle is None:
            in_circle = False
        else:
            in_circle, _ = circle.contains(event)

        if in_circle:
            return MoveCircleState(self)
        else:
            return CreateCircleState(self)

    def on_motion(self, event):
        if not self.displayable.is_event_in_axes(event):
            return

        self.state.on_motion(event)

    def on_release(self, event):
        self.state.on_release(event)
        self.displayable.release_focus()

    def get_circle(self):
        return self.displayable.circle

    def update_circle(self, x, y, radius=None):
        self.displayable.update_circle(x, y, radius)

    def update_colormap(self, dx, dy):
        self.displayable.update_colormap(dx, dy)

    def disconnect(self):
        """Disconnects all the stored connection ids"""
        self.displayable.deregister_mpl_event_handler(self.cidpress)
        self.displayable.deregister_mpl_event_handler(self.cidrelease)
        self.displayable.deregister_mpl_event_handler(self.cidmotion)


class BaseInteractionState(object):
    def __init__(self, context):
        self.context = context
        self._set_blank_state()

    def _set_blank_state(self):
        self.pressed = False
        self.had_drag = False

        self.start_x = None
        self.start_y = None
        self.last_x = None
        self.last_y = None

    def on_press(self, event):
        self.pressed = True

        self.start_x = event.xdata
        self.start_y = event.ydata
        self.last_x = self.start_x
        self.last_y = self.start_y

    def on_motion(self, event):
        if not self.pressed:
            return

        self.had_drag = True
        self.on_drag(event)

        self.last_x = event.xdata
        self.last_y = event.ydata

    def on_drag(self, event):
        """
        Implement to provide state-specific behaviour on motion.
        """
        pass

    def on_release(self, event):
        self._set_blank_state()


class RecenteringState(BaseInteractionState):
    def on_release(self, event):
        if (self.pressed and
                not self.had_drag and
                    self.context.get_circle() is not None):
            self.context.update_circle(self.start_x, self.start_y)

        super(RecenteringState, self).on_release(event)


class MoveCircleState(RecenteringState):
    def __init__(self, context):
        super(MoveCircleState, self).__init__(context)

        if context.get_circle() is None:
            raise MPLViewerError("Can not move a circle if it doesn't exist!")

    def on_drag(self, event):
        center_x, center_y = self.context.get_circle().center

        dx = event.xdata - self.last_x
        dy = event.ydata - self.last_y

        self.context.update_circle(center_x + dx, center_y + dy)


class CreateCircleState(RecenteringState):
    def __init__(self, context):
        super(CreateCircleState, self).__init__(context)

    def on_drag(self, event):
        center_x = float(self.start_x + event.xdata) / 2
        center_y = float(self.start_y + event.ydata) / 2

        radius = max(abs(self.start_x - event.xdata) / 2,
                     abs(self.start_y - event.ydata) / 2)

        self.context.update_circle(center_x, center_y, radius)


class AdjustColormapState(BaseInteractionState):
    def __init__(self, context):
        super(AdjustColormapState, self).__init__(context)

    def on_drag(self, event):
        self.context.update_colormap(event.xdata - self.last_x,
                                     event.ydata - self.last_y)


class Signal(object):
    def __init__(self):
        self._handlers = []

    def connect(self, handler):
        self._handlers.append(handler)

    def disconnect(self, handler):
        self._handlers.remove(handler)

    def fire(self, *args):
        for handler in self._handlers:
            handler(*args)

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, instance_of, equal_to, none
from matplotlib.backend_bases import MouseEvent as MPLMouseEvent
from mock import Mock, patch

from ossos.fitsviewer.displayable import ImageSinglet
from ossos.fitsviewer.interaction import InteractionContext, MoveMarkerState, CreateMarkerState, AdjustColormapState


class InteractionTest(unittest.TestCase):
    def setUp(self):
        mainhdu = Mock()
        mainhdu.data.shape = (100, 100)
        self.hdulist = [mainhdu]
        self.figure = Mock()

        with patch.object(ImageSinglet, "_create_axes"):
            self.image = ImageSinglet(self.hdulist, self.figure, [0, 0, 1, 1])

        self.image.axes = Mock()

        self.interaction_context = InteractionContext(self.image)

    def _create_mouse_event(self, x, y, button, inaxes=True):
        event = Mock(spec=MPLMouseEvent)
        event.x = x
        event.xdata = x
        event.y = y
        event.ydata = y
        event.button = button

        if inaxes:
            event.inaxes = self.image.axes
        else:
            event.inaxes = Mock()  # a new, different axes

        return event

    def fire_press_event(self, x, y, button=InteractionContext.MOUSE_BUTTON_LEFT,
                         inaxes=True):
        self.interaction_context.on_press(
            self._create_mouse_event(x, y, button, inaxes))

    def fire_release_event(self, button=InteractionContext.MOUSE_BUTTON_LEFT):
        event = Mock(spec=MPLMouseEvent)
        event.button = button
        self.interaction_context.on_release(event)

    def fire_motion_event(self, x, y, inaxes=True):
        self.interaction_context.on_motion(
            self._create_mouse_event(x, y, inaxes))

    def test_state_click_in_circle(self):
        x = 10
        y = 10
        radius = 5

        self.image.place_marker(x, y, radius)
        self.fire_press_event(x + 2, y + 2)
        assert_that(self.interaction_context.state, instance_of(MoveMarkerState))

    def test_press_release(self):
        x = 10
        y = 10
        radius = 5

        self.image.place_marker(x, y, radius)
        assert_that(not self.interaction_context.state.pressed)
        self.fire_press_event(x + 2, y + 2)
        assert_that(self.interaction_context.state.pressed)
        self.fire_release_event()
        assert_that(not self.interaction_context.state.pressed)

    def test_state_click_outside_marker(self):
        x = 10
        y = 10
        radius = 5

        self.image.place_marker(x, y, radius)
        self.fire_press_event(x + 2, y + 2)
        assert_that(self.interaction_context.state, instance_of(MoveMarkerState))
        self.fire_release_event()
        assert_that(self.interaction_context.state, instance_of(MoveMarkerState))
        self.fire_press_event(x + 6, y + 6)
        assert_that(self.interaction_context.state, instance_of(CreateMarkerState))

    def test_state_right_click(self):
        x = 10
        y = 10

        self.fire_press_event(x, y, button=InteractionContext.MOUSE_BUTTON_LEFT)
        assert_that(self.interaction_context.state, instance_of(CreateMarkerState))
        self.fire_release_event(button=InteractionContext.MOUSE_BUTTON_LEFT)

        self.fire_press_event(x, y, button=InteractionContext.MOUSE_BUTTON_RIGHT)
        assert_that(self.interaction_context.state, instance_of(AdjustColormapState))
        self.fire_release_event(button=InteractionContext.MOUSE_BUTTON_RIGHT)

        self.fire_press_event(x, y, button=InteractionContext.MOUSE_BUTTON_LEFT)
        assert_that(self.interaction_context.state, instance_of(CreateMarkerState))
        self.fire_release_event(button=InteractionContext.MOUSE_BUTTON_LEFT)

    def test_drag_marker(self):
        x0 = 10
        y0 = 10
        radius = 5

        xclick = x0 + 2
        yclick = y0 + 2
        dx = 10
        dy = 5

        self.image.place_marker(x0, y0, radius)
        assert_that(self.interaction_context.get_marker().center, equal_to((x0, y0)))
        self.fire_press_event(xclick, yclick)

        self.fire_motion_event(xclick + dx, yclick + dy)
        assert_that(self.interaction_context.get_marker().center,
                    equal_to((x0 + dx, y0 + dy)))
        assert_that(self.interaction_context.get_marker().radius, equal_to(radius))

    def test_create_marker(self):
        x0 = 10
        y0 = 10
        dx = 10
        dy = 30

        assert_that(self.interaction_context.get_marker(), none())
        self.fire_press_event(x0, y0)
        self.fire_motion_event(x0 + dx, y0 + dy)
        assert_that(self.interaction_context.get_marker().center,
                    equal_to((15, 25)))
        assert_that(self.interaction_context.get_marker().radius, equal_to(15))

    def test_motion_not_pressed(self):
        x = 10
        y = 10
        radius = 5

        self.image.place_marker(x, y, radius)

        self.interaction_context.state = CreateMarkerState(self.interaction_context)
        self.fire_motion_event(x + 2, y + 2)
        assert_that(self.interaction_context.get_marker().center, equal_to((x, y)))
        assert_that(self.interaction_context.get_marker().radius, equal_to(radius))

        self.interaction_context.state = MoveMarkerState(self.interaction_context)
        self.fire_motion_event(x + 2, y + 2)
        assert_that(self.interaction_context.get_marker().center, equal_to((x, y)))
        assert_that(self.interaction_context.get_marker().radius, equal_to(radius))

    def test_click_no_drag_inside_marker(self):
        x = 10
        y = 10
        radius = 5

        self.image.place_marker(x, y, radius)

        click_x = 12
        click_y = 13
        self.fire_press_event(click_x, click_y)
        self.fire_release_event()

        assert_that(self.interaction_context.get_marker().center,
                    equal_to((click_x, click_y)))

    def test_click_no_drag_outside_marker(self):
        x = 10
        y = 10
        radius = 5

        self.image.place_marker(x, y, radius)

        click_x = 20
        click_y = 21
        self.fire_press_event(click_x, click_y)
        self.fire_release_event()

        assert_that(self.interaction_context.get_marker().center,
                    equal_to((click_x, click_y)))

    def test_xy_changed_event_on_click(self):
        handler = Mock()
        self.image.xy_changed.connect(handler)

        self.image.place_marker(10, 10, 5)

        x_click = 20
        y_click = 30
        self.fire_press_event(x_click, y_click)
        self.fire_release_event()

        handler.assert_called_once_with(x_click, y_click)

    def test_xy_changed_event_on_drag(self):
        handler = Mock()
        self.image.xy_changed.connect(handler)

        x0 = 10
        y0 = 10
        radius = 5
        self.image.place_marker(x0, y0, radius)

        xclick = x0 + 2
        yclick = y0 + 2
        dx = 10
        dy = 20
        self.fire_press_event(xclick, yclick)
        self.fire_motion_event(xclick + dx, yclick + dy)

        handler.assert_called_once_with(x0 + dx, y0 + dy)


if __name__ == '__main__':
    unittest.main()

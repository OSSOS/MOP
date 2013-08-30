__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock

from tests.base_tests import WxWidgetTestCase
from tests.testutil import mock_hdulist
from ossos.astrom import SourceReading
from ossos.downloads.cutouts.source import SourceCutout
from ossos.fitsviewer.singletviewer import SingletViewer


class SingletViewerTest(WxWidgetTestCase):
    def setUp(self):
        super(SingletViewerTest, self).setUp()

        self.canvas = Mock()
        self.viewer = SingletViewer(self.rootframe, self.canvas)
        self.do_render = Mock()
        self.viewer._do_render = self.do_render

    def test_mark_source(self):
        x, y = 50, 100
        fwhm = 4

        cutout = Mock(spec=SourceCutout)
        reading = Mock(spec=SourceReading)
        reading.null_observation = Mock(return_value=False)
        cutout.hdulist = mock_hdulist()
        cutout.pixel_source_point = x, y
        cutout.astrom_header = {"FWHM": fwhm}
        reading.from_input_file = False
        cutout.reading = reading

        self.viewer.display(cutout, mark_source=False)

        current_displayable = self.viewer.current_displayable
        mock_place_marker = Mock()
        current_displayable.place_marker = mock_place_marker

        self.viewer.mark_sources(cutout)
        mock_place_marker.assert_called_once_with(x, y, 2 * fwhm,
                                                  colour="b")

    def test_refresh_marker(self):
        cutout = Mock(spec=SourceCutout)
        cutout.hdulist = mock_hdulist()

        mark_source = Mock()
        self.viewer.mark_sources = mark_source

        self.viewer.display(cutout, mark_source=False)

        self.viewer.refresh_markers()
        mark_source.assert_called_once_with(cutout)

    def test_toggle_reticule(self):
        cutout = Mock(spec=SourceCutout)
        cutout.hdulist = mock_hdulist()

        mark_source = Mock()
        self.viewer.mark_sources = mark_source

        self.viewer.display(cutout)

        toggle_reticule = Mock()
        current_displayable = self.viewer.current_displayable
        current_displayable.toggle_reticule = toggle_reticule

        self.viewer.toggle_reticule()

        toggle_reticule.assert_called_once_with()


if __name__ == '__main__':
    unittest.main()

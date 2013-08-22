__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock, MagicMock

from tests.base_tests import WxWidgetTestCase
from tests.testutil import mock_hdulist
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
        cutout.hdulist = mock_hdulist()
        cutout.pixel_source_point = x, y
        cutout.astrom_header = {"FWHM": fwhm}

        self.viewer.display(cutout, mark_source=False)

        current_displayable = self.viewer.current_displayable
        mock_place_marker = Mock()
        current_displayable.place_marker = mock_place_marker

        self.viewer.mark_source(cutout)
        mock_place_marker.assert_called_once_with(x, y, 2 * fwhm)

    def test_refresh_marker(self):
        cutout = Mock(spec=SourceCutout)
        cutout.hdulist = mock_hdulist()

        mark_source = Mock()
        self.viewer.mark_source = mark_source

        self.viewer.display(cutout, mark_source=False)

        self.viewer.refresh_markers()
        mark_source.assert_called_once_with(cutout)


if __name__ == '__main__':
    unittest.main()

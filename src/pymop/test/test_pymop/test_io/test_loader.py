__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import Mock

from pymop.io.loader import AsynchronousImageLoader


class AsynchronousImageLoaderTest(unittest.TestCase):
    def setUp(self):
        self.resolver = Mock()
        self.image_retriever = Mock()
        self.image_retriever.retrieve_image.return_value = (Mock(), Mock())

        self.loader = AsynchronousImageLoader(self.resolver, self.image_retriever)

    def mock_astrom_data(self, sources, observations):
        astrom_data = Mock()

        reading = Mock()
        reading.obs = Mock()
        source = [reading] * observations
        astrom_data.sources = [source] * sources

        return astrom_data

    def test_do_load(self):
        sources = 3
        observations = 2
        astrom_data = self.mock_astrom_data(sources, observations)

        resolved_uris = ["URI%d" % i for i in range(sources * observations)]
        self.resolver.resolve_uri.side_effect = resolved_uris

        self.loader.do_loading = Mock()

        self.loader.start_loading(astrom_data)

        self.loader.do_loading.assert_called_with([
            ("URI0", astrom_data.sources[0][0], 0, 0),
            ("URI1", astrom_data.sources[0][1], 0, 1),
            ("URI2", astrom_data.sources[1][0], 1, 0),
            ("URI3", astrom_data.sources[1][1], 1, 1),
            ("URI4", astrom_data.sources[2][0], 2, 0),
            ("URI5", astrom_data.sources[2][1], 2, 1),
        ])


if __name__ == '__main__':
    unittest.main()

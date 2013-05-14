import unittest

from mock import Mock
from hamcrest import assert_that, equal_to

from mopgui.data_retrieval.loader import ProgressiveImageLoader


class LoaderTest(unittest.TestCase):
    def setUp(self):
        # Set up mocked data with 2 sources, each with 3 observations
        self.astrom_data = Mock()
        source = Mock()
        reading = Mock()
        reading.obs = Mock()
        source = [reading] * 3
        self.astrom_data.sources = [source] * 2

        self.resolver = Mock()
        self.image_retriever = Mock()
        self.image_retriever.retrieve_image.return_value = (Mock(), Mock())

        self.loader = ProgressiveImageLoader(self.resolver, self.image_retriever)

    def test_loading_callback(self):
        callback = Mock()
        self.loader.start_loading(self.astrom_data, callback)

        assert_that(callback.call_count, equal_to(6))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, equal_to, close_to

from ossos.downloads.cutouts.calculator import CutoutCalculator, CoordinateConverter


class CutoutCalculatorTest(unittest.TestCase):
    def setUp(self):
        self.imgsize = (2000, 2000)

    def test_calc_cutout_internal(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((500, 600), self.imgsize)
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_calc_cutout_internal_str(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((500, 600), self.imgsize)
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_calc_cutout_internal_str_float(self):
        self.calculator = CutoutCalculator(100, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((500.00, 600.00), self.imgsize)
        assert_that(x0, equal_to(400))
        assert_that(x1, equal_to(600))
        assert_that(y0, equal_to(550))
        assert_that(y1, equal_to(650))

    def test_build_cutout_str(self):
        self.calculator = CutoutCalculator(100, 200)

        cutout_str, _ = self.calculator.build_cutout_str(15, (500, 600), self.imgsize)
        assert_that(cutout_str, equal_to("[15][400:600,550:650]"))

    def test_calc_cutout_internal_converter(self):
        self.calculator = CutoutCalculator(100, 200)

        _, converter = self.calculator.calc_cutout((500, 600), self.imgsize)
        assert_that(converter.convert((400, 550)), equal_to((1, 1)))
        assert_that(converter.convert((600, 550)), equal_to((201, 1)))
        assert_that(converter.convert((400, 650)), equal_to((1, 101)))
        assert_that(converter.convert((600, 650)), equal_to((201, 101)))
        assert_that(converter.convert((500, 600)), equal_to((101, 51)))

    def test_build_cutout_str_converter(self):
        self.calculator = CutoutCalculator(100, 200)

        _, converter = self.calculator.build_cutout_str(15, (500, 600), self.imgsize)
        assert_that(converter.convert((400, 550)), equal_to((1, 1)))
        assert_that(converter.convert((600, 550)), equal_to((201, 1)))
        assert_that(converter.convert((400, 650)), equal_to((1, 101)))
        assert_that(converter.convert((600, 650)), equal_to((201, 101)))
        assert_that(converter.convert((500, 600)), equal_to((101, 51)))

    def test_calc_cutout_boundary_x(self):
        self.calculator = CutoutCalculator(200, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((50, 400), self.imgsize)
        assert_that(x0, equal_to(1))
        assert_that(x1, equal_to(201))
        assert_that(y0, equal_to(300))
        assert_that(y1, equal_to(500))

    def test_calc_cutout_boundary_y(self):
        self.calculator = CutoutCalculator(200, 200)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((400, 50), self.imgsize)
        assert_that(x0, equal_to(300))
        assert_that(x1, equal_to(500))
        assert_that(y0, equal_to(1))
        assert_that(y1, equal_to(201))

    def test_calc_cutout_boundary_x_converter(self):
        self.calculator = CutoutCalculator(200, 200)

        _, converter = self.calculator.build_cutout_str(15, (50, 400), self.imgsize)
        assert_that(converter.convert((50, 400)), equal_to((50, 101)))
        assert_that(converter.convert((1, 300)), equal_to((1, 1)))

    def test_calc_cutout_boundary_x_converter(self):
        self.calculator = CutoutCalculator(200, 200)

        _, converter = self.calculator.build_cutout_str(15, (400, 50), self.imgsize)
        assert_that(converter.convert((400, 50)), equal_to((101, 50)))
        assert_that(converter.convert((300, 1)), equal_to((1, 1)))

    def test_calc_cutout_boundary_xmax(self):
        self.imgsize = (200, 200)
        self.calculator = CutoutCalculator(100, 100)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((175, 100), self.imgsize)
        assert_that(x0, equal_to(100))
        assert_that(x1, equal_to(200))
        assert_that(y0, equal_to(50))
        assert_that(y1, equal_to(150))

    def test_calc_cutout_boundary_ymax(self):
        self.imgsize = (200, 200)
        self.calculator = CutoutCalculator(100, 100)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((100, 175), self.imgsize)
        assert_that(x0, equal_to(50))
        assert_that(x1, equal_to(150))
        assert_that(y0, equal_to(100))
        assert_that(y1, equal_to(200))

    def test_calc_cutout_boundary_xmax_converter(self):
        self.imgsize = (200, 200)
        self.calculator = CutoutCalculator(100, 100)

        _, converter = self.calculator.calc_cutout((175, 100), self.imgsize)
        assert_that(converter.convert((175, 100)), equal_to((76, 51)))
        assert_that(converter.convert((100, 50)), equal_to((1, 1)))
        assert_that(converter.convert((200, 150)), equal_to((101, 101)))

    def test_calc_cutout_boundary_xmin(self):
        self.imgsize = (200, 200)
        self.calculator = CutoutCalculator(100, 100)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((25, 100), self.imgsize)
        assert_that(x0, equal_to(1))
        assert_that(x1, equal_to(101))
        assert_that(y0, equal_to(50))
        assert_that(y1, equal_to(150))

    def test_calc_cutout_boundary_xmin_converter(self):
        self.imgsize = (200, 200)
        self.calculator = CutoutCalculator(100, 100)

        _, converter = self.calculator.calc_cutout((25, 100), self.imgsize)
        assert_that(converter.convert((25, 100)), equal_to((25, 51)))

    def test_calc_cutout_boundary_ymax_converter(self):
        self.imgsize = (200, 200)
        self.calculator = CutoutCalculator(100, 100)

        _, converter = self.calculator.calc_cutout((100, 175), self.imgsize)
        assert_that(converter.convert((100, 175)), equal_to((51, 76)))
        assert_that(converter.convert((50, 100)), equal_to((1, 1)))
        assert_that(converter.convert((150, 200)), equal_to((101, 101)))

    def test_calc_cutout_inverted(self):
        self.calculator = CutoutCalculator(20, 20)

        (x0, x1, y0, y1), _ = self.calculator.calc_cutout((20, 20), (200, 200),
                                                          inverted=True)
        assert_that(x0, equal_to(190))
        assert_that(x1, equal_to(170))
        assert_that(y0, equal_to(190))
        assert_that(y1, equal_to(170))

    def test_calc_cutout_inverted_converter(self):
        self.calculator = CutoutCalculator(20, 20)

        _, converter = self.calculator.calc_cutout((20, 20), (200, 200),
                                                   inverted=True)
        assert_that(converter.convert((10, 10)), equal_to((0, 0)))
        assert_that(converter.convert((30, 10)), equal_to((20, 0)))
        assert_that(converter.convert((10, 30)), equal_to((0, 20)))
        assert_that(converter.convert((30, 30)), equal_to((20, 20)))
        assert_that(converter.convert((20, 20)), equal_to((10, 10)))

    def test_build_cutout_str_inverted(self):
        self.calculator = CutoutCalculator(20, 20)

        cutout_str, _ = self.calculator.build_cutout_str("10", (20, 20), (200, 200),
                                                         inverted=True)
        assert_that(cutout_str, equal_to("[10][190:170,190:170]"))

    def test_cutout_near_edge(self):
        calculator = CutoutCalculator(250, 250)

        (x0, x1, y0, y1), _ = calculator.calc_cutout(
            (1970.17, 4611.65), (2112, 4644), inverted=False)

        assert_that(x0, equal_to(1845))
        assert_that(x1, equal_to(2095))
        assert_that(y0, equal_to(4394))
        assert_that(y1, equal_to(4644))

    def test_converter_near_edge(self):
        calculator = CutoutCalculator(250, 250)

        _, converter = calculator.calc_cutout(
            (1970.17, 4611.65), (2112, 4644), inverted=False)

        x, y = converter.convert((1970.17, 4611.65))

        delta = 0.01
        # note 1-based indexing not 0
        assert_that(x, close_to(126.17, delta))
        assert_that(y, close_to(218.65, delta))

    def test_cutout_over_two_edges(self):
        calculator = CutoutCalculator(250, 250)

        (x0, x1, y0, y1), _ = calculator.calc_cutout(
            (1997.68, 4618.31), (2112, 4644), inverted=False)

        delta = 0.01
        assert_that(x0, close_to(1862, delta))
        assert_that(x1, close_to(2112, delta))
        assert_that(y0, close_to(4394, delta))
        assert_that(y1, close_to(4644, delta))

    def test_inverse_converter(self):
        original_coords_x = 1500
        original_coords_y = 2000

        converter = CoordinateConverter(1000, 500)

        new_coords_x, new_coords_y = converter.convert(
            (original_coords_x, original_coords_y))

        assert_that(new_coords_x, equal_to(500))
        assert_that(new_coords_y, equal_to(1500))

        inverse_converter = converter.get_inverse_converter

        final_x, final_y = inverse_converter.convert(
            (new_coords_x, new_coords_y))

        assert_that(final_x, equal_to(original_coords_x))
        assert_that(final_y, equal_to(original_coords_y))


if __name__ == '__main__':
    unittest.main()

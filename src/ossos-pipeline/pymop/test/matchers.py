__author__ = "David Rusk <drusk@uvic.ca>"

import math

from hamcrest.core.base_matcher import BaseMatcher


def round_sigfigs(x, sigfigs):
    return round(x, -int(math.floor(math.log10(math.fabs(x)))) + (sigfigs - 1))


class IsAlmostEqualSigFigs(BaseMatcher):
    def __init__(self, expected, sigfigs):
        self.expected = expected
        self.sigfigs = sigfigs

    def _matches(self, actual):
        return (round_sigfigs(self.expected, self.sigfigs) ==
                round_sigfigs(actual, self.sigfigs))

    def describe_to(self, description):
        description.append_text(
            "expected: %f to %d "
            "significant figures" % (self.expected, self.sigfigs))


def almost_equal(expected, sigfigs):
    return IsAlmostEqualSigFigs(expected, sigfigs)

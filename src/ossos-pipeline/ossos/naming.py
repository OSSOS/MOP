__author__ = "David Rusk <drusk@uvic.ca>"

ALPHABET_BASE_26 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET_BASE_36 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET_BASE_52 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
ALPHABET_BASE_62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


def base26encode(number):
    return encode(number, ALPHABET_BASE_26)


def base36encode(number):
    return encode(number, ALPHABET_BASE_36)


def base36decode(number):
    return decode(number, 36)


def base52encode(number):
    return encode(number, ALPHABET_BASE_52)


def base62encode(number):
    """
    Converts an integer to a base62 string (i.e. using the upper and lower
    case alphabet and the digits 0-9).
    """
    return encode(number, ALPHABET_BASE_36)


def encode(number, alphabet):
    """
    Converts an integer to a base n string where n is the length of the
    provided alphabet.

    Modified from http://en.wikipedia.org/wiki/Base_36
    """
    if not isinstance(number, (int, long)):
        raise TypeError("Number must be an integer.")

    base_n = ""
    sign = ""

    if number < 0:
        sign = "-"
        number = -number

    if 0 <= number < len(alphabet):
        return sign + alphabet[number]

    while number != 0:
        number, i = divmod(number, len(alphabet))
        base_n = alphabet[i] + base_n

    return sign + base_n


def decode(number, base):
    return int(number, base)


class ProvisionalNameGenerator(object):
    """
    Creates provisional names for a new sources.

    These names must be stored in a 7-character packed form:
    http://www.minorplanetcenter.net/iau/info/PackedDes.html
    """

    def name_source(self, source):
        """
        Generates a name for a source.
        """
        reading0 = source.get_reading(0)
        name = (base52encode(int(reading0.get_exposure_number())) +
                base62encode(int(reading0.x + reading0.y)))

        # Pad with 0's if short
        name = name.ljust(7, "0")

        if len(name) > 7:
            raise ValueError("Name is more than 7 characters: %s" % name)

        return name

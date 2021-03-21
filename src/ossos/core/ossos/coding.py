__author__ = "David Rusk <drusk@uvic.ca>"

ALPHABET_BASE_26 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET_BASE_36 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET_BASE_52 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
ALPHABET_BASE_62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


def base36encode(number, pad_length=0):
    return encode(number, ALPHABET_BASE_36).rjust(pad_length, "0")


def base36decode(number):
    return decode(number, 36)


def encode(number, alphabet):
    """
    Converts an integer to a base n string where n is the length of the
    provided alphabet.

    Modified from http://en.wikipedia.org/wiki/Base_36
    """
    if not isinstance(number, int):
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

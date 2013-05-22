__author__ = "David Rusk <drusk@uvic.ca>"


def get_order(pv):
    """
    Determine the order of a PV matrix.

    Args:
      pv: 2d array
        PV coefficients.

    Returns:
      order: int
        1 for linear, 2
    """
    if len(pv) != 2:
        raise ValueError("PV matrix must have 2 rows, but had: %d" % len(pv))

    # TODO should check all rows same length and throw exception if not
    row_len = len(pv[0])

    if row_len == 3:
        return 1
    elif row_len == 6:
        return 2
    elif row_len == 10:
        return 3
    else:
        raise ValueError("PV matrix has unknown order: "
                         "%d coefficients per row" % row_len)


def xy2sky(x, y, crpix1, crpix2, cd, pv, crval1, crval2):
    """
    Transforms from pixel coordinates to celestial coordinates taking
    non-linear distortion into account with the World Coordinate System
    FITS keywords as used in MegaPipe.

    See: http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/megapipe/docs/CD_PV_keywords.pdf

    Args:
      x, y: int
        Input pixel coordinate
      crpix1, crpix2: int
        Linear offset of WCS
      cd: 2d array
        Expresses the scale, the rotation and any possible skew of the image
        with respect to the sky.

    Returns:
      ra: float
        Right ascension
      dec: float
        Declination
    """
    xp = x - crpix1
    yp = y - crpix2

    x_deg = cd[1][1] * xp + cd[1][2] * yp
    y_deg = cd[2][1] * xp + cd[2][2] * yp

    order = get_order(pv)

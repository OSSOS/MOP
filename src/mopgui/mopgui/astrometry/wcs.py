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

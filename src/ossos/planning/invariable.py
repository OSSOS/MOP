import numpy

secrad = numpy.pi / 180.0 / 3600.0


def convert(lat, lon):
    """convert lat/lon from the ecliptic to the invariable plane."""

    x = numpy.cos(lon) * numpy.cos(lat)
    y = numpy.sin(lon) * numpy.cos(lat)
    z = numpy.sin(lat)

    # Invariable plane: values in arcseconds.
    epsilon = 5713.86
    omega = 387390.8

    coseps = numpy.cos(epsilon * secrad)
    sineps = numpy.sin(epsilon * secrad)
    cosom = numpy.cos(omega * secrad)
    sinom = numpy.sin(omega * secrad)

    xi = x * cosom + y * sinom
    yi = coseps * (-sinom * x + cosom * y) + sineps * z
    zi = - sineps * (-sinom * x + cosom * y) + coseps * z

    lat = numpy.arcsin(zi)
    lon = numpy.arctan2(yi, xi)
    return (lat, lon)


def trevonc(lat, lon, chiangchoi_plane=False):
    """convert lat/lon from the invariable to the ecliptic plane."""

    x = numpy.cos(lon) * numpy.cos(lat)
    y = numpy.sin(lon) * numpy.cos(lat)
    z = numpy.sin(lat)

    # default: the invariable plane. Values in arcseconds.
    # epsilon is the generalised descriptor of inclination
    # omega is the longitude of the ascending node
    epsilon = 5713.86
    omega = 387390.8
    if chiangchoi_plane is True:
        # an amplitude w.r.t. the ecliptic of 1.8 deg and nodal longitude of about 90 deg
        epsilon = 1.8 * 3600
        omega = 90. * 3600

    coseps = numpy.cos(epsilon * secrad)
    sineps = numpy.sin(epsilon * secrad)
    cosom = numpy.cos(omega * secrad)
    sinom = numpy.sin(omega * secrad)

    xi = cosom * x - sinom * (coseps * y - sineps * z)
    yi = sinom * x + cosom * (coseps * y - sineps * z)
    zi = sineps * y + coseps * z

    lat = numpy.arcsin(zi)
    lon = numpy.arctan2(yi, xi)
    return (lat, lon)

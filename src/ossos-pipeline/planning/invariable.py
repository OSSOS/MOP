import scipy
import numpy
def convert(lat,lon):
      """convert lat/lon from the ecliptic to the invariable plane."""

      x = numpy.cos(lon)*numpy.cos(lat)
      y = numpy.sin(lon)*numpy.cos(lat)
      z = numpy.sin(lat)

      epsilon = 5713.86
      omega = 387390.8
      Pi = 3.141592653589793238
      secrad = Pi/180.0/3600.0

      coseps = numpy.cos(epsilon*secrad)
      sineps = numpy.sin(epsilon*secrad)
      cosom = numpy.cos(omega*secrad)
      sinom = numpy.sin(omega*secrad)

      xi =   x*cosom + y*sinom
      yi =   coseps*(-sinom*x + cosom*y) + sineps*z
      zi = - sineps*(-sinom*x + cosom*y) + coseps*z
     
      lat = numpy.arcsin(zi)
      lon = numpy.arctan2(yi,xi)
      return (lat,lon)

def trevonc(lat,lon):
      """convert lat/lon from the invariable to the ecliptic plane."""

      x = numpy.cos(lon)*numpy.cos(lat)
      y = numpy.sin(lon)*numpy.cos(lat)
      z = numpy.sin(lat)

      epsilon = 5713.86
      omega = 387390.8
      Pi = 3.141592653589793238
      secrad = Pi/180.0/3600.0

      coseps = numpy.cos(epsilon*secrad)
      sineps = numpy.sin(epsilon*secrad)
      cosom = numpy.cos(omega*secrad)
      sinom = numpy.sin(omega*secrad)

      xi =   cosom*x - sinom*(coseps*y - sineps*z)
      yi =   sinom*x + cosom*(coseps*y - sineps*z)
      zi =   sineps*y + coseps*z 
     
      lat = numpy.arcsin(zi)
      lon = numpy.arctan2(yi,xi)
      return (lat,lon)


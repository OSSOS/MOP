# horizons.py
# Query JPL Horizons via the web service
# Wes Fraser, 2010
# rewritten and documented by Michele Bannister, Dec 2010
# rewritten again by MB, Jan 2015

import urllib2 as url
import time
import io

import pandas as pd


'''
Ephemeris parameters:
1. Astrometric RA & DEC  15. Sun sub-long & sub-lat  29. Constellation ID
 *2. Apparent RA & DEC     16. Sub Sun Pos. Ang & Dis  30. Delta-T (CT - UT)
  3.   Rates; RA & DEC     17. N. Pole Pos. Ang & Dis *31. Obs eclip. lon & lat
 *4. Apparent AZ & EL      18. Helio eclip. lon & lat  32. North pole RA & DEC
  5.   Rates; AZ & EL      19. Helio range & rng rate  33. Galactic latitude
  6. Sat. X & Y, pos. ang  20. Obsrv range & rng rate  34. Local app. SOLAR time
  7. Local app. sid. time  21. One-Way Light-Time      35. Earth->Site lt-time
  8. Airmass               22. Speed wrt Sun & obsrvr >36. RA & DEC uncertainty
  9. Vis mag. & Surf Brt   23. Sun-Obs-Targ ELONG ang >37. POS error ellipse
 10. Illuminated fraction  24. Sun-Targ-Obs PHASE ang >38. POS uncertainty (RSS)
 11. Defect of illumin.    25. Targ-Obsrv-Moon/Illum% >39. Range & Rng-rate sig.
 12. Sat. angle separ/vis  26. Obs-Primary-Targ angl  >40. Doppler/delay sigmas
 13. Target angular diam.  27. Pos. Ang;radius & -vel  41. True anomaly angle
 14. Obs sub-lng & sub-lat 28. Orbit plane angle       42. Local app. hour angle
'''

# PARAMS = OrderedDict([('RA-DEC', 1),                    # Units: HMS (HH MM SS.ff) and DMS (DD MM SS.f)
# ('RA_rate-DEC_rate', 3),          # d(RA)/dt is multiplied by the cosine of the declination.
# # Units: ARCSECONDS PER HOUR
#                       ('v_mag-surface_brightness', 9),  # Units: None and VISUAL MAGNITUDES PER SQUARE ARCSECOND
#                       ('r_helio-r_helio_rate', 19),
#                       ('RA_uncert-DEC_uncert', 36),     # RA and DEc uncertainties are 3-sigma. Units: ARCSECONDS
# ])

def parse_orbital_elements(urlData):
    # Get required information back out of the page Horizons returns
    # Extract the orbital elements and epoch of origin - works as long as the name parses, even if the ephemeris
    # request fails
    epoch_possible = False
    for i in range(len(urlData)):
        S = urlData[i].split()
        if len(S) > 0:
            if S[0] == 'EPOCH=':  # moons don't have epochs so this can't be required
                epoch_possible = True
                epochStr = "/".join(S[3].split('-'))
                S = urlData[i + 1].split('=')
                e = float(S[1].split(' QR')[0])
                S = urlData[i + 2].split('=')
                Omega = float(S[1].strip(' W'))
                W = float(S[2].strip(' IN'))
                inc = float(S[3])
                S = urlData[i + 3].split('=')
                a = float(S[1].strip(' MA'))
                M = float(S[2].strip(' ADIST'))
                break

    if epoch_possible:
        return {'a': a, 'e': e, 'i': inc, 'Omega': Omega, 'W': W, 'M': M, 'Epoch': epochStr}
    else:
        return None


# Run a Horizons query
# eg. output = batch("Haumea", "2010-12-28 10:00", "2010-12-29 10:00", 1, su='d')

def batch(object, t, T, step,
          su='d',
          location=None,
          params=[1, 3, 9, 19, 36]):
    if step == None:  # default
        step = 1
    else:
        step = int(step)

    # Construct the query url
    s = "'"
    if not params:
        for i in range(1, 40):  # There are 40 possible pieces of info that Horizons can give back
            s += str(i) + ','
            s += "40'"  # python leaves one off the end in range()
    else:
        for p in params:
            s += "{},".format(p)

    # The pieces of the url that Horizons needs for its processing instructions.
    urlArr = ["http://ssd.jpl.nasa.gov/horizons_batch.cgi?batch=1&COMMAND=",
              '',
              "&MAKE_EPHEM='YES'&TABLE_TYPE='OBSERVER'&START_TIME=",
              '',
              "&STOP_TIME=",
              '',
              "&STEP_SIZE=",
              '',
              "&QUANTITIES=" + s,
              "&CSV_FORMAT='YES'"]

    # Break the object name, start & end dates and the timestep up into appropriate url-formatting
    url_style_output = []
    for obj in [object, t, T]:
        os = obj.split()
        if len(os) > 1:
            ob = "'" + os[0] + '%20' + os[1] + "'"
        else:
            ob = "'" + object + "'"
        url_style_output.append(ob)
    step = "'" + str(step) + "%20" + su + "'"

    # url components
    urlArr[1] = url_style_output[0]  # formatted object name
    urlArr[3] = url_style_output[1]  # start time
    urlArr[5] = url_style_output[2]  # end time
    urlArr[7] = step  # timestep

    urlStr = "".join(urlArr)  # create the url to pass to Horizons

    # Query Horizons; if it's busy, wait and try again in a minute
    done = 0
    while not done:
        urlHan = url.urlopen(urlStr)
        urlData = urlHan.readlines()
        urlHan.close()
        if len(urlData[0].split()) > 1:
            if "BUSY:" <> urlData[0].split()[1]:
                done = 1
            else:
                print urlData[0],
                print "Sleeping 60 s and trying again"
                time.sleep(60)
        else:
            done = 1

    EPHEM_CSV_START_MARKER = '$$SOE'
    EPHEM_CSV_END_MARKER = '$$EOE'
    ephemCSV_start = None
    ephemCSV_end = None
    for i, dataLine in enumerate(urlData):
        if dataLine.strip() == EPHEM_CSV_START_MARKER:
            ephemCSV_start = i
        elif dataLine.strip() == EPHEM_CSV_END_MARKER:
            ephemCSV_end = i
    assert ephemCSV_start is not None, 'No ephem start'
    assert ephemCSV_end is not None, 'No ephem end'
    assert ephemCSV_start < ephemCSV_end, 'Ephem are a bit odd'
    # The header, the lines *after* the start marker, up to (but not including, because it is a slice) the end marker
    csv_lines = [urlData[ephemCSV_start - 2]] + urlData[ephemCSV_start + 1: ephemCSV_end]
    ephemCSV = ''.join(csv_lines)
    ephemCSVfile = io.BytesIO(ephemCSV)
    ephemerides = pd.DataFrame.from_csv(ephemCSVfile)

    orbital_elements = parse_orbital_elements(urlData)

    return orbital_elements, ephemerides

# Run the script from the command line
if __name__ == "__main__":
    elems, ephems = batch("Ceres", "2010-12-28 10:00", "2010-12-30 10:00", 1, su='d')  # for example

    # just a few test print statements
    print(elems)  # elements
    print(ephems.keys())

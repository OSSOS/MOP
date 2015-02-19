# horizons.py
# Query JPL Horizons via the web service
# Wes Fraser, 2010
# rewritten and documented by Michele Bannister, Dec 2010

import urllib2 as url
import time

# Stores the indexed values returned by Horizons web-interface
def outDict(horizons_string):
    S = horizons_string.split(',')
    outDic = {}
    keys = ['DATE', 'RA', 'DEC', 'AppRA', 'AppDec', 'dRA', 'dDec', 'AZ', 'EL', 'dAZ', 'dEL', 'v_mag', 'r_helio']
    if len(S) == 73:
        spots = [0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 18, 34]  # the object is a tno
    elif len(S) == 74 or len(S) == 75:
        # the object is an asteroid: surface brightness is returned for asteroids when a radius is known. Off-by-one
        # error.
        # moons are different again at len = 75. Not fully tested that it works for moons.
        spots = [0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 18, 35]
    for ii in range(len(spots)):  # match the values to the keys, loading up the dictionary
        spot = spots[ii]
        key = keys[ii]
        outDic[key] = S[spot]
    return outDic


# Run a Horizons query
# eg. output = batch("Haumea", "2010-12-28 10:00", "2010-12-29 10:00", 1, su='d')

def batch(object, t, T, step, su='d'):
    if step == None:  # default
        step = 1
    else:
        step = int(step)

    # Construct the query url
    s = "'"
    for i in range(1, 40):  # There are 40 possible pieces of info that Horizons can give back
        s += str(i) + ','
    s += "40'"  # python leaves one off the end in range()
    # The pieces of the url that Horizons needs for its processing instructions. Leave intact.
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

    # print urlData  #testing

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
        orbital_elements = {'a': a, 'e': e, 'i': inc, 'Omega': Omega, 'W': W, 'M': M, 'Epoch': epochStr}
    else:
        orbital_elements = None

    # Keep the lines in urlData that have ephemeris information
    request_worked = False
    for i in range(len(urlData)):
        # '$$SOE\n' and '$$EOE\n' bracket the returned ephemerides if the request worked
        if urlData[i] == '$$SOE\n':
            request_worked = True
            start = i + 1
        if request_worked and urlData[i] == '$$EOE\n':
            end = i - 1
    # Send back the problem if the request didn't work (catch this)
    if request_worked is False:
        raise EnvironmentError, 'Horizons request did not return ephemerides. Check the problem: ' + urlData[24]

    # Transfer the good lines to the dictionary, which makes all the values indexed and accessible
    ephemerides = []
    for line in urlData[start:end + 1]:
        ephemerides.append(outDict(line))

    return orbital_elements, ephemerides

# Run the script from the command line
if __name__ == "__main__":
    elems, ephems = batch("Haumea", "2010-12-28 10:00", "2010-12-30 10:00", 1, su='d')  # for example

    # just a few test print statements
    print elems  # elements
    for ephemeris in ephems:  # outDict
        print ephemeris['DATE'], ephemeris['RA'], ephemeris['DEC'], ephemeris['dRA'], ephemeris['dDec']

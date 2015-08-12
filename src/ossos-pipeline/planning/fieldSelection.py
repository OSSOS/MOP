# fields must be
#
# - within 25 degrees of opposition during New Moon in April
# - visible for +4 hours per night in April Dark Time
# - visible for +2 hours per night in Feb/Mar and May/Jun

import math
import sys

import ephem

cfht = ephem.Observer()
cfht.lat = 0.344
cfht.lon = -2.707
cfht.horizon = math.radians(20)
cfht.elevation = 3900
o = cfht
target_latitude = math.radians(0)
p = ephem.Ecliptic(0, target_latitude)

start_date = ephem.date('2015/06/01 00:00:00')
Months = ["", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
field = ephem.FixedBody()
Moon = ephem.Moon()
Sun = ephem.Sun()

if len(sys.argv) != 3:

    # for each day in the semester
    sys.stdout.write("%8s | %17s | %17s | %38s  %38s  %38s  \n" % (
        "Day", "%Ill Moon-2015 RA", "%Ill Moon-2016 RA", "Min-RA [uptime] (Glat)", "Opp-RA [uptime] (Glat)",
        "Max-RA [uptime] (Glat)"))

    for day in range(0, 360):
        o.date = ephem.date(start_date + day)
        Sun.compute(o)

        # Start at local Noon
        solar_noon = Sun.transit_time

        # Determine the next Sun Set and next Sun Rise (using dark time)
        o.horizon = "-18:00"
        o.date = solar_noon
        sun_set = o.next_setting(Sun)
        sun_rise = o.next_rising(Sun)

        # flip back to requiring source above airmass 1.5
        o.horizon = math.radians(20)

        Moon.compute(o)

        # Figure out where opposition is
        opp = ephem.Ecliptic(ephem.Sun(o))
        opp.lon += math.radians(180.0)
        opp.lat = target_latitude
        t = o.date.tuple()
        sys.stdout.write("%5s %2d | %3.0f %13s | " % (Months[t[1]], t[2], Moon.phase, Moon.ra))
        sys.stdout.write("%3.0f %13s |" % (ephem.Moon(o.date + 365).phase, ephem.Moon(o.date + 365).ra))

        field_centres = [(False, False, False),
                         (False, False, False),
                         (False, False, False)]

        prev_uptime = 0

        for ang in range(-20, 20, 1):
            o.horizon = math.radians(40)
            pos = ephem.Ecliptic(opp)
            pos.lon += math.radians(ang)
            pos.lat = target_latitude
            (field._ra, field._dec) = pos.to_radec()
            o.date = solar_noon
            field.compute(o)

            # we can start observing when the field is up and the sun is down.
            field_rising = o.next_rising(field)
            start_time = max(sun_set, field_rising)

            # next compute how long till the source sets or the sun rises again
            o.date = start_time
            field_setting = o.next_setting(field)
            end_time = min(field_setting, sun_rise)

            # also need to compute the angle between the moon and the field.
            uptime = (end_time - start_time) * 24.0

            # Now, if the moon is more than 25% illuminated we only observe when
            # the moon is down.
            o.horizon = 0.0
            Moon.compute(o)
            # remove from uptime the time the moon and the field where both up.
            both_up_start = max(Moon.rise_time, field_rising)
            both_up_end = min(Moon.set_time, field_setting)

            if Moon.phase > 25:
                if both_up_end > both_up_start:
                    uptime -= (both_up_end - both_up_start) * 24.0
            uptime = max(0, uptime)
            # print sun_set, field_rising, both_up_start, Moon.rise_time, Moon.set_time, both_up_end, field_setting,
            #  sun_rise, start_time, end_time, uptime
            # print o.date, o.next_setting(field), opp.lon, field.ra, field.dec, uptime
            if ang == 0 and uptime > 2:
                field_centres[1] = (field.ra, field.dec, uptime)
                prev_uptime = uptime
                continue
            if uptime < 2:
                continue
            if not field_centres[0][0]:
                field_centres[0] = (field.ra, field.dec, uptime)
                continue
            field_centres[2] = (field.ra, field.dec, uptime)

        o.horizon = math.radians(40)
        for p in field_centres:
            if not p:
                sys.stdout.write(" %13s %13s [%3.1f]" % (p[0], p[1], p[2]))
                field._ra = p[0]
                field._dec = p[1]
                field.compute(o)
                g = ephem.Galactic(field)
                sys.stdout.write(" (%3.0f)" % (math.degrees(g.lat)))
            else:
                sys.stdout.write("%40s" % "N/A")

        sys.stdout.write('\n')

else:

    start_date = ephem.date('2015/01/01 00:00:00')

    field._ra = ephem.hours(sys.argv[1])
    field._dec = ephem.degrees(sys.argv[2])
    field.compute(cfht)
    ec = ephem.Ecliptic(field)
    # ec.lat = target_latitude
    # (field._ra, field._dec) = ec.to_radec()
    # field.compute(cfht)

    for day in range(0, 180):
        o.date = ephem.date(start_date + day)
        Sun.compute(o)
        solar_noon = Sun.transit_time
        o.date = solar_noon
        o.horizon = math.radians(-18.0)
        sun_set = o.next_setting(Sun)
        sun_rise = o.next_rising(Sun)

        o.horizon = math.radians(40)
        field.compute(o)
        t = o.date.tuple()

        opp = ephem.Ecliptic(ephem.Sun(o))
        target = ephem.Ecliptic(field)
        opp.lat = target.lat
        elongation = math.degrees(target.lon - opp.lon)

        # we can start observing when the field is up and the sun is down.
        field_rising = o.next_rising(field)
        start_time = max(sun_set, field_rising)

        # next compute how long till the source sets or the sun rises again
        o.date = start_time
        field_setting = o.next_setting(field)
        end_time = min(field_setting, sun_rise)
        uptime = (end_time - start_time) * 24.0

        # also need to compute the angle between the moon and the field.
        # if the moon is more than 25% illuminated we only observe when
        # the moon is down.
        o.horizon = 0.0
        Moon.compute(o)
        # remove from uptime the time the moon and the field where both up.
        both_up_start = max(Moon.rise_time, field_rising)
        both_up_end = min(Moon.set_time, field_setting)

        if Moon.phase > 25:
            if both_up_end > both_up_start:
                uptime -= (both_up_end - both_up_start) * 24.0
        uptime = max(0, uptime)
        # print sun_set, field_rising, both_up_start, Moon.rise_time, Moon.set_time, both_up_end, field_setting,
        # sun_rise, start_time, end_time, uptime

        # Do the same computations for a year later.  For planning of recovery on this field.
        o.date += 365
        o.date = o.next_rising(field)
        Moon2 = ephem.Moon(o)
        uptime2 = (o.next_setting(field) - o.date) * 24

        sys.stdout.write("%5s %2d | %3.0f %13s [%3.1f] | " % (Months[t[1]], t[2], Moon.phase, Moon.ra, uptime))
        sys.stdout.write("%3.0f %13s |" % (Moon2.phase, Moon2.ra))
        sys.stdout.write(" %13s %13s |" % (field.ra, field.dec))
        sys.stdout.write(" %5.0f" % elongation)
        sys.stdout.write('\n')

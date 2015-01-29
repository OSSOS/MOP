import sqlalchemy as sa
import ephem
import datetime

from web.field_obs.queries import ImagesQuery
from web.discoveries.queries import DiscoveriesQuery
from web.block.queries import BlockQuery


class SurveyQuery(object):
    def __init__(self):
        self.ims = ImagesQuery()
        self.disc = DiscoveriesQuery()
        self.bk = BlockQuery()
        # pixel scale is symmetric, 0.1850 arcsec/pixel. Dectector is [1:23219,1:19354] pixels.
        self.field_area = ((0.185 * 23219) / 3600.) * ((0.185 * 19354) / 3600.)  # sq. deg

    def fields_observed(self):
        fields = self.ims.what_fields_have_any_observations()
        # don't include the wallpaper in the overall count
        tno_fields = [f for f in fields if not f['fieldId'].__contains__('WP')]
        num_fields = len(tno_fields)

        sqdeg_observed = num_fields * self.field_area
        retval = (sqdeg_observed, num_fields)

        return retval

    def fields_processed(self):
        retval = 42 * self.field_area  # FIXME: currently hardwired for field 13AE & 13AO only
        return retval

    def most_recent_observation(self):
        it = self.ims.images
        ss = sa.select([it.c.obs_end], order_by=it.c.obs_end)
        dates = [n[0] for n in self.ims.conn.execute(ss)]

        return dates[-1]

    def next_moondark(self):  # this is producing an off by one error: fix!
        mn = ephem.Moon()
        mp = []
        for n in range(0, 29 * 4):
            mn.compute(ephem.now() + (n / 4.))
            mp.append((ephem.now() + (n / 4.), mn.moon_phase))
        next_new_moon = ephem.Date(min(mp, key=lambda x: x[1])[0]).datetime()
        # next_new_moon = pytz.utc.localize(next_new_moon)  # it's calculated in UTC
        #	hst = pytz.timezone('HST')

        retval = next_new_moon  #.astimezone(hst)

        return retval

    def next_observing_window(self):
        # we observe if the near-new moon is down, or if the moon is up
        # but we are within 3 days of new moon.

        next_nm = self.next_moondark()
        next_period = (next_nm - datetime.timedelta(3), next_nm + datetime.timedelta(3))

        if next_period[0] < datetime.datetime.now() < next_period[1]:
            retval = 'tonight (hopefully).'
        else:
            retval = 'in no more than ' + str((next_period[0] - datetime.datetime.now()).days) + ' days.'

        return retval

    def megacam_schedule(self):
        # tuples bracket the ends of date ranges that MegaCam is on the telescope
        schedule = [(datetime.datetime(2014, 1, 27), datetime.datetime(2014, 2, 03)),
                    (datetime.datetime(2014, 2, 21), datetime.datetime(2014, 3, 06)),
                    (datetime.datetime(2014, 3, 24), datetime.datetime(2014, 4, 07)),
                    (datetime.datetime(2014, 4, 24), datetime.datetime(2014, 5, 06)),
                    (datetime.datetime(2014, 5, 20), datetime.datetime(2014, 6, 02)),
                    (datetime.datetime(2014, 6, 20), datetime.datetime(2014, 7, 02)),
                    (datetime.datetime(2014, 7, 17), datetime.datetime(2014, 7, 28))
        ]
        return schedule

    def nearest_megacam_run(self):
        now = datetime.datetime.now()
        schedule = self.megacam_schedule()
        for run in schedule:
            if (run[0] - now > datetime.timedelta(0)) or (run[1] - now > datetime.timedelta(0)):
                nearest_run = run
                break

        if (nearest_run[0] <= now <= nearest_run[1]):
            retval = 'now.'
        else:
            if (nearest_run[0] - now).days > 0:
                retval = "in " + str((nearest_run[0] - now).days) + " days."
            else:
                hrs = (nearest_run[0] - now).seconds / 3600.
                retval = "in " + '%2.1f' % hrs + " hours."

        return retval
		


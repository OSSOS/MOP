import datetime

import ephem

import sqlalchemy as sa
from web.field_obs.queries import ImagesQuery
from web.block.queries import BlockQuery


class SurveyQuery(object):
    def __init__(self):
        self.ims = ImagesQuery()
        self.bk = BlockQuery()
        # pixel scale is symmetric, 0.1850 arcsec/pixel. Dectector is [1:23219,1:19354] pixels.
        self.field_area = ((0.184 * 23219) / 3600.) * ((0.184 * 19354) / 3600.)  # sq. deg

    def fields_observed(self):
        fields = self.ims.what_fields_have_any_observations()
        # don't include the wallpaper in the overall count
        tno_fields = [f for f in fields if not f['fieldId'].__contains__('WP')]
        num_fields = len(tno_fields)

        sqdeg_observed = num_fields * self.field_area
        retval = (sqdeg_observed, num_fields)

        return retval

    def fields_processed(self):
        retval = 3 * 21 * self.field_area  # 3 blocks processed at 04-02-2015
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
        # hst = pytz.timezone('HST')

        retval = next_new_moon  # .astimezone(hst)

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
        schedule = [(datetime.datetime(2015, 7, 8), datetime.datetime(2015, 7, 21)),
        ]
        return schedule

    def nearest_megacam_run(self):
        now = datetime.datetime.now()
        schedule = self.megacam_schedule()
        nearest_run = []
        for run in schedule:
            if (run[0] - now > datetime.timedelta(0)) or (run[1] - now > datetime.timedelta(0)):
                nearest_run = run
                break

        if nearest_run[0] <= now <= nearest_run[1]:
            retval = 'now.'
        else:
            if (nearest_run[0] - now).days > 0:
                retval = "in " + str((nearest_run[0] - now).days) + " days."
            else:
                hrs = (nearest_run[0] - now).seconds / 3600.
                retval = "in " + '%2.1f' % hrs + " hours."

        return retval

    def num_discoveries(self):
        # Characterized discoveries only.
        status = self.bk.all_blocks()['status']

        return sum([int(n[1]) for n in list(status.values()) if (n is not None and n[1].isdigit())])


    def mpc_informed(self):
        # status = self.bk.all_blocks()['blocks']
        # Implement more comprehensive info later
        # return sum([n[1] for n in status.values() if n[1].isdigit()])

        return 0


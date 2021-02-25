from pyramid.view import view_config
from zope.cachedescriptors import property

from .queries import SurveyQuery


class Overview(object):
    def __init__(self, request):
        self.request = request
        self.surveyQuery = SurveyQuery()

    @property.Lazy
    def fractionSurveyed(self):
        retval = self.surveyQuery.fields_observed()
        return retval

    @property.Lazy
    def fractionProcessed(self):
        retval = self.surveyQuery.fields_processed()
        return retval

    @property.Lazy
    def numDiscoveries(self):
        retval = self.surveyQuery.num_discoveries()
        return retval

    @property.Lazy
    def mpcTold(self):
        retval = self.surveyQuery.mpc_informed()
        return retval

    @property.Lazy
    def most_recent_obs(self):
        retval = self.surveyQuery.most_recent_observation()
        return retval

    @property.Lazy
    def nextScheduledObservations(self):
        retval = self.surveyQuery.next_observing_window()
        return retval

    @property.Lazy
    def next_moondark(self):
        retval = self.surveyQuery.next_moondark()
        return retval

    @property.Lazy
    def nearest_megacam_run(self):
        retval = self.surveyQuery.nearest_megacam_run()
        return retval

    @property.Lazy
    def blocks(self):
        retval = self.surveyQuery.bk.all_blocks()
        return retval


    @view_config(route_name='overview', renderer='overview.pt', permission='ossos')
    def general_overview(self):
        retval = {'surveyed': self.fractionSurveyed,
                  'processed_sqdeg': self.fractionProcessed,
                  'num_discoveries': self.numDiscoveries,
                  'mpc_told': self.mpcTold,
                  'most_recent_obs': self.most_recent_obs,
                  'next_obs': self.nextScheduledObservations,
                  'next_moondark': self.next_moondark,
                  'nearest_megacam_run': self.nearest_megacam_run,
                  'blocks': self.blocks
        }
        return retval

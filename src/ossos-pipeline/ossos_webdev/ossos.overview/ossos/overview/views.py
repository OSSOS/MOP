from pyramid.response import Response
from pyramid.view import view_config
from queries import SurveyQuery

# won't need this soon
import ossos.block.queries as bq

class Overview(object):

	def __init__(self, request):
		self.request = request
		self.surveyQuery = SurveyQuery()
		

	@property
	def percentComplete(self):
		retval = self.surveyQuery.survey_proportion_complete()
		return retval

	@property
	def fractionSurveyed(self):
		retval = self.surveyQuery.fields_observed()   # remember to also do fields_observed_to_completion
		return retval

	@property
	def fractionProcessed(self):
		retval = self.surveyQuery.fields_processed()
		return retval

	@property
	def numDiscoveries(self):
		retval = self.surveyQuery.disc.num_discoveries()
		return retval

	@property
	def mpcTold(self):
		retval = self.surveyQuery.disc.mpc_informed()
		return retval
	
	@property
	def surveyEfficiency(self):
		retval = self.surveyQuery.survey_efficiency()
		return retval

	@property
	def most_recent_obs(self):
		retval = self.surveyQuery.most_recent_observation()
		return retval

	@property
	def nextScheduledObservations(self):
		retval = self.surveyQuery.next_observing_window()
		return retval

	@property
	def next_moondark(self):
		retval = self.surveyQuery.next_moondark()
		return retval

	@property
	def nearest_megacam_run(self):
		retval = self.surveyQuery.nearest_megacam_run()
		return retval

	@property
	def blocks(self):
		retval = self.surveyQuery.bk.all_blocks()
		return retval


	@view_config(route_name='overview', renderer='template.pt', permission='ossos')
	def general_overview(self):
		retval = {'propor_complete': self.percentComplete,
					'surveyed': self.fractionSurveyed,
					'processed_sqdeg': self.fractionProcessed,
					'num_discoveries': self.numDiscoveries,
					'mpc_told': self.mpcTold,
					'efficiency': self.surveyEfficiency,
					'most_recent_obs': self.most_recent_obs,
					'next_obs': self.nextScheduledObservations,
					'next_moondark': self.next_moondark,
					'nearest_megacam_run': self.nearest_megacam_run,
					'blocks': self.blocks
					}
		return retval

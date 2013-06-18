from pyramid.response import Response
from pyramid.view import view_config
import queries
import ossos.block.queries as bq
import ossos.discoveries.queries as dq

class Overview(object):

	def __init__(self, request):
		self.request = request
		self.images = queries.ossuaryTable('images')
		self.discoveries = queries.ossuaryTable('discoveries')

	@property
	def percentComplete(self):
		retval = queries.survey_proportion_complete(self.images)
		return retval

	@property
	def fractionSurveyed(self):
		retval = queries.fields_observed_to_completion(self.images)
		return retval

	@property
	def fractionProcessed(self):
		retval = queries.fields_processed(self.images)
		return retval

	@property
	def numDiscoveries(self):
		retval = dq.num_discoveries(self.discoveries)
		return retval

	@property
	def mpcTold(self):
		retval = dq.mpc_informed(self.discoveries)
		return retval
	
	@property
	def surveyEfficiency(self):
		retval = queries.survey_efficiency(self.images)
		return retval

	@property
	def mostRecentBlockCompletionObs(self):
		retval = bq.most_recent_block_completion(self.images)
		return retval

	@property
	def most_recent_obs(self):
		retval = queries.most_recent_observation(self.images)
		return retval

	@property
	def nextScheduledObservations(self):
		retval = queries.next_observing_window()
		return retval

	@property
	def next_moondark(self):
		retval = queries.next_moondark()
		return retval

	@property
	def nearest_megacam_run(self):
		retval = queries.nearest_megacam_run()
		return retval

	@property
	def blocks(self):
		retval = bq.all_blocks(self.images)
		return retval


	@view_config(route_name='overview', renderer='template.pt')
	def general_overview(self):
		retval = {'propor_complete': self.percentComplete,
					'surveyed': self.fractionSurveyed,
					'processed_sqdeg': self.fractionProcessed,
					'num_discoveries': self.numDiscoveries,
					'mpc_told': self.mpcTold,
					'efficiency': self.surveyEfficiency,
					'most_recent_completion': self.mostRecentBlockCompletionObs,
					'most_recent_obs': self.most_recent_obs,
					'next_obs': self.nextScheduledObservations,
					'next_moondark': self.next_moondark,
					'nearest_megacam_run': self.nearest_megacam_run,
					'blocks': self.blocks
					}
		return retval

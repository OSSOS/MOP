from pyramid.response import Response
from pyramid.view import view_config
import queries
import ossos.overview.queries as oq

class Discovery(object):

	def __init__(self, request):
		self.request = request
		self.discoveries = oq.ossuaryTable('discoveries')


	@property
	def num_discoveries(self):
		retval = queries.num_discoveries(self.discoveries)
		return retval

	@property
	def ossos_discoveries(self):
		retval = []   # AWAITING NEED TO BE IMPLEMENTED
		return retval


	@view_config(route_name='discoveries', renderer='discoveries.pt')
	def inspect_observations(self):
		# uid = self.request.matchdict['uid']
		# return dict(title='Triplet', uid=uid)
		
		retval = {'num_discoveries': self.num_discoveries,
		'discoveries': self.ossos_discoveries
		}
		return retval


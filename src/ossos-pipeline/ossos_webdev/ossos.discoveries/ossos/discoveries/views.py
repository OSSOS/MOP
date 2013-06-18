from pyramid.response import Response
from pyramid.view import view_config
from queries import DiscoveriesQuery


class Discovery(object):

	def __init__(self, request):
		self.request = request
		self.discoveriesQuery = DiscoveriesQuery()


	@property
	def num_discoveries(self):
		retval = self.discoveriesQuery.num_discoveries()
		return retval

	@property
	def ossos_discoveries(self):
		retval = self.discoveriesQuery.ossos_discoveries()
		return retval


	@view_config(route_name='discoveries', renderer='discoveries.pt')
	def inspect_observations(self):	
		retval = {'num_discoveries': self.num_discoveries,
		'discoveries': self.ossos_discoveries
		}
		return retval


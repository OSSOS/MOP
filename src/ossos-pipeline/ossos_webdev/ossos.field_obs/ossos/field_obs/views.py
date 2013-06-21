from pyramid.response import Response
from pyramid.view import view_config
from queries import ImagesQuery

class Field(object):

	def __init__(self, request):
		self.request = request
		self.imagesQuery = ImagesQuery()

		# BIT HACKED - FIX! Transfer to urllib parsing. More Reliable.
		fi = request.current_route_url().rpartition('/')[2]
		if fi.__contains__('%2B'):
			fi2 = fi.split('%2B')
			fi3 = '+'.join(fi2)
			self.fieldId = fi3
		else:
			self.fieldId = fi

	@property
	def observations(self):
		retval = self.imagesQuery.field_images(self.fieldId)
		return retval
	
	@property
	def ra(self):
		retval = self.imagesQuery.field_ra(self.fieldId)
		return retval

	@property
	def dec(self):
		retval = self.imagesQuery.field_dec(self.fieldId)
		return retval

	@property
	def discovery_triplet(self):
		retval = self.imagesQuery.discovery_triplet(self.fieldId)
		return retval

	@property
	def numObs(self):
		retval = len(self.observations['obs'])
		return retval

	@property
	def num_precoveries(self):
		retval = self.imagesQuery.num_precoveries(self.fieldId)
		return retval

	@property
	def num_nailings(self):
		retval = self.imagesQuery.num_nailings(self.fieldId)
		return retval

	@property
	def num_doubles(self):
		retval = self.imagesQuery.num_doubles(self.fieldId)
		return retval


	@view_config(route_name='field_obs', renderer='field_obs.pt', permission='ossos')
	def inspect_observations(self):
		# uid = self.request.matchdict['uid']
		# return dict(title='Triplet', uid=uid)
		
		retval = {'fieldId':self.fieldId,
		'observations': self.observations,
		'ra': self.ra,
		'dec': self.dec,
		'discovery_triplet': self.discovery_triplet,
		'totalObs': self.numObs,
		'precoveries': self.num_precoveries,
		'nailings': self.num_nailings,
		'doubles': self.num_doubles
		}
		return retval

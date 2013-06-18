from pyramid.response import Response
from pyramid.view import view_config
from queries import BlockQuery


class Block(object):

	def __init__(self, request):
		self.request = request
		self.blockQuery = BlockQuery()
		# BIT HACKED but works, until I figure out how to do it properly
		# reengineer for robustness with urllib.
		self.blockID = request.current_route_url().rpartition('/')[2]
	
	@property
	def num_fields(self):
		# number of fields in the block
		retval = self.blockQuery.num_fields_in_block(self.blockID)
		return retval

	@property
	def observed_fields(self):
		# HACKED FOR TESTING
		retval = self.blockQuery.link_images_to_tripleplus_nights(self.blockID)
		return retval
	
	@property
	def block_ra(self):
		retval = 'TEST' #queries.field_ra(self.blockID)
		return retval

	@property
	def block_dec(self):
		retval = 'TEST' #queries.field_dec(self.blockID)
		return retval

	@property
	def numObs(self):
		retval = len(self.blockQuery.block_images(self.blockID)['obs'])
		return retval

	@property
	def num_precoveries(self):
		retval = self.blockQuery.bk.num_precoveries('E+0+0')  # HACKED FOR TESTING
		return retval

	@property
	def num_nailings(self):
		retval = self.blockQuery.bk.num_nailings('E+0+0')  # HACKED FOR TESTING
		return retval

	@property
	def num_doubles(self):
		retval = self.blockQuery.bk.num_doubles('E+0+0')  # HACKED FOR TESTING
		return retval


	@view_config(route_name='block', renderer='block.pt')
	def inspect_observations(self):
		# uid = self.request.matchdict['uid']
		# return dict(title='Triplet', uid=uid)
		
		retval = {'blockID': self.blockID,
		'num_fields': self.num_fields,
		'observedFields': self.observed_fields,
		'ra': self.block_ra,
		'dec': self.block_dec,
		'totalObs': self.numObs,
		'precoveries': self.num_precoveries,
		'nailings': self.num_nailings,
		'doubles': self.num_doubles
		}
		return retval


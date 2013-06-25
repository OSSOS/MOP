import sqlalchemy as sa
from numpy import median
from ossos.field_obs.queries import ImagesQuery
from ossos.overview.ossuary import OssuaryTable


class BlockQuery(object):

	def __init__(self):
		self.bk = ImagesQuery()


	def all_blocks(self):
		retval = {}
		# HARDWIRED FOR TESTING/BUILDING
		blocks = ['2013A-E', '2013A-O', '2013A-WP-E', '2013A-WP-O'] 
		bks = retval.get('blocks', [])
		for block in blocks:
			bk = []
			bk.append(block)
			bk.append(self.num_block_images(block))  # no. observations
			if block.__contains__('WP'):  # Wallpaper can't have recoveries!
				for k in range(2,6):  
					bk.append('-')
			else:
				# need: precoveries, nailings, doubles, discoveries
				bk.append(self.block_precoveries(block))
				bk.append(self.block_nailings(block))
				bk.append(self.block_doubles(block))
				bk.append(self.block_discoveries(block))

			bks.append(bk)

		retval['blocks'] = bks
	 
		return retval


	def fields_in_block(self, blockID):
		parsing = self.parse_blockID(blockID)
		all_observed_fields = self.bk.what_fields_have_any_observations() # [dicts]
		# filter these for the ones that are in this block
		retval = []
		retval2 = []
		for obs in all_observed_fields:
			if obs['fieldId'].startswith(parsing):
				retval.append(obs)
				retval2.append(obs['fieldId']) 

		return retval, retval2  # returns a list of dictionaries. Also return just keys?
	

	def format_imquery_return(self, ims_query):
		ret_images = []
		for row in ims_query:
			print row
			ret_images.append([row[1], row[2], (row[3])])
		ims_query.close()

		return ret_images


	def parse_blockID(self, blockID):
		if blockID.__contains__('WP'):
			retval = '-'.join(blockID.split('-')[1:])
		else:  # it's an E or O field, less fancy!
			retval = blockID.partition('-')[2]

		return retval


	def num_block_images(self, blockID):

		blockstart = self.parse_blockID(blockID)
		# number of images on all fields that are in the block
		it = self.bk.images
		cols = [it.c.cfht_field, sa.func.count(it.c.image_id)]
		ss = sa.select(cols, it.c.cfht_field.like('{0}%'.format(blockstart)), 
						group_by=it.c.cfht_field)

		ims_query = self.bk.conn.execute(ss)
		retval = sum([r[1] for r in ims_query])
		return retval
		

	def block_discovery_triples(self, blockID):

		retval, fieldIds = self.fields_in_block(blockID)
		for field in fieldIds:
			triplet = self.bk.discovery_triplet(field)
			if triplet:  # yay we have enough observations to have a discovery triplet!
				retfield = [n for n in retval if n['fieldId'] == field][0]
				retfield['triplet'] = triplet[1]
				retfield['worstIQ'] = triplet[2]

		return retval


	def block_precoveries(self, blockID):
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = 0
			for field in fields:
				pc = self.bk.num_precoveries(field)
				if isinstance(pc, int):
					retval += pc
		else:
			retval = '-'

		return retval


	def block_nailings(self, blockID):
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = 0
			for field in fields:
				nc = self.bk.num_nailings(field)
				if isinstance(nc, int):
					retval += nc
		else:
			retval = '-'

		return retval


	def block_doubles(self, blockID):
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = 0
			for field in fields:
				doub = self.bk.num_doubles(field)
				if isinstance(doub, int):
					retval += doub
		else:
			retval = '-'

		return retval


	def block_discoveries(self, blockID):  # AWAITING IMPLEMENTATION
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = []
			return len(retval)
		else:
			retval = '-'

		return retval










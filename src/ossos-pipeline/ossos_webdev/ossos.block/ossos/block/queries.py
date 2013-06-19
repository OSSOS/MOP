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

		print retval

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
		

	def images_in_tripleplus_night(self, field, date):

		ss = sa.text(
			"""select cfht_field, extract(year from obs_end) as year, 
				extract(month from obs_end) as month, 
				extract(day from obs_end) as day, image_id, obs_end, 
				iq_ossos from images
				where (cfht_field = :field
				and extract(year from obs_end) = :year
				and extract(month from obs_end) = :month
				and extract(day from obs_end) = :day)
				order by obs_end""")
		pp = {'field':field, 'year':date[0], 'month':date[1], 'day':date[2]}
		tplus_res = self.bk.conn.execute(ss, pp)

		ret_images = []
		for row in tplus_res:
			ret_images.append([row[5], row[4], (row[6])])


		return ret_images


	def link_images_to_tripleplus_nights(self, blockID):

		retval, fieldIds = self.fields_in_block(blockID)

		# now add in the triples info for those fields that have it
		threeplus_fields = self.bk.do_triples_exist()

		for field, nights in threeplus_fields.items():
			retfs = [n for n in retval if n['fieldId'] == field]
			if len(retfs) > 0:
				retfield = retfs[0]
				for night in nights[0:1]:  # TESTING
					images_info = self.images_in_tripleplus_night(field, night)
					retfield['triplet'] = images_info
					retfield['worstIQ'] = max([n[2] for n in images_info])

		return retval


	def block_precoveries(self, blockID):
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = 0
			for field in fields:
				# CAN TAKE OUT LEN ONCE THIS IS FULLY IMPLEMENTED
				retval += len(self.bk.num_precoveries(field))
		else:
			retval = '-'

		return retval


	def block_nailings(self, blockID):
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = 0
			for field in fields:
				# CAN TAKE OUT LEN ONCE THIS IS FULLY IMPLEMENTED
				retval += len(self.bk.num_nailings(field))
		else:
			retval = '-'

		return retval


	def block_doubles(self, blockID):
		if not blockID.__contains__('WP'):
			empty_units, fields = self.fields_in_block(blockID)
			retval = 0
			for field in fields:
				# CAN TAKE OUT LEN ONCE THIS IS FULLY IMPLEMENTED
				retval += len(self.bk.num_doubles(field))
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










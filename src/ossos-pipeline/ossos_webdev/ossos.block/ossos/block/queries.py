import sqlalchemy as sa
from numpy import median
from ossos.field_obs.queries import ImagesQuery
from ossos.overview.ossuary import OssuaryTable


class BlockQuery(object):

	def __init__(self):
		self.bk = ImagesQuery()


	def all_blocks(self):
		# HARDWIRED FOR TESTING/BUILDING
		# Needs to do a search in images eventually, once the fields are tagged correctly
		retval = {}
		blocks = ['2013A-E', '2013A-O', '2013A-WP-E', '2013A-WP-O'] 
		bks = retval.get('blocks', [])
		for block in blocks:
			bk = []
			bk.append(block)
			# need: observations, precoveries, nailings, doubles, discoveries
			for k in range(1,6):  # HACKED FOR TESTING
				bk.append(0)
			bks.append(bk)

		retval['blocks'] = bks
	 
		return retval


	def format_imquery_return(self, ims_query):
		ret_images = []
		for row in ims_query:
			ret_images.append([row[1], row[2], (row[3])])
		ims_query.close()

		return ret_images


	def parse_blockID(self, blockID):
		if blockID.__contains__('WP'):
			retval = '-'.join(blockID.split('-')[0:2])
		else:  # it's an E or O field, less fancy!
			retval = blockID.partition('-')[2]

		return retval


	def num_fields_in_block(self, blockID):
		blockstart = self.parse_blockID(blockID)

		# number of images on all fields that are in the block
		it = self.bk.images
		ss = sa.select([it.c.cfht_field])  # sa.func.count ?
		ss.append_whereclause(it.c.cfht_field.like("%:blockstart%"))
		retval = len([n[0] for n in self.bk.conn.execute(ss)])

		return retval


	def block_images(self, blockID):

		blockstart = self.parse_blockID(blockID)

		# number of images on all fields that are in the block
		it = self.bk.images
		cols = [it.c.cfht_field, it.c.obs_end, it.c.iq_ossos, it.c.image_id]
		ss = sa.select(cols)
		ss.append_whereclause(it.c.cfht_field.like("%:blockstart%"))
		# NOT WORKING ON WP FIELDS??!
		ims_query = self.bk.conn.execute(ss)
		ret_images = self.format_imquery_return(ims_query)

		retval = {'obs':ret_images}

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
		tplus_res.close()

		return ret_images


	def fields_in_block(self, blockID):
		parsing = self.parse_blockID(blockID)
		observed_fields = self.bk.what_fields_have_any_observations()  # list of dicts
		# filter these for the ones that are in this block
		retval = []
		for obs in observed_fields:
			if obs['fieldId'].startswith(parsing):
				retval.append(obs)

		return retval


	def link_images_to_tripleplus_nights(self, blockID):

		retval = self.fields_in_block(blockID)

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




import sqlalchemy as sa
from ossos.overview.ossuary import OssuaryTable


class ImagesQuery(object):

	def __init__(self):
		ot = OssuaryTable('images')
		self.images = ot.table
		self.conn = ot.conn


	def field_images(self, field):
		# print field
		# if not(isinstance(field, str)):
		# 	msg = '"{0}" is not a string'.format(type(field))
		# 	raise ValueError(msg)

		it = self.images
		cols = [it.c.cfht_field, it.c.obs_end, it.c.iq_ossos, it.c.image_id]
		ss = sa.select(cols, order_by=it.c.obs_end)
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)
		ret_images = self.format_imquery_return(ims_query)

		retval = {'obs':ret_images}

		return retval


	def format_imquery_return(self, ims_query):
		ret_images = []
		for row in ims_query:
			ret_images.append([row[1], row[2], (row[3])])
		ims_query.close()

		return ret_images


	def field_ra(self, field):
		assert isinstance(field, str), '{0} is not a str'.format(type(field))
		it = self.images
		ss = sa.select([it.c.cfht_field, it.c.crval_ra])
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)

		retval = 0
		for row in ims_query:
			retval = row[1] 	# HACKED FOR QUICK RESULT (precision not needed)

		return retval


	def field_dec(self, field):
		it = self.images
		ss = sa.select([it.c.cfht_field, it.c.crval_dec])
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)

		retval = 0
		for row in ims_query:
			retval = row[1] 	# HACKED FOR QUICK RESULT (precision not needed)

		return retval


	# used in BlockQuery to get observed fields in the block.
	def what_fields_have_any_observations(self):
		it = self.images
		ss = sa.select([it.c.cfht_field],
						distinct=it.c.cfht_field,
						order_by=it.c.cfht_field)
		
		fields = [{'fieldId': n[0], 'triplet':None} for n in self.conn.execute(ss)]

		return fields


	# Process of getting the three best images that form a discovery triplet.
	def discovery_triplet(self, field):
		retval = None  # is set to a value if a valid triplet exists.
		# Is there even a valid triplet observed yet?
		if len(self.field_images(field)['obs']) > 2:
			threeplus_nights = self.do_triples_exist(field)  # just the dates
			if len(threeplus_nights) > 0:
				# The images on the nights that have 3+ images, this field.
				threeplus_night_images = {}
				for date in threeplus_nights:
					date_images = self.images_in_tripleplus_night(field, date)
					threeplus_night_images[date] = date_images

				retval = self.parse_for_best_triple(threeplus_night_images)
	
		return retval


	def do_triples_exist(self, field):

		tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, 
			count(image_id) from images 
			where (cfht_field = :field)
			group by cfht_field, year, month, day 
			having count(image_id) > 2
			order by cfht_field, year, month, day;""")
		pp = {'field': field}
		tplus_res = self.conn.execute(tplus, pp)

		threeplus_nights = []
		for row in tplus_res:  
			threeplus_nights.append(row[1:4])  # year, month, day

		return threeplus_nights


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
		tplus_res = self.conn.execute(ss, pp)

		ret_images = []
		for row in tplus_res:
			ret_images.append(row[1:]) # keep year, month, day, image_id, obs_end, iq_ossos

		return ret_images


	def parse_for_best_triple(self, threeplus_night_images):
		# input is a dict of {date: [rows of images]}
		good_triples = []
		for date, ims in threeplus_night_images.items():
			# for each night, create the best possible triple that meets constraints.

			print date, len(ims)
			# HACK FOR TESTING PREVIOUS CODE
			if len(ims) == 3:
				# format as ([image_ids], [3 rows of remaining info], worst_iq)
				good_triples.append(([im[3] for im in ims], 
									ims,
									max([im[5] for im in ims])
									))

		if len(good_triples) > 0:
			# Return the set of 3 images that have the lowest value of 'worst iq'. 
			lowest_worst_iq = min([g[2] for g in good_triples]) 
			print lowest_worst_iq
			retval = good_triples[[g[2] for g in good_triples].index(lowest_worst_iq)]  
		else:
			retval = None
		
		return retval

			# # is their temporal span sufficiently wide for a triplet?
			# if (nt[-1] - nt[0]) > datetime.timedelta(minutes=90):
			# 	print 'can check for triple_sets'
			# 	# what is the widest-spaced 

			# else:
			# 	print 'too close!'

# # def for best set of three
# def find_best_intranight_triple():
# 	triple_sets = [[],[],[]]
# 	# construct all possible sets of three 
# 	for ii in times:
# 		for jj in times:
# 			for kk in times:
# 				if (ii < jj < kk) and (jj-ii > 20 minutes) and (kk-jj > 20 minutes):
#
#   # then pick the set with the 


	def num_precoveries(self, field):
		# how many of the images in self.observations occur 
		# before the first image of the discovery triplet?
		triplet = self.discovery_triplet(field)
		retval = 'no discovery triplet'
		if triplet:
			images = self.field_images(field)
			precoveries = [im for im in images['obs'] if (im[0] < triplet[1][0][4])]
			retval = len(precoveries)

		return retval


	def num_nailings(self, field):
		# nailings are single images that occur
		# at least one night after the night of the discovery triplet?
		triplet = self.discovery_triplet(field)
		retval = 'no discovery triplet'
		if triplet:
			images = self.field_images(field)
			# NEED TO FIX THIS TO ONLY COUNT SINGLE OBSERVATIONS WITHIN THE NIGHT
			after = [im for im in images['obs'] if (im[0] > triplet[1][2][4])]
			# ie. if there's a spare image left in the night after a triple or double, can it count?
			retval = len(after)

		return retval


	def num_doubles(self, field):
		# double is a pair of images ~ an hour apart taken on the same night
		# the night of the pair is at least a night after the discovery triplet night
		triplet = self.discovery_triplet(field)
		retval = 'no discovery triplet'
		if triplet:
			images = self.field_images(field)
			# NEED TO FIX THIS TO BE MORE SUBTLE ABOUT COLLECTING DOUBLES IN THE NIGHT (if multiple obs)
			# nailings = [im for im in images['obs'] if (im[0] > triplet[1][2][4])]
			double_nights = self.do_doubles_exist(field, triplet[1][2][4])
			retval = len(double_nights)*2

		return retval


	def do_doubles_exist(self, field, last_triplet_image):

		tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, 
			count(image_id) from images 
			where (cfht_field = :field
				and obs_end > :date)
			group by cfht_field, year, month, day 
			having count(image_id) = 2
			order by cfht_field, year, month, day;""")
		pp = {'field': field, 'date':last_triplet_image}
		tplus_res = self.conn.execute(tplus, pp)

		double_nights = []
		for row in tplus_res:  
			double_nights.append(row[1:4])  # year, month, day

		return double_nights



	def export_discovery_triplet(field):
		# write discovery_triplet to a file in VOSpace
		return



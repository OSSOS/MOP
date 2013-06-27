import sqlalchemy as sa
from ossos.overview.ossuary import OssuaryTable
import os, vos, ephem, datetime

CERTFILE=os.path.join(os.getenv('HOME'),
                      '.ssl',
                      'cadcproxy.pem')


class ImagesQuery(object):

	def __init__(self):
		ot = OssuaryTable('images')
		self.images = ot.table
		self.conn = ot.conn


	def field_images(self, field):
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
			ret_images.append([row[1], row[2], (row[3])])  # obs_end, iq_ossos, image_id
		ims_query.close()

		return ret_images


	def field_ra(self, field):
		it = self.images
		ss = sa.select([it.c.cfht_field, it.c.crval_ra])
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)

		retval = 0
		for row in ims_query:
			retval = str(ephem.hours(ephem.degrees(str(row[1])))) 	# HACKED FOR QUICK RESULT (precision not needed)

		return retval


	def field_dec(self, field):
		it = self.images
		ss = sa.select([it.c.cfht_field, it.c.crval_dec])
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)

		retval = 0
		for row in ims_query:
			retval = str(ephem.degrees(str(row[1]))) 	# HACKED FOR QUICK RESULT (precision not needed)

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
		# Is there even a night with 3+ images observed yet?
		threeplus_nights = self.do_triples_exist(field)  # just the dates
		if len(threeplus_nights) > 0:
			# Get details of the images on the nights that have 3+ images, this field.
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
		# input is a dict of {date: [rows of images]} where [rows] > 2
		good_triples = []
		for date, ims in threeplus_night_images.items():
			# for each night, create the best possible triple that meets constraints.
			print date, len(ims)
			# is their temporal span sufficiently wide for a triplet to exist?
			if (ims[-1][4] - ims[0][4]) > datetime.timedelta(minutes=90):
				# return is [im, im, im, worst_iq]
				best_triple_of_night = self.suitable_triples(ims)
				if best_triple_of_night is not None:
					good_triples.append(best_triple_of_night)

		if len(good_triples) > 0:
			# Return the set of 3 images that have the lowest value of 'worst iq'. 
			lowest_worst_iq = min([g[2] for g in good_triples]) 
			print lowest_worst_iq
			retval = good_triples[[g[2] for g in good_triples].index(lowest_worst_iq)]  
		else:
			retval = None
		
		return retval


	def suitable_triples(self, ims):
		triple_sets = []
	 	# construct all possible sets of three 
	 	times = range(0, len(ims))
	 	twenty = datetime.timedelta(minutes=20)
	 	ninety = datetime.timedelta(minutes=90)
		for ii in times:
			for jj in times:
				for kk in times:
					imset = [ims[ii], ims[jj], ims[kk]]
					imset.append(max([im[5] for im in imset]))

					j_minus_i = imset[1][4] - imset[0][4]
					k_minus_j = imset[2][4] - imset[1][4]
					span = imset[2][4] - imset[0][4]
	 				if (ii < jj < kk) and (j_minus_i > twenty) and (k_minus_j > twenty) and span > ninety:
						triple_sets.append(imset)

		# return the set with the lowest worst iq. All have adequate spacing.
		triple_sets.sort(key=lambda x:x[3])
		# if there's one available in triple_sets!
		if len(triple_sets) > 0:
			# format as ([image_ids], [3 rows of remaining info], worst_iq)
			retval = ([t[3] for t in triple_sets[0][0:3]], triple_sets[0][0:3], triple_sets[0][3]) 
		else:
			retval = None

		return retval



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
		# nailings: single images, at least one night after the night of the discovery triplet
		triplet = self.discovery_triplet(field)
		retval = 'no discovery triplet'
		if triplet:
			images = self.field_images(field)
			after = [im for im in images['obs'] if 
					((im[0] > triplet[1][2][4]) 
						and im[0].strftime('%Y-%m-%d') != triplet[1][2][4].strftime('%Y-%m-%d'))]

			retval = len(after)

		return retval


	def num_doubles(self, field):
		# double is a pair of images ~ an hour apart taken on the same night
		# the night of the pair is at least a night after the discovery triplet night
		# there are no precovery doubles
		triplet = self.discovery_triplet(field)
		retval = 'no discovery triplet'
		if triplet:
			images = self.field_images(field)
			double_nights = self.do_doubles_exist(field, triplet[1][2][4])
			retval = len(double_nights)*2

		return retval


	def do_doubles_exist(self, field, last_triplet_image):

		# NEED TO ACCOUNT FOR >2 IMAGES ON THE SAME NIGHT THAT COULD FORM A DOUBLE AS A SUBSET

		tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, obs_end,
			count(image_id) from images 
			where (cfht_field = :field
				and obs_end > :date)
			group by cfht_field, year, month, day, obs_end 
			having count(image_id) > 1
			order by cfht_field, year, month, day, obs_end;""")
		pp = {'field':field, 'date':last_triplet_image}
		tplus_res = self.conn.execute(tplus, pp)

		double_nights = []
		for row in tplus_res:  # Filtering output for now, can't get condition into select yet.

			# NEED THIS TO ACCOUNT FOR IMAGES ALSO BEING SPACED BY AT LEAST HALF AN HOUR

			print row[5].strftime('%Y-%m-%d'),last_triplet_image.strftime('%Y-%m-%d')
			if row[5].strftime('%Y-%m-%d') != last_triplet_image.strftime('%Y-%m-%d'):
				double_nights.append(row[1:4])  # year, month, day

		return double_nights



	def export_discovery_triplet(self, field):
		# write discovery_triplet to a file in VOSpace
		triplet = self.discovery_triplet(field)

		if triplet:
#			try:
			stem = 'vos:OSSOS/triplets/'
			vospace = vos.Client(certFile=CERTFILE)
			# TESTING TESTING REMOVE BEFORE FLIGHT
			tmpfile = 'E_13A_discovery_expnums.txt'
			blockuri = stem + tmpfile

			# does a file for this block already exist in VOSpace? If so, copy it back.
			if vospace.access(blockuri):  # Does this work this way?
				# vospace.create(blockuri)
				vospace.copy(blockuri, './'+tmpfile)
				print tmpfile, '<-', blockuri
				# the field is already present
				with open(tmpfile, 'r+') as scratch:
					lines = scratch.readlines()
					for line in lines:				
						if line.split()[3] == field:  # It's present already. Add updating later
							return
					scratch.write('%s %s %s %s\n' % (triplet[0][0], triplet[0][1], triplet[0][2], field))
			else:
				with open(tmpfile, 'w') as scratch:
					# 3-line file: id id id field
					scratch.write('%s %s %s %s\n' % (triplet[0][0], triplet[0][1], triplet[0][2], field))

			vospace.copy(tmpfile, blockuri)
			print tmpfile, '->', blockuri

#			except Exception, e:
#				print e

		return







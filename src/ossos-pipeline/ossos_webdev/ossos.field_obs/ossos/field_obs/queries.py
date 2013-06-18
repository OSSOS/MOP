import sqlalchemy as sa
import ossos.overview.queries as oq
import ephem


class ImagesQuery(object):

	def __init__(self):
		ot = oq.ossuaryTable('images')
		self.images = ot.table
		self.conn = ot.conn


	def field_images(self, field):
		if not(isinstance(field, str)):
			msg = '"{0}" is not a string'.format(type(field))
			raise ValueError(msg)

		it = self.images
		cols = [it.c.cfht_field, it.c.obs_end, it.c.iq_ossos, it.c.image_id]
		ss = sa.select(cols)
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)
		ret_images = self.format_imquery_return(ims_query)

		retval = {'obs':ret_images}
		print retval
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

		for row in ims_query:
			retval = row[1] 	# HACKED FOR QUICK RESULT (precision not needed)

		return retval


	def field_dec(self, field):
		it = self.images
		ss = sa.select([it.c.cfht_field, it.c.crval_dec])
		ss.append_whereclause(it.c.cfht_field == field)
		ims_query = self.conn.execute(ss)

		for row in ims_query:
			retval = row[1] 	# HACKED FOR QUICK RESULT (precision not needed)

		return retval


	def find_field_triples(self, field):
		tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, 
			count(image_id) from images 
			where cfht_field = :field
			group by cfht_field, year, month, day 
			having count(image_id) > 2
			order by cfht_field, year, month, day;""")
		pp = {'field': field}
		tplus_res = self.conn.execute(tplus, pp)

		threeplus_fields = {}
		for row in tplus_res:  
			field = row[0]  # preceding 'u' indicates unicode
			night = row[1:4]
			field_nights = threeplus_fields.get(field, [])
			field_nights.append(night)
			threeplus_fields[field] = field_nights

		return threeplus_fields


	def what_fields_have_any_observations(self):
		it = self.images
		ss = sa.select([it.c.cfht_field],
						distinct=it.c.cfht_field,
						order_by=it.c.cfht_field)
		
		fields = [{'fieldId': n[0], 'triplet':None} for n in self.conn.execute(ss)]

		return fields


	def do_triples_exist(self):

		tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, 
			count(image_id) from images 
			group by cfht_field, year, month, day 
			having count(image_id) > 2
			order by cfht_field, year, month, day;""")
		tplus_res = self.conn.execute(tplus)

		threeplus_fields = {}
		for row in tplus_res:  
			field = row[0]  # preceding 'u' indicates unicode
			night = row[1:4]
			field_nights = threeplus_fields.get(field, [])
			field_nights.append(night)
			threeplus_fields[field] = field_nights

		return threeplus_fields

		

	def discovery_triplet(self, field):
	#	triples = oq.link_images_to_tripleplus_nights()
	    # tt = field_triples(field)
		retval = [long('1607779'), long('1609158'), long('1612253')]  # TESTING

		return retval


	def num_precoveries(self, field):
		# how many of the images in self.observations occur 
		# before the first image of the discovery triplet?
		precoveries = []

		triples = self.discovery_triplet(field)


		return precoveries


	def num_nailings(self, field):
		# nailings are single images that occur
		# at least one night after the night of the discovery triplet?
		nailings = []

		return nailings


	def num_doubles(self, field):
		# double is a pair of images ~ an hour apart taken on the same night
		# the night of the pair is at least a night after the discovery triplet night

		doubles = []
		return doubles



	def export_discovery_triplet(field):
		# write discovery_triplet to a file in VOSpace
		return





# then which are the three best in that night if there are more than three
# but it has to be a little more complex than that, because they can't just be
# the best three, it has to be the best widest-spaced three.

# and if there are multiple sets of three...
# which is the best overall set out of each field's threeplus_nights.
# Define 'best' by dispersion of seeing from the mean, of those that meet the timing constraint.

# if len(threeplus_fields.keys()) > 0:
# 	for field, dates in threeplus_fields.items():  # TESTING 
# 		print field 
# 		for date in dates:
# 			night_images = images_in_tripleplus_night(field, date)
			# nt = night_images.keys()
			# nt.sort()

			# print nt[0].strftime('%Y-%m-%d')
			# for n in nt:
			# 	print n.strftime('%H:%M:%S'), night_images[n][1]

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

# accept triplet for searching if IQ of each image better than 0.8"?
# or may have to relax that.


# cfht_field | year | month | day | count 
# ------------+------+-------+-----+-------
#  E+0+0      | 2013 |     3 |   7 |     3
#  E+0+0      | 2013 |     4 |   7 |     7
#  E+0+0      | 2013 |     4 |   8 |     4
#  E+0+0      | 2013 |     4 |   9 |     3
#  E+0+1      | 2013 |     3 |   7 |     3
#  E+0+1      | 2013 |     4 |   9 |     3
#  E+0-1      | 2013 |     2 |   8 |     6
#  E+0-1      | 2013 |     4 |   4 |     3
#  E+1+0      | 2013 |     4 |   9 |     3
#  E+1+1      | 2013 |     4 |   9 |     3
#  E+1-1      | 2013 |     3 |   7 |     3
#  E+1-1      | 2013 |     4 |   9 |     3
#  E+2+0      | 2013 |     4 |   9 |     3
#  E+2+0      | 2013 |     4 |  19 |     3
#  E+2+1      | 2013 |     4 |   9 |     3
#  E+2-1      | 2013 |     4 |   9 |     3
#  E+3+0      | 2013 |     4 |   7 |     4
#  E+3+0      | 2013 |     4 |   8 |     4
#  E+3+0      | 2013 |     4 |   9 |     6
#  E+3+1      | 2013 |     3 |   7 |     3
#  E+3+1      | 2013 |     4 |   7 |     3
#  E+3+1      | 2013 |     4 |   8 |     5
#  E+3+1      | 2013 |     4 |   9 |     5
#  E+3-1      | 2013 |     3 |   7 |     3
#  E+3-1      | 2013 |     4 |   7 |     5
#  E+3-1      | 2013 |     4 |   9 |     3
#  E-1+0      | 2013 |     4 |   4 |     3
#  E-1+0      | 2013 |     4 |   5 |     4
#  E-1+1      | 2013 |     4 |   4 |     3
#  E-1-1      | 2013 |     2 |   8 |     3
#  E-1-1      | 2013 |     4 |   4 |     3
#  E-1-1      | 2013 |     4 |   5 |     6
#  E-2+0      | 2013 |     2 |   8 |     4
#  E-2+0      | 2013 |     4 |   4 |     3
#  E-2+1      | 2013 |     4 |   4 |     3
#  E-2-1      | 2013 |     4 |   4 |     3
#  E-2-1      | 2013 |     4 |   5 |     3
#  E-3+0      | 2013 |     2 |   8 |     3
#  E-3+0      | 2013 |     4 |   4 |     3
#  E-3+0      | 2013 |     4 |   5 |     4
#  E-3+1      | 2013 |     4 |   4 |     3
#  E-3-1      | 2013 |     4 |   4 |     3
#  E-3-1      | 2013 |     4 |   5 |     5
#  WP-E-5-2   | 2013 |     3 |  11 |     5




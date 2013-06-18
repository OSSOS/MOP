from sqlalchemy import *
from numpy import median
import ossos.field_obs.queries as foq
from ossos.overview.ossuary import OssuaryTable


def all_blocks(images):
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


def format_imquery_return(ims_query):
	ret_images = []
	for row in ims_query:
		ret_images.append([row[1], row[2], (row[3])])
	ims_query.close()

	return ret_images


def parse_blockID(blockID):
	if blockID.__contains__('WP'):
		retval = '-'.join(blockID.split('-')[0:2])
	else:  # it's an E or O field, less fancy!
		retval = blockID.partition('-')[2]

	return retval


def block_images(blockID, images):

	blockstart = parse_blockID(blockID)

	# number of images on all fields that are in the block
	ss = text("""
		select cfht_field, obs_end, iq_ossos, image_id from images where cfht_field like :blockstart;
		""")
	pp = {'blockstart':blockstart+'%'}  # NOTE THE HACK: ISN'T WORKING ON WP FIELDS
	ims_query = images.conn.execute(ss, pp)
	ret_images = format_imquery_return(ims_query)

	retval = {'obs':ret_images}

	return retval


def most_recent_block_completion():

	retval = 'Awaiting May data processing to be sure'
	return retval
		

def images_in_tripleplus_night(field, date, images):

	ss = text(
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
	tplus_res = images.conn.execute(ss, pp)

	ret_images = []
	for row in tplus_res:
		ret_images.append([row[5], row[4], (row[6])])
	tplus_res.close()

	return ret_images


def fields_in_block(images, blockID):
	parsing = parse_blockID(blockID)
	observed_fields = foq.what_fields_have_any_observations(images)  # list of dicts
	# filter these for the ones that are in this block
	retval = []
	for obs in observed_fields:
		if obs['fieldId'].startswith(parsing):
			retval.append(obs)

	return retval


def link_images_to_tripleplus_nights(images, blockID):

	retval = fields_in_block(images, blockID)

	# now add in the triples info for those fields that have it
	threeplus_fields = foq.do_triples_exist(images)

	for field, nights in threeplus_fields.items():
		retfs = [n for n in retval if n['fieldId'] == field]
		if len(retfs) > 0:
			retfield = retfs[0]
			for night in nights[0:1]:  # TESTING
				images_info = images_in_tripleplus_night(field, night, images)
				retfield['triplet'] = images_info
				retfield['worstIQ'] = max([n[2] for n in images_info])

	return retval




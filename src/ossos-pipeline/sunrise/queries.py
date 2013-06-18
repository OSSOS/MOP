# handles the querying in sqlalchemy for ossuary db
# 
# MTB 14 May 2013

from sqlalchemy import *


def connect_to_images():
	# Development testing database on local machine provided by Postgres.App
	engine = create_engine('postgresql://localhost/ossuary', echo=False)  
	metadata = MetaData(bind=engine)
	images = Table('images', metadata, autoload=True, autoload_with=engine)  # reflect existing table
	conn = engine.connect()

	return conn, images


def retrieve_processed_images():
	conn, images = connect_to_images()

	ims = select([images.c.image_id])
	ims_out = [n[0] for n in conn.execute(ims)]  # clean this up to just be the image_ids

	return ims_out


def put_image_in_database(image):
	conn, images = connect_to_images()

	ins = images.insert(values=image)
	conn.execute(ins)

	return


def do_triples_exist():
	conn, images = connect_to_images()

	tplus = text("""
		select cfht_field, extract(year from obs_end) as year, 
		extract(month from obs_end) as month, extract(day from obs_end) as day, 
		count(image_id) from images 
		group by cfht_field, year, month, day 
		having count(image_id) > 2
		order by cfht_field, year, month, day;""")
	tplus_res = conn.execute(tplus)

	threeplus_fields = {}
	for row in tplus_res:  
		field = row[0]  # preceding 'u' indicates unicode
		night = row[1:4]
		field_nights = threeplus_fields.get(field, [])
		field_nights.append(night)
		threeplus_fields[field] = field_nights

	return threeplus_fields
	

def images_in_tripleplus_night(field, date):
	conn, images = connect_to_images()
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
	tplus_res = conn.execute(ss, pp)

	ret_images = {}
	for row in tplus_res:
		ret_images[row[5]] = (row[4], row[6])
	tplus_res.close()

	return ret_images

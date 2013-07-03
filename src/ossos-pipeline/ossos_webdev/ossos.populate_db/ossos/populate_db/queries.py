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



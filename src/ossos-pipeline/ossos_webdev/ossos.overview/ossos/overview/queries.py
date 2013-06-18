# handles the querying in sqlalchemy for ossuary db
# 
# MTB 14 May 2013

from sqlalchemy import *
import math, ephem, pytz, datetime


class ossuaryTable(object):

	def __init__(self, tablename):
		# reflect_table_from_ossuary
		# Development testing database on local machine provided by Postgres.App
		engine = create_engine('postgresql://localhost/ossuary', echo=False)  
		metadata = MetaData(bind=engine)
		table = Table(tablename, metadata, autoload=True, autoload_with=engine)  # reflect existing table
		conn = engine.connect()

		self.tablename = tablename
		self.table = table
		self.conn = conn





# Defs specifically relating to display of Overview page. 

def survey_proportion_complete(images):


	retval = 'unknown'  # UPDATE THIS
	return retval


def fields_observed_to_completion(images):


	retval = '0'  # UPDATE THIS
	return retval


def fields_processed(images):


	retval = '0'
	return retval


def survey_efficiency(images):

	retval = 'unknown'
	return retval



def most_recent_observation(images):

	ss = text("""
		select obs_end from images order by obs_end;
		""")
	dates = [n[0] for n in images.conn.execute(ss)]

	return dates[-1]


def next_moondark():  # this is producing an off by one error: fix!
	mn = ephem.Moon()
	mp = []
	for n in range(0,29*4):
		mn.compute(ephem.now()+(n/4.))
		mp.append((ephem.now()+(n/4.), mn.moon_phase))
	next_new_moon = ephem.Date(min(mp, key=lambda x:x[1])[0]).datetime()
#	next_new_moon = pytz.utc.localize(next_new_moon)  # it's calculated in UTC
#	hst = pytz.timezone('HST')

	retval = next_new_moon #.astimezone(hst)

	return retval


def next_observing_window():
	# we observe if the near-new moon is down, or if the moon is up 
	# but we are within 3 days of new moon. 

	next_nm = next_moondark()
	next_period = (next_nm-datetime.timedelta(3), next_nm+datetime.timedelta(3))

	if next_period[0] < datetime.datetime.now() < next_period[1]:
		retval = 'tonight (hopefully).'
	else:
		 retval = 'in no more than ' + str((next_period[0]-datetime.datetime.now()).days) + ' days.'

	return retval


def megacam_schedule():
	# tuples bracket the ends of date ranges that MegaCam is on the telescope
	schedule = [(datetime.datetime(2013,5,31), datetime.datetime(2013,6,14)), 
		(datetime.datetime(2013,07,01), datetime.datetime(2013,7,14)),
		(datetime.datetime(2013,07,29), datetime.datetime(2013,7,31))
		]
	return schedule


def nearest_megacam_run():
	now = datetime.datetime.now()
	schedule = megacam_schedule()
	for run in schedule:
		if (run[0]-now > datetime.timedelta(0)) or (run[1]-now > datetime.timedelta(0)):
			nearest_run = run
			break
	
	if (nearest_run[0] <= now <= nearest_run[1]):
		retval = 'now.'
	else:
		if (nearest_run[0]-now).days > 0:
			retval = "in "+ str((nearest_run[0]-now).days) + " days."
		else:
			hrs = (nearest_run[0]-now).seconds/3600.
			retval = "in "+ '%2.1f' % hrs + " hours."

	return retval
	


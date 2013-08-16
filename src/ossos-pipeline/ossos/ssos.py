__author__ = 'Michele Bannister'

import urllib
import urllib2
import cStringIO
from ossos.mpc import MPCWriter


class SSOSQuery(object):
	""" Query the CADC's Solar System Object search
	for a given set of MPC-formatted moving object detection lines. 
	Inputs:
	- a list of ossos.mpc.Observation instances
	Optional:
	- a tuple of the start and end times to be searched between. Format '%Y-%m-%d'
	Otherwise the temporal range defaults to the start of OSSOS surveying & the present day.

	"""

	def __init__(self, observations, daterange=None):
		self.observations = observations
		if daterange is None:  # default to retrieving images across span of survey
			self.date_start = '2013-02-07' # first survey observations are on 2013-02-08
			self.date_end = datetime.datetime.now().strftime('%Y-%m-%d')
		else:
			self.date_start = daterange[0]
			self.date_end = daterange[1]


	def query(self):
		url = self.format_url()
		header = {'User-Agent':'OSSOS Target Characterisation'}
		query = urllib2.Request(url, header)
		try:
    		response = urllib2.urlopen(query)
 			print response.info()
			html = response.read()

			retval = {}
			for line in html:

				print line
				# formatting of the retval: let's just make a dict for now
				if line.startswith('16'): # HACK FOR NOW

				def format_tableline(line):
					# table is formatted as
					tablefields = ['Image', 'Ext', 'X', 'Y', 'MJD', 'Filter',
								   'Exptime', 'Object_RA', 'Object_Dec', 'Image_target',
								   'Telescope_Instrument', 'MetaData','Datalink']
 
					# also exclude from consideration if exposure time is < 200 sec: it's wallpaper
					for ct, item in 
					for ct, item in enumerate(line.split('\t')):
						if item != '-9999': # Ext, X, Y show this when header WCS not yet updated



			# TODO: make the output formatting of B&K data match that of JJ's pyOrbfit 
			# https://github.com/ijiraq/pyOrbfit

			response.close()

	 	except URLError as e:
		    if hasattr(e, 'reason'):
		        print 'Failed to reach the server.'
		        print 'Reason: ', e.reason
		    elif hasattr(e, 'code'):
		        print 'The server couldn\'t fulfill the request.'
		        print 'Error code: ', e.code
			else:
				# all fine.

		return


	def format_url(self):
		base = 'http://www3.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssos.pl?'

		# http://beta.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssosclf.pl?format=tsv&verbose=true&
		# obs=+++++DRY0002*+C2013+04+04.40583+14+02+28.179-12+57+52.71+++++++++23.43r++++++568+1615904p33+2026.46+725.35+%0D%0A+++++DRY0002++C2013+04+04.44572+14+02+27.988-12+57+51.52+++++++++23.55r++++++568+1615914p33+2037.97+731.52+%0D%0A+++++DRY0002++C2013+04+04.48637+14+02+27.800-12+57+50.15+++++++++23.68r++++++568+1615924p33+2046.68+737.95+%0D%0A&
		# search=bern&epoch1=2013-01-01&epoch2=2013-08-13&eunits=none&extres=yes&xyres=yes

		# format=tsv loads only the table of observations; verbose=true also gets B&K orbit info.
		query_args = {'format':'tsv', 'verbose':'true', 'epoch1':date_start, 'epoch2':date_end,
					  'search':'bern', 'eunits':'none', 'extres':'yes', 'xyres':'yes'}

		mpc_obs = MPCWriter(cStringIO.StringIO(), auto_flush=False)
		for obs in self.observations:
			mpc_obs.write(obs)           # FIXME: should it be '\r\n': carriage return + line feed?
		query_args['obs'] = mpc_obs.flush()  
		mpc_obs.close()

		encoded_query = urllib.urlencode(query_args)
		retval = base + encoded_query

		return retval






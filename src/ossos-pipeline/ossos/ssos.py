__author__ = 'Michele Bannister'

import urllib
import urllib2
import cStringIO
import datetime
from ossos.mpc import URLWriter


class SSOSQuery(object):

    def __init__(self, observations,
                 daterange=('2013-02-07', datetime.datetime.now().strftime('%Y-%m-%d'))):
        """
        :param observations: a list of ossos.mpc.Observation instances
        :param daterange: a tuple of the start and end times to be searched between. Format '%Y-%m-%d'
        """
        self.observations = observations
        self.date_start = daterange[0]
        self.date_end = daterange[1]
        self.url = None

    def query(self, ossos_only=True):
        """
        Send the correctly formatted query to SSOS.
        :param ossos_only: restrict the output to only images from the OSSOS survey.
        :return: Appropriate input to ImageSlicer. Dictionary keyed by
        ['Image', 'ccd', 'X', 'Y', 'MJD', 'Filter',
        'Exptime', 'Object_RA', 'Object_Dec', 'Image_target',
        'Telescope_Instrument', 'Datalink']
        """
        self.url = self.format_url()
        header = {"User-Agent": 'OSSOS Target Characterisation'}
        query = urllib2.Request(self.url, header)

        try:
            response = urllib2.urlopen(query)
            html = response.read()
            available_data = self.format_response(html)
            if ossos_only:
                self.restrict_to_ossos_data(available_data)
            response.close()

        except urllib2.URLError as e:
            if hasattr(e, 'reason'):
                print 'Failed to reach the server.'
                print 'Reason: ', e.reason
            elif hasattr(e, 'code'):
                print 'The server couldn\'t fulfill the request.'
                print 'Error code: ', e.code

        return retval

    def format_url(self, verbose=False):
        """
        Format the URL for SSOS requirements.
        :param self:
        :param verbose: format=tsv loads only the table of observations; verbose=true also gets B&K orbit info.
        :return: A URL correctly formatted for a SSOS query.
        """
        base = 'http://www3.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/ssos/ssos.pl?'
        query_args = dict(format='tsv', verbose=verbose, epoch1=self.date_start, epoch2=self.date_end, search='bern',
                          eunits='none', extres='yes', xyres='yes')

        mpc_obs = cStringIO.StringIO()
        writer = URLWriter(mpc_obs, auto_flush=False)
        for obs in self.observations:
            writer.write(obs)
        writer.flush()
        mpc_obs.seek(0)
        encoded_query = urllib.urlencode(query_args)
        retval = base + encoded_query + '&obs=' + mpc_obs.readline()

        return retval

    def format_response(self, html):
        retval = []
        # file parsing for verbose=True not implemented yet.
        for line in html[1:]: # first line is table column labels
            if line != 'An error occured getting the ephemeris':  # query may be incorrectly formatted.
                rr = self.format_tableline(line)
                if len(rr.keys()) > 0:
                    retval.append(rr)

        return retval

    def format_tableline(self, line):
        """
        Parse table line into returnable values.
        :param line: line from SSOS Request return
        :return: dict of useful values. If the values don't exist, keyword won't be in dictionary.
        """
        table_fields = ['Image', 'Ext', 'X', 'Y', 'MJD', 'Filter',
                        'Exptime', 'Object_RA', 'Object_Dec', 'Image_target',
                        'Telescope_Instrument', 'Datalink']
        retval = {}
        for ct, item in table_fields:
            ln = line.split('\t')
            if ln[ct] != '-9999': # Ext, X, Y show this when the header WCS is not yet updated
                retval[item] = line[ct]

        return retval

    def restrict_to_ossos_data(self, data):
        retval = []
        for observation in data:
            # want our telescope, and exposures > 200 sec to keep out the wallpaper
            assert observation.has_key('Telescope_Instrument')
            assert observation.has_key('Exptime')
            if observation['Telescope_Instrument'] == 'CFHT/MegaCam' and float(observation['Exptime']) > 200.:
                retval.append(observation)

        return retval
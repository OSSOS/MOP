from pyramid.view import view_config
from zope.cachedescriptors import property

from .queries import BlockQuery


class Block(object):
    def __init__(self, request):
        self.request = request
        self.blockQuery = BlockQuery()
        # BIT HACKED but works, until I figure out how to do it properly
        # reengineer for robustness with urllib.
        self.blockID = request.current_route_url().rpartition('/')[2]

    @property.Lazy
    def num_fields(self):
        empty_units, fields = self.blockQuery.fields_in_block(self.blockID)
        retval = len(fields)
        return retval

    @property.Lazy
    def observed_fields(self):
        rv = self.blockQuery.block_discovery_triples(self.blockID)
        # print rv
        # both processing and blinking status only have to show the discovery triplets
        # proc_rv = self.blockQuery.bk.get_processing_status(rv)
        # blink_rv = self.blockQuery.block_blinking_status(self.blockID)
        return rv  # MODIFY TO SHOW PROCESSING STATUS AND BLINKING STATUS

    @property.Lazy
    def central_radec(self):
        ret = self.blockQuery.central_radec(self.blockID)
        # parse just the first bit for niceness
        ra = ret[0].split(':')[0]
        ra2 = ret[0].split(':')[1]
        dec = ret[1].split(':')[0]
        dec2 = ret[1].split(':')[1] + "'"
        return (ra, ra2, dec, dec2)

    @property.Lazy
    def ecliptic_lat_span(self):
        retval = self.blockQuery.ecliptic_lat_span(self.blockID)
        return retval

    @property.Lazy
    def numObs(self):
        retval = self.blockQuery.num_block_images(self.blockID)
        return retval

    @property.Lazy
    def num_precoveries(self):
        retval = self.blockQuery.block_precoveries(self.blockID)
        return retval

    @property.Lazy
    def num_nailings(self):
        retval = self.blockQuery.block_nailings(self.blockID)
        return retval

    @property.Lazy
    def num_doubles(self):
        retval = self.blockQuery.block_doubles(self.blockID)
        return retval


    @view_config(route_name='block', renderer='block.pt', permission='ossos')
    def inspect_observations(self):
        # uid = self.request.matchdict['uid']
        # return dict(title='Triplet', uid=uid)

        retval = {'blockID': self.blockID,
                  'num_fields': self.num_fields,
                  'observedFields': self.observed_fields,
                  'radec': self.central_radec,
                  'ec_lat': self.ecliptic_lat_span,
                  'totalObs': self.numObs,
                  'precoveries': self.num_precoveries,
                  'nailings': self.num_nailings,
                  'doubles': self.num_doubles
        }
        return retval


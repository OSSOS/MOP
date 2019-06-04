from math import degrees

from zope.cachedescriptors.property import Lazy
from pyramid.view import view_config
import ephem

from .queries import ImagesQuery


class SimpleStatus(object):
    def __init__(self, componentName):
        self.componentName = componentName

    def render(self):
        retval = '{0}&nbsp;&#10003;'.format(self.componentName)
        return retval


class ErrorStatus(object):
    def __init__(self, error, image_id):
        self.error = error
        self.image_id = str(image_id)

    def render(self):
        retval = ''
        for component, errors in list(self.error.items()):
            retval += '<span class="text-error">{0}&nbsp;&#x2718; </span>'.format(component)
            assert isinstance(errors, dict)
            for errtype, ccds in list(errors.items()):
                assert isinstance(ccds, list)
                if len(ccds) > 30:  # how many errors ARE there?
                    retval += '{0}/36: BAD!'.format(len(ccds))
                # FIXME: Add a link to at least one of the bad ones
                else:
                    for ccd in ccds:
                        ccd_url = self.joblog_url(component, ccd)
                        retval += '<a href="' + ccd_url + '">' + str(ccd) + '</a> '

        return retval

    def joblog_url(self, component, ccd):
        canfar_url = 'http://www.canfar.phys.uvic.ca/vospace/nodes/OSSOS/joblog/'
        # if component == 'plant':  # this job type doesn't produce plant.txt files...FIXME
        # component = 'Object.planted'  # this file doesn't actually have a .txt at the end, either
        # if component.startswith('fk'):
        # component = 'fk_'+ component.lstrip('fk')
        # if component != 'plant':  # grr getting convoluted...
        #     retval = canfar_url + component +  '/' + self.image_id + '.txt'
        # else:
        retval = canfar_url + component + '/' + self.image_id + '.txt?view=data'

        return retval


def mk_status(val, image_id):
    if isinstance(val, str):
        retval = SimpleStatus(val)
    else:
        assert isinstance(val, dict)
        retval = ErrorStatus(val, image_id)

    return retval


class Field(object):
    def __init__(self, request):
        self.request = request
        self.imagesQuery = ImagesQuery()

        # BIT HACKED - FIX! Transfer to urllib parsing. More Reliable.
        fi = request.current_route_url().rpartition('/')[2]
        if fi.__contains__('%2B'):
            fi2 = fi.split('%2B')
            fi3 = '+'.join(fi2)
            self.fieldId = fi3
        else:
            self.fieldId = fi

        print(self.fieldId)

    @Lazy
    def discovery_triplet(self):
        retval = self.imagesQuery.discovery_triplet(self.fieldId)
        return retval

    @Lazy
    def observations(self):
        rv = self.imagesQuery.field_images(self.fieldId)
        proc_rv = self.imagesQuery.processing_status(rv, update=True)  # use this to trigger renewal

        # format the errors in html with links to their joblogs
        retproc = []
        for row in proc_rv:
            statuses = [mk_status(s, row[2]) for s in row[len(row) - 1]]  # statuses are always at the end
            retrow = row[:-1]  # without the unformatted errors
            retrow.append(statuses)
            retproc.append(retrow)

        retval = {'obs': retproc}
        return retval

    @Lazy
    def ra(self):
        ret = self.imagesQuery.field_ra(self.fieldId)
        # parse just the first bit for niceness
        retval = (ret.split(':')[0], ret.split(':')[1])
        return retval

    @Lazy
    def dec(self):
        ret = self.imagesQuery.field_dec(self.fieldId)
        # FIXME: need these to catch the ones that are being stored as ints and parse them properly
        dec = ret.split(':')[0]
        dec2 = ret.split(':')[1] + "'"
        retval = (dec, dec2)
        return retval

    @Lazy
    def ecliptic_loc(self):
        rr = ephem.Equatorial(ephem.hours(self.imagesQuery.field_ra(self.fieldId)),
                              ephem.degrees(self.imagesQuery.field_dec(self.fieldId)))
        ec = ephem.Ecliptic(rr)
        retval = (degrees(ec.lat), str(ec.lon))  # eclat is float (deg), eclon is str in deg
        return retval

    @Lazy
    def numObs(self):
        retval = len(self.observations['obs'])
        return retval

    @Lazy
    def num_precoveries(self):
        retval = self.imagesQuery.num_precoveries(self.fieldId)
        return retval

    @Lazy
    def num_nailings(self):
        retval = self.imagesQuery.num_nailings(self.fieldId)
        return retval

    @Lazy
    def num_doubles(self):
        retval = self.imagesQuery.num_doubles(self.fieldId)
        return retval

    # Only uncomment and set this active when a triplet file in vos:OSSOS/triplets needs to be generated.
    # @property
    # def export_triplet(self):   # CHECK TMPFILE HAS BEEN SET TO CORRECT BLOCK
    # retval = self.imagesQuery.export_discovery_triplet(self.fieldId)


    @view_config(route_name='field_obs', renderer='field_obs.pt', permission='ossos')
    def inspect_observations(self):
        # uid = self.request.matchdict['uid']
        # return dict(title='Triplet', uid=uid)

        retval = {'fieldId': self.fieldId,
                  'observations': self.observations,
                  'ra': self.ra,
                  'dec': self.dec,
                  'ec_loc': self.ecliptic_loc,
                  'discovery_triplet': self.discovery_triplet,
                  'totalObs': self.numObs,
                  'precoveries': self.num_precoveries,
                  'nailings': self.num_nailings,
                  'doubles': self.num_doubles,
                  # 'update_vospace': self.export_triplet
        }
        return retval

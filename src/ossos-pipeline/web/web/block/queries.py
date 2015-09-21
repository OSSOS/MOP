from math import degrees
import os
from collections import OrderedDict

import ephem

import sqlalchemy as sa
from planning.plotting import parameters
from web.field_obs.queries import ImagesQuery


# these are the raw IDs as the telescope has programmed them into the headers
# OSSOS_BLOCKS = ['E', 'O', 'WP-E', 'WP-O', \
# '13BL', '13BH', 'WP-13BL', 'WP-13BH']


class BlockQuery(object):
    def __init__(self):
        self.bk = ImagesQuery()

    def all_blocks(self):

        status = OrderedDict.fromkeys(parameters.BLOCKS.keys())
        status['13AE'] = ['discovery complete', '50', '24.05']
        status['13AO'] = ['discovery complete', '36', '24.40']
        status['13BL'] = ['discovery complete', '79', '24.48']
        status['14BH'] = ['discovery running', '-', '-']
        status['15AP'] = ['discovery running', '-', '-']
        status['15AM'] = ['discovery running', '-', '-']

        '''Overview tal table is expecting:
        ID   observations   processing status   discoveries  m_r 40%
        '''
        bks = []
        for block in status.iterkeys():
            bk = [block, self.num_block_images(block)]  # if set in the .fromkeys(), doesn't give a unique list
            if status[block] is not None:
                bk = bk + status[block]
            else:
                bk = bk + ['awaiting triplets', '-', '-']
            bks.append(bk)

        retval = {'blocks': bks, 'status': status}

        return retval


    def fields_in_block(self, blockID):  # REWRITE THIS AS INEFFICIENT
        # it = self.bk.images
        # ss = sa.select(it.c.cfht_field, it.c.cfht_field.like('{0}%'.format(blockID)),
        # group_by=it.c.cfht_field)
        # ims_query = self.bk.conn.execute(ss)
        # retval = [r[1] for r in ims_query]

        all_observed_fields = self.bk.what_fields_have_any_observations()  # [dicts]
        # filter these for the ones that are in this block
        retval = []
        retval2 = []
        for obs in all_observed_fields:
            if obs['fieldId'].startswith(blockID):
                retval.append(obs)
                retval2.append(obs['fieldId'])

        return retval, retval2  # returns a list of dictionaries. Also return just keys?


    def format_imquery_return(self, ims_query):
        ret_images = []
        for row in ims_query:
            ret_images.append([row[1], row[2], (row[3])])
        ims_query.close()

        return ret_images


    def num_block_images(self, blockID):
        # number of images on all fields that are in the block
        it = self.bk.images
        cols = [it.c.cfht_field, sa.func.count(it.c.image_id)]
        ss = sa.select(cols, it.c.cfht_field.like('{0}%'.format(blockID)),
                       group_by=it.c.cfht_field)

        ims_query = self.bk.conn.execute(ss)
        retval = sum([r[1] for r in ims_query])
        return retval


    def central_radec(self, blockID):
        junk, fieldIds = self.fields_in_block(blockID)
        ras = []
        decs = []
        for field in fieldIds:
            ras.append(ephem.hours(self.bk.field_ra(field)))  # str in hours of RA
            decs.append(ephem.degrees(self.bk.field_dec(field)))  # str in deg of Dec

        # mean ra, mean dec: APPROXIMATING
        ra = ephem.hours((sum(ras) / len(ras)))
        dec = ephem.degrees((sum(decs) / len(decs)))

        retval = (str(ra), str(dec))

        return retval


    def ecliptic_lat_span(self, blockID):
        junk, fieldIds = self.fields_in_block(blockID)
        ecs = []
        for field in fieldIds:
            rr = ephem.Equatorial(ephem.hours(self.bk.field_ra(field)), ephem.degrees(self.bk.field_dec(field)))
            ec = ephem.Ecliptic(rr)
            ecs.append(ec)
        ecs.sort(key=lambda x: x.__getattribute__('lat'))

        retval = (degrees(ecs[0].lat), degrees(ecs[-1].lat))  # eclat is float (deg), min-max

        return retval


    def block_discovery_triples(self, blockID):

        retval, fieldIds = self.fields_in_block(blockID)
        for field in fieldIds:
            triplet = self.bk.discovery_triplet(field)
            if triplet is not None:  # yay we have enough observations to have a discovery triplet!
                retfield = [n for n in retval if n['fieldId'] == field][0]
                retfield['triplet'] = triplet[1]
                retfield['worstIQ'] = triplet[2]

        return retval


    def block_precoveries(self, blockID):
        if not blockID.__contains__('WP'):
            empty_units, fields = self.fields_in_block(blockID)
            retval = 0
            for field in fields:
                pc = self.bk.num_precoveries(field)
                if isinstance(pc, int):
                    retval += pc
        else:
            retval = '-'

        return retval


    def block_nailings(self, blockID):
        if not blockID.__contains__('WP'):
            empty_units, fields = self.fields_in_block(blockID)
            retval = 0
            for field in fields:
                nc = self.bk.num_nailings(field)
                if isinstance(nc, int):
                    retval += nc
        else:
            retval = '-'

        return retval


    def block_doubles(self, blockID):
        if not blockID.__contains__('WP'):
            empty_units, fields = self.fields_in_block(blockID)
            retval = 0
            for field in fields:
                doub = self.bk.num_doubles(field)
                if isinstance(doub, int):
                    retval += doub
        else:
            retval = '-'

        return retval


    def block_discoveries(self, blockID):  # AWAITING IMPLEMENTATION
        if not blockID.__contains__('WP'):
            empty_units, fields = self.fields_in_block(blockID)
            retval = []
            return len(retval)
        else:
            retval = '-'

        return retval


    def block_blinking_status(self, blockID):
        # 36*21*2 = 1512 (or 1440 if it's a 20-field block.)
        # combine.py makes a given prefix-field-ccd (type doesn't seem to be included)
        # real files have either a .measure3.cands.astrom or a .no_candidates file.
        # fk files have more stuff. And there's a lot more of them, because every ccd gets one.
        #
        # sometimes weird stuff gets in there. So check on a field-by-field basis, I think.
        empty_units, fields = self.fields_in_block(blockID)
        for field in fields:
            uris = [os.path.join(storage.MEASURE3, str(field), '.measure3.cands.astrom'),
                    os.path.join(storage.MEASURE3, str(field), '.no_candidates')]
            for uri in uris:
                if storage.exists(uri):
                    node = storage.vospace.get_node(uri, force=force).props

            done = storage.tag_uri('done')
        # get_tags is built to obtain tags on the dbimages

        # RIGHT. There can be 'done' tags without there being a corresponding 'reals' file.
        # this is NOT IDEAL...

        # vtag vos:OSSOS/measure3/fk_E+3-1_17.measure3.cands.astrom
        # shows
        # 'ivo://canfar.uvic.ca/ossos#done': 'michele'
        # when done, but will show
        # 'ivo://canfar.uvic.ca/ossos#lock_holder': 'michele'
        # when someone has it out to blink.

        # if no ccds blinked, show 'Not started'
        # if some, show who has locks on those, group by lock_holder
        # if all ccds show 'done', show 'Completed'.

        # for the blocks page, just show 'x/36' under 'Blinked.'
        # has to show that for both fk and real.

        return None

    # def get_cands_tags():



    def block_processing_status(self, blockID):
        try:
            self.bk.get_processing_status(self.block_discovery_triples(blockID))
        except:
            print 'bother'

        return None







import cPickle

import sqlalchemy as sa
import ephem
import datetime
from ossos import storage

from web.overview.ossuary import OssuaryTable


class ImagesQuery(object):
    def __init__(self):
        """
        An ImagesQuery allows queries to ossuary's images table, and marshalls vtags associated with each image.
        """
        ot = OssuaryTable('images')
        self.images = ot.table
        self.conn = ot.conn
        self.field_triplets = {}
        self.steps = ['preproc', 'preproc_o', 'update_header', 'update_header_p', 'mkpsf', 'mkpsf_p',
                      'step1', 'step1_p', 'step2', 'step2_p', 'step3', 'step3_p', 'combine', 'combine_p',
                      'scramble', 'scramble_s', 'plant', 'plant_s', 'mkpsf_s', 'step1_s', 'fkstep1', 'fkstep1_s',
                      'fkstep2', 'fkstep2_s', 'fkstep3', 'fkstep3_s', 'fkcombine', 'fkcombine_s']

    def field_images(self, field):
        it = self.images
        cols = [it.c.cfht_field, it.c.obs_end, it.c.iq_ossos, it.c.image_id, it.c.zeropt, it.c.exptime, it.c.comment,
                it.c.snr]
        ss = sa.select(cols, order_by=it.c.obs_end)
        ss.append_whereclause(it.c.cfht_field == field)
        ims_query = self.conn.execute(ss)
        ret_images = self.marshall_imquery_return(ims_query)

        return ret_images


    def marshall_imquery_return(self, ims_query):
        ret_images = []
        for row in ims_query:
            retrow = [r for r in row[1:]]  # trim off the unnecessary field
            if (row[2] is None) or (
                    row[4] is None and row[2] == 0.74):  # catch None fwhm, zeropoint from unprocessed images.
                retrow[1] = -1.  # displaced by one as retrow starts after field_id. This one is iq_ossos
                retrow[3] = -1.  # zeropoint
            if row[7] is None:
                retrow[6] = -1.  # snr is None until set
            if row[6] is None:
                retrow[5] = ''  # comment can be an empty string for display
            ret_images.append(retrow)
        ims_query.close()

        return ret_images


    def image_errors(self, image_id):
        # Retrieve existing information on processing errors from the db.
        retval = ''
        it = self.images
        cols = [it.c.image_id, it.c.proc_status]
        ss = sa.select(cols)
        ss.append_whereclause(it.c.image_id == image_id)
        ims_query = self.conn.execute(ss)
        for row in ims_query:
            if row[1] is None:
                # Go and get it anew (this also adds it to the db for next time)
                retval = self.check_VOSpace_for_proc_status(image_id)
            else:
                retval = cPickle.loads(str(row[1]))

        return retval


    def add_err_to_db(self, image_id, ordered_errors):
        # Turn the list/dict object of errors into a linear string, save that in the db
        strerr = cPickle.dumps(ordered_errors)
        ss = self.images.update(self.images.c.image_id == image_id)
        params = {'proc_status': strerr}
        self.conn.execute(ss, params)  # , self.images.c.proc_status=strerr)

        return


    def check_VOSpace_for_proc_status(self, image_id):
        proc_keys = self.clean_keys(image_id)
        errors = self.collate_errors(proc_keys)
        ordered_errors = self.order_errors_in_pipeline(errors)
        self.add_err_to_db(image_id, ordered_errors)

        return ordered_errors


    def processing_status(self, ret_images, update=False):
        # want to show: [vtag, vtag:{error:[ccds] sorted in ascending ccd order}]
        # where the vtags are shown in their order of processing.
        retval = []
        for row in ret_images:
            image = row[2]
            retrow = row
            if update:  # retrieve new data from VOSpace
                ordered_errors = self.check_VOSpace_for_proc_status(image)
            else:  # retrieve existing data from local database
                ordered_errors = self.image_errors(image)
            retrow.append(ordered_errors)
            retval.append(retrow)

        return retval


    def clean_keys(self, image_id):
        # first retrieve and clean off the keys
        node = storage.get_tags(image_id, force=True)
        unwanted = ['creator', 'date', 'groupread', 'groupwrite', 'ispublic', 'length']
        proc_keys = []
        for vtag, value in node.items():
            if (vtag not in unwanted) and not vtag.__contains__('fwhm') and not vtag.__contains__('zeropoint'):
                try:
                    clean_key = vtag.split("#")[1]
                except:
                    clean_key = vtag
                proc_keys.append((clean_key, value))

        return proc_keys


    def collate_errors(self, proc_keys):
        # split each vtag to its root and collate errors under
        # {root:{error:[ccds]}}, where ccds are sorted in ascending order.
        errors = {}
        for key, val in proc_keys:
            root, ccd = key[0:len(key) - 2].strip('_'), key[-2:]
            tag = errors.get(root, {})

            # TODO: need a collate_successes part of this that counts the successes for the tag types
            # TODO: that would send up an error if the number of successes is < 36 in cases where it should be 36.
            # i.e. catch incomplete processing where mkpsf or similar didn't yet finish

            if not val == 'success':  # not successful :(
                err = tag.get(val, [])
                err.append(ccd)
                tag[val] = err
            errors[root] = tag  # entirely successful vtags will have just root:[] in errors

        retval = self.sort_ccds(errors)

        return retval


    def sort_ccds(self, errors):
        # sort the ccds into order for each error
        retval = {}
        for root, errs in errors.items():
            rr = retval.get(root, {})
            for error_type, ccds in errs.items():
                temp = ccds
                temp.sort()
                rr[error_type] = temp
            retval[root] = rr

        return retval


    def order_errors_in_pipeline(self, errors):
        # display the steps existing and their errors, if any.
        order = []
        # there's now a fwhm_p and a fwhm_s. And the same for zeropoint. FIXME: check if needed with JJ.
        errkeys = errors.keys()
        # print errkeys
        # Now accounts for all codes that have been used over time, with them in order, both old and new styles.
        for step in self.steps:
            if step in errors.keys():
                if len(errors[step]) == 0:  # step was completed successfully for all ccds
                    order.append(step)
                else:
                    order.append({step: errors[step]})

        return order


    def field_ra(self, field):
        it = self.images
        ss = sa.select([it.c.cfht_field, it.c.crval_ra])
        ss.append_whereclause(it.c.cfht_field == field)
        ims_query = self.conn.execute(ss)

        retval = 0
        for row in ims_query:
            retval = str(ephem.hours(ephem.degrees(str(row[1]))))  # HACKED FOR QUICK RESULT (precision not needed)

        return retval


    def field_dec(self, field):
        it = self.images
        ss = sa.select([it.c.cfht_field, it.c.crval_dec])
        ss.append_whereclause(it.c.cfht_field == field)
        ims_query = self.conn.execute(ss)

        retval = 0
        for row in ims_query:
            retval = str(ephem.degrees(str(row[1])))  # HACKED FOR QUICK RESULT (precision not needed)

        return retval


    # used in BlockQuery to get observed fields in the block.
    def what_fields_have_any_observations(self):
        it = self.images
        ss = sa.select([it.c.cfht_field],
                       distinct=it.c.cfht_field,
                       order_by=it.c.cfht_field)

        fields = [{'fieldId': n[0], 'triplet': None} for n in self.conn.execute(ss)]

        return fields


    # Process of getting the three best images that form a discovery triplet.
    def discovery_triplet(self, field):
        retval = None  # is set to a value if a valid triplet exists.
        if field in self.field_triplets:
            retval = self.field_triplets[field]
        else:
            # Is there even a night with 3+ images observed yet?
            threeplus_nights = self.do_triples_exist(field)  # just the dates
            if len(threeplus_nights) > 0:
                # Get details of the images on the nights that have 3+ images, this field.
                threeplus_night_images = {}
                for date in threeplus_nights:
                    date_images = self.images_in_tripleplus_night(field, date)
                    threeplus_night_images[date] = date_images

                retval = self.parse_for_best_triple(threeplus_night_images)
                self.field_triplets[field] = retval

        return retval


    def do_triples_exist(self, field):

        tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, 
			count(image_id) from images 
			where (cfht_field = :field)
			group by cfht_field, year, month, day 
			having count(image_id) > 2
			order by cfht_field, year, month, day;""")
        pp = {'field': field}
        tplus_res = self.conn.execute(tplus, pp)

        threeplus_nights = []
        for row in tplus_res:
            threeplus_nights.append(row[1:4])  # year, month, day

        return threeplus_nights


    def images_in_tripleplus_night(self, field, date):

        ss = sa.text(
            """select cfht_field, extract(year from obs_end) as year,
				extract(month from obs_end) as month, 
				extract(day from obs_end) as day, image_id, obs_end, 
				iq_ossos, zeropt from images
				where (cfht_field = :field
				and extract(year from obs_end) = :year
				and extract(month from obs_end) = :month
				and extract(day from obs_end) = :day)
				order by obs_end""")
        pp = {'field': field, 'year': date[0], 'month': date[1], 'day': date[2]}
        tplus_res = self.conn.execute(ss, pp)

        ret_images = []
        for row in tplus_res:
            # keep year, month, day, image_id, obs_end, iq_ossos
            rr = list(row[1:])
            if (row[6] is None) or (row[7] is None and row[6] == 0.74):  # iq_ossos hasn't been calculated yet!
                rr[5] = -1.
                rr[6] = -1.
            ret_images.append(rr)

        return ret_images


    def parse_for_best_triple(self, threeplus_night_images):
        # input is a dict of {date: [rows of images]} where [rows] > 2
        good_triples = []
        for date, ims in threeplus_night_images.items():
            # for each night, create the best possible triple that meets constraints.
            # is their temporal span sufficiently wide for a triplet to exist?
            if (ims[-1][4] - ims[0][4]) > datetime.timedelta(minutes=90):
                # return is [im, im, im, worst_iq]
                best_triple_of_night = self.suitable_triples(ims)
                if best_triple_of_night is not None:
                    good_triples.append(best_triple_of_night)

        if len(good_triples) > 0:
            # Return the set of 3 images that have the lowest value of 'worst iq'.
            lowest_worst_iq = min([g[2] for g in good_triples if g[2] is not None])  # check against not-set value
            retval = good_triples[[g[2] for g in good_triples].index(lowest_worst_iq)]
        else:
            retval = None

        return retval


    def suitable_triples(self, ims):
        triple_sets = []
        # construct all possible sets of three
        times = range(0, len(ims))
        twenty = datetime.timedelta(minutes=20)
        ninety = datetime.timedelta(minutes=90)
        for ii in times:
            for jj in times:
                for kk in times:
                    imset = [ims[ii], ims[jj], ims[kk]]
                    imset.append(max([im[5] for im in imset]))

                    j_minus_i = imset[1][4] - imset[0][4]
                    k_minus_j = imset[2][4] - imset[1][4]
                    span = imset[2][4] - imset[0][4]
                    if (ii < jj < kk) and (j_minus_i > twenty) and (k_minus_j > twenty) and span > ninety:
                        triple_sets.append(imset)

        # return the set with the lowest worst iq. All have adequate spacing.
        triple_sets.sort(key=lambda x: x[3])
        # if there's one available in triple_sets!
        if len(triple_sets) > 0:
            # if there's multiple sets, keep only the one where there's FWHM info available for everything in the set.
            clean_triple_sets = []
            for tset in triple_sets:
                fwhms = [ts[2] for ts in tset[0:3] if ts is not None]  # ts[0:3] guards against unset IQ (None) in ts[3]
                if len(fwhms) == 3:  # it's fine, none of the fwhms haven't been set
                    clean_triple_sets.append(tset)

            # format as ([image_ids], [3 rows of remaining info], worst_iq)
            retval = ([t[3] for t in clean_triple_sets[0][0:3]], clean_triple_sets[0][0:3], clean_triple_sets[0][3])
        else:
            retval = None

        return retval


    def num_precoveries(self, field):
        # how many of the images in self.observations occur
        # before the first image of the discovery triplet?
        retval = 'no discovery triplet'
        if field in self.field_triplets.keys() and self.field_triplets[field] is not None:
            triplet = self.field_triplets[field]
            images = self.field_images(field)
            precoveries = [im for im in images if (im[0] < triplet[1][0][4])]
            retval = len(precoveries)

        return retval


    def num_nailings(self, field):
        # nailings: single images, at least one night after the night of the discovery triplet
        retval = 'no discovery triplet'
        if field in self.field_triplets.keys() and self.field_triplets[field] is not None:
            triplet = self.field_triplets[field]
            images = self.field_images(field)
            after = [im for im in images if
                     ((im[0] > triplet[1][2][4])
                      and im[0].strftime('%Y-%m-%d') != triplet[1][2][4].strftime('%Y-%m-%d'))]

            retval = len(after)

        return retval


    def num_doubles(self, field):
        # double is a pair of images ~ an hour apart taken on the same night
        # the night of the pair is at least a night after the discovery triplet night
        # there are no precovery doubles

        retval = 'no discovery triplet'
        if field in self.field_triplets.keys() and self.field_triplets[field] is not None:
            triplet = self.field_triplets[field]
            images = self.field_images(field)
            double_nights = self.do_doubles_exist(field, triplet[1][2][4])
            retval = len(double_nights) * 2

        return retval


    def do_doubles_exist(self, field, last_triplet_image):

        # NEED TO ACCOUNT FOR >2 IMAGES ON THE SAME NIGHT THAT COULD FORM A DOUBLE AS A SUBSET

        tplus = sa.text("""
			select cfht_field, extract(year from obs_end) as year, 
			extract(month from obs_end) as month, extract(day from obs_end) as day, obs_end,
			count(image_id) from images 
			where (cfht_field = :field
				and obs_end > :date)
			group by cfht_field, year, month, day, obs_end 
			having count(image_id) > 1
			order by cfht_field, year, month, day, obs_end;""")
        pp = {'field': field, 'date': last_triplet_image}
        tplus_res = self.conn.execute(tplus, pp)

        double_nights = []
        for row in tplus_res:  # Filtering output for now, can't get condition into select yet.

            # NEED THIS TO ACCOUNT FOR IMAGES ALSO BEING SPACED BY AT LEAST HALF AN HOUR

            # print row[5].strftime('%Y-%m-%d'), last_triplet_image.strftime('%Y-%m-%d')
            if row[5].strftime('%Y-%m-%d') != last_triplet_image.strftime('%Y-%m-%d'):
                double_nights.append(row[1:4])  # year, month, day

        return double_nights


    def export_discovery_triplet(self, field):
        # write discovery_triplet to a file in VOSpace
        triplet = self.discovery_triplet(field)

        if triplet:
            stem = 'vos:OSSOS/triplets/'
            # TESTING TESTING REMOVE BEFORE FLIGHT
            tmpfile = 'H_14B_discovery_expnums.txt'
            blockuri = stem + tmpfile

            # does a file for this block already exist in VOSpace? If so, copy it back.
            if storage.vospace.access(blockuri):  # Does this work this way?
                # vospace.create(blockuri)
                storage.vospace.copy(blockuri, './' + tmpfile)
                print tmpfile, '<-', blockuri
                # the field is already present
                with open(tmpfile, 'r+') as scratch:
                    lines = scratch.readlines()
                    for line in lines:
                        if line.split()[3] == field:  # It's present already. Add updating later
                            return
                    scratch.write('%s %s %s %s\n' % (triplet[0][0], triplet[0][1], triplet[0][2], field))
            else:
                with open(tmpfile, 'w') as scratch:
                    # 3-line file: id id id field
                    scratch.write('%s %s %s %s\n' % (triplet[0][0], triplet[0][1], triplet[0][2], field))

            storage.vospace.copy(tmpfile, blockuri)
            print tmpfile, '->', blockuri

            # return

            # def process_field(self, field, step, ):
            # allchips_scripts = ['preproc', 'update_header', 'mkpsf_all']
            #     # TODO: want to be able to process all of a single field's images for those in allchips_scripts
            #     # and to be able to process only triplets for the others.
            #     # Have two buttons: reprocess all images, reprocess triplets.
            #     triplet_scripts = []
            #
            #     ## Triplets: first do the search images. These are the same commands as in mop.sh.
            #     scripts = {'preproc': '',
            #                'update_header': '',
            #                'mkpsf_all': '',
            #                'mkpsf_p': 'mkpsf.py $1 $2 $3 --ccd $ccd -v  ${force}',
            #                'step1_p': 'step1.py $1 $2 $3 --ccd $ccd -v  ${force}',
            #                'step2_p': 'step2.py $1 $2 $3 --ccd $ccd -v  ${force}',
            #                'step3_p': 'step3.py $1 $2 $3 --ccd $ccd -v  ${force}',
            #                'combine_p': 'combine.py $1 -v  --ccd $ccd ${force} --measure3
            # vos:OSSOS/measure3/2013A-O --field '
            #                             'O-2+0',
            #                'scramble_s': 'scramble.py $1 $2 $3 --ccd $ccd -v  ${force}',
            #                'mkpsf_s': 'mkpsf.py $1 $2 $3 --ccd $ccd -v  ${force} --type s',
            #                'step1_s': 'step1.py  $1 $2 $3 --ccd $ccd -v  ${force} --type s',
            #                'step2_s': 'step2.py   $1 $2 $3 --ccd $ccd -v  ${force} --type s',
            #                'plant_s': 'plant.py $1 $2 $3 --ccd $ccd -v ${force} --type s',
            #                'fkstep1_s': 'step1.py $1 $2 $3 --ccd $ccd --fk --type s -v  ${force}',
            #                'fkstep2_s': 'step2.py $1 $2 $3 --ccd $ccd --fk --type s -v  ${force}',
            #                'fkstep3_s': 'step3.py $1 $2 $3 --ccd $ccd --fk --type s -v ${force}',
            #                'fkcombine_s': 'combine.py $1 --fk --type s -v --ccd $ccd ${force} --measure3 '
            #                               'vos:OSSOS/measure3/2013A-O --field O-2+0'}
            #
            #     d = {'wibble': 'arg1', 'blarg': 'arg2'}
            #     foo = 'cmd {wibble} -o {blarg}'.format(**d)
            #
            #     for im in self.field_images(field):
            #         if step in allchips_scripts:
            #             # Log formatting now EXPOSURE_CCD_SCRIPT_DATE  ${exp1}_preproc_`date -u +%Y-%m-%dT%H:%M:%S`
            #             joblog = {'im': im, 'step': step, 'date': datetime.datetime.strftime('%Y-%m-%d_%H:%M:%S')}
            #             str_cmd = 'ossos_submit/submit_job.sh ' + '{im}_{step}_{date}'.format(**joblog) + scripts[
            # step].format(
            #                 **)
            #
            #         # Execute a script
            #         run_cmd = shlex.split(str_cmd)
            #         subprocess.Popen(run_cmd)
            #
            #     return






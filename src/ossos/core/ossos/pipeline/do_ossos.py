url="""http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/tap/sync"""
params = {"LANG": "ADQL",
          "REQUEST": "doQuery",
          "FORMAT": "csv" }

QUERY="""SELECT Observation.sequenceNumber AS expnum FROM caom2.Plane AS Plane JOIN caom2.Observation AS Observation ON Plane.obsID = Observation.obsID WHERE  ( Observation.instrument_name = 'MegaPrime' AND Observation.collection = 'CFHT' AND INTERSECTS( INTERVAL( 57754.0, 1.7976931348623157E308 ), Plane.time_bounds ) = 1 AND lower(Observation.proposal_id) IN ('17bt01', '17bs02', '17ad96', '17bf14', '17bc09' ) AND  Plane.calibrationLevel=1 AND ( Plane.quality_flag IS NULL OR Plane.quality_flag != 'junk') ) ORDER BY Observation.sequenceNumber """

params["QUERY"] = QUERY

import argparse
import smtplib
from email.mime.text import MIMEText
import requests
from io import StringIO
from astropy.table import Table
from astropy.io import ascii
import tempfile
import vos

parser = argparse.ArgumentParser()
parser.add_argument('-o','--output', action="store_true", help="Store to file instead of mailing list")

args = parser.parse_args()

current_list = vos.Client().listdir('vos:OSSOS/dbimages')
recon_exposures = Table.read(StringIO(requests.get(url, params=params).content), format='ascii.csv')
print(recon_exposures)

if args.output:
    with open('list.txt', 'w') as fout:
        cond = [ str(g) not in current_list for g in recon_exposures['expnum']]
        ascii.write(recon_exposures[cond], fout, format='no_header')

if not args.output:
    you = 'Stephen.Gwyn@nrc-cnrc.gc.ca'
    you = 'jj.kavelaars@nrc-cnrc.gc.ca'
    me = 'jjk@uvic.ca'

    with tempfile.NamedTemporaryFile() as fout:
        fout.write("Hi Stephen, \n Here is correct list of OSSOS exposures that need processing (PITCAIRN+ASTGWYN).\n\nThanks\nJJ\n")
        cond = [ str(g) not in current_list for g in recon_exposures['expnum']]
        ascii.write(recon_exposures[cond], fout, format='no_header', overwrite=True)
        fout.seek(0)
        msg = MIMEText(fout.read())
        msg['Subject'] = "OSSOS Exposures to process"
        msg['From'] = me
        msg['To'] = you
        msg['CC'] = me
    

    s = smtplib.SMTP('smtp.uvic.ca')
    s.sendmail(me, [you, me], msg.as_string())
    s.quit()



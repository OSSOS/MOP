from sqlalchemy import *
import ossos.overview.queries as oq
import ossos.field_obs.queries as foq


def num_discoveries(discoveries):
	ss = text(
		"""select count(discovery_id) from discoveries;"""
		)
	retval = [n[0] for n in discoveries.conn.execute(ss)][0]
	
	return retval 


def mpc_informed(discoveries):
	ss = text(
		"""select count(mpc_told) from discoveries;"""
		)
	retval = [n[0] for n in discoveries.conn.execute(ss)][0]

	return retval 


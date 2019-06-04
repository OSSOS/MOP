import sqlalchemy as sa


class OssuaryTable(object):
    def __init__(self, tablename):
        # reflect_table_from_ossuary
        # Development testing database on local machine provided by Postgres.App
        engine = sa.create_engine('postgresql://localhost/ossuary', echo=False)
        metadata = sa.MetaData(bind=engine)
        table = sa.Table(tablename, metadata, autoload=True, autoload_with=engine)  # reflect existing table
        conn = engine.connect()

        self.tablename = tablename
        self.table = table
        self.conn = conn

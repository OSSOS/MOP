print "use cfeps;"

#print "DROP TABLE objects ; "

lines = file('L3_objects.txt').readlines()
scol = lines.pop(0)
scol = """#  object   mag  stdev   dist     dist_E   nobs   time   av_xres av_yres max_x max_y      a          a_E        e        e_E       i       i_E    node     n_E    argperi     w_E           M        M_E   ra_dis  dec_dis """
cols = scol[1:-1].split()

coltype={"object": "varchar(20)"}
for col in cols:
    if not col=="object":
        coltype[col]="double"

createSQL="CREATE TABLE objects ( "
sep=""
for col in cols:
    createSQL+=sep+col+" "+coltype[col]
    sep=","
createSQL+=" ) ; "
print createSQL

sep=""

sep=""
baseSQL="INSERT INTO objects ("
for col in cols:
    baseSQL+=sep+col.strip()
    sep=", "
baseSQL+=") VALUES ("


for line in lines:
    values=line.split()
    SQL=baseSQL
    object=values.pop(0)
    SQL+="'"+object+"'"
    sep=","
    for value in values:
        if value=='nan':
            value="NULL"
        SQL+=sep+value
        sep=","
    SQL+=" ) ;"
    print SQL 

define TCV_R qtbegtr($1,qtbcfnd($1, $2), $3)
define TCV_I qtbegti($1,qtbcfnd($1, $2), $3)
define INC_I    Memi[$1+$2]=Memi[$1+$2]+$3
define INC_R    Memr[$1+$2]=Memr[$1+$2]+$3
define TPV_R call tbeptr($1, qtbcfnd($1, $2), $3, $4)
define TPV_I call tbepti($1, qtbcfnd($1, $2), $3, $4)



define MAXMASKS 100

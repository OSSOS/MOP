#!/usr/bin/env python

import sys,re
import math
import numpy
#cond=sys.argv[1]

OBSERVATION_DATE='2015/03/26'

#print "# "+cond

Number_Mil={'B': 110000, 'C': 120000, 'D': 130000, 'E': 140000, 'F': 150000}
Number_Cent={'J': 1900, 'K': 2000}
def date_unpack(pdate):
    YY={'I': 1800, 'J': 1900, 'K': 2000}
    Ncode='0123456789ABCDEFGHIJKLMNOPQRSTUV'
    yyyy=YY[pdate[0]]+int(pdate[1:3])
    mm=Ncode.rindex(pdate[3])
    dd=float(Ncode.rindex(pdate[4]))
    return (yyyy, mm, dd)


def desig_unpack(desig):
    import re
    if re.match('\d+',desig):
        return str(int(desig))
    Kilo='ABCDEFGHIJKLMNOPQRSTUV'
    j = Kilo.rfind(desig[0])
    if j != -1 :
      if re.match('^\d+$',desig[1:7]):
        return str(100000+j*10000+int(desig[1:7]))
    YY={'I': 1800, 'J': 1900, 'K': 2000}
    try:
       yyyy=str(YY[desig[0]]+int(desig[1:3]))
    except KeyError as e:
       return desig
    Ncode='0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    Mcode=desig[3]+desig[6]
    cycle=Ncode.rindex(desig[4])*10+int(desig[5])
    if (cycle>0) :
	cycle=str(cycle)
    else:
	cycle=''
    return yyyy+' '+Mcode+cycle
    


f=open('/Users/jjk/MPCORB.DAT')
#f=open('schwamb_orbits.dat')
lines=f.readlines()
f.close()

import ephem,sys
kbo=ephem.EllipticalBody()
line=lines.pop(0)
while (line[0:3]!="---") : 
   line=lines.pop(0)

lines.append(line)
nobj=0
lineCount=0

# 2013-11-29 09:59:59| 0:51:16.75|  2:33:57.9|
# 2013-11-30 09:59:59| 0:51:14.24|  2:33:43.0|
# 2013-12-01 09:59:59| 1:26:52.38| 11:41:10.1|


# 2014-08-26 09:59:59| 1:33:56.25| 11:52:27.4|
# 2014-08-26 09:59:59| 1:46:14.87| 14:02:27.9|
# 2014-08-26 09:59:59| 1:21:41.35|  9:40:38.1|

# 13BL-Block (0:55:00 03:57:00)
#ra_low = float(ephem.hours("00:55:00")-math.radians(3.5))
#ra_high = float(ephem.hours("00:55:00")+math.radians(3.5)) 
#dec_low = float(ephem.degrees("12:57:00")-math.radians(1.5))
#dec_high = float(ephem.degrees("13:57:00")+math.radians(1.5))


# 13B-H
ra_low  = float(ephem.hours("01:21:00.0"))
ra_high = float(ephem.hours("01:46:00.0"))
dec_low = float(ephem.degrees("01:02:00"))
dec_high = float(ephem.degrees("05:03:00"))

#ra_low  = float(ephem.hours(math.radians(17)))
#ra_high = float(ephem.hours(math.radians(25)))
#dec_low = float(ephem.degrees("10:00:00"))
#dec_high = float(ephem.degrees("18:00:00"))
# 14BH
ra_low = float(ephem.hours("01:39:26.86"))
ra_high = float(ephem.hours("01:51:45.76"))
dec_low = float(ephem.degrees("+13:12:10.8"))
dec_high = float(ephem.degrees("+15:58:55.9"))

# 15AP?
ra_low = float(ephem.hours("13:21:12.50"))
ra_high = float(ephem.hours("13:51:12.50"))
dec_low = float(ephem.degrees("08:00:11.6"))
dec_high = float(ephem.degrees("11:00:00"))

n_days_to_check = 61
min_rate = []
max_rate = []
kbo_angle = []
[ min_rate.append([]) for n in range(n_days_to_check) ]
[ max_rate.append([]) for n in range(n_days_to_check) ]
[ kbo_angle.append([]) for n in range(n_days_to_check) ]
# min_rate = [[],[],[],[],[],[],[],[],[],[],[],[], [],[],[],[],[]]
# max_rate = [[],[],[],[],[],[],[],[],[],[],[],[], [],[],[],[],[]]
# kbo_angle = [[],[],[],[],[],[],[],[],[],[],[],[], [],[],[],[],[]]
count = numpy.zeros(n_days_to_check)
for line in lines:
    lineCount=lineCount+1
    if lineCount %1000 == 0 : 
	sys.stderr.write("# Line: %d \n" % ( lineCount)) 
    if line[0]=='#' or len(line) < 103 or line[0:3]=='---':
        #sys.stderr.write(line)
        continue
    
    if len(line[8:13].strip()):
        kbo._H=float(line[8:13])
        kbo._G=float(line[14:19])
    else:
        kbo._H=20
        kbo._G=0
    arc = line[127:136]
    try:
      if 'days' in arc:
	arc = int(arc.split()[0])/365.25
      else:  
        dt = -eval(arc)
    except:
      print arc
      continue
    kbo._epoch_M=date_unpack(line[20:25].strip())
    kbo._M=float(line[26:35].strip())
    kbo._om=float(line[37:46].strip())
    kbo._Om=float(line[48:57].strip())
    kbo._inc=float(line[59:68].strip())
    kbo._e=float(line[70:79].strip())
    kbo._epoch='2011/08/01'
    kbo._a=float(line[92:103].strip())
    a= kbo._a
    e= kbo._e
    H= kbo._H
    i= kbo._inc
    startdate = ephem.date(OBSERVATION_DATE)
    kbo.compute(startdate)
    kbo.name=desig_unpack(line[0:7].strip()) 
    T_J = (5.2/a) + 2.0 * math.sqrt((1-e**2)*(a/5.2)) * math.cos(i)
    if not ra_low < float(kbo.ra) < ra_high or not dec_low < float(kbo.dec) < dec_high :
        continue
    for ddays in range(n_days_to_check):
        kbo.compute(startdate)
        ra1 = kbo.ra
        dec1 = kbo.dec
        enddate = startdate + 1/24.0
        kbo.compute(enddate)
        ra2 = kbo.ra
        dec2 = kbo.dec
        delta = math.degrees(ephem.separation((ra1,dec1),(ra2,dec2)))*3600.0
        angle = math.degrees(math.atan2((dec2-dec1),(ra2-ra1)))
	retrograde = ra1 < ra2
        rate = delta/(24.0*(enddate-startdate))
        if kbo.earth_distance < 2:
            if rate < 15:
                count[ddays] += 1 
            min_rate[ddays].append(rate)
        if kbo.earth_distance > 10:
            if rate < 5:
                count[ddays] += 1 
            max_rate[ddays].append(rate)
            kbo_angle[ddays].append(angle)
        startdate = enddate

startdate = ephem.date(OBSERVATION_DATE)
for rate in range(len(min_rate)):
    try:
    	today = ephem.date(startdate+rate)
    	print rate, today, numpy.min(min_rate[rate]), numpy.max(max_rate[rate]), numpy.min(max_rate[rate]), numpy.min(kbo_angle[rate]), numpy.max(kbo_angle[rate])
    except:
        pass

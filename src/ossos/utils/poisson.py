#!/usr/bin/python

import math
import sys

def poisson(lamda,k):
        p=0
        try: 
	    fact = factorial(k)
        except:
	    fact = 1e10
        try:
	    p= (math.exp(-1.0*lamda)*lamda**k)/fact
        except:
 	    p=0E0
	return p

def factorial(f):
    if f>1.0:
        return(f*factorial(f-1))
    if f==0.0:
        f=1.0
    return(f)

def comp(a,b):
	return -1*cmp(a[1],b[1])


expected = 1.7*0.23/20.0
expected = 0.5*0.23/20.0
expected = 0.65*0.23/20.0
expected = 4.0*0.23/20.0
expected = float(sys.argv[1])
p=[]
for measured in range(max(0,int(expected-5.0*math.sqrt(expected))),max(10,int(expected+5*math.sqrt(expected))),1):
    p.append([measured,poisson(float(expected),float(measured))])

#p.sort(comp)
#print p
t=0
for l in range(len(p)):
   t+= p[l][1]
   print "%5.1f %5.3f %5.3f" % (p[l][0],p[l][1],t)

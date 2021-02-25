import re
import sys

import ephem


#Number_Mil={'B': 110000, 'C': 120000, 'D': 130000, 'E': 140000, 'F': 150000}
#Number_Cent={'J': 1900, 'K': 2000}
def date_unpack(pdate):
    (yyyy, mm, dd) = (2000, 0o1, 0o1)
    try:
        YY={'I': 1800, 'J': 1900, 'K': 2000}
        Ncode='0123456789ABCDEFGHIJKLMNOPQRSTUV'
        yyyy=YY[pdate[0]]+int(pdate[1:3])
        mm=Ncode.rindex(pdate[3])
        dd=float(Ncode.rindex(pdate[4]))
    except:
        sys.stderr.write("ERROR converting date part {}:".format(pdate))
    return (yyyy, mm, dd)


def desig_unpack(desig):
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
    
def getKBOs(mpc_file, cond='a > 30'):

    f=open(mpc_file)
    lines=f.readlines()
    f.close()

    # while "------" not in lines[0]:
    # lines.pop(0)
    # lines.pop(0)
    nobj=0
    lineCount=0
    kbos = []
    for line in lines:
        lineCount=lineCount+1
        if line[0] == '#' or len(line) < 103 or line[0:3] == '---' or line[0:3] == 'Des':
            continue
        kbo=ephem.EllipticalBody()

        try:
            if len(line[8:13].strip()):
                kbo._H = float(line[8:13])
                kbo._G = float(line[14:19])
            else:
                kbo._H = 20
                kbo._G = 0
        except:
            print(line)
            kbo._H = 20
            kbo._G = 0
        arc = line[127:136]
        try:
            if 'days' in arc:
                arc = int(arc.split()[0])/365.25
            else:  
                arc = -eval(arc)
        except:
            arc = 0
        kbo._epoch_M=date_unpack(line[20:25].strip())
        kbo._M=float(line[26:35].strip())
        kbo._om=float(line[37:46].strip())
        kbo._Om=float(line[48:57].strip())
        kbo._inc=float(line[59:68].strip())
        kbo._e=float(line[70:79].strip())
        kbo._epoch='2011/08/01'
        kbo._a=float(line[92:103].strip())
        kbo.compute()

        a = kbo._a
        q = kbo._a*(1-kbo._e)
        H = kbo._H

        if eval(cond): 
            kbo.name=desig_unpack(line[0:7].strip()) 
            kbos.append(kbo)

    return kbos

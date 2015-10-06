import getpass
import netrc
import os
import sys
import urllib2

import vos


def getCert(certHost=vos.vos.SERVER, certfile=None,
            certQuery="/cred/proxyCert?daysValid=",daysValid=2):
    """Access the cadc certificate server"""

    if certfile is None:
        certfile = os.path.join(os.getenv("HOME","/tmp"),".ssl/cadcproxy.pem")

    dirname = os.path.dirname(certfile)
    try:
        os.makedirs(dirname)
    except OSError as e:
        if os.path.isdir(dirname):
            pass
        elif e.errno == 20 or e.errno == 17:
            sys.stderr.write(str(e)+": %s \n" % dirname)
            sys.stderr.write("Expected %s to be a directory.\n" % ( dirname))
            sys.exit(e.errno)
        else:
            raise e
    
    # Example taken from voidspace.org.uk
    # create a password manager
    password_mgr = urllib2.HTTPPasswordMgrWithDefaultRealm()

    (username, passwd) = getUserPassword(host=certHost)

    # Add the username and password.
    # If we knew the realm, we could use it instead of ``None``.
    top_level_url = "http://"+certHost
    password_mgr.add_password(None, top_level_url, username, passwd)
    
    handler = urllib2.HTTPBasicAuthHandler(password_mgr)
    
    # create "opener" (OpenerDirector instance)
    opener = urllib2.build_opener(handler)
    
    # Install the opener.   
    urllib2.install_opener(opener)

    # Now all calls to urllib2.urlopen use our opener.
    url="http://"+certHost+certQuery+str(daysValid)
    r= urllib2.urlopen(url)
    w= file(certfile,'w')
    while True:
        buf=r.read()
        if not buf:
            break
        w.write(buf)
    w.close()
    r.close()
    return 

def getUserPassword(host='www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca'):
    """"Getting the username/password for host from .netrc filie """
    if os.access(os.path.join(os.environ.get('HOME','/'),".netrc"),os.R_OK):
        auth=netrc.netrc().authenticators(host)
    else:
        auth=False
    if not auth:
        sys.stdout.write("CADC Username: ")
        username=sys.stdin.readline().strip('\n')
        password=getpass.getpass().strip('\n')
    else:
        username=auth[0]
        password=auth[2]
    return (username,password)




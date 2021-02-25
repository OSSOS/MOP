"""interact with the CADC GMS

"""

import cgi
import http.client
import sys
import tempfile
import urllib.request, urllib.error, urllib.parse
import urllib.request, urllib.parse, urllib.error
import logging

from OpenSSL import crypto


_SERVER = 'www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca'
_GMS = '/gms/groups'
_PROXY = '/cred/proxyCert'


# @view_config(context=HTTPForbidden)
# def basic_challenge(request):
# response = HTTPUnauthorized()
# response.headers.update(forget(request))
#     return response


def login_view(request):
    next = request.params.get('next') or request.route_url('home')
    login = ''
    did_fail = False
    if 'submit' in request.POST:
        login = request.POST.get('login', '')
        passwd = request.POST.get('passwd', '')

        user = USERS.get(login, None)
        logging.debug("Connection attempt from : {}".format(login))
        if user and user.check_password(passwd):
            headers = remember(request, login)
            return HTTPFound(location=next, headers=headers)
        did_fail = True

    return {
        'login': login,
        'next': next,
        'failed_attempt': did_fail,
        'users': USERS,
    }


def getCert(username, password,
            certHost=_SERVER,
            certfile=None,
            certQuery=_PROXY):
    """Access the cadc certificate server."""

    if certfile is None:
        certfile = tempfile.NamedTemporaryFile()

    # Add the username and password.
    # If we knew the realm, we could use it instead of ``None``.
    password_mgr = urllib.request.HTTPPasswordMgrWithDefaultRealm()
    top_level_url = "http://" + certHost
    logging.debug(top_level_url)
    password_mgr.add_password(None, top_level_url, username, password)
    handler = urllib.request.HTTPBasicAuthHandler(password_mgr)
    logging.debug(str(handler))
    # create "opener" (OpenerDirector instance)
    opener = urllib.request.build_opener(handler)

    # Install the opener.   
    urllib.request.install_opener(opener)

    # buuld the url that with 'GET' a certificat using user_id/password info
    url = "http://" + certHost + certQuery
    logging.debug(url)

    r = None
    try:
        r = opener.open(url)
    except urllib.error.HTTPError as e:
        logging.debug(url)
        logging.debug(str(e))
        return False

    logging.debug(str(r))

    if r is not None:
        while True:
            buf = r.read()
            logging.debug(buf)
            if not buf:
                break
            certfile.write(buf)
        r.close()

    return certfile


def getGroupsURL(certfile, group):
    """given a certfile load a list of groups that user is a member of"""

    GMS = "https://" + _SERVER + _GMS

    certfile.seek(0)
    buf = certfile.read()

    x509 = crypto.load_certificate(crypto.FILETYPE_PEM, buf)
    sep = ""
    dn = ""
    parts = []
    for i in x509.get_issuer().get_components():
        #print i
        if i[0] in parts:
            continue
        parts.append(i[0])
        dn = i[0] + "=" + i[1] + sep + dn
        sep = ","

    return GMS + "/" + group + "/" + urllib.parse.quote(dn)


def isMember(userid, password, group):
    """Test to see if the given userid/password combo is an authenticated member of group.

    userid: CADC Username (str)
    password: CADC Password (str)
    group: CADC GMS group (str)
    
    """
    try:
        certfile = getCert(userid, password)

        group_url = getGroupsURL(certfile, group)
        logging.debug("group url: %s" % ( group_url))

        con = http.client.HTTPSConnection(_SERVER,
                                      443,
                                      key_file=certfile.name,
                                      cert_file=certfile.name,
                                      timeout=600)

        con.connect()
        con.request("GET", group_url)
        resp = con.getresponse()
        if resp.status == 200:
            return True
    except Exception as e:
        logging.error(str(e))

    #logging.debug(str(resp.status))

    return False


def stub():
    """Just some left over code"""
    form = cgi.FieldStorage()
    userid = form['userid'].value
    password = form['passwd'].value
    group = form['group'].value


if __name__ == '__main__':
    userid = sys.argv[1]
    password = sys.argv[2]
    group = sys.argv[3]
    logging.basicConfig(level=logging.DEBUG)
    logging.debug("userid: %s" % ( userid))
    logging.debug("passwd: %s" % ( password))
    print(isMember(userid, password, group))


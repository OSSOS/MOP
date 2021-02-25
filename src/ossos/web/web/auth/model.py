from pyramid.httpexceptions import HTTPForbidden
from pyramid.httpexceptions import HTTPFound
from pyramid.security import authenticated_userid
from pyramid.security import forget
from pyramid.security import remember
from pyramid.view import forbidden_view_config
from pyramid.view import view_config

from . import gms


USERS = {}

# ## DEFINE MODEL
class User(object):
    def __init__(self, login, password, groups=None):
        self.login = login
        self.password = password
        self.groups = groups or []
        USERS[login] = self

    def check_password(self, passwd, group):
        # This provides authentication via CADC.
        """check that the passwd provided matches the required password."""
        return gms.isMember(self.login, passwd, group)


@forbidden_view_config()
def forbidden_view(request):
    # do not allow a user to login if they are already logged in
    if authenticated_userid(request):
        return HTTPForbidden()

    loc = request.route_url('login', _query=(('next', request.path),))
    return HTTPFound(location=loc)


@view_config(
    route_name='login',
    renderer='login.pt',
)
def login_view(request):
    # NEED TO SET THESE PROPERLY
    # next = request.params.get('next') or request.route_url('home')
    # print request.params
    # next = request.params.get('overview')
    next = '/'  # TESTING ONLY but it does seem to work.

    login = ''
    did_fail = False
    if 'submit' in request.POST:
        login = request.POST.get('login', '')
        passwd = request.POST.get('passwd', '')
        user = USERS.get(login, User(login, passwd, groups=['OSSOS']))
        if user.check_password(passwd, 'OSSOS'):
            headers = remember(request, login)
            return HTTPFound(location=next, headers=headers)
        did_fail = True

    return {
        'login': login,
        'next': next,
        'failed_attempt': did_fail,
        'users': USERS,
    }


@view_config(
    route_name='logout',
)
def logout_view(request):
    headers = forget(request)
    loc = request.route_url('login')
    return HTTPFound(location=loc, headers=headers)


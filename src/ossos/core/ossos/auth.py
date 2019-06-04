import getpass


def get_cadc_username():
    """
    get a name to use for locking and logging
    """
    return getpass.getuser()

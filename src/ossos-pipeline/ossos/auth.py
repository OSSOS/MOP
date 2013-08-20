import os

from OpenSSL import crypto


def get_cadc_username(
        certfilename=os.path.join(os.getenv('HOME'), '.ssl', 'cadcproxy.pem')):
    """
    Returns:
      cadc_username: str
    """
    common_name = get_issuer_common_name(certfilename)

    last_index = common_name.rfind("_")
    if last_index > 0:
        return common_name[:last_index]
    else:
        return common_name


def get_issuer_common_name(
    certfilename = os.path.join(os.getenv('HOME'), '.ssl', 'cadcproxy.pem')):

    certfile = open(certfilename, "rb")
    x509 = crypto.load_certificate(crypto.FILETYPE_PEM, certfile.read())

    return x509.get_issuer().commonName

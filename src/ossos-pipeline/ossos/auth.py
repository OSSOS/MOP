import os

from OpenSSL import crypto


def get_cadc_id(
        certfilename=os.path.join(os.getenv('HOME'), '.ssl', 'cadcproxy.pem')):
    """
    Returns:
      id: str
        The "common name" of the issuer in the certificate file.  Note that
        this may not match the CADC username.
    """
    certfile = open(certfilename, "rb")
    x509 = crypto.load_certificate(crypto.FILETYPE_PEM, certfile.read())

    return x509.get_issuer().commonName

from astropy import units
from xml.dom import minidom
import xml
from astropy.coordinates import ICRSCoordinates

COLUMN_SEPARATOR = "|"


def create_astrores_document():
    implementation = xml.dom.getDOMImplementation()
    doctype = implementation.createDocumentType('ASTRO',
                                                None,
                                                "http://vizier.u-strasbg.fr/xml/astrores.dtd")
    dom = implementation.createDocument("http://vizier.u-strasbg.fr/doc/astrores.htx", "ASTRO", doctype)
    dom.getElementsByTagName("ASTRO")[0].setAttribute("ID", "v0.8")
    dom.getElementsByTagName("ASTRO")[0].setAttribute("xmlns:ASTRO", "http://vizier.u-strasbg.fr/doc/astrores.htx")
    assert isinstance(dom, minidom.Document)
    return dom


class EphemTarget(object):

    nodes = {"NAME": "Ephemeris",
             "TITLE": "Ephemeris for CFHT QSO"}
    fields = {"DATE_UTC": {"attr": {"datatype": "A", "width": "19", "format": "YYYY-MM-DD hh:mm:ss"},
                           "DESCRIPTION": "UTC Date"},
              "RA_J2000": {"attr": {"datatype": "A", "width": "11", "format": "RAh:RAm:RAs", "unit": "h"},
                           "DESCRIPTION": "Right ascension of target"},
              "DEC_J2000": {"attr": {"datatype": "A", "width": "11", "format": "DEd:DEm:DEs", "unit": "deg"},
                            "DESCRIPTION": "Declination of target"}}

    def __init__(self, name, column_separator=COLUMN_SEPARATOR):
        """
        create an ephmeris target, either with a 'orbfit' object or some mean rate of motion.

        :param name: a string containing the name of the target.
        """

        self.name = str(name)

        self.doc = create_astrores_document()
        table = self.doc.createElement("TABLE")
        table.setAttribute("ID", "Table")
        self.doc.lastChild.appendChild(table)

        self.nodes = EphemTarget.nodes
        self.fields = EphemTarget.fields
        self.column_separator = column_separator
        nodes = self.nodes
        fields = self.fields
        self.field_names = ["DATE_UTC", "RA_J2000", "DEC_J2000"]

        for (key, value) in nodes.iteritems():
            element = self.doc.createElement(key)
            element.appendChild(self.doc.createTextNode(value))
            table.appendChild(element)

        table.getElementsByTagName("TITLE")[0].lastChild.appendData(" target {}".format(self.name))

        table.appendChild(self.doc.createComment("Definition of each field"))

        for fieldName in self.field_names:
            field = self.doc.createElement("FIELD")
            field.setAttribute("name", fieldName)
            for (key, value) in fields[fieldName]['attr'].iteritems():
                field.setAttribute(key, value)
            description = self.doc.createElement("DESCRIPTION")
            description.appendChild(self.doc.createTextNode(fields[fieldName]['DESCRIPTION']))
            field.appendChild(description)
            table.appendChild(field)

        table.appendChild(self.doc.createComment("Data table"))

        data = self.doc.createElement("DATA")
        table.appendChild(data)

        header_lines = self._cdata_header(colsep=self.column_separator)

        csv = self.doc.createElement("CSV")
        csv.setAttribute("headlines", str(len(header_lines)))
        csv.setAttribute("colsep", self.column_separator)
        data.appendChild(csv)

        self.cdata = self.doc.createCDATASection("\n"+"\n".join(header_lines)+"\n")
        csv.appendChild(self.cdata)

    @staticmethod
    def _entry(value, width, colsep):
        return "{value:{width}.{width}}{colsep}".format(value=value, width=width, colsep=colsep)

    def _cdata_header(self, colsep="|"):
        """
        Create a header for the CDATA section, as a visual guide.
        """
        fields = self.fields
        header_lines = []
        line = ""
        for fieldName in self.field_names:
            width = int(fields[fieldName]['attr']['width'])
            line += self._entry(fieldName, width, colsep)
        header_lines.append(line)

        line = ""
        for fieldName in self.field_names:
            width = int(fields[fieldName]['attr']['width'])
            line += self._entry(fields[fieldName]['attr']['format'], width=width, colsep=colsep)
        header_lines.append(line)

        line = ""
        for fieldName in self.field_names:
            width = int(fields[fieldName]['attr']['width'])
            (l, m) = divmod(width, 10)
            guide = ""
            for i in range(l):
                guide += "".join(map(str, range(10)))
            guide += "".join(map(str, range(m)))
            line += self._entry(guide, width=width, colsep=colsep)
        header_lines.append(line)

        line = ""
        for fieldName in self.field_names:
            width = int(fields[fieldName]['attr']['width'])
            guide = "-" * width
            line += self._entry(guide, width=width, colsep=colsep)
        header_lines.append(line)

        return header_lines

    def append(self, coordinate):
        """
        Append an target location to the ephemeris listing.
        """
        fields = self.fields
        sra = coordinate.ra.format(units.hour, sep=':', precision=2, pad=True)
        sdec = coordinate.dec.format(units.degree, sep=':', precision=1, alwayssign=True)
        coord = ICRSCoordinates(sra+" "+sdec, unit=(units.hour, units.degree))
        sra = coord.ra.format(units.hour, sep=":", precision=2, pad=True)
        sdec = coord.dec.format(units.degree, sep=":", precision=1, pad=True)
        sdate = str(coordinate.obstime.replicate(format('iso')))
        self.cdata.appendData(self._entry(sdate, fields["DATE_UTC"]['attr']['width'], colsep=self.column_separator))
        self.cdata.appendData(self._entry(sra, fields["RA_J2000"]['attr']['width'], colsep=self.column_separator))
        self.cdata.appendData(self._entry(sdec, fields["DEC_J2000"]["attr"]["width"], colsep=self.column_separator))
        self.cdata.appendData("\n")

    def writer(self, f_handle):
            self.doc.writexml(f_handle, indent="  ", addindent="  ", newl='\n')

    def save(self, filename=None):
        if filename is None:
            filename = "ET_"+self.name+".xml"
        with file(filename, 'w') as f_handle:
            self.writer(f_handle)



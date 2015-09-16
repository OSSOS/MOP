__author__ = 'bannisterm'

from contextlib import contextmanager

# no. columns must match no. c's
# table has small font and is horizontal.
header = r"\begin{deluxetable}{ccccccccccccc}" + '\n' + \
         r"\tabletypesize{\scriptsize}" + '\n' + \
         r"\rotate" + '\n' + \
         r"\tablecolumns{13}" + '\n' + \
         r"\tablehead{\colhead{$m_{r}$} \vspace{-0.2cm} & " \
         r"\colhead{$\sigma$ $m_{r}$} & " \
         r"\colhead{Detectability} & " \
         r"\colhead{RA} & " \
         r"\colhead{Dec} & " \
         r"\colhead{a} & " \
         r"\colhead{e} & " \
         r"\colhead{i} & " \
         r"\colhead{r$_{H}$} & " \
         r"\colhead{H$_{r}$} & " \
         r"\colhead{MPC} & " \
         r"\colhead{Object} & " \
         r"\colhead{Status} \\" \
         r"\colhead{discovery} & " \
         r"\colhead{all obs} & " \
         r"\colhead{} & " \
         r"\colhead{discov.} & " \
         r"\colhead{discov.} & " \
         r"\colhead{(AU)} & " \
         r"\colhead{} & " \
         r"\colhead{($^{\circ}$)} & " \
         r"\colhead{(AU)} & " \
         r"\colhead{} & " \
         r"\colhead{design.} & " \
         r"\colhead{} & " \
         r"\colhead{} " \
         + r"}" \
         + "\n" \
         + "\startdata \n" \
         + r"\cutinhead{Centaurs}" + "\n"

footer = r"\enddata " + "\n" + \
         r"\tablecomments{$p:q$: object is in the $p:q$ resonance; I: the orbit classification is currently " \
         r"insecure; " \
         r"" \
         r"" \
         r"H: the human operator intervened to declare the orbit security status. " \
         r"$a, e, i$ are J2000 ecliptic barycentric coordinates, with uncertainties from the covariant matrix fit " \
         r"of \citet{Bernstein:2000p444}; full barycentric elements are available at \url{http://www.ossos-survey" \
         r".org/}." \
         r"The full heliocentric orbital elements are available in electronic form from the Minor Planet Center.} " "\n" + \
         "\end{deluxetable} \n"


@contextmanager
def deluxe_table_formatter(outfile):
    ofile = open(outfile, 'w')
    ofile.write(header)

    yield ofile

    ofile.write(footer)
    ofile.close()
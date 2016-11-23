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

extreme_header = r"\begin{deluxetable*}{lDDDDDDDD}" + '\n' + \
                 r"\tablecolumns{10}" + '\n' + \
                 r"\tablecaption{Heliocentric osculating orbital elements of known trans-Neptunian objects " \
                 r"with $q > 40$ and $a > 160$ au for epoch MJD 57600" + '\n' + \
                 r"\label{tab:orbits}"+ '\n' + \
                 r"}"+ '\n' + \
                 r"\tablehead{" \
                 r"\colhead{Name} & " \
                 r"\twocolhead{$q$} & " \
                 r"\twocolhead{$a$} & " \
                 r"\twocolhead{$e$} & " \
                 r"\twocolhead{$i$} & " \
                 r"\twocolhead{$\Omega$} & " \
                 r"\twocolhead{$\omega$} & " \
                 r"\twocolhead{Arc} & " \
                 r"\twocolhead{Discovery} \\ " \
                 r"\colhead{} & " \
                 r"\twocolhead{(au)} & " \
                 r"\twocolhead{(au)} & " \
                 r"\twocolhead{} & " \
                 r"\twocolhead{($^{\circ}$)} & " \
                 r"\twocolhead{($^{\circ}$)} & " \
                 r"\twocolhead{($^{\circ}$)} & " \
                 r"\twocolhead{(days)}  & " \
                 r"\twocolhead{survey} " \
                 r"}"+ '\n' + \
                 r"\startdata "+ '\n'

extreme_footer = r"\enddata"+ '\n' + \
                 r"\tablecomments{Orbital elements retrieved from JPL Horizons on 6 Oct 2016."+ '\n' + \
                 r"$\Omega$ is the longitude of ascending node, $\omega$ the argument of perihelion. " \
                 r"The uncertainties are the $1\sigma$ estimates based on the covariance matrix at the best-fit orbit.}" \
                 r"\end{deluxetable*} "+ '\n'



@contextmanager
def deluxe_table_formatter(outfile):
    ofile = open(outfile, 'w')
    ofile.write(header)

    yield ofile

    ofile.write(footer)
    ofile.close()

@contextmanager
def extreme_tno_table_formatter(outfile):
    ofile = open(outfile, 'w')
    ofile.write(extreme_header)

    yield ofile

    ofile.write(extreme_footer)
    ofile.close()
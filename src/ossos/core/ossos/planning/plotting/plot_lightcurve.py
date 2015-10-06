__author__ = 'Michele Bannister   git:@mtbannister'

import sys

import matplotlib.pyplot as plt
import numpy

from ossos.mpc import TNOdbComment
import parsers
# Define some CSS to control our custom labels
css = """
table
{
  border-collapse: collapse;
}
th
{
  color: #ffffff;
  background-color: #000000;
}
td
{
  background-color: #cccccc;
}
table, th, td
{
  font-family:Sofia Pro, sans-serif;
  border: 1px solid black;
  text-align: right;
}
"""


def all_clean_phot(obj):
    try:
        tno = parsers.ossos_discoveries(single_object=obj, no_nt_and_u=False)[0]
    except:
        return [], [], []
    # print len(tno.orbit.observations)

    # consider only the cleanest photometry: nothing with involvement or other weirdness
    tnoobs = []
    for n in tno.orbit.observations:
        if isinstance(n.comment, TNOdbComment):
            if n.comment.photometry_note == 'Y' and (n.comment.mpc_note == ' ' or n.comment.mpc_note == ''):
                tnoobs.append(n)
    if len(tnoobs) > 0:  # there better be some observations that are untainted
        tno_mags = [n.comment.mag for n in tnoobs]
        mag_dates = [n.date.jd for n in tnoobs]
        mag_err = [n.comment.mag_uncertainty for n in tnoobs]
        return tno_mags, mag_err, mag_dates
    else:
        print 'no unflagged photometry exists for {}'.format(obj)
        return [], [], []


def stddev_phot(obj):
    tno_mags, mag_err, mag_dates = all_clean_phot(obj)
    # without additional parameters, this returns the population std deviation.
    retval = numpy.std(numpy.array(tno_mags))
    # print obj, retval

    return retval


def plot_all_obs_single_object(obj):
    tno_mags, mag_err, mag_dates = all_clean_phot(obj)

    plt.figure()
    plt.errorbar(mag_dates, tno_mags, yerr=mag_err, fmt='.', )
    plt.gca().invert_yaxis()
    plt.title(obj)
    plt.draw()
    plt.show()


if __name__ == '__main__':
    obj = sys.argv[1]
    plot_all_obs_single_object(obj)

    fig, ax = plt.subplots(1, 1, figsize=(35, 10))

    variation = []
    objects = parsers.ossos_discoveries()
    labels = []
    for tno in objects:  # [n for n in objects if n.name in parameters.COLOSSOS]:
        tno_mags, mag_err, mag_dates = all_clean_phot(obj)
        if len(tno_mags) > 0:
            tnomax = max(tno_mags)
            tnomin = min(tno_mags)
            variation.append(tno_mags)
            html = '<table><tr><th>Name</th><td>{:}</td></tr></table>'.format(tno.name)
            # '<tr><th>Dist</th><td>{:.2f}</td></tr>' \
            # '<tr><th>Peri</th><td>{:.2f}</td></tr>
            labels.append(tno.name)
        else:
            print("{} has no clean observations.\n".format(tno.name))

    # points = ax.errorbar(range(0, len(labels)), variation, fmt='.', ecolor='b')
    points = ax.boxplot(variation, labels=labels, showbox=False, showcaps=False, whiskerprops={'linestyle': '-'})
    ax.grid(True, alpha=0.3)
    ax.set_ylabel('$m_{r}$')
    plt.gca().invert_yaxis()
    plt.setp(ax.get_xticklabels(), rotation=90)
    plt.draw()
    outfile = 'All-OSSOS-variation.pdf'
    plt.savefig(outfile, transparent=True, bbox_inches='tight')

    # # Activate the interactivity
    # tooltip = plugins.PointHTMLTooltip(points[0], labels, voffset=10, hoffset=10, css=css)
    # plugins.connect(fig, tooltip)
    # mpld3.show()


    # FIXME: want to add a plot
    #  play with standard deviation of mag of objects vs H vs r_helio

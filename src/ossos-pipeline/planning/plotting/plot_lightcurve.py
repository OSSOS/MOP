__author__ = 'Michele Bannister   git:@mtbannister'

import matplotlib.pyplot as plt

import parsers
import parameters

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


def plot_all_obs_single_object(obj):
    tno = parsers.ossos_discoveries(single_object=obj)[0]
    print len(tno.orbit.observations)
    plt.figure()

    # can have valid measurements with photometry blanked
    tnoobs = [n for n in tno.orbit.observations if not n.null_observation and not n.comment.photometry_note == 'Z']
    tno_mags = [n.comment.mag for n in tnoobs]
    mag_dates = [n.date.jd for n in tnoobs]
    mag_err = [n.comment.mag_uncertainty for n in tnoobs]

    print len(tno_mags), len(mag_dates), len(mag_err)
    print tno_mags
    print mag_dates
    print mag_err

    plt.errorbar(mag_dates, tno_mags, yerr=mag_err, fmt='.', )
    plt.gca().invert_yaxis()
    plt.title(obj)
    plt.draw()
    plt.show()


if __name__ == '__main__':
    # obj = sys.argv[1]
    # plot_all_obs_single_object(obj)

    fig, ax = plt.subplots(1, 1, figsize=(25, 10))

    variation = []
    objects = parsers.ossos_discoveries()
    labels = []
    for tno in [n for n in objects if n.name in parameters.COLOSSOS]:
        # consider only the cleanest photometry: nothing with involvement or other weirdness
        tnoobs = []
        for n in tno.orbit.observations:
            if n.comment.photometry_note == 'Y' and (n.comment.mpc_note == ' ' or n.comment.mpc_note == ''):
                tnoobs.append(n)
        if len(tnoobs) > 0:  # there better be some observations that are untainted
            tno_mags = [n.comment.mag for n in tnoobs]

            # mag_dates = [n.date.jd for n in tnoobs]
            # mag_err = [n.comment.mag_uncertainty for n in tnoobs]
            # mag_maxes = []
            # mag_minima = []
            # for i, n in enumerate(tno_mags):
            # mag_maxes.append(n + mag_err[i])
            #     mag_minima.append(n + mag_err[i])

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
    outfile = 'ColOSSOS-variation.pdf'
    plt.savefig(outfile, transparent=True, bbox_inches='tight')

    # # Activate the interactivity
    # tooltip = plugins.PointHTMLTooltip(points[0], labels, voffset=10, hoffset=10, css=css)
    # plugins.connect(fig, tooltip)
    # mpld3.show()

from matplotlib import cm
from matplotlib import rcParams
import math 

def setFigForm():
    """set the rcparams to EmulateApJ columnwidth=245.26 pts """

    fig_width_pt = 245.26*2
    inches_per_pt = 1.0/72.27
    golden_mean = (math.sqrt(5.)-1.0)/2.0
    fig_width = fig_width_pt*inches_per_pt
    fig_height = fig_width*golden_mean
    fig_size = [1.5*fig_width, fig_height]
    
    params = {'backend': 'ps',
              'axes.labelsize': 12,
              'text.fontsize': 12,
              'legend.fontsize': 7,
              'xtick.labelsize': 11,
              'ytick.labelsize': 11,
              'text.usetex': True,
              'font.family': 'serif',
              'font.serif': 'Times',
              'image.aspect': 'auto',
              'figure.subplot.left': 0.1,
              'figure.subplot.bottom': 0.1,
              'figure.subplot.hspace': 0.25,
              'figure.figsize': fig_size}
    
    rcParams.update(params)


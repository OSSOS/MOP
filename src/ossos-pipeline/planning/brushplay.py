__author__ = 'Michele Bannister   git:@mtbannister'

import matplotlib
import matplotlib.pyplot as plt
import mpld3
from mpld3 import plugins, utils

# matplotlib.rcParams['font.size'] = 12   # good for posters/slides
matplotlib.rcParams['font.family'] = 'sans-serif'
matplotlib.rcParams['font.sans-serif'] = ['Sofia Pro', 'Tahoma']


class LinkedBrush(plugins.PluginBase):
    JAVASCRIPT = r"""
    var LinkedBrushPlugin = function(fig, prop){
      this.fig = fig;
      this.prop = mpld3.process_props(this, prop, {}, ["id"]);

      var brush_plug = this;

      mpld3.Toolbar.prototype.buttonDict["brush"] = mpld3.ButtonFactory({
        onClick: this.onClick.bind(this),
        draw: function(){
            mpld3.BaseButton.prototype.draw.apply(this);
            var enable_zoom = brush_plug.fig.enable_zoom.bind(brush_plug.fig);
            var disable_brush = brush_plug.disable.bind(brush_plug);
            brush_plug.fig.enable_zoom = function(){
                   disable_brush();
                   fig.toolbar.toolbar.selectAll(".mpld3-brushbutton")
                       .classed({pressed: false,
                                 active: false});
                   enable_zoom();
            };
            this.onClick();  // enable the button
        },
        icon: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBI
        \nWXMAAEQkAABEJAFAZ8RUAAAAB3RJTUUH3gMCEiQKB9YaAgAAAWtJREFUOMuN0r1qVVEQhuFn700k\nnfEvBq0iNiIiOKXgH4KCaBeIhWARK
        /EibLwFCwVLjyAWaQzRGG9grC3URkHUBKKgRuWohWvL5pjj\nyTSLxcz7rZlZHyMiItqzFxGTEVF18/UoODNFxDIO4x12dkXqTcBPsCUzD
        +AK3ndFqhHwEsYz82gn\nN4dbmMRK9R/4KY7jAvbiWmYeHBT5Z4QCP8J1rGAeN3GvU3Mbl/Gq3qCDcxjLzOV+v78fq/iFIxFx
        \nPyJ2lNJpfBy2g59YzMyzEbEVLzGBJjOriLiBq5gaJrCIU3hcRCbwAtuwjm/Yg/V6I9NgDA1OR8RC\nZq6Vcd7iUwtn5h8fdMBdETGPE
        +Xe4ExELDRNs4bX2NfCUHe+7UExyfkCP8MhzOA7PuAkvrbwXyNF
        \nxF3MDqxiqlhXC7SPdaOKiN14g0u4g3H0MvOiTUSNY3iemb0ywmfMdfYyUmAJ2yPiBx6Wr/oy2Oqw\n+A1SupBzAOuE
        /AAAAABJRU5ErkJggg==\n",
       });
    }

    LinkedBrushPlugin.prototype.onClick = function(){
      if(this.enabled){
        this.disable();
      }else{
        this.enable();
        this.fig.disable_zoom();
        this.fig.toolbar.toolbar.selectAll(".mpld3-movebutton")
                   .classed({pressed: false,
                             active: false});
      }
      this.fig.toolbar.toolbar.selectAll(".mpld3-brushbutton")
                .classed({pressed: this.enabled,
                          active: !this.enabled});
    }

    LinkedBrushPlugin.prototype.draw = function(){
      var obj = mpld3.get_element(this.prop.id);
      var fig = this.fig;
      var dataKey = ("offsets" in obj.prop) ? "offsets" : "data";

      mpld3.insert_css("#" + fig.figid + " rect.extent",
                       {"fill": "#000",
                        "fill-opacity": .125,
                        "stroke": "#fff"});

      mpld3.insert_css("#" + fig.figid + " path.mpld3-hidden",
                       {"stroke": "#ccc !important",
                        "fill": "#ccc !important"});

      var dataClass = "mpld3data-" + obj.prop[dataKey];

      var brush = d3.svg.brush()
                      .on("brushstart", brushstart)
                      .on("brush", brushmove)
                      .on("brushend", brushend);

      // Label all data points for access below
      fig.axes.forEach(function(ax){
         ax.elements.forEach(function(el){
            if(el.prop[dataKey] === obj.prop[dataKey]){
               el.group.classed(dataClass, true);
            }
         });
         brush.x(ax.x).y(ax.y);
         //ax.axes.call(brush);
      });


      // For fast brushing, precompute a list of selection properties
      // properties to apply to the selction.
      var data_map = [];
      var dataToBrush = fig.canvas.selectAll("." + dataClass)
                           .each(function(){
                              for(var i=0; i<fig.axes.length; i++){
                                var ax = fig.axes[i];
                                for(var j=0; j<ax.elements.length; j++){
                                  var el = ax.elements[j];
                                  if("group" in el && el.group[0][0] === this){
                                    data_map.push({i_ax: i,
                                                   ix: el.prop.xindex,
                                                   iy: el.prop.yindex});
                                    return;
                                  }
                                }
                              }
                            });

      dataToBrush.data(data_map)
                 .call(brush);

      var currentData;

      function brushstart(d){
        if(currentData != this){
          d3.select(currentData).call(brush.clear());
          currentData = this;
          brush.x(fig.axes[d.i_ax].x)
               .y(fig.axes[d.i_ax].y);
        }
      }

      function brushmove(d){
        var e = brush.extent();
        dataToBrush.selectAll("path")
                   .classed("mpld3-hidden",
                       function(p) {
                           return e[0][0] > p[d.ix] ||  e[1][0] < p[d.ix] ||
                                  e[0][1] > p[d.iy] || e[1][1] < p[d.iy];
                       });
      }

      function brushend(d){
        if (brush.empty()){
            dataToBrush.selectAll("path")
                       .classed("mpld3-hidden", false);
        }
      }

      function brushend_clear(){
        d3.select(this).call(brush.clear());
      }

      this.enable = function(){
        brush.on("brushstart", brushstart)
             .on("brush", brushmove)
             .on("brushend", brushend);
        this.fig.canvas.selectAll("rect.background")
              .style("cursor", "crosshair");
        this.fig.canvas.selectAll("rect.extent, rect.resize")
              .style("display", null);
        this.enabled = true;
      }

      this.disable = function(){
        brush.on("brushstart", null)
             .on("brush", null)
             .on("brushend", brushend_clear)
             .clear();
        this.fig.canvas.selectAll("rect.background")
              .style("cursor", null);
        this.fig.canvas.selectAll("rect.extent, rect.resize")
              .style("display", "none");
        this.enabled = false;
      }

      this.disable();
    }

    mpld3.register_plugin("linkedbrush", LinkedBrushPlugin);
    """

    def __init__(self, points):
        if isinstance(points, matplotlib.lines.Line2D):
            suffix = "pts"
        else:
            suffix = None

        self.dict_ = {"type": "linkedbrush",
                      "clear_toolbar": False,
                      "id": utils.get_id(points, suffix),
                      "buttons": "brush"}

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


class tno(object):
    def __init__(self):
        self.classification = None
        self.name = None
        self.mag = None
        self.mag_stdev = None
        self.dist = None
        self.dist_e = None
        self.nobs = None
        self.arclen = None
        self.a = None
        self.a_e = None
        self.e = None
        self.e_e = None
        self.i = None
        self.i_e = None
        self.peri = None
        self.diameter = None


def data_parser():
    retval = []
    infs = ['/Users/michele/Dropbox/OSSOS/o13eBlockV2.dat', '/Users/michele/Dropbox/OSSOS/OblockStage0_ossosv2.txt']
    for inf in infs:
        with open(inf, 'r') as infile:
            if inf == infs[0]:
                # columns:
                # classification  object  mag  stdev   dist  ..E nobs time av_xres av_yres max_x max_y (not sure what
                #  use those)
                # a ..E  e  ..E  i ..E    node     ..E    argperi     ..E           M        ..E   ra_dis  dec_dis
                for line in infile.readlines()[
                            11:len(infile.readlines()) - 5]:  # FIXME: better parsing for other discovery files
                    ln = line.split()
                    obj = tno()
                    obj.classification = ln[0]
                    obj.name = ln[1]
                    obj.mag = float(ln[2])
                    obj.mag_stdev = float(ln[3])
                    obj.dist = float(ln[4])
                    obj.dist_e = float(ln[5])
                    obj.nobs = int(ln[6])
                    obj.arclen = float(ln[7])
                    obj.a = float(ln[12])
                    obj.a_e = float(ln[13])
                    obj.e = float(ln[14])
                    obj.e_e = float(ln[15])
                    obj.i = float(ln[16])
                    obj.i_e = float(ln[17])
                    obj.peri = obj.a * (1. - obj.e)
                    retval.append(obj)
            else:
                for line in infile.readlines()[14:]:
                    ln = line.split()
                    obj = tno()
                    obj.name = ln[0]
                    obj.mag = float(ln[1])
                    obj.mag_stdev = float(ln[2])
                    obj.dist = float(ln[3])
                    obj.dist_e = float(ln[4])
                    obj.nobs = int(ln[5])
                    obj.arclen = float(ln[6])
                    obj.i = float(ln[7])
                    obj.i_e = float(ln[8])
                    retval.append(obj)

    return retval


dat = data_parser()
data = [d for d in dat if not d.name.__contains__('nt')]  # leave off the uncharacterised for now, they fill up the plot

# suitably bright ones
bright = [d for d in data if d.mag < 23.5]
print len(bright)

# just pulling out the cold classicals...
temp = [d for d in data if (42. < d.a < 46.) and d.e < 0.15 and d.i < 5. and d.mag < 23.5]
print len(temp)
data = temp

fig, ax = plt.subplots(3, 1, sharex=True, figsize=(10, 10))
ax[0].set_title('OSSOS cold classical discoveries after 2013 observations in v2 release')
fig.subplots_adjust(hspace=0.12)

# first all the objects. Some of these don't have all their orbital parameters yet, but dist and i are ok
points = ax[0].errorbar([d.dist for d in data], [d.i for d in data],
                        xerr=[d.dist_e for d in data], yerr=[d.i_e for d in data],
                        fmt='.', alpha=0.3, ecolor='0.1', ms=10)
# ax[0].set_ylim([-1, 90])
ax[0].grid(True, alpha=0.3)
ax[0].set_ylabel('inclination (degrees)')
ax[0].set_xlabel('heliocentric distance (AU)')
# ax[0].annotate('')

# now the plots for objects where we have much better arcs
dset = [d for d in data if d.a is not None]
ax[1].errorbar([d.a for d in dset], [d.i for d in dset],
               xerr=[d.a_e for d in dset], yerr=[d.i_e for d in dset],
               fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[1].set_ylabel('inclination (degrees)')
# ax[1].set_ylim([-1, 35])
ax[1].grid(True, alpha=0.3)
ax[1].set_xlabel('semimajor axis (AU)')

ax[2].errorbar([d.a for d in dset], [d.e for d in dset],
               xerr=[d.a_e for d in dset], yerr=[d.e_e for d in dset],
               fmt='.', alpha=0.4, ecolor='0.1', ms=10)
ax[2].set_ylabel('eccentricity')
# ax[2].set_ylim([-0.01, .8])
ax[2].grid(True, alpha=0.3)
ax[2].set_xlabel('semimajor axis (AU)')
# plt.xlim([30, 80])
plt.xlim([42, 46])

# plt.xlabel('semimajor axis (AU)')

# plt.legend()
plt.draw()
outfile = 'plots/2013_OSSOS_discoveries_orbits'
plt.savefig(outfile + '.pdf', transparent=True)

# plugins.connect(fig, LinkedBrush(points))

labels = []
for obj in data:
    if obj.peri is not None:
        # label = {'dist': obj.dist, 'peri':obj.peri, 'obj':obj.name}
        html = '<table><tr><th>Name</th><td>{:}</td></tr>' \
               '<tr><th>Dist</th><td>{:.2f}</td></tr>' \
               '<tr><th>Peri</th><td>{:.2f}</td></tr></table>'.format(obj.name, obj.dist, obj.peri)
        labels.append(html)

tooltip = plugins.PointHTMLTooltip(points[0], labels, voffset=10, hoffset=10, css=css)
plugins.connect(fig, tooltip)
mpld3.show()





# # now plot again with the tno sizes indicated (approximating albedo to 4%)
#
# fig, ax = plt.subplots(3, 1, sharex=True, figsize=(10,10))
# ax[0].set_title('OSSOS discoveries in v2 release with points scaled by object size')
# fig.subplots_adjust(hspace=0.12)
#
# for p in data:
#     #diameter = ( (p.dist**2)*(10**((18.8 - p.mag)/5.)) )/math.sqrt(0.04)  # Fraser and Kavelaars, 2008
#     au = 149597871.
#     K = 5.61 # Vilenius et al., 2012
#     albedo = 0.1 # reasonable interval between cold and hot, could subdivide later as desired by incl.
#     diameter = 10**(math.sqrt((5.*math.log10((p.dist*au)**2) + K - p.mag)/2.5*albedo))
#
#     print p.dist, diameter, diameter*100., p.mag
#     p.diameter = diameter
#
# # first all the objects. Some of these don't have all their orbital parameters yet, but dist and i are ok
# for p in data:  # do this point-by-point to calculate individual object sizes
#     ax[0].errorbar(p.dist, p.i,
#                    xerr=p.dist_e, yerr=p.i_e,
#                    fmt='.', alpha=0.3, ecolor='0.1', ms=p.diameter/100., mfc='b', mec='b')
# ax[0].set_ylim([-1, 90])
# ax[0].grid(True, alpha=0.3)
# ax[0].set_ylabel('inclination (degrees)')
# ax[0].set_xlabel('heliocentric distance (AU)')
# # ax[0].annotate('')
#
# # now the plots for objects where we have much better arcs
# dset = [d for d in data if d.a is not None]
# for p in dset:
#     ax[1].errorbar(p.a, p.i,
#                     xerr=p.a_e, yerr=p.i_e,
#                     fmt='.', alpha=0.4, ecolor='0.1', ms=p.diameter/100., mfc='b', mec='b')
# ax[1].set_ylabel('inclination (degrees)')
# ax[1].set_ylim([-1, 35])
# ax[1].grid(True, alpha=0.3)
# ax[1].set_xlabel('semimajor axis (AU)')
#
# for p in dset:
#     ax[2].errorbar(p.a, p.e,
#                    xerr=p.a_e, yerr=p.e_e,
#                    fmt='.', alpha=0.4, ecolor='0.1', ms=p.diameter/100., mfc='b', mec='b')
# ax[2].set_ylabel('eccentricity')
# ax[2].set_ylim([-0.01, .8])
# ax[2].grid(True, alpha=0.3)
# ax[2].set_xlabel('semimajor axis (AU)')
#
# # plt.show()
#
# plt.draw()
# outfile = 'plots/2013_OSSOS_discoveries_sizes'
# plt.savefig(outfile+'.pdf', transparent=True)

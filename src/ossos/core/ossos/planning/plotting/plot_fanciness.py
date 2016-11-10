__author__ = 'Michele Bannister   git:@mtbannister'

import numpy as np
import matplotlib
import matplotlib.pyplot as plt

# from mpld3 import plugins, utils
import brewer2mpl

ALMOST_BLACK = '#262626'

# nicer defaults
# every colourbrewer scale: http://bl.ocks.org/mbostock/5577023
# hues create primary visual differences between classes; does not imply magnitude differences between classes
set2 = brewer2mpl.get_map('Set2', 'qualitative', 8).mpl_colors
# # sequential for ordered data where light is low values, dark is high values
# ylorrd = brewer2mpl.get_map('YlOrRd', 'sequential', 9).mpl_colors
# # diverging for equal emphasis on mid-range critical values and extremes at both ends of data range
# puor = brewer2mpl.get_map('PuOr', 'diverging', 11).mpl_colors

# FIXME: figure out how to export these to a figure

# rcParams['figure.figsize'] = (10, 10)
# rcParams['figure.dpi'] = 150
# rcParams['axes.color_cycle'] = puor
matplotlib.rcParams['font.size'] = 12  # good for posters/slides
matplotlib.rcParams['patch.facecolor'] = set2[0]
matplotlib.rcParams['font.family'] = 'sans-serif'
matplotlib.rcParams['font.sans-serif'] = ['Sofia Pro', 'Tahoma']


def remove_border(axes=None, keep=('left', 'bottom'), remove=('right', 'top'), labelcol=ALMOST_BLACK):
    """
    Minimize chart junk by stripping out unnecessary plot borders and axis ticks.
    The top/right/left/bottom keywords toggle whether the corresponding plot border is drawn
    """
    ax = axes or plt.gca()
    for spine in remove:
        ax.spines[spine].set_visible(False)
    for spine in keep:
        ax.spines[spine].set_linewidth(0.5)
        # ax.spines[spine].set_color('white')

    # remove all ticks, then add back the ones in keep
    # Does this also need to specify the ticks' colour, given the axes/labels are changed?
    ax.yaxis.set_ticks_position('none')
    ax.xaxis.set_ticks_position('none')
    # ax.xaxis.set_ticklabels("")
    # ax.yaxis.set_ticklabels("")

    for spine in keep:
        if spine == 'top':
            ax.xaxis.tick_top()
        if spine == 'bottom':
            ax.xaxis.tick_bottom()
            # match the label colour to that of the axes
            ax.xaxis.label.set_color(labelcol)
            ax.xaxis.set_tick_params(color=labelcol, labelcolor=labelcol)
        if spine == 'left':
            ax.yaxis.tick_left()
            ax.yaxis.label.set_color(labelcol)
            ax.yaxis.set_tick_params(color=labelcol, labelcolor=labelcol)
        if spine == 'right':
            ax.yaxis.tick_right()

    return


def clean_legend():
    # as per prettyplotlib.
    # Remove the line around the legend box, and instead fill it with a light grey
    # Also only use one point for the scatterplot legend because the user will
    # get the idea after just one, they don't need three.
    light_grey = np.array([float(248) / float(255)] * 3)
    legend = ax.legend(frameon=True, scatterpoints=1)
    rect = legend.get_frame()
    rect.set_facecolor(light_grey)
    rect.set_linewidth(0.0)

    # Change the legend label colors to almost black, too
    texts = legend.texts
    for t in texts:
        t.set_color(almost_black)


# class LinkedBrush(plugins.PluginBase):
#     JAVASCRIPT = r"""
#     var LinkedBrushPlugin = function(fig, prop){
#       this.fig = fig;
#       this.prop = mpld3.process_props(this, prop, {}, ["id"]);
#
#       var brush_plug = this;
#
#       mpld3.Toolbar.prototype.buttonDict["brush"] = mpld3.ButtonFactory({
#         onClick: this.onClick.bind(this),
#         draw: function(){
#             mpld3.BaseButton.prototype.draw.apply(this);
#             var enable_zoom = brush_plug.fig.enable_zoom.bind(brush_plug.fig);
#             var disable_brush = brush_plug.disable.bind(brush_plug);
#             brush_plug.fig.enable_zoom = function(){
#                    disable_brush();
#                    fig.toolbar.toolbar.selectAll(".mpld3-brushbutton")
#                        .classed({pressed: false,
#                                  active: false});
#                    enable_zoom();
#             };
#             this.onClick();  // enable the button
#         },
#         icon: "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBI
#         \nWXMAAEQkAABEJAFAZ8RUAAAAB3RJTUUH3gMCEiQKB9YaAgAAAWtJREFUOMuN0r1qVVEQhuFn700k\nnfEvBq0iNiIiOKXgH4KCaBeIhWARK
#         /EibLwFCwVLjyAWaQzRGG9grC3URkHUBKKgRuWohWvL5pjj\nyTSLxcz7rZlZHyMiItqzFxGTEVF18/UoODNFxDIO4x12dkXqTcBPsCUzD
#         +AK3ndFqhHwEsYz82gn\nN4dbmMRK9R/4KY7jAvbiWmYeHBT5Z4QCP8J1rGAeN3GvU3Mbl/Gq3qCDcxjLzOV+v78fq/iFIxFx
#         \nPyJ2lNJpfBy2g59YzMyzEbEVLzGBJjOriLiBq5gaJrCIU3hcRCbwAtuwjm/Yg/V6I9NgDA1OR8RC\nZq6Vcd7iUwtn5h8fdMBdETGPE
#         +Xe4ExELDRNs4bX2NfCUHe+7UExyfkCP8MhzOA7PuAkvrbwXyNF
#         \nxF3MDqxiqlhXC7SPdaOKiN14g0u4g3H0MvOiTUSNY3iemb0ywmfMdfYyUmAJ2yPiBx6Wr/oy2Oqw\n+A1SupBzAOuE
#         /AAAAABJRU5ErkJggg==\n",
#        });
#     }
#
#     LinkedBrushPlugin.prototype.onClick = function(){
#       if(this.enabled){
#         this.disable();
#       }else{
#         this.enable();
#         this.fig.disable_zoom();
#         this.fig.toolbar.toolbar.selectAll(".mpld3-movebutton")
#                    .classed({pressed: false,
#                              active: false});
#       }
#       this.fig.toolbar.toolbar.selectAll(".mpld3-brushbutton")
#                 .classed({pressed: this.enabled,
#                           active: !this.enabled});
#     }
#
#     LinkedBrushPlugin.prototype.draw = function(){
#       var obj = mpld3.get_element(this.prop.id);
#       var fig = this.fig;
#       var dataKey = ("offsets" in obj.prop) ? "offsets" : "data";
#
#       mpld3.insert_css("#" + fig.figid + " rect.extent",
#                        {"fill": "#000",
#                         "fill-opacity": .125,
#                         "stroke": "#fff"});
#
#       mpld3.insert_css("#" + fig.figid + " path.mpld3-hidden",
#                        {"stroke": "#ccc !important",
#                         "fill": "#ccc !important"});
#
#       var dataClass = "mpld3data-" + obj.prop[dataKey];
#
#       var brush = d3.svg.brush()
#                       .on("brushstart", brushstart)
#                       .on("brush", brushmove)
#                       .on("brushend", brushend);
#
#       // Label all data points for access below
#       fig.axes.forEach(function(ax){
#          ax.elements.forEach(function(el){
#             if(el.prop[dataKey] === obj.prop[dataKey]){
#                el.group.classed(dataClass, true);
#             }
#          });
#          brush.x(ax.x).y(ax.y);
#          //ax.axes.call(brush);
#       });
#
#
#       // For fast brushing, precompute a list of selection properties
#       // properties to apply to the selction.
#       var data_map = [];
#       var dataToBrush = fig.canvas.selectAll("." + dataClass)
#                            .each(function(){
#                               for(var i=0; i<fig.axes.length; i++){
#                                 var ax = fig.axes[i];
#                                 for(var j=0; j<ax.elements.length; j++){
#                                   var el = ax.elements[j];
#                                   if("group" in el && el.group[0][0] === this){
#                                     data_map.push({i_ax: i,
#                                                    ix: el.prop.xindex,
#                                                    iy: el.prop.yindex});
#                                     return;
#                                   }
#                                 }
#                               }
#                             });
#
#       dataToBrush.data(data_map)
#                  .call(brush);
#
#       var currentData;
#
#       function brushstart(d){
#         if(currentData != this){
#           d3.select(currentData).call(brush.clear());
#           currentData = this;
#           brush.x(fig.axes[d.i_ax].x)
#                .y(fig.axes[d.i_ax].y);
#         }
#       }
#
#       function brushmove(d){
#         var e = brush.extent();
#         dataToBrush.selectAll("path")
#                    .classed("mpld3-hidden",
#                        function(p) {
#                            return e[0][0] > p[d.ix] ||  e[1][0] < p[d.ix] ||
#                                   e[0][1] > p[d.iy] || e[1][1] < p[d.iy];
#                        });
#       }
#
#       function brushend(d){
#         if (brush.empty()){
#             dataToBrush.selectAll("path")
#                        .classed("mpld3-hidden", false);
#         }
#       }
#
#       function brushend_clear(){
#         d3.select(this).call(brush.clear());
#       }
#
#       this.enable = function(){
#         brush.on("brushstart", brushstart)
#              .on("brush", brushmove)
#              .on("brushend", brushend);
#         this.fig.canvas.selectAll("rect.background")
#               .style("cursor", "crosshair");
#         this.fig.canvas.selectAll("rect.extent, rect.resize")
#               .style("display", null);
#         this.enabled = true;
#       }
#
#       this.disable = function(){
#         brush.on("brushstart", null)
#              .on("brush", null)
#              .on("brushend", brushend_clear)
#              .clear();
#         this.fig.canvas.selectAll("rect.background")
#               .style("cursor", null);
#         this.fig.canvas.selectAll("rect.extent, rect.resize")
#               .style("display", "none");
#         this.enabled = false;
#       }
#
#       this.disable();
#     }
#
#     mpld3.register_plugin("linkedbrush", LinkedBrushPlugin);
#     """
#
#     def __init__(self, points):
#         if isinstance(points, matplotlib.lines.Line2D):
#             suffix = "pts"
#         else:
#             suffix = None
#
#         self.dict_ = {"type": "linkedbrush",
#                       "clear_toolbar": False,
#                       "id": utils.get_id(points, suffix),
#                       "buttons": "brush"}

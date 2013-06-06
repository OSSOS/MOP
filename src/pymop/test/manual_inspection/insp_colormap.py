__author__ = "David Rusk <drusk@uvic.ca>"

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider

from pymop.gui import imgviewer


def create_test_image():
    x = np.linspace(0, 2 * np.pi, 120)
    y = np.linspace(0, 2 * np.pi, 100).reshape(-1, 1)
    return np.sin(x) + np.cos(y)


def main():
    img = create_test_image()
    colormap = imgviewer.GrayscaleColorMap()

    img_ax = plt.subplot(1, 1, 1)
    plt.subplots_adjust(left=0.15, bottom=0.25)

    axcolor = 'lightgoldenrodyellow'
    contrast_ax = plt.axes([0.15, 0.15, 0.65, 0.03], axisbg=axcolor)
    bias_ax = plt.axes([0.15, 0.1, 0.65, 0.03], axisbg=axcolor)

    contrast_slider = Slider(contrast_ax, "Contrast", 0.0, 1.0, valinit=0.5)
    bias_slider = Slider(bias_ax, "Bias", 0.0, 1.0, valinit=0.5)

    def draw():
        cmap = colormap.as_mpl_cmap()
        plt.subplot(1, 1, 1)
        plt.imshow(img, cmap=cmap)
        plt.colorbar(orientation="horizontal")
        plt.show()

    def update_contrast(contrast):
        print "Setting contrast to %f" % contrast
        colormap.set_contrast(contrast)
        # draw()

    def update_bias(bias):
        print "Setting bias to %f" % bias
        colormap.set_bias(bias)
        # draw()

    contrast_slider.on_changed(update_contrast)
    bias_slider.on_changed(update_bias)

    draw()


if __name__ == "__main__":
    main()

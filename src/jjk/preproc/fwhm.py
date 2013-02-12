import matplotlib.pyplot as plt
import numpy as np

f = np.loadtxt('fwhm',unpack=True)
plt.plot(f,',')
plt.ylabel('FWHM')
plt.axhline(y=1/0.187)
plt.axhline(y=0.8/0.187)
plt.savefig('fwhm.png')


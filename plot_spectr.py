import numpy as np
import matplotlib.pyplot as plt

wn, ka = np.loadtxt(fname='SPECTR', skiprows=1, unpack=True)
fig, ax = plt.subplots()

ax.plot(wn, ka, color='r')

plt.xlabel('Wavenumber [cm^{-1}]')
plt.ylabel('log absorption koefficien [km^{-1}]')
plt.grid(visible=True, which='major', color='g')
plt.show()
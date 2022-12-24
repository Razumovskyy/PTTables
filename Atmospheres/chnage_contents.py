import numpy as np

height, pressure, temperature, gas1, gas2, gas3 = np.loadtxt(fname='Venus_3gases.dat', skiprows=5, unpack=True)
np.savetxt("Venus_1gas.dat", np.transpose((height, pressure, temperature, gas2)))
import numpy as np
import matplotlib.pyplot as plt
from algo import TA

# global parameters
n = 100
A = 10
B = 2*np.pi
phi_A = 1
phi_B = 0

# define charge distribution
def rho(x):
    return A*np.cos(B*x)

# exact sol
def phi(x):
    A/B**2*np.cos(B*x) - x + 1 -A/B**2


# define x domain
X = np.linspace(0, 1, num=n-1)
h = X[1] - X[0]

# define Matrix for 2nd deriv approx
diag =  np.array([-2 for i in range(n)] * 1 / h ** 2)
diag =  np.concatenate(([1], diag, [1]))

off = np.array([1 for i in range(n-1)] * 1 / h ** 2)
off =  np.concatenate(([0], off, [0]))

F = -rho(X)
F = np.concatenate(([1], F, [0]))
print(TA(off, diag, F))
plt.plot(TA(off, diag, F))
plt.show()
"Done"
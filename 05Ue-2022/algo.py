import numpy as np

# Thomas algorithm
def TA(off, diag, sol):
    n = len(sol)

    alpha = np.zeros(n)
    beta = np.zeros(n)
    out = np.zeros(n)
    out[0] = 1

    for i in range(n-1, 0, -1):
        alpha[i-1] = - off[i] / (diag[i] + off[i] * alpha[i])
        beta[i-1] =  (sol[i] - off[i] * beta[i])/(diag[i] + off[i] * alpha[i])

    for i in range(1, n):
        out[i] = alpha[i-1] * out[i-1] + beta[i-1]

    return out
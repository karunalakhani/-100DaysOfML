#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 2_2 : use gradient descent to optimize w0 in loglikelihood

import numpy as np
from scipy import optimize

def f(w):
    t_ = np.log(np.exp(w)/(1 + np.exp(w)))
    f_ = np.log(1./(1 + np.exp(w)))
    return (6*t_) + (5*f_)

def fp(w):
    t_ = 1./(1 + np.exp(w))
    f_ = (-1 * np.exp(w))/(1 + np.exp(w))
    return (6*t_) + (5*f_)
 
sol = optimize.minimize(lambda x: -f(x),
                        x0=1,
                        jac=lambda x: -fp(x),
                        method='BFGS')

print(sol.x)
print (sol)

print(np.exp(sol.x[0])/(1 + np.exp(sol.x[0])))
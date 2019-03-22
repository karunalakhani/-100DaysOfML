#!/usr/bin/python3
# -*- coding: utf-8 -*-

# quizz 10a : simple neural networks output value

import numpy as np


# Question 1 : simple multilayer NN
# 3 inputs, 1 hidden tanh, 1 output binary sigmoid
x1=2; x2=-1; x3=3
v0=2.; v1=1.; v2=1.; v3=-2.
w0=2; w=1

z=np.tanh(v0 + x1*v1 + x2*v2 + x3*v3)
y1=1./(1+np.exp(-w0-w*z))

print("Q1: ",y1)


#Question 2 : perceptron
# 6 inputs, output sign function

x1=2; x2=3; x3=6
w0=1.; w1=2.; w2=3.; w3=-4.

y2=np.sign(w0 + x1*w1 + x2*w2 + x3*w3)
print("Q2: ",y2)


# Question 3 : multilayer NN
# 3 inputs, 2 hidden tanh, 1 output binary sigmoid
x=[2., -1., 3.]
v1=[2., 1., 1., -2.]
v2=[-2., 1., -1., -2.]
w=[2., -1., 2.]

z1=np.tanh(v1[0] + np.dot(x, v1[1:]))
z2=np.tanh(v2[0] + np.dot(x, v2[1:]))
y3=1./(1+np.exp(-w[0]-np.dot(w[1:],[z1,z2])))

print("Q3: ",y3)

# Question 4 : one neuron
# 3 inputs, 1 output binary sigmoid
x=[2., -4., 1.]
w=[2., 2., 1., -3.]

y4=1./(1+np.exp(-w[0]-np.dot(x, w[1:])))

print("Q4: ",y4)

# Question 5 : classification / gradient of error
# 1 input, 1 hidden tanh, 1 output binary sigmoid
x=1.; t=1.
v0=1.; v1=-2.
w0=0.; wz=-1.

z=np.tanh(v0 + x*v1)
y=1./(1+np.exp(-w0-wz*z))
dE_dv=(-t)*(1-y)*wz*(1-z)*(1+z)*x
print("Q5: ",dE_dv)
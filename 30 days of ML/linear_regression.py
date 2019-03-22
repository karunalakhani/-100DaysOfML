#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 5c : use numpy linear regression

import numpy as np # mean

x = [[1, 2, 3, 2], [1, 0, 2, 4], [1, 4, 0, 3], [1, 3, 4, 0], [1, 1, 1, 1]]
y = [11, 7, 11.25, 13, 5.25]

print("x :",x)
print("y :",y)

coeff = np.linalg.lstsq(x, y)[0]

print("wo:%0.2f\nw1:%0.2f\nw2:%0.2f\nw3:%0.2f"
  %(coeff[0],coeff[1],coeff[2],coeff[3]))
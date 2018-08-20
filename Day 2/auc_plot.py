#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 4b : plot AUC

import numpy as np
import matplotlib.pyplot as plt
from sklearn import metrics # auc

def pos(x):
  return(np.count_nonzero(x))

def neg(x):
  return(len(x) - np.count_nonzero(x))

# 8a
#x = [True, False, True, True, False, False, True, False]

# 8b
x= [False,True,True,True,False,False,True,False,False,True,False,False]

fpr=[]
tpr=[]
auc=[]
for i in range(0,len(x)+1):
  tpr.append(pos(x[0:i]) / pos(x))
  fpr.append(neg(x[0:i]) / neg(x))
  print("%0.2f %0.2f" %(tpr[i], fpr[i]))

print(fpr)
print(tpr)

plt.figure()
plt.plot(fpr, tpr, '-ro', color='blue')
plt.grid(True)
plt.axis([0, 1.1, 0, 1.1])
plt.xlabel('FPR')
plt.ylabel('TPR')
plt.title('ROC curve')
plt.show()

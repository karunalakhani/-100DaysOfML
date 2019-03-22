#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 5a : use ridge regression on boston house prices

from sklearn import datasets # load_boston
from sklearn import linear_model # Ridge

boston = datasets.load_boston()
data = boston.data
target = boston.target

classifier = linear_model.Ridge()

classifier.fit(data,target)

instance = [[ 6.71311099e+00, 1.13774704e+01, 1.27703953e+01,
  5.92885375e-02, 5.88693281e-01, 6.23768379e+00,
  7.14363636e+01, 3.43944269e+00, 1.45335968e+01,
  4.95553360e+02, 1.89952569e+01, 3.36734941e+02, 1.39339921e+01]] 

prediction = classifier.predict(instance)
print(prediction)
print("prediction \t%0.1f" %prediction[0])
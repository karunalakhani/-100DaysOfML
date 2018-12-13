#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 5b : use lasso regression on boston house prices

from sklearn import datasets # load_boston
from sklearn import linear_model # Lasso
from sklearn import model_selection # cross_val_score
import numpy as np # mean

boston = datasets.load_boston()
data = boston.data
target = boston.target

classifier = linear_model.Lasso()

neg_mse = model_selection.cross_val_score(
	classifier, data, target, cv=5, scoring='neg_mean_squared_error')

mean_neg_mse = np.mean(neg_mse)
mean_pos_mse = -1.0 * mean_neg_mse

print("mean neg mse \t%0.1f" %mean_pos_mse)
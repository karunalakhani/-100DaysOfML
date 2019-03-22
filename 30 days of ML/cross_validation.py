#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 4a : use cross validation w/ logistic regression on breast cancer data

from sklearn import datasets # load_breast_cancer
from sklearn import model_selection # cross_val_score
from sklearn import linear_model # LogisticRegression
import numpy as np # mean

breast = datasets.load_breast_cancer()
data = breast.data
target = breast.target

classifier = linear_model.LogisticRegression()

accuracy = model_selection.cross_val_score(
	classifier, data, target, cv=5, scoring='accuracy')

precision = model_selection.cross_val_score(
	classifier, data, target, cv=5, scoring='precision')

recall = model_selection.cross_val_score(
	classifier, data, target, cv=5, scoring='recall')

auc = model_selection.cross_val_score(
	classifier, data, target, cv=5, scoring='roc_auc')

print("accuracy \t%0.3f" %np.mean(accuracy))
print("precision \t%0.3f" %np.mean(precision))
print("recall \t%0.3f" %np.mean(recall))
print("auc \t%0.3f" %np.mean(auc))
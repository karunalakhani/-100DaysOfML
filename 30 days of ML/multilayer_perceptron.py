#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 6a : use cross validation w/ multilayer perceptron classifier

from sklearn import datasets # load_breast_cancer
from sklearn import neural_network # MLPClassifier
from sklearn import model_selection # cross_val_score
import numpy as np # mean

breast = datasets.load_breast_cancer()
data = breast.data
target = breast.target

classifier = neural_network.MLPClassifier(random_state=9)

accuracy = model_selection.cross_val_score(
	classifier, data, target, cv=5, scoring='accuracy')

print("accuracy \t%0.3f" %np.mean(accuracy))
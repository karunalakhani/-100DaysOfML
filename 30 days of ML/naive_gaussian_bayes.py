#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 2_1 : use a Gaussian Naive Bayes classifier

from sklearn import datasets # load_breast_cancer
from sklearn import model_selection # train_test_split
from sklearn import naive_bayes # GaussianNB

breast = datasets.load_breast_cancer()
data = breast.data
target = breast.target

data_train, data_test, target_train, target_test = model_selection.train_test_split(
    data, target, train_size = 0.67, random_state = 0)

classifier = naive_bayes.GaussianNB()
classifier.fit(data_train, target_train)

accuracy = classifier.score(data_test, target_test)

print(accuracy)
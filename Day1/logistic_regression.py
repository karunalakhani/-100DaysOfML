#!/usr/bin/python3
# -*- coding: utf-8 -*-

# coding assignement 3 : use logistic regression on breast cancer data

from sklearn import datasets # load_breast_cancer
from sklearn import model_selection # train_test_split
from sklearn import linear_model # LogisticRegression
from sklearn import metrics # accuracy_score precision_score recall_score roc_auc_score

breast = datasets.load_breast_cancer()
data = breast.data
target = breast.target

data_train, data_test, target_train, target_test = model_selection.train_test_split(
    data, target, train_size = 0.67, random_state = 2)

classifier = linear_model.LogisticRegression()
classifier.fit(data_train, target_train)

predicted = classifier.predict(data_test)
actual = target_test
score = classifier.decision_function(data_test)

accuracy = metrics.accuracy_score(predicted,actual)
precision = metrics.precision_score(actual, predicted)
recall = metrics.recall_score(actual, predicted)
auc = metrics.roc_auc_score(actual, score)

print("accuracy \t%0.3f" %accuracy)
print("precision \t%0.3f" %precision)
print("recall \t%0.3f" %recall)
print("auc \t%0.3f" %auc)
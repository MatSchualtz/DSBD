#!/usr/bin/python
# -*- encoding: iso-8859-1 -*-

import sys
import numpy as np
import time

from sklearn import linear_model
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis

from sklearn.metrics import confusion_matrix
from sklearn.datasets import load_svmlight_file

from sklearn.naive_bayes import BernoulliNB
from sklearn.naive_bayes import GaussianNB
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import Perceptron


def main(X_train, y_train, X_test, y_test, history, model=sys.argv[3]):
    # cria o classificador
    if model == 'knn':
        clf = KNeighborsClassifier(n_neighbors=3, metric='euclidean')
        print(f'Model in use: {model}')
    elif model == 'linear-regression':
        clf = linear_model.LogisticRegression()
        print(f'Model in use: {model}')
    elif model == 'naive-bayes':
        clf = GaussianNB()
        print(f'Model in use: {model}')
    else:
        clf = LinearDiscriminantAnalysis()
        print(f'Model in use: {model}')

    X_train_dense = X_train.toarray()
    clf.fit(X_train_dense, y_train)

    X_test_dense = X_test.toarray()
    y_pred = clf.predict(X_test_dense)

    # mostra o resultado do classificador na base de teste
    history.append(clf.score(X_test_dense, y_test))

    # cria a matriz de confusao
    # cm = confusion_matrix(y_test, y_pred)
    # print cm

    # print y_predProb


if __name__ == "__main__":
    if len(sys.argv) != 4: #Corrected argument number check.
        sys.exit("Use: lab2.py <dataTR> <dataTS> <model>")

    # loads data
    print("Loading data...")
    X_train, y_train = load_svmlight_file(sys.argv[1])
    X_test, y_test = load_svmlight_file(sys.argv[2])
    size = X_train.shape
    batchsize = 1000
    ini = 0
    end = batchsize

    history = []
    while (end <= size[0]):

        xt = X_train[0:end]
        yt = y_train[0:end]

        print("Training size... ", end)

        start_time = time.time()
        main(xt, yt, X_test, y_test, history)
        print("--- %s seconds ---" % (time.time() - start_time))

        end = end + batchsize

    print(history)

#!/usr/bin/python
# -*- encoding: iso-8859-1 -*-

import sys
import numpy as np
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn import preprocessing
from sklearn.model_selection import train_test_split
from sklearn import preprocessing


def main(fname,exibir=False):

        # loads data
        print ("Loading data...")
        data = np.loadtxt(fname)
        X_data = data[:, 1:]
        y_data = data[:,0]

        print ("Spliting data...")
        X_train, X_test, y_train, y_test =  train_test_split(X_data, y_data, test_size=0.5, random_state = 5)

        #scaler = preprocessing.MinMaxScaler()
        #X_train = scaler.fit_transform(X_train)
        #X_test = scaler.fit_transform(X_test)

        # cria um kNN
        neigh = KNeighborsClassifier(n_neighbors=3, metric='euclidean')

        print ('Fitting knn')
        neigh.fit(X_train, y_train)

        # predicao do classificador
        print ('Predicting...')
        y_pred = neigh.predict(X_test)

        # mostra o resultado do classificador na base de teste
        print ('Accuracy: ',  neigh.score(X_test, y_test))

        # cria a matriz de confusao
        if exibir == True:
          cm = confusion_matrix(y_test, y_pred)
          print (cm)
          print(classification_report(y_test, y_pred))
          cr = classification_report(y_test, y_pred,output_dict=True)
          precision = cr['weighted avg']['precision']
          recall = cr['weighted avg']['recall']
          f1_score = cr['weighted avg']['f1-score']
          support = cr['weighted avg']['support']
        else:
          cr = classification_report(y_test, y_pred,output_dict=True)
          precision = cr['weighted avg']['precision']
          recall = cr['weighted avg']['recall']
          f1_score = cr['weighted avg']['f1-score']
          support = cr['weighted avg']['support']
          acuracia = neigh.score(X_test, y_test)

        return {
            'acuracia':acuracia,
            'precision':precision,
            'recall':recall,
            'f1_score':f1_score
        }


if __name__ == "__main__":
        if len(sys.argv) != 2:
                sys.exit("Use: knn.py <file>")

        main(sys.argv[1])



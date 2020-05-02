import numpy as np
from sklearn.decomposition import TruncatedSVD
import matplotlib
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn import metrics
from numpy import eye, asarray, dot, sum, diag
from numpy.linalg import svd
from scipy.stats import spearmanr
'''

Original files
usersFile = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/users.csv")
likesFile = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/likes.csv")
ulFile = open("/Users/apurvabharatia/Desktop/SWM/sample_dataset/users-likes.csv")

'''


#Following is the user footprint matrix of the users who have liked more than 50 pages
#and pages which have more than 150 likes
#along with the user info of the users present in the updated user footprint matrix

denseMatrixFile = open("/Users/apurvabharatia/Desktop/DenseMatrix.csv")
newUsersFile = open("/Users/apurvabharatia/Desktop/newUsers.csv")

#Get above files in a matrix format
M = []
i = 0
for line in denseMatrixFile:
    if i == 0:
        i += 1
        continue
    currLine = line.split(",")
    M.append(currLine[1:])

#Users file values: id, gender, age, pol, ocean
users, gender, age, political, o, c, e, a, n = [], [], [], [], [], [], [], [], []
i = 0
for line in newUsersFile:
    if i == 0:
        i+= 1
        continue
    currLine = line.split(",")
    gender.append(currLine[2])
    age.append(currLine[3])
    political.append(currLine[4])
    o.append(currLine[5])
    c.append(currLine[6])
    e.append(currLine[7])
    a.append(currLine[8])
    n.append(currLine[9])

    users.append(currLine[1:])


print("Dimensions of M: ", len(M), len(M[0]))
print("Dimensions of newUsers: ", len(users), len(users[0]))

M = np.matrix(M, dtype='float')
def varimax(Phi, gamma = 1, q = 20, tol = 1e-6):
    p,k = Phi.shape
    R = eye(k)
    d=0
    for i in xrange(q):
        d_old = d
        Lambda = dot(Phi, R)
        u,s,vh = svd(dot(Phi.T,asarray(Lambda)**3 - (gamma/p) * dot(Lambda, diag(diag(dot(Lambda.T,Lambda))))))
        R = dot(u,vh)
        d = sum(s)
        if d_old == 0 or d/d_old < tol: break
    return dot(Phi, R)

U, D, V = np.linalg.svd(M)
v_rot = varimax(V)
u_rot = M * v_rot

X = u_rot
Y = o
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=0)
regressor = LinearRegression()
regressor.fit(X_train, Y_train)

y_pred = regressor.predict(X_test)

coef, p = spearmanr(y_pred, Y_test)
print(coef, p)

sentiments = []
sentiments.append(political)
sentiments.append(gender)
sentiments.append(o)
sentiments.append(c)
sentiments.append(e)
sentiments.append(a)
sentiments.append(n)
result = []
for i in range(len(sentiments)):
    Y = sentiments[i]
    leny = len(Y)
    lenFold = leny // 10
    #Create folds and create model for each fold
    foldresult = []
    for j in range(10):
        start = lenFold * j
        end = min(start + lenFold, leny)
        X_train, X_test, Y_train, Y_test = X[:start] + X[end:], X[start: end], Y[:start] + Y[end:], Y[start: end]
        regressor = LinearRegression()
        regressor.fit(X_train, Y_train)

        y_pred = regressor.predict(X_test)

        coef, p = spearmanr(y_pred, Y_test)
        foldresult.append(coef)
    result.append(foldresult / 10)


print(result)
#print(y_pred)
#print(Y_test)




#SVD

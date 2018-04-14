#!/usr/bin/python

import readin
import readin_SupplData
# example of training a final regression model
from sklearn.linear_model import LinearRegression
from sklearn.datasets import make_regression
import matplotlib.pyplot as plt
#import matplotlib.image as mpimg
#import plotly.plotly as py
import numpy as np

'''
Multiple Linear Regression Predictions: 

* examines relationship between various variables -- a dependent variable and independent variables.
* the relationship is linear, ie when one (or more) independent variables increase (or decrease), the dependent variable increases (or decreases) too.

The example below demonstrates how to make regression predictions on multiple data instances with an unknown expected outcome.

from https://machinelearningmastery.com/make-predictions-scikit-learn/ 
'''

# generate regression dataset
y = readin.prop()
print(y)
X_old = readin_SupplData.readinDVar()


X_labels = X_old[0][:] # column names
X_sampleID = [r.pop(0) for r in X_old] # remove sample IDs

#print(X_old)
del X_old[0][:] # remove column names
X = [x for x in X_old if x != []] # to remove first empty list

print(X)
#print(X_labels)
#print(X_sampleID)
print(len(X))



# OPTION 1:
# only use the samples that are complete, ie have no gaps
y_new = []
X_new = []
for j in range(len(X)):
	el_new = [x for x in X[j] if x != '']
	if len(X[j]) == len(el_new):
		y_new.append(y[j])
		X_new.append(X[j])

print('y_new = ', y_new)
print('X_new = ', X_new)
print(len(y_new))
print(len(X_new))

y_new_h = [item[0] for item in y_new] # list of only host proportions
print(y_new_h)


'''
# OPTION 2:
# only use the dependent variables that are complete, ie contained in all the samples
X_new2 = X
col_to_del = []
print(len(X))
for v in range(len(X[0])):
	for u in range(len(X)):
		if X[u][v] == '':
			#print('yeaaaaaaâhhhhhh: ', u, v)
			#del X_new2[:][v]
			#delV = [a.pop(v) for a in X_new2]
			col_to_del.append(v)
			break
			#print(len(X[u]))
			#print(X_new2)
print(col_to_del) # --- only column 0 and 19 are complete...
#print(X_new2)
#print(len(X_new2))
'''

#X, y = make_regression(n_samples=46, n_features=21, noise=0.1)
#X_new, y_new = make_regression(n_samples=23, n_features=21, noise=0.1)
X_new, y_new_h = make_regression(n_samples=23, n_features=21, noise=0.)

# fits final model
model = LinearRegression()
#fitted = model.fit(X, y)
#fitted = model.fit(X_new, y_new)
fitted = model.fit(X_new, y_new_h)
#print(fitted)

# predicts the y (dependent variable) usind the linear model we fitted
predictions = model.predict(X_new)
print('predictions:')
print('y = ', predictions)

#model.summary()

# returns the R^2 score of the model, ie percentage of explained variance of the predictions
print('R^2 score of the model = ', model.score(X_new, y_new_h))

# coeffcients for the predictors
print('coefficients of the predictors = ', model.coef_)



# PLOT OF THE COEFFICIENTS OF THE LINEAR MODEL
X_labels.pop(0) # remove sample_ID
print(X_labels)
print(len(X_labels))

fig = plt.figure()
coeff = model.coef_
coeff_abs = [abs(number) for number in coeff]
print('absolute values of coefficients: ', coeff_abs)
x = range(1,22)

'''
to_plot = zip(X_labels, coeff_abs)
print('to plot: ', to_plot)

plt.plot(*zip(*to_plot))
plt.show()
'''

#N = len(y)
#x = range(N)
#width = 1/1.
#plt.bar(x, y, width, align='center', color="blue")
#plt.bar(x, coeff_abs, align='center', color="blue")
y_pos = np.arange(len(X_labels))
plt.xticks(rotation=90)
plt.xticks(y_pos, X_labels)
plt.bar(y_pos, coeff_abs, align='center', alpha=0.5)
plt.ylabel('weight of independent variable in model')
#plt.bar(X_labels, coeff_abs, align='center', color="blue")
plt.title('Coefficients of linear regression model')

#fig = plt.gcf()
#plot_url = py.plot_mpl(fig, filename='mpl-basic-bar')
plt.show()
plt.savefig('img/coefficients.png')
plt.close(fig)


'''
# new instances where we do not know the answer
Xnew, _ = make_regression(n_samples=46, n_features=21, noise=0.1, random_state=1)
# make a prediction
ynew = model.predict(Xnew)
# show the inputs and predicted outputs
for i in range(len(Xnew)):
	print("X=%s, Predicted=%s" % (Xnew[i], ynew[i]))
'''

## Discussion

Main problems

- not enough data

- Machine Learning methods not applied since overfitting and interpretability...

- outliers, dimension of data
--> transformations for multiple predictors

We might want to consider models where outliers do not have much influence on the model (see tab "Discussion" INSERT HYPERLINK TO TAB), eg tree-based classification models if we formulate our problem as a classification problem. Also SVM (Support Vector Machines) for classification generally disregard a part of the training set (that may be far away from the decision boundary and outside of the data mainstream) when creating a prediction equation. However, there is always a trade-off between performance on existent data set and over-fitting, and also of interpretability in case of ML methods.

If the model is very senstive to outliers (which is not the case here), we could use the __spaial sign__ transformation (Seernels et al. 2006 INSERT IN REFERENCE LIST) which projects the predictor values onto a multidimensional sphere and thus makes all the samples the same distance from the centre of the sphere. This is mathematically done by each sample divided by its squared norm INSERT FORMULA HERE. One should note that centering and scaling of the predictor data is necessary because the denominator is intended to measure the squared distance to the centre of thre predictor's distribution. Also, this method transforms the predictors as a group, so we should not remove predictor variables after it. 

- analyse different subject ID groups


- parasitemia stages
spreadsheet with the SPVs (surrogate proportion variables) for each parasite developmental stage
You can use these as covariates, treating each as if it is a real number. You can think of them as representing the relative abundance of each developmental stage when compared between samples. I think they are scaled from -1 to +1.
0hrs, 24hrs and 48 hrs represent the different time point in the life cycle, progressing from most immature to most mature stages. Gametocytes are the sexual stage of the parasite.
They may help explain some of the variation in your data, but they may not be very useful for the end user of the website because they don’t easily convert to real proportions or numbers

 
 
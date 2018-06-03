## Discussion

A parametric model was appropriate in our case, as the data showed fairly normal distributions (see chapter _Methods_ [INSERT LINK HERE]) and we could identify linear relationships between variables and the response variable. Also, we had very few data available that it was not possible to divide the data into separate train and test data sets to use more elaborate Machine Learning methods, including Random Forest, Support Vector Machines (SVMs), and Elastic map.  
We wanted to avoid such models that generate highly complex, non-linear relationships between the predictors and the outcome. Also, we wanted to keep the model interpretable, more complex models quickly tend to lose interpretability and more importantly, overfit the data.
Another aspect we considered when designing the models was which type of data data a user is most likely to have. 
Once more data has become available, it might be meaningful to explore more elaborate non-parametric predictive models that are more powerful. 

We might want to consider models where outliers do not have much influence on the model, eg tree-based classification models if we formulate our problem as a classification problem. Also SVM (Support Vector Machines) for classification generally disregard a part of the training set (that may be far away from the decision boundary and outside of the data mainstream) when creating a prediction equation. However, there is always a trade-off between performance on existent data set and over-fitting, and also the loss of interpretability in case of Machine Learning methods. In any case, the complex model should be properly validated.
<!--
Thre predictive performance, provided the complex model is properly validated, may compensate the loss in interpretation. 
-->
However, this was not the purpose of this project.

For more complex models, one might want to incorporate the information of the developmental stage of the disease (given by the surrogate proportion variables (SPVs) at different time points in the life cycle) as well the level of gametocytes (Gam5) representing the sexual stage of the parasite.
The observations found in chapter _Data preparation_ [INSERT LINK HERE] might help for that.

Also, additional insight might be gained by comparing different species. We developed a logistic regression on the mouse data set; however, comparison to our models behind this web tool here, turned out to be difficult as it was not based on the same measurements and very few data was available. 

Once more data has become available, we might also want analyse with respect to different subject ID groups.

<!---
Main problems

- not enough data

- Machine Learning methods not applied since overfitting and interpretability...

- outliers, dimension of data
transformations for multiple predictors

-->

<!--
If the model is very senstive to outliers (which is not the case here), we could use the __spaial sign__ transformation (Seernels et al. 2006 INSERT IN REFERENCE LIST) which projects the predictor values onto a multidimensional sphere and thus makes all the samples the same distance from the centre of the sphere. This is mathematically done by each sample divided by its squared norm INSERT FORMULA HERE. One should note that centering and scaling of the predictor data is necessary because the denominator is intended to measure the squared distance to the centre of thre predictor's distribution. Also, this method transforms the predictors as a group, so we should not remove predictor variables after it. 
-->


 
 
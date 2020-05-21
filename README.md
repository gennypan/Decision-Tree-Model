# Decision-Tree-Model
This package can deal with both classification and regression tree. Two model quality measures are:
1.Impurity measure: Entropy and Gini --- Information index 
2.The "rel error" is 1−R2  Root mean squared error;The "xerror" is related to the PRESS statistic. This is the error on the observations from cross validation data.

## Install
rpart is the library to train the decision tree model. If not installed already, run 
```install.packages("rpart")```

## Use Examples:
Let's say the dataset is called NPS_prediction.

1. The first step is to split the data into training and test sets.
a <- data_perc(NPS_prediction,12345,0.5)

2. The next step is to train the model.
tree <- tree(model_formula, a$trainset, a$testset, method = "class",opt_method=3, morecontrol = rpart.control(maxdepth = 4))

## Fine turn model parameters (From R Help Document):
1. minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.
2. minbucket: the minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
3. cp: complexity parameter
  Cα(T)=C(T)+α|T|,Small α results in larger trees and potential overfitting, large α in small trees and potential underfitting
4. maxcompete: the number of competitor splits retained in the output. e.g the second best, the third best

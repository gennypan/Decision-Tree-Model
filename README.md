# Decision-Tree-Model

## Use Examples:
Let's say the dataset is called NPS_prediction.

1. The first step is to split the data into training and test sets.
a <- data_perc(NPS_prediction,12345,0.5)

2. The next step is to train the model.
tree <- tree(model_formula, a$trainset, a$testset, method = "class",opt_method=3, morecontrol = rpart.control(maxdepth = 4))

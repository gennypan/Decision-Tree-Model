library(rpart)

#Partition Data
data_perc<-function(df,seed,train_perc){
	set.seed(seed)
	index <- nrow(df)
	trainindex <- sample(1:nrow(df), trunc(index*train_perc))
	trainset <- df[trainindex, ]
	testset <- df[-trainindex, ]
	list(trainset=trainset,testset=testset)
}

#Train the model
tree<-function(formula,traindata,testdata,method,opt_method=1,morecontrol=NULL,title="Classification Tree"){

	regressor<-as.formula(formula)

	if (is.null(morecontrol)) {
		fit <- rpart(formula =regressor,data= traindata, method = method)
		print(fit)
	}
	else {
		fit <- rpart(formula =regressor,data= traindata,method = method,control=(noquote(morecontrol)))
		print(fit)
	}

## Different optimization methods
#Prune tree based on minimum cross validation error	
#Find splits for both classfication tree and regression
	if (opt_method==1)
		{cp<-fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
		ptree<- prune(fit,cp=cp)}

#Find #splits for regression tree	
	if (opt_method==2)
		{cp<-fit$cptable[which.min(fit$cptable[,"xstd"]),"CP"]
		ptree<- prune(fit,cp=cp)}
	
## Reference http://statweb.stanford.edu/~lpekelis/talks/13_datafest_cart_talk.pdf
	if (opt_method==3)		
		{cpstat <- dim(traindata)[1] * fit$cptable[, 3] +fit$cptable[, "CP"]   * (fit$cptable[, 2] + which.min(fit$cptable[, "xerror"]))
		 cp<-fit$cptable[which.min(cpstat), ][1]
		ptree<- prune(fit,cp= cp)}
	
# plot tree 
	plot(ptree, uniform=TRUE,margin=0.1,main=title)
	text(ptree, use.n=TRUE, all=TRUE, cex=.5)
# create attractive postscript plot of tree 
	post(ptree, file = "/Users/kanhuap/Projects/Partner NPS 2019/tree_v1.ps",title = title)
	
	dep_var<-unlist(strsplit(formula, "~"))[1]

if (method=="class")
	{#### Classification Tree
	#training misclassification	
	
	tot<-nrow(traindata)
	exp <- predict(ptree,type="class")    	#predicted values
	# obs <- factor(traindata$cover>0)          #actual presences
	mper<-sum(as.numeric(exp!=traindata[, dep_var]))/tot#misclassifications%
	
	mtable<-table(exp, traindata[, dep_var])
	print(mtable)
	print(mper)
	
	# #Validation misclassification
	tot_test<-nrow(testdata)
	test <- predict(ptree,type="class",newdata=testdata) 
	mper_test<-sum(as.numeric(test!= testdata[, dep_var]))/tot_test
	
	mtable<-table(test, testdata[, dep_var])
	print(mtable)
	print(mper_test)}
	
if (method=="anova")	
	{#predict<- predict(ptree,newdata=traindata)
	 predict<- xpred.rpart(ptree,cp=cp)
	 plot(predict, traindata[, dep_var],type="p",xlab = "Predict",ylab = "Actual")}
}

#This package can deal with both classification and regression tree
# Impurity measure: Entropy and Gini  --- Information index 
#The "rel error" is 1−R2  Root mean squared error;The "xerror" is related to the PRESS statistic. This is the error on the observations from cross validation data.

#Code reference of dependent variable as 0
#In terms of formula, leave no space
setwd("/Users/kanhuap/Projects/Partner NPS 2019")
raw_nps_data <- read.csv("data_for_model_wo_gfm.csv", strip.white = TRUE, na.strings=c("", "NA"),stringsAsFactors=FALSE,fileEncoding="latin1")

raw_nps_data$survey_flag <- rowSums(raw_nps_data[12:29])

library(rpart)

#Partition Data
#Example Call: out<-data_perc, out$trainset &out$testset
data_perc<-function(df,seed,train_perc){
	set.seed(seed)
	index <- nrow(df)
	trainindex <- sample(1:nrow(df), trunc(index*train_perc))
	trainset <- df[trainindex, ]
	testset <- df[-trainindex, ]
	list(trainset=trainset,testset=testset)
}

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


# From R Help Document
# # minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.
# # minbucket: the minimum number of observations in any terminal <leaf> node. If only one of minbucket or minsplit is specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
# # cp: complexity parameter
# # Cα(T)=C(T)+α|T|,Small α results in larger trees and potential overfitting, large α in small trees and potential underfitting
# # maxcompete: the number of competitor splits retained in the output. e.g the second best, the third best

# # An eg with different costs for errors: 
# # lmat <- matrix(c(0,1,2,
# # 1,0,100,
# # 2,100,0), ncol = 3)
# # eg call: rpart.tree <- rpart(Species ~ ., data=train.set, parms = list(loss = lmat))

# nps$communication <- paste(nps$Customer_Support_Tools,"|",nps$Integration_Support,"|",nps$Relationship_Management)

# a <- data_perc(nps[nps$Managed == 1,],12345,1)
raw_nps_data_dt <- raw_nps_data[raw_nps_data$survey_flag>0&raw_nps_data$NPS != 'P',]
# raw_nps_data_dt$NPS_n <- ifelse(raw_nps_data_dt$NPS == 'N','D',raw_nps_data_dt$NPS)

# raw_nps_data_dt_1 <- raw_nps_data_dt[raw_nps_data_dt$Partner_Name!='GoFundMe',]

a <- data_perc(raw_nps_data_dt[,c("NPS","managed","top20","How.would.you.describe.your.primary.job.function.","How.would.you.describe.yourÂ.role.with.your.employer.","How.long.have.you.been.interacting.with.WePay.",
                                  "API","WePay.Website","Developer.documentation","Reliability.of.WePay.platform","Reporting.capabilities","Settlement.timeframe",
                                  "Fraud.Protection","Supported.payment.methods","Geographical.footprint","Relationship.management","Technical.account.management","Sales.team",
                                  "Integration.team","Product.team","API.support","Merchant.Partner.customer.support",
                                  "Risk.operations","Chargeback.management")],12345,1)


tree <- tree("NPS~managed+top20+API+WePay.Website+Developer.documentation+Reliability.of.WePay.platform+
             Reporting.capabilities+Settlement.timeframe+Fraud.Protection+Supported.payment.methods+
             Geographical.footprint+Relationship.management+Technical.account.management+Sales.team+
             Integration.team+Product.team+API.support+Merchant.Partner.customer.support+Risk.operations+Chargeback.management", a$trainset, a$testset,
           method = "class",opt_method=3, morecontrol = rpart.control(maxdepth = 4))

# 
# 
# # Random Forest prediction of Kyphosis data
# library(randomForest)
# rf_fit <- randomForest(as.factor(NPS)~payment_rating+refund_rating+chargeback_rating+report_rating+support_rating+last_first_payment+blacklist
#                     +tickets_n+cb_cnt_n+refund_cnt_n+ach_tpv_n+cc_tpv_n+managed,type="classification",data=a$trainset)
# print(rf_fit) # view results 
# importance(rf_fit) # importance of each predictor
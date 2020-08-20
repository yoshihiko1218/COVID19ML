library(pROC)
library(glmnet)
set.seed(1)

X = read.csv("COVIDandFLUdata.csv", header=T, stringsAsFactor=T, sep=",")
X = X[, c("Diagnosis",
"Age",
"Sex",
"neutrophil",
"neutrophilCategorical",
"serumLevelsOfWhiteBloodCell",
"serumLevelsOfWhiteBloodCellCategorical",
"lymphocytes",
"lymphocytesCategorical",
"CTscanResults",
"XrayResults",
"RiskFactors",
"Diarrhea",
"Fever",
"Coughing",
"ShortnessOfBreath",
"SoreThroat",
"NauseaVomitting",
"Temperature",
"Fatigue")]
N = ncol(X)

#mark empty as NA
for (i in 1:N) {
 X[which(X[,i] == ""), i] = NA
}

#take care of NA.
for (i in 1:N) {
 if (class(X[,i]) == "integer") {
     mm = mean(X[!is.na(X[,i]), i]) 
     X[is.na(X[,i]),i] = mm
     next
 } 
 if (class(X[,i]) == "numeric") {
     mm = mean(X[!is.na(X[,i]), i]) 
     X[is.na(X[,i]),i] = mm
     next
 }
 if (class(X[,i]) == "factor") {
    lpp = length(levels(X[,i]))+1
    X[,i]= as.numeric(X[,i]) -1
    X[is.na(X[,i]),i] = lpp -1 
    next
 }
 if (class(X[,i]) == "logical") {
    X[,i]= as.numeric(X[,i])
    X[is.na(X[,i]),i] = 2
    next
 }
}
for (i in N:1) {
   if (sum(X[,i]) == 0) X=X[,-i]
   else if (is.na(sd(X[,i]))) X=X[,-i]
   else if (sd(X[,i]) == 0) X=X[,-i]
}

set.seed(1)
#train using 80% of rows.
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
x = as.matrix(X)
xx = (x[, -1])
y = x[, 1]

#glmnet with alpha = 1 is LASSO
fit1 = glmnet(xx[train,], y[train], alpha=1, family="binomial")

#predict the result for remaining 20%
pred1 = predict(fit1, newx = x[-train , -1], s= 0.01)

pdf("Covid_ML_Lasso.pdf")
#plot roc
rr1 = roc(x[-train, 1], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
     ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE, main="LASSO regression model.")
dev.off()

set.seed(1)
fit2 = glmnet(xx[train,], y[train], alpha=0, family="binomial")
pred2 = predict(fit2, newx = x[-train , -1], s= 0.01)

pdf("Covid_ML_Ridge.pdf")
rr2 = roc(x[-train, 1], (pred2[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
     ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, main="RIDGE regression model")
dev.off()

xm=x[which(x[, "Sex"]==2),]  #males
xw=x[which(x[, "Sex"]==1),]  #females
xa1=x[which(x[, "Age"]>=18 & x[, "Age"]<40),]  #Young
xa2=x[which(x[, "Age"]>=40 & x[, "Age"]<65),]  #middle aged
xa3=x[which(x[, "Age"]>65),]                   #old

set.seed(1)
num = nrow(xm)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xm[, -1])
y = xm[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=1, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Male_ML_Lasso.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
     ylab="True Postive Percentage", col="red", lwd=4, print.auc=TRUE, main="LASSO regression model for Male Patients.")
dev.off()


set.seed(1)
num = nrow(xw)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xw[, -1])
y = xw[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=1, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Female_ML_Lasso.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
     ylab="True Postive Percentage", col="blue", lwd=4, print.auc=TRUE, main="LASSO regression model for Female Patients.")
dev.off()


set.seed(1)
num = nrow(xa1)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xa1[, -1])
y = xa1[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=1, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Young_ML_Lasso.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
ylab="True Postive Percentage", col="green", lwd=4, print.auc=TRUE, main="LASSO regression model for Young Patients.")
dev.off()


set.seed(1)
num = nrow(xa2)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xa2[, -1])
y = xa2[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=1, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Middle_ML_Lasso.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
ylab="True Postive Percentage", col="orange", lwd=4, print.auc=TRUE, main="LASSO regression model for Middle Aged Patients.")
dev.off()


set.seed(1)
num = nrow(xa3)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xa3[, -1])
y = xa3[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=1, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Old_ML_Lasso.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
ylab="True Postive Percentage", col="purple", lwd=4, print.auc=TRUE, main="LASSO regression model for Old Patients.")
dev.off()

set.seed(1)
num = nrow(xm)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xm[, -1])
y = xm[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=0, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Male_ML_Ridge.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
     ylab="True Postive Percentage", col="red", lwd=4, print.auc=TRUE, main="RIDGE regression model for Male Patients.")
dev.off()


set.seed(1)
num = nrow(xw)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xw[, -1])
y = xw[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=0, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Female_ML_Ridge.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
     ylab="True Postive Percentage", col="blue", lwd=4, print.auc=TRUE, main="RIDGE regression model for Female Patients.")
dev.off()


set.seed(1)
num = nrow(xa1)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xa1[, -1])
y = xa1[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=0, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Young_ML_Ridge.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
ylab="True Postive Percentage", col="green", lwd=4, print.auc=TRUE, main="RIDGE regression model for Young Patients.")
dev.off()


set.seed(1)
num = nrow(xa2)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xa2[, -1])
y = xa2[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=0, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Middle_ML_Ridge.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
ylab="True Postive Percentage", col="orange", lwd=4, print.auc=TRUE, main="RIDGE regression model for Middle Aged Patients.")
dev.off()


set.seed(1)
num = nrow(xa3)
train = sample (1:num, .8*num, replace=FALSE)
xx = (xa3[, -1])
y = xa3[, 1]
fit1 = glmnet(xx[train,], y[train], alpha=0, family="binomial")
pred1 = predict(fit1, newx = xx[-train , ], s= 0.01)

pdf("Covid_Old_ML_Ridge.pdf")
rr1 = roc(y[-train], (pred1[,1]) , plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
ylab="True Postive Percentage", col="purple", lwd=4, print.auc=TRUE, main="RIDGE regression model for Old Patients.")
dev.off()



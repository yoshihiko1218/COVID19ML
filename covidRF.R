library(pROC)
library(glmnet)
library(randomForest)
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
for (i in 1:N) {
 X[which(X[,i] == ""), i] = NA
}

# dont make Diagnosis numeric. RF needs output variable with 5 or less
# levels to be catagorical (aka factor).  hence 2:N rather than 1:N below:
for (i in 2:N) {
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
for (i in N:2) {
   if (sum(X[,i]) == 0) X=X[,-i]
   else if (is.na(sd(X[,i]))) X=X[,-i]
   else if (sd(X[,i]) == 0) X=X[,-i]
}

# train with 80% of the rows.
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
classifier = randomForest(formula = Diagnosis ~ ., data = X[train,])

#predict for remaining 20%
pred = predict(classifier, X[-train, -1])
pdf("Covid_RF.pdf")
rr = roc(as.numeric(X[-train, 1]), as.numeric(pred), plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
         ylab="True Postive Percentage", col="magenta", lwd=4, print.auc=TRUE, main="Random Forest model.")
dev.off()

xm=X[which(X[, "Sex"]==2),]                      #males
xw=X[which(X[, "Sex"]==1),]                      #females
xa1=X[which(X[, "Age"]>=18 & X[, "Age"]<40),]    #young
xa2=X[which(X[, "Age"]>=40 & X[, "Age"]<65),]    #middle aged
xa3=X[which(X[, "Age"]>65),]                     #old
SX = X

set.seed(1)
num = nrow(xm)
X = xm
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
classifier = randomForest(formula = Diagnosis ~ ., data = X[train,])
pred = predict(classifier, X[-train, -1])
pdf("Covid_Male_RF.pdf")
rr = roc(as.numeric(X[-train, 1]), as.numeric(pred), plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
         ylab="True Postive Percentage", col="red", lwd=4, print.auc=TRUE, main="Random Forest model for Male patients.")
dev.off()

set.seed(1)
num = nrow(xm)
X = xw
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
classifier = randomForest(formula = Diagnosis ~ ., data = X[train,])
pred = predict(classifier, X[-train, -1])
pdf("Covid_Female_RF.pdf")
rr = roc(as.numeric(X[-train, 1]), as.numeric(pred), plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
         ylab="True Postive Percentage", col="blue", lwd=4, print.auc=TRUE, main="Random Forest model for Female patients.")
dev.off()

set.seed(1)
num = nrow(xm)
X = xa1
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
classifier = randomForest(formula = Diagnosis ~ ., data = X[train,])
pred = predict(classifier, X[-train, -1])
pdf("Covid_Young_RF.pdf")
rr = roc(as.numeric(X[-train, 1]), as.numeric(pred), plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
         ylab="True Postive Percentage", col="green", lwd=4, print.auc=TRUE, main="Random Forest modeli for Young patients.")
dev.off()

set.seed(1)
num = nrow(xm)
X = xa2
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
classifier = randomForest(formula = Diagnosis ~ ., data = X[train,])
pred = predict(classifier, X[-train, -1])
pdf("Covid_Middle_RF.pdf")
rr = roc(as.numeric(X[-train, 1]), as.numeric(pred), plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
         ylab="True Postive Percentage", col="orange", lwd=4, print.auc=TRUE, main="Random Forest model for Middle aged patients.")
dev.off()

set.seed(1)
num = nrow(xm)
X = xa3
train = sample (1:nrow(X), .8*nrow(X), replace=FALSE)
classifier = randomForest(formula = Diagnosis ~ ., data = X[train,])
pred = predict(classifier, X[-train, -1])
pdf("Covid_Old_RF.pdf")
rr = roc(as.numeric(X[-train, 1]), as.numeric(pred), plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage",
         ylab="True Postive Percentage", col="purple", lwd=4, print.auc=TRUE, main="Random Forest modeli for old patients.")
dev.off()


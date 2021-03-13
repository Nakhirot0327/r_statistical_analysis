####DSL medium class 1 - 4. Outliner Correction (Binning) and Decision Tree Application####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160521_M_Data_Cleansing/Data/")

#Data Input
dat <- read.csv("M1_Bank_Customer_v2.csv")
head(dat)
dat <- dat[,-1]

#####1. Package Insall#####
if("discretization" %in% rownames(installed.packages()) == FALSE) {install.packages("discretization")}
library(discretization)
if("infotheo" %in% rownames(installed.packages()) == FALSE) {install.packages("infotheo")}
library(infotheo)
if("gplots" %in% rownames(installed.packages()) == FALSE) {install.packages("gplots")}
library(gplots)
if("gdata" %in% rownames(installed.packages()) == FALSE) {install.packages("gdata")}
library(gdata)
if("randomForest" %in% rownames(installed.packages()) == FALSE) {install.packages("randomForest")}
library(randomForest)
if("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart")}
library(rpart)
if("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot")}
library(rpart.plot)

#####2. Data Confirmation and Correction#####
####Change variable types
sapply(dat, class)
fa <- c(1,3,4,5,6,8,9,12)
for(i in fa) {
  dat[,i] <- as.factor(dat[,i])
}
str(dat)

####Confirm distribution of quantitative variables
inte <- which(sapply(dat, class) == "integer")
par(mfrow=c(1,4))
for(i in inte) {
  hist(dat[,i], xlab = colnames(dat)[i], main = "Histgram")
}
par(mfrow=c(1,1))

####Correct long tail distribution in "Credit", "age", "duration_time" and "contact_count"
###Make bins of objective variable("Credit")
dat[,"credit"] <- discretize(dat[,"credit"], disc="equalfreq", nbins = 5)
dat[,"credit"] #values are large in order of 5, 4,...,1
dat[,"credit"] <- factor(dat[,"credit"]) #order = TRUE) #Ordered factor
str(dat)

###Make bins of explanatory variables based on supervised binning method
select <- c("age", "duration_time", "contact_count","credit")
dat.n <- dat[,select]
str(dat.n)

###Apply binning method (Chi-merge)
dat.n.cm <- chiM(dat.n, alpha = 0.05) #Need 30sec - 1Min
str(dat.n.cm)
dat.n.cm.data <- dat.n.cm$Disc.data
str(dat.n.cm.data)

select <- c("age", "duration_time", "contact_count")
#for(i in select) {
#  dat.n.cm.data[,i] <- factor(dat.n.cm.data[,i], ordered = TRUE)
#}
str(dat.n.cm.data)

reject <- which(colnames(dat) %in% select)
dat <- dat[,-reject]
dat <- cbind(dat, dat.n.cm.data[,select])
str(dat)

inte <- c("age", "credit", "duration_time", "contact_count")
par(mfrow=c(1,4))
for(i in inte) {
  barplot(table(dat[,i]), xlab = colnames(dat)[i], main = "Histgram")
}
par(mfrow=c(1,1))

#####3. Model Development (Decision Tree)#####
####3.1 Divide the data into train and test data####
set.seed(50)
trainNO <- sample(nrow(dat),round(nrow(dat)*0.70,0),replace=FALSE)
#replace=TRUE:Extract sample NO from uniform distribution of without replacement
train_dat <- dat[trainNO,]
test_dat <- dat[-trainNO,]

####3.2 Develop Decision Tree Model####
###Develop decision tree
fit <- rpart(credit~., data=train_dat,
             control=rpart.control(minsplit=30, cp=0.005), #cp: COMPLEXITY PARAMETER
             method="class", parms=list(split="information"))
#minsplit:Minimum number of each node
#cp:Complexity Parameter

###Plot decision tree
par(ps=10) #Font size
rpart.plot(fit, type=4, extra=1)  #PLOT DECISION TREE

###Confirm cross validation relative error
printcp(fit)
plotcp(fit)

###Prune decision tree
fit2 <- prune(fit, cp=fit$cptable[4,1])
rpart.plot(fit2, type=4, extra=5)

###Confirm variable importance
plot(fit2$variable.importance, type="n")
text(labels=names(fit2$variable.importance),
     x=seq(1, length(fit2$variable.importance), by=1),
     y=fit2$variable.importance, cex=0.75)

####3.3 Prediction validation####
predicted <- predict(fit2, newdata=test_dat, type="class")
correct <- test_dat[,"credit"]
(cm <- table(correct, predicted))   #Confusing Matrix
(accuracyTraining <- sum(diag(cm))/sum(cm)) #Accuracy

#####4. Model Development (Random Forest)#####
####4.1 Divide the data into train and test data####
y <- dat[,"credit"]
x <- dat[, -which(colnames(dat) == "credit")]

#Divide the data into train data and test data
train.y <- y[trainNO]
train.x <- x[trainNO,]
test.y <- y[-trainNO]
test.x <- x[-trainNO,]

####4.2 Identify optimized hyper parameters####
###Hyper parameter tests
ntree <- seq(50,450,by=100) #Number of trees to grow
mtry <- seq(2,ncol(x),by=3) #Number of variables randomly sampled as candidates at each split
cm <- NULL #Confusion Matrix
k <- 1 #CM counter
accuracy <- matrix(0,nrow=length(ntree),ncol=length(mtry)) #Accuracy
rownames(accuracy) <- ntree
colnames(accuracy) <- mtry

#Container for Accuracy of each credit level
for(h in 1:length(levels(y))) {
  eval(parse(text = paste0("Credit",h," <- matrix(0,nrow=length(ntree),ncol=length(mtry))")))
  eval(parse(text = paste0("rownames(Credit",h,") <- ntree")))
  eval(parse(text = paste0("colnames(Credit",h,") <- mtry")))
}

for(i in 1:length(ntree)) {
  for(j in 1:length(mtry)){
    set.seed(50)
    train.rf <- randomForest(x=train.x,y=train.y,ntree=ntree[i],mtry=mtry[j]) #Develop Model
    predicted <- predict(train.rf,test.x) #Prediction for test data
    cm[[k]] <- table(test.y, predicted) #Confusion Matrix
    accuracy[i,j] <- sum(diag(cm[[k]]))/sum(cm[[k]]) #Accuracy
    
    for(h in 1:length(levels(y))) {
      eval(parse(text = paste0("Credit",h,"[",i,",",j,"] <- cm[[k]][",h,",",h,"]/sum(cm[[k]][",h,",])")))
    }
    print(paste("Number of trees =",ntree[i],"Mtry =",mtry[j]))
    print(paste("Accuracy of the model is",round(accuracy[i,j],2)))
    k <- k + 1
  }
}

###Draw heatmap to identify the best hyper parameters - mtree and Mtry
#Container for Accuracy of each credit level
heatmap.2(accuracy,col = redgreen(256),dendrogram="none",Rowv = NA,Colv = NA,
          xlab="Number of variables",ylab="Number of trees",main="Accuracy")
op.row <- which(accuracy==max(accuracy),arr.ind=TRUE)[1]
op.col <- which(accuracy==max(accuracy),arr.ind=TRUE)[2]
op.ntree <- as.numeric(rownames(accuracy)[op.row])
op.mtry <- as.numeric(colnames(accuracy)[op.col])

labels <- "Accuracy of Credit"
none <- "none"
xlab <- "Number of Variables"
ylab <- "Number of Trees"
par(ps = 10)

for(h in 1:length(levels(y))) {
  eval(parse(text = paste0("heatmap.2(Credit",h,",col = redgreen(256),dendrogram=none,Rowv = NA,
                           Colv = NA,xlab=xlab,ylab=ylab,main=paste0(labels,",h,"))")))
  Sys.sleep(2)
}

####4.3 Set optimized parameter and re-conduct model development####
###Re-develop random forest model
set.seed(50)
rf <- randomForest(x=train.x,y=train.y,
                   ntree=op.ntree,  #Number of trees
                   mtry=op.mtry,    #Number of variables
                   prob.model=TRUE) #When you want probability prediction set this TRUE

###Accuracy analysis
#(A) Prediction based on test data
predicted <- predict(rf,test.x,type="response")

#(B) Confusion Matrix
(cm <- table(test.y, predicted))
#Calucaltion of accracy of the model
AccuracyPct <- rep(0,ncol(cm))
cm <- rbind(cm, AccuracyPct)
AccuracyPct <- rep(0,nrow(cm))
cm <- cbind(cm,AccuracyPct)

cm[nrow(cm),ncol(cm)] <- sum(diag(cm))/sum(cm)
for(i in (1:ncol(cm)-1)) {
  cm[nrow(cm),i] <- round(cm[i,i]/sum(cm[,i]),2)
  cm[i,ncol(cm)] <- round(cm[i,i]/sum(cm[i,]),2)
}

cm

## (E) Variable of Importance
varImpPlot(rf,main="Variable importance")

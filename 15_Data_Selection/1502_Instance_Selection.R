####DSL medium class 2 - 2. Instance Selection by FRPS####
###FRPS = Fuzzy Rough Prototype Selection
#Data Input
data(spam7)
str(spam7)
dat <- spam7
table(dat[,7])

#####1. Package Insall#####
if("RoughSets" %in% rownames(installed.packages()) == FALSE) {install.packages("RoughSets")}
library(RoughSets)
if("DMwR" %in% rownames(installed.packages()) == FALSE) {install.packages("DMwR")}
library(DMwR)
if("frbs" %in% rownames(installed.packages()) == FALSE) {install.packages("frbs")}
library(frbs)

#####2. Model development (Logistic Regression)#####
####2.1 Divide the data into train and test data####
set.seed(50)
trainNO <- sample(nrow(dat),round(nrow(dat)*0.70,0),replace=FALSE)
#replace=TRUE:Extract sample NO from uniform distribution of without replacement
train_dat <- dat[trainNO,]
test_dat <- dat[-trainNO,]

####2.2 Develop logistic regression model with SMOTE####
###Develop model
str(train_dat)
glm_a <- glm(yesno~., data=train_dat, family=binomial)
summary(glm_a)

###Apply step function to improve AIC
glm_a2 <- step(glm_a, direction="both")
summary(glm_a2)
glm_a2$coefficients

###Model evaluation
##Prediction for test data
pred <- predict(glm_a2, newdata=test_dat, type="response")
str(pred)
act <- test_dat$yesno

##(a) Confusion Matrix
prediction <- pred

#Change data type of prediction from probability to factor
for (i in 1:length(prediction)) {
  if (prediction[i] < 0.5) {
    prediction[i] <- 0
  } else {
    prediction[i] <- 1
  }
}
#Confusion matrix
(op_mat <- table(act, prediction))

#Accuracy, FPR, FNR
accuracy <- sum(diag(op_mat))/sum(op_mat)
FNR <- op_mat[1,2]/sum(op_mat[1,]) #Ratio number of predicted "0" to number of actual "1" 
FPR <- op_mat[2,1]/sum(op_mat[2,]) #Ratio number of predicted "1" to number of actual "0"

aff <- data.frame(c("accuracy", "false negative rate", "false positive rate"), c(accuracy, FNR, FPR))
names(aff) <- c("variable", "value")
aff

##(b) Draw prediction histgram
dat_obj_0 <- subset(test_dat, yesno=="n")
dat_obj_1 <- subset(test_dat, yesno=="y")

dat_obj_0$p <- predict(glm_a2, newdata=dat_obj_0, type="response")
dat_obj_1$p <- predict(glm_a2, newdata=dat_obj_1, type="response")

par(mfrow=c(1,1), mar=c(5,4,5,4))
hist(dat_obj_0$p, breaks=10, col = "#ff00ff40", border = "#ff00ff", 
     ylim=c(0, 1000), xlab="probablity", ylab="frequency",
     main="Probablity of Spam")
hist(dat_obj_1$p, breaks=10, col = "#0000ff40", border = "#0000ff", add=TRUE)
legend("topright",legend=c("Non-Spam","Spam"),
       fill=c("#ff00ff40","#0000ff40"))

##(c) Calcualte R-squared
(R2 <- 1 - with(glm_a2, deviance/null.deviance))

#####3. Model development with data reduction##### ###TBD
str(train_dat)
de <- c(7) #objective variable's location
train_dat_IS <- SF.asDecisionTable(dataset = train_dat, decision.attr = de)
str(train_dat_IS)

##evaluate and select instances
res.1 <- IS.FRPS.FRST(train_dat_IS, type.alpha = "FRPS.3")

##generate new decision table
new.decTable <- SF.applyDecTable(decision.table, res.1)

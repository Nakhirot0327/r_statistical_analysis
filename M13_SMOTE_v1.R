####DSL medium class 1 - 3. SMOTE Application####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160521_M_Data_Cleansing/Data/")

#Data Input
dat <- read.csv("M1_Bank_Customer_v2.csv")
head(dat)
dat <- dat[,-1]

#####1. Package Insall#####
if("DMwR" %in% rownames(installed.packages()) == FALSE) {install.packages("DMwR")}
library(DMwR)

#####2. Data Confirmation#####
####Change variable types
sapply(dat, class)
fa <- c(1,3,4,5,6,8,9,12)
for(i in fa) {
  dat[,i] <- as.factor(dat[,i])
}
str(dat)

####Check distribution of categorical data
for(i in fa) {
  print(colnames(dat)[i])
  print(table(dat[,i]))
}

#####3. Model development (Logistic Regression)#####
####3.1 Divide the data into train and test data####
set.seed(50)
trainNO <- sample(nrow(dat),round(nrow(dat)*0.70,0),replace=FALSE)
#replace=TRUE:Extract sample NO from uniform distribution of without replacement
train_dat <- dat[trainNO,]
test_dat <- dat[-trainNO,]

####3.2 Develop logistic regression model normally####
str(dat)
glm_a <- glm(camp_result~., data=train_dat, family=binomial)
summary(glm_a)

###Apply step function to improve AIC
glm_b <- step(glm_a, direction="both")
summary(glm_b)
glm_b$coefficients

###Model evaluation
##Prediction for test data
pred <- predict(glm_b, newdata=test_dat, type="response")
str(pred)
act <- test_dat$camp_result

##(a) Confusion Matrix
prediction1 <- pred

#Change data type of prediction from probability to factor
for (i in 1:length(prediction1)) {
  if (prediction1[i] < 0.5) {
    prediction1[i] <- 0
  } else {
    prediction1[i] <- 1
  }
}
#Confusion matrix
(op_mat <- table(act, prediction1))

#Accuracy, FPR, FNR
accuracy <- sum(diag(op_mat))/sum(op_mat)
FNR <- op_mat[1,2]/sum(op_mat[1,]) #Ratio number of predicted "0" to number of actual "1" 
FPR <- op_mat[2,1]/sum(op_mat[2,]) #Ratio number of predicted "1" to number of actual "0"

aff <- data.frame(c("accuracy", "false negative rate", "false positive rate"), c(accuracy, FNR, FPR))
names(aff) <- c("variable", "value")
aff

##(b) Draw prediction histgram
dat_obj_0 <- subset(test_dat, camp_result==0)
dat_obj_1 <- subset(test_dat, camp_result==1)

dat_obj_0$p <- predict(glm_b, newdata=dat_obj_0, type="response")
dat_obj_1$p <- predict(glm_b, newdata=dat_obj_1, type="response")

par(mfrow=c(1,1), mar=c(5,4,5,4))
hist(dat_obj_0$p, breaks=10, col = "#ff00ff40", border = "#ff00ff", 
     ylim=c(0, 2500), xlab="probablity", ylab="frequency",
     main="Probablity of Campaign Success")
hist(dat_obj_1$p, breaks=10, col = "#0000ff40", border = "#0000ff", add=TRUE)
legend("topright",legend=c("campaign result = 0","campaign result = 1"),
       fill=c("#ff00ff40","#0000ff40"))

##(c) Calcurate R-squared
(R2 <- 1 - with(glm_b, deviance/null.deviance))

####3.3 Develop logistic regression model with SMOTE####
###Boost minority category by SMOTE
train_dat_sm <- SMOTE(camp_result~., data = train_dat, perc.over = 1000, perc.under = 100)
#perc.over:Increase the minority category as multiplied by perc.over/100
#perc.under:Decrease the non-minority category to total number of data * perc.under/100

#Confirm change of the data
table(train_dat_sm$camp_result) #Ratio of "0" to "1" is approximately 50%

###Develop model
glm_a2 <- glm(camp_result~., data=train_dat_sm, family=binomial)
summary(glm_a2)

###Apply step function to improve AIC
glm_b2 <- step(glm_a2, direction="both")
summary(glm_b2)
glm_b2$coefficients

###Model evaluation
##Prediction for test data
pred2 <- predict(glm_b2, newdata=test_dat, type="response")
str(pred2)
act <- test_dat$camp_result

##(a) Confusion Matrix
prediction2 <- pred2

#Change data type of prediction from probability to factor
for (i in 1:length(prediction2)) {
  if (prediction2[i] < 0.5) {
    prediction2[i] <- 0
  } else {
    prediction2[i] <- 1
  }
}
#Confusion matrix
(op_mat2 <- table(act, prediction2))

#Accuracy, FPR, FNR
accuracy <- sum(diag(op_mat2))/sum(op_mat2)
FNR <- op_mat2[1,2]/sum(op_mat2[1,]) #Ratio number of predicted "0" to number of actual "1" 
FPR <- op_mat2[2,1]/sum(op_mat2[2,]) #Ratio number of predicted "1" to number of actual "0"

aff2 <- data.frame(c("accuracy", "false negative rate", "false positive rate"), c(accuracy, FNR, FPR))
names(aff2) <- c("variable", "value")
aff2

##(b) Draw prediction histgram
dat_obj_0 <- subset(test_dat, camp_result==0)
dat_obj_1 <- subset(test_dat, camp_result==1)

dat_obj_0$p <- predict(glm_b2, newdata=dat_obj_0, type="response")
dat_obj_1$p <- predict(glm_b2, newdata=dat_obj_1, type="response")

par(mfrow=c(1,1), mar=c(5,4,5,4))
hist(dat_obj_0$p, breaks=10, col = "#ff00ff40", border = "#ff00ff", 
     ylim=c(0, 1000), xlab="probablity", ylab="frequency",
     main="Probablity of Campaign Success")
hist(dat_obj_1$p, breaks=10, col = "#0000ff40", border = "#0000ff", add=TRUE)
legend("topright",legend=c("campaign result = 0","campaign result = 1"),
       fill=c("#ff00ff40","#0000ff40"))

##(c) Calcualte R-squared
(R2 <- 1 - with(glm_b2, deviance/null.deviance))
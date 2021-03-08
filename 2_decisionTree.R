## 2-0 Preparing ##
#GET AND INSTALL PACKAGES if needed
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("polycor")
#install.packages("ROCR")

#SET CURRENT DIRECTORY
#setwd("/work/R/140726/bank") #set your own folder
getwd()


## 2-1 READ AND CHECK DATA ##
#READ CSV
bank <- read.csv("bank1.csv", header=TRUE)
bank <- bank[,-1]   #DELETE FIRST COLUMN (ID COLUMN)
head(bank)
#DATA DESCRIPTION (unit or category example is shown in paratheses)
#age: customer's age (years)
#occupation: customer's occupation
#(administrator:1, technician:2, management:3, blue collar:4, entrepreneur:5, student:6,
#services:7, unemployed:8, housemaid:10, self employed:11, retired:12, unknown:9)
#marriage: customer's marriage status (1=single, 2=marriage, 3=divorced, unknown:9)
#academic_background: customer's academic background (high-school:1, college:2, graduate-college:3, unknown:9)
#debt_default: customer has debt default whether or not (default:1, not:0)
#credit: customer's credit (yen)
#house_loan: customer has house loan whether or not (loan:1, not:0)
#personal_loan: customer has personal loan whether or not (loan:1, not:0)
#duration_time: duration time in last campaign (day)
#contact_count: count to contact customer (count)
#previous_campaign: success of previous campaign (success:1, failure:2, other:3, unknown:9)
dim(bank)  #CONFIRM MATRIX SIZE
#CONFIRM DATA TYPE
for (i in 1:ncol(bank)) {
  print(c(names(bank[i]), class(bank[,i])))
}
#CHANGE DATA TYPE(INTEGER -> CHARACTER)
cols <- c("occupation", "marriage", "academic_background", "debt_default", 
  "house_loan", "personal_loan", "previous_campaign")
for (col in cols) {
  bank[,col] <- as.character(bank[,col])  
}


## 2-2 DISTINGUISH TEST AND TRAINED DATA ##
#SET SEED FOR GETTING SAME DATA IN REPEATED RUNS
set.seed(5)

#EXTRACT 80% ROW NUMBERS FROM DATA SET AT RANDOM  
trainID <- sample(nrow(bank), nrow(bank)*0.8, replace=FALSE)

#SET TRAINED DATA AND TEST DATA FROM DATA SET (80% DATA ARE FOR TRAINED, 20% ARE FOR TEST)
train_dat <- bank[trainID,]
test_dat <- bank[-trainID,]


## 2-3 CREATE DECISION TREE ##
library(rpart)
library(rpart.plot)

#RUN DECISION TREE
fit <- rpart(campaign_result~., data=train_dat,
    control=rpart.control(minsplit=30, cp=0.005), #cp: COMPLEXITY PARAMETER
    method="class", parms=list(split="information"))
    #minsplit: MINIMUM NUMBER OF EACH NODE
    #cp: COMPLEXITY PARAMETER

#CONFIRM DECISION TREE
par(ps=80)  #SET FONT SIZE
rpart.plot(fit, type=4, extra=1)  #PLOT DECISION TREE

#CONFIRM xerror ON EACH cp TO FIND ADEQUATE xerror VALUE
par(ps=15)  #SET FONT SIZE
#par(mai=c(rep(1.2, 4.0)), ps=12)  #SET MARGIN SIZE AND FONT SIZE
printcp(fit)    #PRINT xerror
plotcp(fit) #PLOT xerror(*)

#prune FUNCTION CAN MODIFY cp
fit2 <- prune(fit, cp=fit$cptable[4,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (¦)
#CONFIRM RESULT AFTER CHANGING cp VALUE
rpart.plot(fit2, type=4, extra=6)

#CONFIRM VARIABLE IMPORTANCE
attributes(fit2) #fit2 HAS variable.importance ATTRIBUTION

#PLOT variable importance (DISPLAY ONLY OUTER FLAME)
plot(fit2$variable.importance, type="n")
#PLOT variable importance (ADD LABELS AS TEXT)
text(labels=names(fit2$variable.importance),
     x=seq(1, length(fit2$variable.importance), by=1),
     y=fit2$variable.importance, cex=0.75)


## 2-4 VALIDATE PREDICTION MODEL WITH TEST DATA ##
#(A) PREDICTION WITH TEST DATA
#PREDICT evaluation (GOOD STORE OR BAD) WITH TEST DATA
predicted <- predict(fit2, newdata=test_dat, type="class")
#1ST ARGUMENT: PREDICTION MODEL(fit2)
#newdata: DATA TO PREDICT
#type: "class" IS PARAMETER TO PREDICT CATEGORICAL OBJECTIVE VARIABLES

#(B) MIXING MATRIX
#CONFIRM CORRECT AND PREDICTED COUNT FOR CALUCULATING ACCURACY
correct <- test_dat[,1] #SET CORRECT OBJECTIVE VARIABLES FROM TEST DATA
(cm <- table(correct, predicted))   #MAKE AND VIEW MATRIX OF CORRECT AND PREDICTED COUNT

#CALCULATE ACCURACY OF PREDICTION
(accuracyTraining <- sum(diag(cm))/sum(cm)) #ACCURACY
(FPR <- cm[2,1]/sum(cm[2,])) #FALSE NEGATIVE RATE (RATE OF "1" IN DATA PREDICTED "0")
(FNR <- cm[1,2]/sum(cm[1,])) #FALSE POSITIVE RATE (RATE OF "0" IN DATA PREDICTED "1")

#(C) HISTGRAM
#DEVIDE DATA BY evaluation VALUE (0 or 1)
test_dat0 <- subset(test_dat, campaign_result==0)
test_dat1 <- subset(test_dat, campaign_result==1)

#PREDICT ON EACH DATA SET TO ADAPT fit2 MODEL
test_dat0$P <- predict(fit2, newdata=test_dat0, type="prob") 
test_dat1$P <- predict(fit2, newdata=test_dat1, type="prob")
#type="prob" OPTION IS TO PREDICT POSSIBILITY OF OBJECTIVE VARIABLES

#DRAW HISTGRAM
par(mfrow=c(1,1), ps=12) #SET LAYOUT MARGIN SIZE AND FONT SIZE
head(test_dat1$P) #CONFIRM DATA TYPE
hist(test_dat1$P[,"1"], col = "#ff00ff40", border = "#ff00ff", breaks = 5, 
     xlab="possibility of success", xlim=c(0,1), ylim=c(0,2000), 
     main="Possibility of campaign success in test data")
hist(test_dat0$P[,"1"], col = "#0000ff40", border = "#0000ff", breaks = 5, add = TRUE)
legend("topright", legend=c("success", "failure"), fill=c("#ff00ff40","#0000ff40")) 
# #ff00ff40,#0000ff40 IS TRANSLUCENCE

##(D) ROC/AUC
#DRAW ROC CURVE test_dat USING PREDICTION RESULT
par(mfrow=c(1,1)) #ADAPT GRAPH LAYOUT

library(ROCR) #PACKAGE TO DRAW ROC CURVE
predicted.p <- predict(fit2, newdata=test_dat, type="prob")
predObj <- prediction(predicted.p[,2], test_dat$campaign_result)
rocObj <- performance(predObj, measure="tpr", x.measure="fpr")
aucObj <- performance(predObj, measure="auc")
(auc <- aucObj@y.values[[1]])

plot(rocObj,main ="ROC curve", col="blue")
par(new=T)
y <- function(x) x
plot(y ,0, 1, xlab="", ylab="")
legend("bottomright", legend=c("Decision Tree"), col=c("blue"), lty=c(1))

## (E) R-SQUARED
#CALCULATE R-SQUARED
library(polycor)
predicted.p_class <- predict(fit2, newdata=test_dat, type="class")

#CALUCULATE CORRELATION COEFFICIENT AMONG CATEGORICAL VARIABLES USING polychor FUNCTION
R <- polychor(predicted.p_class, test_dat[,"campaign_result"], ML=TRUE)
#ML=MAXMUM LIKELIHOOD
R_sq <- R^2 #CALCULATE R-SQUARED
sprintf("R: %4.3f, R^2: %4.3f", R, R_sq)  #PRINT R AND R-SQUARED
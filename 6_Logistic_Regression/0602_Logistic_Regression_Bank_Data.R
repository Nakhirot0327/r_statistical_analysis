## 2-0 Preparing ##
#SET CURRENT DIRECTORY
#setwd("/work/R/140927")
#getwd()

## 2-1 READ AND CHECK DATA ##
#READ CSV
dat <- read.csv("bankruptcy.csv", header=TRUE)
head(dat) #CONFIRM FIRST SIX ROWS
#DATA DESCRIPTION
#Bankruptcy:Whether each company went bankrupt or not
#Name:Company name
#Category:Business category
#GrossMargin:Gross margin(Uriage-so-rieki)
#CaptialRatio:Capital ratio(Jiko-shihon-hiritsu)
#SalesGrowth:Sales growth at latest year
#OperationMarginGrowth:Operating margin at latest year
#RatioOfOPCFtoCurrentDebt:Ratio of Oprating cash flow to current debt
#LogSalesPerEmployee:Logarithm of Sales per one employee
dim(dat)  #CONFIRM MATRIX SIZE

#CONFIRM DATA TYPE
for (i in 1:ncol(dat)) { print(c(names(dat[i]), class(dat[,i]))) }

#OMIT COLUMN TO BE UNUSED
delete <- c(2,3)
dat <- dat[,-delete]

## 1-2 LOGISTIC REGRESSION ##
#(a) LOGISTIC REGRESSION WITH ALL VARIABLES
#RUN LOGISTIC REGRESSION WITH ALL VARIABLES
glm_a <- glm(Bankruptcy~., data=dat, family=binomial)
summary(glm_a)

#CHECK MULTICOLLINEARITY
library(DAAG)
vif(glm_a)   #CHECK MULTICOLLINEARITY

#(b) RUN LOGISTIC REGRESSION WITH STEP WISE
#RUN LOGISTIC REGRESSION WITH VARIABLE SELECTION (STEP WISE)
glm_b <- step(glm_a, direction="both") 
summary(glm_b) #CHECK RESULT OFLOGISTIC REGRESSION 
glm_b$coefficients #CHECK COEFFICIENTS
vif(glm_b)   #CHECK MULTICOLLINEARITY (IF VALUE IS OVER 10, EXCLUDE THE VALUE.)

## 2-3 EVALUATE MODEL ##
#(b) DRAW GRAPH OF PREDICTION AND RESULT VALUE
dat_obj_0 <- subset(dat, campaign_result==0)
dat_obj_1 <- subset(dat, campaign_result==1)

dat_obj_0$p <- predict(glm_b, newdata=dat_obj_0, type="response")
dat_obj_1$p <- predict(glm_b, newdata=dat_obj_1, type="response")

par(mfrow=c(1,1), mar=c(5,4,5,4))
hist(dat_obj_0$p, breaks=10, col = "#ff00ff40", border = "#ff00ff", 
     ylim=c(0, 10000), xlab="probablity", ylab="frequency",
     main="Probablity of Campaign Success")
hist(dat_obj_1$p, breaks=10, col = "#0000ff40", border = "#0000ff", add=TRUE)
legend("topright",legend=c("campaign result = 0","campaign result = 1"),
       fill=c("#ff00ff40","#0000ff40")) # #ff00ff40 AND #0000ff ARE TRANSMISSIVE COLORS

#(c) CALCULATE R-SQUARE
(R2 <- 1 - with(glm_b, deviance/null.deviance))

#(d) ROC/AUC
library(ROCR)
#(d-1) CALCULATE AUC VALUE
par(mfrow=c(1,1))   #SET CANVAS
objs <- dat$campaign_result
preds <- predict(glm_b, newdata=dat, type="response")    #SET PREDICTED VALUE WITHOUT OUTLINERS

pred_op <- prediction(preds, objs)
perf_op <- performance(pred_op, measure="tpr", x.measure="fpr")
auc_tmp <- performance(pred_op, measure="auc")
(auc_cr <- auc_tmp@y.values[[1]])   #CONFIRM AUC

#(d-2) DRAW ROC CURVE
plot(perf_op, main ="ROC curve", col="red",
    xlab="False Positive Rate", ylab="True Positive Rate") #DRAW ROC CURVE
par(new=T)  #SET OVERRIDE OPTION
y <- function(x) x  #MAKE Y=X LENEAR
plot(y, 0, 1, xlab="", ylab="")  #DRAW Y=X LINEAR
legend("bottomright", legend=c("Logistic Regression"), col=c("red"), lty=c(1))  #ADD LEGEND

#(e) CONFUSION MATRIX AND ACCURACY, FALSE POSITIVE RATE, FALSE NEGATIVE RATE
#(e-1) CONFUSION MATRIX
#MAKE A COPY OF preds FOR CHANGING DATA TYPE
preds_0_1 <- preds
#ROUND PREDICTED VALUE FOR MAKING CONFUSION MATRIX
for (i in 1:length(preds_0_1)) {
    if (preds_0_1[i] < 0.5) {
        preds_0_1[i] <- 0
    } else {
        preds_0_1[i] <- 1
    }
}
(op_mat <- table(actual=objs, predicted=preds_0_1))  #MAKE CONFUSION MATRIX

#(e-2) ACCURACY, FALSE POSITIVE RATE, FALSE NEGATIVE RATE
#CALCULATE ACCURACY, FALSE POSITIVE RATE, FALSE NEGATIVE RATE
accuracy <- sum(diag(op_mat))/sum(op_mat) #ACCURACY
FNR <- op_mat[1,2]/sum(op_mat[1,]) #FALSE NEGATIVE RATE (RATE OF "1" IN DATA PREDICTED "0")
FPR <- op_mat[2,1]/sum(op_mat[2,]) #FALSE POSITIVE RATE (RATE OF "0" IN DATA PREDICTED "1")

aff <- data.frame(c("accuracy", "false negative rate", "false positive rate"),
          c(accuracy, FNR, FPR))
names(aff) <- c("variable", "value")
#DISPLAY ACCURACY, FALSE POSITIVE RATE, FALSE NEGATIVE RATE
aff

#(f) ODDS RATIO
summary(glm_b)$coefficients

#CALCULATE ODDS RATIO ON EACH EXPPLANATORY VARIABLE
odds <- sort(exp(glm_b$coefficients), decreasing=TRUE)
#PLOT EXPLANATORY VARIABLES AND ODDS RAIO
par(mfrow=c(1,1), mar=c(5,4,5,4), ps=15)
plot(odds, type="n", main="odds ratio", xlab="explanatory variable", ylab="odds ratio")
text(1:length(odds), odds,names(odds))

## 2-4 COMPARE WITH DECISION TREE ##
## READ AND CHECK DATA ##
#READ CSV
bank <- read.csv("bank.csv", header=TRUE)
bank <- bank[,-1]   #DELETE FIRST COLUMN (ID COLUMN)
head(bank)
dim(bank)  #CONFIRM MATRIX SIZE
#CONFIRM DATA TYPE
for (i in 1:ncol(bank)) { print(c(names(bank[i]), class(bank[,i]))) }
#CHANGE DATA TYPE(INTEGER -> CHARACTER)
cols <- c("occupation", "marriage", "academic_background", "debt_default", 
          "house_loan", "personal_loan", "previous_campaign")
for (col in cols) { bank[,col] <- as.character(bank[,col]) }


## DISTINGUISH TEST AND TRAINED DATA ##
#SET SEED FOR GETTING SAME DATA IN REPEATED RUNS
set.seed(5)

#EXTRACT 80% ROW NUMBERS FROM DATA SET AT RANDOM  
trainID <- sample(nrow(bank), nrow(bank)*0.7, replace=FALSE)

#SET TRAINED DATA AND TEST DATA FROM DATA SET (80% DATA ARE FOR TRAINED, 20% ARE FOR TEST)
train_dat <- bank[trainID,]
test_dat <- bank[-trainID,]

## CREATE DECISION TREE ##
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
fit2 <- prune(fit, cp=fit$cptable[4,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (※)
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


## VALIDATE PREDICTION MODEL WITH TEST DATA ##
# PREDICTION WITH TEST DATA
#PREDICT evaluation (GOOD STORE OR BAD) WITH TEST DATA
predicted <- predict(fit2, newdata=test_dat, type="class")
#1ST ARGUMENT: PREDICTION MODEL(fit2)
#newdata: DATA TO PREDICT
#type: "class" IS PARAMETER TO PREDICT CATEGORICAL OBJECTIVE VARIABLES


## (c) R-SQUARED
#CALCULATE R-SQUARED
library(polycor)
predicted.p_class <- predict(fit2, newdata=test_dat, type="class")

#CALUCULATE CORRELATION COEFFICIENT AMONG CATEGORICAL VARIABLES USING polychor FUNCTION
R <- polychor(predicted.p_class, test_dat[,"campaign_result"], ML=TRUE)
#ML=MAXMUM LIKELIHOOD
R_sq <- R^2 #CALCULATE R-SQUARED
sprintf("R-square(Decision Tree): %4.3f", R_sq)  #PRINT R-SQUARED
sprintf("R-square(Logistic Regression): %4.3f", R2)  #PRINT R AND R-SQUARED


##(d) ROC/AUC
#(d-1) CALCULATE AUC VALUE
#DRAW ROC CURVE test_dat USING PREDICTION RESULT
par(mfrow=c(1,1)) #ADAPT GRAPH LAYOUT

library(ROCR) #PACKAGE TO DRAW ROC CURVE
predicted.p <- predict(fit2, newdata=test_dat, type="prob")
predObj <- prediction(predicted.p[,2], test_dat$campaign_result)
rocObj <- performance(predObj, measure="tpr", x.measure="fpr")
aucObj <- performance(predObj, measure="auc")
auc <- aucObj@y.values[[1]] #CONFIRM AUC(Decision Tree)

sprintf("auc(Decision Tree): %4.3f", auc)
sprintf("auc(Logistic Regression): %4.3f", auc_cr)

#(d-2) DRAW ROC CURVE
par(mfrow=c(1,1), mar=c(3,3,3,3))   #SET CANVAS
plot(rocObj, main ="ROC curve", col="blue",
     xlab="False Positive Rate", ylab="True Positive Rate")
par(new=T)  #SET OVERRIDE OPTION
plot(perf_op, col="red") #DRAW ROC CURVE
par(new=T)
y <- function(x) x
plot(y ,0, 1, xlab="", ylab="")
legend("bottomright",legend=c("Decision Tree", "Logistic Regression"), 
       col=c("blue", "red"), lty=c(1))  #ADD LEGEND




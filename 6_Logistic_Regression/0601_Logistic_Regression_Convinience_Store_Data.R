## 1-0 Preparing ##
#GET AND INSTALL PACKAGES
install.packages("rpart")
install.packages("rpart.plot")
install.packages("polycor")
install.packages("ROCR")
install.packages("DAAG")

#SET CURRENT DIRECTORY
#setwd("/work/R/140927/")
#getwd()

## 1-1 READ AND CHECK DATA ##
#READ CSV
dat <- read.csv("convenience-store.csv", header=TRUE)
dat <- dat[,-1]   #DELETE FIRST COLUMN (ID COLUMN)
head(dat) #CONFIRM FIRST SIX ROWS
#DATA DESCRIPTION (unit is shown in paratheses)
#ealuation: when this value equals "1", it is a store whose revenue is increased. Otherwise when this value is "0", a store's revenue is decreased
#carSpace: car parking space (cars)
#floorSpace: floor space in store (meter^2)
#storeFrontTraffic: traffic in front of store (persons/day)
#frontage: frontage of store (meter)
dim(dat)  #CONFIRM MATRIX SIZE
table(dat$evaluation)  #CONFIRM NUMBER OF evaluation ("0" IS BAD STORE, OTHERWISE "1" IS GOOD STORE)

#CONFIRM DATA TYPE
for (i in 1:ncol(dat)) { print(c(names(dat[i]), class(dat[,i]))) }

## 1-2 LOGISTIC REGRESSION ##
#(a) LOGISTIC REGRESSION WITH ALL VARIABLES
#RUN LOGISTIC REGRESSION WITH ALL VARIABLES
glm_a <- glm(evaluation~., data=dat, family=binomial)
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


## 1-3 EVALUATE MODEL ##
#(a) CONFIRM LOGISTIC REGRESSION MODEL FROM GRAPH
par(mfrow=c(1,1), mar=c(5,5,5,5))   #SET CANVAS
plot(glm_a$linear.predictors, dat$evaluation, 
     xlab="Z", ylab="probability")   #PLOT ALL OBJECTIVE VARIABLES
glm_b_f <- data.frame(glm_b$linear.predictors, glm_b$fitted.values)     #MAKE PREDICTION MODEL
glm_b_f <- glm_b_f[order(glm_b_f[,1]),]     #SORT BY X-AXIS
lines(glm_b_f[,1], glm_b_f[,2], type="l", col="blue")   #DRAW PREDICTIVE VALUE FROM LOGISTIC REGRESSION MODEL()
legend("bottomright", legend=c("Logistic Regression"), col=c("blue"), lty=c(1)) #ADD LEGEND

#(b) DRAW GRAPH OF PREDICTION AND RESULT VALUE
dat_obj_0 <- subset(dat, evaluation==0)
dat_obj_1 <- subset(dat, evaluation==1)

dat_obj_0$p <- predict(glm_b, newdata=dat_obj_0, type="response") #type="response": OUTPUT ONLY PREDICTED PROBABILITY
dat_obj_1$p <- predict(glm_b, newdata=dat_obj_1, type="response") #type="response": OUTPUT ONLY PREDICTED PROBABILITY

par(mfrow=c(1,1), mar=c(3,3,3,3))
hist(dat_obj_0$p, breaks=10, col = "#ff00ff40", border = "#ff00ff", 
    xlim=c(0, 1), ylim=c(0, 40), xlab="probablity", ylab="frequency",
    main="Probablity of Revenue increased store")
hist(dat_obj_1$p, breaks=5, col = "#0000ff40", border = "#0000ff", add=TRUE)
legend("topright",legend=c("revenue decreased","revenue increased"),
       fill=c("#ff00ff40","#0000ff40")) # #ff00ff40 AND #0000ff ARE TRANSMISSIVE COLORS

#(c) CALCULATE R-SQUARE
(R2 <- 1 - with(glm_b, deviance/null.deviance))

#(d) ROC/AUC
library(ROCR)
#(d-1) CALCULATE AUC VALUE
par(mfrow=c(1,1), mar=c(3,3,3,3))   #SET CANVAS
objs <- dat$evaluation
preds <- predict(glm_b, newdata=dat, type="response")    #SET PREDICTED VALUE WITHOUT OUTLINERS

pred_op <- prediction(preds, objs)
perf_op <- performance(pred_op, measure="tpr", x.measure="fpr")
auc_tmp <- performance(pred_op, measure="auc")
(auc_cr <- auc_tmp@y.values[[1]])   #CONFIRM AUC

#(d-2) DRAW ROC CURVE
par(mfrow=c(1,1), mar=c(3,3,3,3))   #SET CANVAS
plot(perf_op, main ="ROC curve", col="red",
     xlab="False Positive Rate", ylab="True Positive Rate") #DRAW ROC CURVE
par(new=T)  #SET OVERRIDE OPTION
y <- function(x) x  #MAKE Y=X LENEAR
plot(y, 0,1, xlab="", ylab="")  #DRAW Y=X LINEAR
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

## 1-4 DECISION TREE ##
## READ AND CHECK DATA ##
#READ CSV
cs <- read.csv("convenience-store.csv", header=TRUE)
cs <- cs[,-1]   #DELETE FIRST COLUMN (ID COLUMN)

## DISTINGUISH TEST AND TRAINED DATA ##
#IN THIS CHAPTER, DATA IS NOT DISTINGUISHED (ONLY TRAINED DATA)
train_dat <- cs


## CREATE DECISION TREE ##
library(rpart)
library(rpart.plot)

#RUN DECISION TREE
fit <- rpart(evaluation~., data=train_dat,
             control=rpart.control(minsplit=2, cp=0.000001), 
             method="class", parms=list(split="information"))
#minsplit: MINIMUM NUMBER OF EACH NODE
#cp: COMPLEXITY PARAMETER

#CONFIRM DECISION TREE
par(ps=80)  #SET FONT SIZE
rpart.plot(fit,type=4, extra=1)  #PLOT DECISION TREE
#"0" = bad store, "1" = good store 

#CONFIRM xerror ON EACH cp TO FIND ADEQUATE xerror VALUE
par(mai=c(rep(1.2, 4.0)), ps=12)  #SET LAYOUT MARGIN SIZE AND FONT SIZE
printcp(fit)    #PRINT xerror
plotcp(fit) #PLOT xerror(*)

#RECREATE DECISION TREE TO USE prune FUNCTION WITH ADEQUATE cp
fit2 <- prune(fit, cp=fit$cptable[3,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (çª¶?½»)
##PLOT DECISION TREE AFTER CHANGING cp
rpart.plot(fit2, type=4, extra=1, cex=2)

#CONFIRM variable.importance
attributes(fit2) #fit2 HAS variable.importance ATTRIBUTION

#PLOT OUTER FLAME FOR PLOTTING variable importance
plot(fit2$variable.importance, type="n")
par(ps=15)  #SET FONT SIZE
#PLOT variable importance (ADD LABELS AS TEXT)
text(labels=names(fit2$variable.importance),
     x=seq(1, length(fit2$variable.importance), by=1),
     y=fit2$variable.importance, cex=0.75)
par(mfrow=c(1,1), mar=c(3,3,3,3))   #SET CANVAS


#(c) CALCULATE R-SQUARE
library(polycor)
predicted.p_class <- predict(fit2, newdata=dat, type="class")

#ploychor IS USED TO CALCULATE COEFFICIENT OF CORRELATION WITH CATEGORICAL VARIABLES
R <- polychor(predicted.p_class, dat[,"evaluation"], ML=TRUE) #ML MEANS "MAXMUM LIKELIHOOD"
R_sq <- R^2

sprintf("R-square(Decision Tree): %4.3f", R_sq)
sprintf("R-square(Logistic Regression): %4.3f", R2)


#(d) ROC/AUC
#(d-1) CALCULATE AUC VALUE
predicted.p <- predict(fit2, newdata=cs, type="prob")

predObj <- prediction(predicted.p[,2], cs$evaluation)
rocObj <- performance(predObj,measure="tpr",x.measure="fpr")
aucObj <- performance(predObj,measure="auc")

print("auc(Decision Tree)")
(auc <- aucObj@y.values[[1]]) #CONFIRM AUC(Decision Tree)

print("auc(Logistic Regression)")
(auc_cr <- auc_tmp@y.values[[1]])   #CONFIRM AUC(Logistic Regression)

#(d-2)DRAW ROC CURVE
par(mfrow=c(1,1), mar=c(3,3,3,3))   #SET CANVAS
plot(rocObj, main ="ROC curve", col="blue",
     xlab="False Positive Rate", ylab="True Positive Rate")
par(new=T)
plot(perf_op, col="red") #DRAW ROC CURVE
par(new=T)  #SET OVERRIDE OPTION
y <- function(x) x
plot(y, 0, 1, xlab="", ylab="")
legend("bottomright",legend=c("Decision Tree", "Logistic Regression"), 
       col=c("blue", "red"), lty=c(1))  #ADD LEGEND

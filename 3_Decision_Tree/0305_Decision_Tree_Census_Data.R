## 4-0 PREPARING ##
#GET AND INSTALL PACKAGES if needed
install.packages("rpart")
install.packages("rpart.plot")
install.packages("polycor")
install.packages("ROCR")
install.packages("DAAG")

#SET CURRENT DIRECTORY
#setwd("/work/R/140927") #set your own folder

## 4-1 READ AND CHECK DATA ##
dat <- read.csv("MIC_population_stat_kadai2_v3_en.csv", head=TRUE, row.names=1)
head(dat)
###Data description###
#PopdecRatio00to05 = How many percent of population decrease from 2000 to 2005
#RatioUnder15 = Ratio of population under 15
#RatioUnder15to64 Ratio of population over 15 and under 64
#RatioOver65 = Ratio of population over 65
#RatioofNucfamily = Ratio of number of nuclear families
#RatioSingle = Ratio of number of singles
#RatioofElderSingle = Ratio of number of singles who are over 65
#RatioofElderTwo = Ratio of number of couples who are over 65(man) / 60(woman)
#RatioofEmp = Ratio of number of employees
#RatioPrimarySector = Ratio of number of employees of Dai-ichiji-sangyo
#RatioSecondorySector = Ratio of number of employees of Dai-niji-sangyo
#RatioTertiarySector = Ratio of number of employees of Dai-sanji-sangyo
#RatioDayPop = Ratio of day population to night population (Chu-kan Jinkou)
#Disappear = whether the city disappeared or not between 2005 to 2010 (0=not disappeared,1=disappeared)


#CONFIRM DATA TYPE
for (i in 1:ncol(dat)) { print(c(names(dat[i]), class(dat[,i]))) }


## 1-2 LOGISTIC REGRESSION ##
#(a) LOGISTIC REGRESSION WITH ALL VARIABLES
#RUN LOGISTIC REGRESSION WITH ALL VARIABLES
glm_a <- glm(Disappear~., data=dat, family=binomial)
summary(glm_a)

#CHECK MULTICOLLINEARITY
library(DAAG)
(v_a <- vif(glm_a))   #CHECK MULTICOLLINEARITY

#(b) LOGISTIC REGRESSION WITHOUT MULTICOLINEARITY COLUMNS
#CHECK MAX VIF VALUE OVER 10
while (max(v_a) > 10) {
  for (i in 1:length(v_a)) {
    if (v_a[i] == max(v_a)) {
      dat <- dat[,-i]
    }
  }
  glm_a <- glm(Disappear~., data=dat, family=binomial)
  summary(glm_a)
  (v_a <- vif(glm_a))
}
summary(glm_a)

#CHECK MULTICOLLINEARITY
vif(glm_a)   #CHECK MULTICOLLINEARITY


#(b) RUN LOGISTIC REGRESSION WITH STEP WISE
#RUN LOGISTIC REGRESSION WITH VARIABLE SELECTION (STEP WISE)
glm_b <- step(glm_a, direction="both") 
summary(glm_b) #CHECK RESULT OFLOGISTIC REGRESSION 
glm_b$coefficients #CHECK COEFFICIENTS
vif(glm_b)   #CHECK MULTICOLLINEARITY (IF VALUE IS OVER 10, EXCLUDE THE VALUE.)

## 2-3 EVALUATE MODEL ##
#(b) DRAW GRAPH OF PREDICTION AND RESULT VALUE
dat_obj_0 <- subset(dat, Disappear==0)
dat_obj_1 <- subset(dat, Disappear==1)

dat_obj_0$p <- predict(glm_b, newdata=dat_obj_0, type="response")
dat_obj_1$p <- predict(glm_b, newdata=dat_obj_1, type="response")

par(mfrow=c(1,1), mar=c(5,4,5,4))
hist(dat_obj_0$p, breaks=5, col = "#ff00ff40", border = "#ff00ff", 
     xlim=c(0,1), ylim=c(0,1000), xlab="probablity", ylab="frequency",
     main="Probablity of Revenue increased store")
hist(dat_obj_1$p, breaks=10, col = "#0000ff40", border = "#0000ff", add=TRUE)
legend("topright",legend=c("Existed","Disappeared"),
       fill=c("#ff00ff40","#0000ff40")) # #ff00ff40 AND #0000ff ARE TRANSMISSIVE COLORS

#(c) CALCULATE R-SQUARE
(R2 <- 1 - with(glm_b, deviance/null.deviance))

#(d) AUC
library(ROCR)
#(d-1) CALCULATE AUC VALUE
par(mfrow=c(1,1))   #SET CANVAS
objs <- dat$Disappear
preds <- predict(glm_b, newdata=dat, type="response")    #SET PREDICTED VALUE WITHOUT OUTLINERS

pred_op <- prediction(preds, objs)
perf_op <- performance(pred_op, measure="tpr", x.measure="fpr")
auc_tmp <- performance(pred_op, measure="auc")
(auc_cr <- auc_tmp@y.values[[1]])   #CONFIRM AUC

#(d-2) DRAW AUC CURVE
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
(op_mat <- table(objs, preds_0_1))  #MAKE CONFUSION MATRIX

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


## 3-4 COMPARE WITH DECISION TREE ##
#IN THIS CHAPTER, DATA IS NOT DISTINGUISHED (ONLY TRAINED DATA)
dat <- read.csv("MIC_population_stat_kadai2_v3_en.csv", head=TRUE, row.names=1)

#SET SEED FOR GETTING SAME DATA IN REPEATED RUNS
set.seed(5)

#EXTRACT 80% ROW NUMBERS FROM DATA SET AT RANDOM  
trainID <- sample(nrow(dat), nrow(dat)*0.7, replace=FALSE)

#SET TRAINED DATA AND TEST DATA FROM DATA SET (80% DATA ARE FOR TRAINED, 20% ARE FOR TEST)
train_dat <- dat[trainID,]
test_dat <- dat[-trainID,]





## CREATE DECISION TREE ##
library(rpart)
library(rpart.plot)

#RUN DECISION TREE
fit <- rpart(Disappear~., data=train_dat,
             control=rpart.control(minsplit=2, cp=0.01), 
             method="class", parms=list(split="information"))
#minsplit: MINIMUM NUMBER OF EACH NODE
#cp: COMPLEXITY PARAMETER

#CONFIRM DECISION TREE
par(ps=80)  #SET FONT SIZE
rpart.plot(fit,type=4, extra=1)  #PLOT DECISION TREE
#"0" = Not disappear, "1" = Disappear 

#CONFIRM xerror ON EACH cp TO FIND ADEQUATE xerror VALUE
par(mai=c(rep(1.2, 4.0)), ps=12)  #SET LAYOUT MARGIN SIZE AND FONT SIZE
printcp(fit)    #PRINT xerror
plotcp(fit) #PLOT xerror(*)

#RECREATE DECISION TREE TO USE prune FUNCTION WITH ADEQUATE cp
fit2 <- prune(fit, cp=fit$cptable[2,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (*)
##PLOT DECISION TREE AFTER CHANGING cp
rpart.plot(fit2, type=4, extra=4, cex=0.75)

#CONFIRM variable.importance
attributes(fit2) #fit2 HAS variable.importance ATTRIBUTION

#PLOT OUTER FLAME FOR PLOTTING variable importance
plot(fit2$variable.importance, type="n")
par(ps=15)  #SET FONT SIZE
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
R <- polychor(predicted.p_class, test_dat[,"Disappear"], ML=TRUE)
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
predObj <- prediction(predicted.p[,2], test_dat$Disappear)
rocObj <- performance(predObj, measure="tpr", x.measure="fpr")
aucObj <- performance(predObj, measure="auc")
auc <- aucObj@y.values[[1]] #CONFIRM AUC(Decision Tree)

sprintf("auc(Decision Tree): %4.3f", auc)
sprintf("auc(Logistic Regression): %4.3f", auc_cr)

#(d-2)DRAW ROC CURVE
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



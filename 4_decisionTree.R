## 5-0 Preparing ##
#GET AND INSTALL PACKAGES if needed
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("polycor")
#install.packages("ROCR")

#SET CURRENT DIRECTORY
#setwd("/work/R/140726/bank") Set your own folder
getwd()


## 5-1 READ AND CHECK DATA ##
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
#duration_time: duration time in last campaign (sec)
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


## 5-2 DISTINGUISH TEST AND TRAINED DATA ##
#SET SEED FOR GETTING SAME DATA IN REPEATED RUNS
set.seed(4)

#EXTRACT 80% ROW NUMBERS FROM DATA SET AT RANDOM  
trainID <- sample(nrow(bank), nrow(bank)*0.8, replace=FALSE)

#SET TRAINED DATA AND TEST DATA FROM DATA SET (80% DATA ARE FOR TRAINED, 20% ARE FOR TEST)
train_dat <- bank[trainID,]
test_dat <- bank[-trainID,]


## 5-3 CREATE DECISION TREE ##
library(rpart)
library(rpart.plot)

#RUN DECISION TREE
fit <- rpart(credit~., train_dat,
    control=rpart.control(minsplit=30, cp=0.001), method="anova")
    #minsplit: MINIMUM NUMBER OF EACH NODE
    #cp: COMPLEXITY PARAMETER

#CONFIRM DECISION TREE
par(ps=50)  #SET FONT SIZE
rpart.plot(fit, type=4, extra=1, cex=0.2)  #PLOT DECISION TREE

#CONFIRM xerror ON EACH cp TO FIND ADEQUATE xerror VALUE
par(ps=20)  #SET FONT SIZE
#par(mai=c(rep(1.2, 4.0)), ps=12)  #SET MARGIN SIZE AND FONT SIZE
printcp(fit)    #PRINT xerror
plotcp(fit) #PLOT xerror(*)

#prune FUNCTION CAN MODIFY cp
fit2 <- prune(fit, cp=fit$cptable[3,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (¦)
#CONFIRM RESULT AFTER CHANGING cp VALUE
rpart.plot(fit2, type=4, extra=1, cex=0.6)

#CONFIRM VARIABLE IMPORTANCE
attributes(fit2) #fit2 HAS variable.importance ATTRIBUTION

#PLOT variable importance (DISPLAY ONLY OUTER FLAME)
plot(fit2$variable.importance, type="n")
#PLOT variable importance (ADD LABELS AS TEXT)
text(labels=names(fit2$variable.importance),
     x=seq(1, length(fit2$variable.importance), by=1),
     y=fit2$variable.importance, cex=0.5)


## 5-4 VALIDATE PREDICTION MODEL WITH TEST DATA ##
#(A) PREDICTION WITH TEST DATA
#PREDICT evaluation (GOOD STORE OR BAD) WITH TEST DATA
predicted <- predict(fit2, newdata=test_dat)
#1ST ARGUMENT: PREDICTION MODEL(fit2)
#newdata: DATA TO PREDICT
#type: NO SET (DEFAULT IS ON THE SCALE OF LINEAR PREDICTIONS)


correct <- test_dat[,"credit"] #SET CORRECT OBJECTIVE VARIABLES(credit) FROM TEST DATA

#SET PARAMETER FOR CALCULATION OF CORRELATION COEFFICIENT
N <- dim(test_dat)[1]
p <- dim(test_dat)[2]


#CALCULATE CORRELATION COEFFICIENT
cor(correct, predicted) * (N-1) / (N-p-1) #R
(cor(correct, predicted) * (N-1) / (N-p-1))^2 #R^2

#CONFIRM correct AND predicted
head(data.frame(correct, predicted))
minLim = -500000; maxLim = 3000000  #SET FOR xlim AND ylim
plot(correct, predicted, xlim=c(minLim,maxLim), ylim=c(minLim,maxLim),
    pch=16, cex=1, col="#0000ff40")
abline(a=0, b=1, col="red")    #ADD LINEAR (y=x)
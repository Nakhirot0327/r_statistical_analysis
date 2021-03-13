## 3-0 Preparing ##
#GET AND INSTALL PACKAGES
install.packages("rpart")
install.packages("rpart.plot")

#SET CURRENT DIRECTORY
setwd("/work/R/140726/vehicle")
getwd()

## 3-1 READ AND CHECK DATA ##
#READ CSV
vehicle <- read.csv("vehicle.csv", header=TRUE)
head(vehicle) #CONFIRM FIRST SIX ROWS
#DATA DESCRIPTION (unit is shown in paratheses)
#Class: car classes are "van", "saab", "bus", "opel"
#Compactness: (average perim)**2/area 
#Circlularity: (average radius)**2/area 
#Distance_circularity: area/(av.distance from border)**2 
#Radius_ratio: (max.rad-min.rad)/av.radius 
#Praxis_aspect_ratio: minor axis)/(major axis)
#Max_length_aspect_ratio: (length perp. max length)/(max length) 
#Scatter_ratio: (inertia about minor axis)/(inertia about major axis) 
#Elongatedness: area/(shrink width)**2 
#Praxis_rectangular: area/(pr.axis length*pr.axis width)
#Length_rectangular: area/(max.length*length perp. to this) 
#Major_variance: (2nd order moment about minor axis)/area 
#Minor_variance: (2nd order moment about major axis)/area 
#Gyration_radius: (mavar+mivar)/area 
#Major_skewness: (3rd order moment about major axis)/sigma_min**3
#Minor_skewness: (3rd order moment about minor axis)/sigma_maj**3 
#Major_kurtosis: (4th order moment about major axis)/sigma_min**4 
#Minor_kurtosis: (4th order moment about minor axis)/sigma_maj**4 
#Hollows_ratio: (area of hollows)/(area of bounding polygon) 

dim(vehicle)  #CONFIRM MATRIX SIZE
summary(vehicle$Class)  #CONFIRM NUMBER OF EACH CLASS (vehicle$Class IS OBJECTIVE VARIABLE)

#CONFIRM DATA TYPE
for (i in 1:ncol(vehicle)) {
  print(c(names(vehicle[i]), class(vehicle[,i])))
}


## 3-2 DISTINGUISH TEST AND TRAINED DATA ##
#SET SEED FOR GETTING SAME DATA IN REPEATED RUNS
set.seed(2)

#EXTRACT 80% ROW NUMBERS FROM DATA SET AT RANDOM  
trainNO <- sample(nrow(vehicle), nrow(vehicle)*0.8, replace=FALSE)

#SET TRAINED DATA AND TEST DATA FROM DATA SET (80% DATA ARE FOR TRAINED, 20% ARE FOR TEST)
train_dat <- vehicle[trainNO,]
test_dat <- vehicle[-trainNO,]


## 3-3 CREATE DECISION TREE ##
library(rpart)
library(rpart.plot)

#RUN DECISION TREE
fit <- rpart(Class~., data=train_dat,
    control=rpart.control(minsplit=30, cp=0.00001),
    method="class", parms=list(split="information"))
    #minsplit: MINIMUM NUMBER OF EACH NODE
    #cp: COMPLEXITY PARAMETER

#CONFIRM DECISION TREE
par(ps=80)  #SET FONT SIZE
rpart.plot(fit, type=4, extra=1)  #PLOT DECISION TREE

#CONFIRM xerror ON EACH cp TO FIND ADEQUATE xerror VALUE
par(ps=20)  #SET FONT SIZE
#par(mai=c(rep(1.2, 4.0)), ps=12)  #SET MARGIN SIZE AND FONT SIZE
printcp(fit)    #PRINT xerror
plotcp(fit) #PLOT xerror(*)

#prune FUNCTION CAN MODIFY cp
fit2 <- prune(fit, cp=fit$cptable[7,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (¦)
#CONFIRM RESULT AFTER CHANGING cp VALUE
rpart.plot(fit2, type=4, extra=1)

#CONFIRM VARIABLE IMPORTANCE
attributes(fit2) #fit2 HAS variable.importance ATTRIBUTION

#PLOT variable importance (DISPLAY ONLY OUTER FLAME)
plot(fit2$variable.importance, type="n")
#PLOT variable importance (ADD LABELS AS TEXT)
text(labels=names(fit2$variable.importance),
     x=seq(1, length(fit2$variable.importance), by=1),
     y=fit2$variable.importance, cex=0.5)


## 3-4 VALIDATE MODEL ACCURACY USING TEST DATA ##
#(A) PREDICTION WITH TEST DATA
#PREDICT evaluation (GOOD STORE OR BAD) WITH TEST DATA
predicted <- predict(fit2, newdata=test_dat, type="class")
#1ST ARGUMENT: PREDICTION MODEL(fit2)
#newdata: DATA TO PREDICT
#type: "class" IS PARAMETER TO PREDICT CATEGORICAL OBJECTIVE VARIABLES

#(B) MIXING MATRIX
#CONFIRM CORRECT AND PREDICTED COUNT FOR CALCULATING ACCURACY
correct <- test_dat[,1] #SET CORRECT OBJECTIVE VARIABLES FROM TEST DATA
(cm <- table(correct, predicted))   #MAKE AND VIEW MATRIX OF CORRECT AND PREDICTED COUNT

#CALCULATE ACCURACY OF PREDICTION
(accuracyTraining <- sum(diag(cm))/sum(cm)) #ACCURACY

#FALSE POSITIVE RATE (RATE OF INCORRECT IN DATA WHOSE CLASSES ARE "A"(E.X. bus))
FPR <- 1:ncol(cm)
for (i in 1:ncol(cm)) {
  FPR[i] <- (sum(cm[,i]) - cm[i,i])/sum(cm[,i])
}

#FALSE NEGATIVE RATE (RATE OF INCORRECT IN DATA WHOSE CLASSES ARE "A"(E.X. bus))
FNR <- 1:nrow(cm) 
for (j in 1:nrow(cm)) {
  FNR[j] <- (sum(cm[j,]) - cm[j,j])/sum(cm[j,])
}
(ftable <- data.frame(colnames(cm), FNR, FPR))
lines(ftable)

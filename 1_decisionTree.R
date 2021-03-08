## 1-0 Preparing ##
#GET AND INSTALL PACKAGES if needed
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("polycor")
#install.packages("ROCR")

#SET CURRENT DIRECTORY
#setwd("/work/R/140726/convenience-store") #SET your own folder
getwd()

## 1-1 READ AND CHECK DATA ##
#READ CSV
cs <- read.csv("convenience-store.csv", header=TRUE)
cs <- cs[,-1]   #DELETE FIRST COLUMN (ID COLUMN)
head(cs) #CONFIRM FIRST SIX ROWS
#DATA DESCRIPTION (unit is shown in paratheses)
#ealuation: when this value equals "1", it is a store whose revenue is increased. Otherwise when this value is "0", a store's revenue is decreased
#carSpace: car parking space (cars)
#floorSpace: floor space in store (meter^2)
#storeFrontTraffic: traffic in front of store (persons/day)
#frontage: frontage of store (meter)
dim(cs)  #CONFIRM MATRIX SIZE
summary(cs$evaluation)  #CONFIRM NUMBER OF evaluation ("0" IS BAD STORE, OTHERWISE "1" IS GOOD STORE)

#CONFIRM DATA TYPE
for (i in 1:ncol(cs)) {
    print(c(names(cs[i]), class(cs[,i])))
}


## 1-2 DISTINGUISH TEST AND TRAINED DATA ##
#IN THIS CHAPTER, DATA IS NOT DISTINGUISHED (ONLY TRAINED DATA)
train_dat <- cs


## 1-3 CREATE DECISION TREE ##
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
rpart.plot(fit,type=4,extra=1)  #PLOT DECISION TREE
#"0" = bad store, "1" = good store 

#CONFIRM xerror ON EACH cp TO FIND ADEQUATE xerror VALUE
par(mai=c(rep(1.2, 4.0)), ps=12)  #SET LAYOUT MARGIN SIZE AND FONT SIZE
printcp(fit)    #PRINT xerror
plotcp(fit) #PLOT xerror(*)

#RECREATE DECISION TREE TO USE prune FUNCTION WITH ADEQUATE cp
fit2 <- prune(fit, cp=fit$cptable[3,1]) #CHANGE x, y (IN cptable[x,y]) IN RESPONSE TO RESULT OF (※)
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
## 3 Statistic Test - Practice 11 Chi-square Test##
## Edited by DSL https://www.facebook.com/dsl.statclass/ ##

#Firstly set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160723_B_Statistics_Test/Data/")

####0. Package Install####
if("dummies" %in% rownames(installed.packages()) == FALSE) {install.packages("dummies")}
library(dummies)
if("discretization" %in% rownames(installed.packages()) == FALSE) {install.packages("discretization")}
library(discretization)
if("arules" %in% rownames(installed.packages()) == FALSE) {install.packages("arules")}
library(arules)
if("gplots" %in% rownames(installed.packages()) == FALSE) {install.packages("gplots")}
library(gplots)

####1. Data Input####
dat <- read.csv("10_Questionnaire_Answer.csv")
str(dat) #Data type
dim(dat) #Data size

for(i in 1:23) {
  dat[,i] <- as.factor(dat[,i])
}

for(i in 84:93) {
  dat[,i] <- as.factor(dat[,i])
}

for(i in 95:110) {
  dat[,i] <- as.factor(dat[,i])
}

str(dat)

####2. Data Preparation####
##For Q3, Q4, Q5 and Q6
colnames(dat) #from 24th column to 83rd column

#Replace "1" and "2" with "1or2", and "4" and "5" with "4or5"
for(i in 24:83) {
  dat[,i] <- replace(dat[,i], which(dat[,i] == 1|dat[,i] == 2), "1or2")
  dat[,i] <- replace(dat[,i], which(dat[,i] == 4|dat[,i] == 5), "4or5")
  dat[,i] <- as.factor(dat[,i])
}

##For Q7
colnames(dat) #from 84th column to 91st column

dat_q <- dat[,84:91] #Choose Q7 columns
dat_q <- dummy.data.frame(dat_q) #Make dummy variables
dat <- cbind(dat[,1:83],dat_q,dat[,92:110]) #Replace Q7 columns with dammied data

##For F3(Age)
table(dat[,"F3"]) #Should divide the data into 20s, 30s, 40s and 50s
dat[,"F3"] <- discretize(dat[,"F3"], method = "fixed",
                         categories = c(-Inf,30,40,50,Inf),
                         labels = c("20s","30s","40s","50s"))
table(dat[,"F3"])

##For F6(Occupation)
table(dat[,"F6"]) #2,3,4,5,8,9 and 10 should be merged due to small sample size
me <- c(2,3,4,5,8,9,10)
dat[,"F6"] <- as.character(dat[,"F6"])
dat[,"F6"] <- replace(dat[,"F6"], which(dat[,"F6"] %in% me), "Others")
dat[,"F6"] <- replace(dat[,"F6"], which(dat[,"F6"] == 1), "Employee")
dat[,"F6"] <- replace(dat[,"F6"], which(dat[,"F6"] == 6), "Part-time")
dat[,"F6"] <- replace(dat[,"F6"], which(dat[,"F6"] == 7), "Housewife")
dat[,"F6"] <- as.factor(dat[,"F6"])
table(dat[,"F6"])

##For F7.1(Number of children)
table(dat[,"F7.1"]) #2,3 and 4 should be integrated
dat[,"F7.1"] <- as.character(dat[,"F7.1"])
dat[,"F7.1"] <- replace(dat[,"F7.1"], 
                        which(dat[,"F7.1"] == 2 |
                                dat[,"F7.1"] == 3 |
                                dat[,"F7.1"] == 4), "MoreThan2")
dat[,"F7.1"] <- replace(dat[,"F7.1"], which(dat[,"F7.1"] == 5), "None")
dat[,"F7.1"] <- as.factor(dat[,"F7.1"])

##For F7.2(Number of children living together)
table(dat[,"F7.2"]) #2,3 and 4, 1 and 5 should be integrated
dat[,"F7.2"] <- as.character(dat[,"F7.2"])
dat[,"F7.2"] <- replace(dat[,"F7.2"], 
                        which(dat[,"F7.2"] == 2 |
                                dat[,"F7.2"] == 3 |
                                dat[,"F7.2"] == 4), "MoreThan2")
dat[,"F7.2"] <- replace(dat[,"F7.2"], 
                        which(dat[,"F7.2"] == 1 |
                                dat[,"F7.2"] == 5), "0or1")
dat[,"F7.2"] <- as.factor(dat[,"F7.2"])
table(dat[,"F7.2"])

####3. Chisq-test####
##confirm data
colnames(dat)
#From 2nd column to 163rd column - answer variable
#From 164th column to 182nd column - attribute variable
exp <- c(2:163)
atr <- c(164:182)
pmat <- matrix(0,nrow=length(exp),ncol=length(atr))
rownames(pmat) <- colnames(dat[,exp])
colnames(pmat) <- colnames(dat[,atr])
k <- 1
l <- 1

##chisq-test iteration for all conbination of answers and attributes  
for(i in exp) {
  for(j in atr) {
    tab <- table(dat[,c(i,j)])
    result <- chisq.test(tab,correct = TRUE)
    pmat[k,l] <- result$p.value
    print(paste("i =",i,"j =",j,"is done"))
    l <- l + 1
  }
  k <- k + 1
  l <- 1
}

####4. Result Visualization####
##heatmap of p-values of the chi-square tests
heatmap.2(t(pmat), #data
          col = redgreen(256), #color of heatmap
          #Colv = NA, #Omit dendrogram of rows
          margin = c(10,5), #Adjustment of space of the heatmap
          cexCol = 0.6, #Adjustment of scale of letters of rows
          trace="none", #Omit value on heatmap
          xlab="Attributes", #Title of x-variable
          ylab="Answers") #Title of y-variable



write.table(pmat,file = "11_ChisqTest_out.csv",sep=",",row.names=FALSE)

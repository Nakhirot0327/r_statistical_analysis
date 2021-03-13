####DSL medium class 1 - 1. Missing Value Imputation####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160521_M_Data_Cleansing/Data/")

#Data Input
dat <- read.csv("M1_Bank_Customer_v2.csv")
head(dat)
dat <- dat[,-1]

#####1. Package Insall#####
if("mice" %in% rownames(installed.packages()) == FALSE) {install.packages("mice")}
library(mice)
if("lattice" %in% rownames(installed.packages()) == FALSE) {install.packages("lattice")}
library(lattice)
if("VIM" %in% rownames(installed.packages()) == FALSE) {install.packages("VIM")}
library(VIM)
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}
library(corrplot)

#####2. Data Confirmation#####
####Confirm data class and correct data type
sapply(dat, class)
fac <- c("camp_result", "occupation", "marriage", "acad_backg",
         "debt_default", "house_loan", "personal_loan", "prev_camp")
for(i in fac) {
  dat[,i] <- as.factor(dat[,i])
}
sapply(dat, class)

####Change "Unknown" values to NA
dat2 <- dat
select <- c("occupation","marriage","acad_backg","prev_camp")

for(i in select) {
  repl <- which(dat[,i] == 9)
  dat2[,i] <- replace(dat[,i], repl, NA)
}

###Reject unused levels for factor variables
dat2 <- droplevels(dat2)

####Visualize missing value status
par(ps=8)
aggr(dat2, prop=FALSE,number=TRUE) #Red is missing values - about 80% of rows include missing values
par(ps=10)
#Assume that structure of missing values is MAR

#####3. Develop Linear Regression Models#####
####Liner regression with categorized missing values
lm1 <- step(glm(camp_result~.,dat,family="binomial"), direction = "both")
lm1.s <- summary(lm1)
lm1.s

####Liner regression (NA omitted)
dat3 <- na.omit(dat2)
lm2 <- step(glm(camp_result~.,dat3,family="binomial"), direction = "both")
lm2.s <- summary(lm2)
lm2.s

####Multiple Imputation
###Imputation by using polytomous logistic regression
m <- 10 #Number of iteration in missing value imputation
imp <- mice(dat2, seed = 123, method = "polyreg", m = m)
#"polyreg" is polytomous regression imputation for unordered categorical data (factor >= 2 levels)

###Develop Linear Regression Models for each imputed data set
fit <- with(imp, glm(camp_result~credit + age + occupation +
                       marriage + acad_backg + debt_default +
                       house_loan + personal_loan + duration_time + 
                       contact_count + prev_camp, family = "binomial"))

##Apply Step Function For Each Result
#Adjust rownames of "coefficients matrix"
bef <- c("camp_result2","debt_default2","house_loan2","personal_loan2","occupation9","occupation10","occupation11")
aft <- c("camp_result1","debt_default1","house_loan1","personal_loan1","occupation10","occupation11","occupation12")

for(i in 1:m) {
  eval(parse(text = paste0("lm3",i,".s <- summary(step(fit$analyses[[",i,"]]))")))
  eval(parse(text = paste0("be <- which(rownames(lm3",i,".s$coefficients) %in% bef)")))
  eval(parse(text = paste0("af <- which(bef %in% rownames(lm3",i,".s$coefficients))")))
  eval(parse(text = paste0("rownames(lm3",i,".s$coefficients)[be] <- aft[af]")))
}

##Integrate results of analysis based on multiple imputation
pool.fit <- pool(fit)
sum.pf <- summary(pool.fit)

#Adjust rownames
be <- which(rownames(sum.pf) %in% bef)
af <- which(bef %in% rownames(sum.pf))
rownames(sum.pf)[be] <- aft[af]

#####4. Compare Result of the Analysis#####
labels <- c("Categorized","Omitted","MI1","MI2","MI3","MI4","MI5",
            "MI6","MI7","MI8","MI9","MI10","Integrated_MI")

###Coefficients & p-values
nam <- rownames(summary(glm(camp_result~.,dat,family="binomial"))$coefficients)
coef <- as.data.frame(matrix(0, nrow = length(nam), ncol = m+4))
coef[,1] <- c(nam)
colnames(coef) <- c("Variable", labels)
p.value <- coef

##Gather coefficients and p-values
varis <- which(coef[,1] %in% rownames(lm1.s$coefficients))
coef[varis,2] <- lm1.s$coefficients[,"Estimate"]
p.value[varis,2] <- lm1.s$coefficients[,"Pr(>|z|)"]

varis <- which(coef[,1] %in% rownames(lm2.s$coefficients))
coef[varis,3] <- lm2.s$coefficients[,"Estimate"]
p.value[varis,3] <- lm2.s$coefficients[,"Pr(>|z|)"]

for(i in 1:m) {
  eval(parse(text = paste0("varis <- which(coef[,",1,"] %in% rownames(lm3",i,".s$coefficients))")))
  eval(parse(text = paste0("coef[varis,",i+3,"] <- lm3",i,".s$coefficients[,",1,"]")))
  eval(parse(text = paste0("p.value[varis,",i+3,"] <- lm3",i,".s$coefficients[,",4,"]")))
}

varis <- which(coef[,1] %in% rownames(sum.pf))
coef[varis,ncol(coef)] <- sum.pf[,"est"]
p.value[varis,ncol(coef)] <- sum.pf[,"Pr(>|t|)"]

##Visualize coefficients and p-values
rownames(coef) <- coef[,1]
rownames(p.value) <- p.value[,1]
coef <- as.matrix(coef[,-1])
p.value <- as.matrix(p.value[,-1])

p.value[which(p.value == 0)] <- 1
p.value["age",ncol(p.value)] <- 0

heatmap.2(t(p.value), #data
          col = redgreen(256), #color of heatmap
          Colv = NA, #Omit dendrogram of rows
          margin = c(7,10), #Adjustment of space of the heatmap
          cexRow = 1, #Row font size
          cexCol = 1, #Column font size
          trace="none", #Omit value on heatmap
          main="P-value comparison\nby solution for missing values", #Title
          ylab="") #Title of y-variable

for (i in 1:nrow(coef)) {
  B <- barplot(coef[i,], las = 3, ylim = c(if(min(coef[i,])*1.2>0){0} else {min(coef[i,])*1.2},
                                           if(max(coef[i,])*1.2<0){0} else {max(coef[i,])*1.2}),
               main = paste0("Coefficients comparison by solution for missing values\n",rownames(coef)[i]))
  text(B, coef[i,], labels = round(p.value[i,],3))
  Sys.sleep(1)
}
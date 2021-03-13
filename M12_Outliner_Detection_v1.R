####DSL medium class 1 - 2. Outliner Detection####
#Set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160521_M_Data_Cleansing/Data/")

#Data Input
dat <- read.csv("M2_MIC_Population_breakdown_en.csv")
str(dat)

#####1. Package Insall#####
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}
library(corrplot)
if("outliers" %in% rownames(installed.packages()) == FALSE) {install.packages("outliers")}
library(outliers)
if("discretization" %in% rownames(installed.packages()) == FALSE) {install.packages("discretization")}
library(discretization)

#####2. Data Confirmation#####
####Confirm data class and correct data type
sapply(dat, class)
name <- dat[,"Name"]
dat <- dat[,-(1:2)]
str(dat)

####Confirm Correlation
par(ps = 8, mar = c(12, 4, 4, 2))
corrplot(cor(dat))
par(ps = 10, mar = c(5, 4, 4, 2))

####Standardizing the data
dat_s <- scale(dat)

#####3. Conduct PCA analysis#####
#Principal component analysis(PCA)
PC <- princomp(dat_s)
#Confrim cumulative proportion of PCA
summary(PC) #First PC accounts for 85% of proportion of variance

#Confirm structure of the first PC
par(ps = 8, mar = c(12, 4, 4, 2))
barplot(PC$loadings[,"Comp.1"], #First PC loadings
        las = 3) #Direction of labels in x-axis
barplot(PC$loadings[,"Comp.2"], #Second PC loadings
        las = 3) #Direction of labels in x-axis
barplot(PC$loadings[,"Comp.3"], #Second PC loadings
        las = 3) #Direction of labels in x-axis
par(ps = 10, mar = c(5, 4, 4, 2))
str(PC$loadings)

#Calculate first PC values
dat_new <- data.matrix(dat_s) %*% unclass(loadings(PC))

#####4. Outliner Detection#####
hist(dat_new[,"Comp.1"], 
     breaks = c((round(min(dat_new[,"Comp.1"]))-1):(round(max(dat_new[,"Comp.1"]))+1)),
     main = "Distribution of PC1", xlab = "Value of PC1")
dat_new[,"Comp.1"][order(dat_new[,"Comp.1"], decreasing = TRUE)][1:10] #Top10 in 1st component value

#Smirnov-Grubbs test
#type=10:Focus on maximum or minimum value
#type=11:Focus on both maximum and minimum value
#type=20:Focus on maximum and second value
set.seed(123)
res <- grubbs.test(x = dat_new[,"Comp.1"], type = 11, two.sided = TRUE)
res

#Check name of the outlier
out1 <- order(dat_new[,"Comp.1"], decreasing = TRUE)[1]
name[out]
out2 <- order(dat_new[,"Comp.1"], decreasing = FALSE)[1]
name[out2]

#Binnning approach
dat_new[,"Comp.1"] <- discretize(dat_new[,"Comp.1"], method="frequency", categories = 5)
hist(dat_new[,"Comp.1"], breaks = c(0:5))
#You can choose method option as following:
#"interval" (equal interval width)
#"frequency" (equal frequency)
#"cluster" (k-means clustering)
#"fixed" (categories specifies interval boundaries)
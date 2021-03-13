#####2 Banking data analysis by PCA#####
#Set your own folder you put the data in
setwd("c:/work/R/150124/")

####2-1 Package install####
#install.packages("dummies")
library(dummies)
#install.packages("corrplot")
library(corrplot)

####2-2 Data install & preparation####
data <- read.csv("bank.csv")
colnames(data) #2nd column is "campaign_result"
head(data)
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
#duration_time: time to maturity date (day)
#contact_count: count to contact customer (count)
#previous_campaign: success of previous campaign (success:1, failure:2, other:3, unknown:9)

data <- data[,-1] #Delete the 1st column(id)
data <- data[,c(-1,-3,-4,-5,-6,-8,-9,-12)] #Delete the categorical data

#Confrim the data type
for(i in 1:ncol(data)) {
  print(paste(i,colnames(data)[i],class(data[,i])))
}


####2-2 Compression of demention by PCA####
data_s <- scale(data) #Standardization of the data
data_cor <- cor(data)@#Correlation matrix
corrplot(data_cor) #Check correlation

PC <- princomp(data_s)@#Principal component analysis(PCA)
summary(PC) #confrim cumulative proportion of PCA
PC$loadings

#Plot data points in two major component
par(ps=15,mar=c(5,5,3,3))
plot(data.matrix(data_s) %*% unclass(loadings(PC))[,c(1,2)],
      main="PCA of banking data",pch=19,col=rgb(0,0,0,0.05))

#plot data points in 1st and 2nd principal component with default field axes
biplot(PC)

####2-3 Cluster analysis####
#k-means(k=3) on trial
b_fit <- kmeans(data_s, 3)
aggregate(data_s,by=list(b_fit$cluster),FUN=mean)
dat_c <- data.frame(b_fit$cluster)
head(dat_c)

#Calculate Within Sum of Squares(WSS)
wss <- numeric(15)
for (i in 1:length(wss)) {
  wss[i] <- sum(kmeans(data_s, centers=i)$withinss)
}

#Plot WSS
plot(1:15,wss,type="b",main="WSS graph",
     xlab="Number of Clusters",ylab="Within groups sum of squares")

#k-means(k=12)
b_fit <- kmeans(data_s, 5)
aggregate(data_s,by=list(b_fit$cluster),FUN=mean)
dat_c <- data.frame(b_fit$cluster)
head(dat_c)

#Plot data points in two major component
par(ps=15,mar=c(5,5,3,3))
plot(data.matrix(data_s) %*% unclass(loadings(PC))[,c(1,2)],
     main="PCA of banking data",pch=19,col=dat_c$b_fit.cluster)


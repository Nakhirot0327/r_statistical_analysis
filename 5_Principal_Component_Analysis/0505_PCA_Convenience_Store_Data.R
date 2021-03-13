## 1-0 Preparing ##
#GET AND INSTALL PACKAGES
install.packages("rpart")
install.packages("rpart.plot")
install.packages("polycor")
install.packages("ROCR")
install.packages("DAAG")

#SET CURRENT DIRECTORY
setwd("c:/work/R/150124/")
getwd()

## 1-1 READ AND CHECK DATA ##
#READ CSV
data <- read.csv("convenience-store.csv", header=TRUE)
data <- data[,-1]   #DELETE FIRST COLUMN (ID COLUMN)
head(data) #CONFIRM FIRST SIX ROWS
#DATA DESCRIPTION (unit is shown in paratheses)
#ealuation: when this value equals "1", it is a store whose revenue is increased. Otherwise when this value is "0", a store's revenue is decreased
#carSpace: car parking space (cars)
#floorSpace: floor space in store (meter^2)
#storeFrontTraffic: traffic in front of store (persons/day)
#frontage: frontage of store (meter)
dim(data)  #CONFIRM MATRIX SIZE
table(data$evaluation)  #CONFIRM NUMBER OF evaluation ("0" IS BAD STORE, OTHERWISE "1" IS GOOD STORE)

#CONFIRM DATA TYPE
for (i in 1:ncol(data)) { print(c(names(data[i]), class(data[,i]))) }

####2-2 Compression of demention by PCA####
delete <- c(1)
data <- data[,-delete] #Delete redundant columns

data_s <- scale(data) #Standardization of the data
data_cor <- cor(data)@#Correlation matrix
corrplot(data_cor) #Check correlation

PC <- princomp(data_s)@#Principal component analysis(PCA)
summary(PC) #confrim cumulative proportion of PCA
PC$loadings

#Plot data points in two major component
par(ps=15,mar=c(5,5,3,3))
plot(data.matrix(data_s) %*% unclass(loadings(PC))[,c(1,2)],
     main="PCA of convinience  store data",pch=19,col=rgb(0,0,0,0.2))

#plot data points in 1st and 2nd principal component with default field axes  
biplot(PC, main="PCA of convinience  store data")
biplot(PC, choices=c(2,3), main="PCA of convinience  store data with 2nd, 3rd principal component")

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

#k-means(k=6)
b_fit <- kmeans(data_s, 6)
aggregate(data_s,by=list(b_fit$cluster),FUN=mean)
dat_c <- data.frame(b_fit$cluster)
head(dat_c)

#Plot data points in two major component
par(ps=15,mar=c(5,5,3,3))
plot(data.matrix(data_s) %*% unclass(loadings(PC))[,c(1,2)],
     main="PCA of banking data",pch=19,col=dat_c$b_fit.cluster)


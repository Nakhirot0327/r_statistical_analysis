#####3 Vehicle data analysis by SVM#####
#Set your own folder you put the data in
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/141122_DSL_Seminer/")

####3-1 Package install####
library(kernlab) #ksvm

####3-2 Data install & preparation####
#READ CSV
data <- read.csv("vehicle.csv", header=TRUE)
head(data) #CONFIRM FIRST SIX ROWS
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

dim(data)  #CONFIRM MATRIX SIZE
summary(data$Class)  #CONFIRM NUMBER OF EACH CLASS (vehicle$Class IS OBJECTIVE VARIABLE)
head(data)

#CONFIRM DATA TYPE
for (i in 1:ncol(data)) {
  print(c(names(data[i]), class(data[,i])))
}

#Extract the campaign result from the data
y <- data[,1]
#Reject 1st column because the column is redundant
data <- data[,-1]

####3-3 Divide the data into train and test data####
set.seed(50)
trainNO <- sample(nrow(data),round(nrow(data)*0.80,0),replace=FALSE)
#replace=TRUE:Extract sample NO from uniform distribution of without replacement

#Divide the data into train data and test data
train_dat <- data[trainNO,]
test_dat <- data[-trainNO,]

train.x <- scale(as.matrix(train_dat))
train.y <- y[trainNO]
test.x <- scale(as.matrix(test_dat))
test.y <- y[-trainNO]

####3-4 Choose the parameters cost penalty C and standard deviation s####
C <- c(0.01,0.03,0.1,0.3,1,3,10,30)*100
s <- c(0.01,0.03,0.1,0.3,1,3,10,30)/10
cm <- NULL
accuracy <- matrix(0,ncol=length(C),nrow=length(s))
bus <- matrix(0,ncol=length(C),nrow=length(s))
opel <- matrix(0,ncol=length(C),nrow=length(s))
saab <- matrix(0,ncol=length(C),nrow=length(s))
van <- matrix(0,ncol=length(C),nrow=length(s))
k <- 1

for(i in 1:length(C)) {
  for(j in 1:length(s)){
    rbfsvm <- ksvm(train.x,train.y,type="C-svc",
                   kernel="rbfdot",scale="FALSE",
                   C=C[i],kpar=list(sigma=s[j]))
    predicted <- predict(rbfsvm,test.x,type="response")
    cm[[k]] <- table(test.y, predicted)
    accuracy[i,j] <- sum(diag(cm[[k]]))/sum(cm[[k]]) #Accuracy
    bus[i,j] <- cm[[k]][1,1]/sum(cm[[k]][1,]) #Accracy in prediction of bus
    opel[i,j] <- cm[[k]][2,2]/sum(cm[[k]][2,]) #Accracy in prediction of opel
    saab[i,j] <- cm[[k]][3,3]/sum(cm[[k]][3,]) #Accracy in prediction of saab
    van[i,j] <- cm[[k]][4,4]/sum(cm[[k]][4,]) #Accracy in prediction of van
    print(paste("C =",C[i],"sigma =",s[j]))
    print(paste("Accuracy of the model is",round(accuracy[i,j],2)))
    k <- k + 1
  }
}

#Check accuracy by heatmap
colnames(accuracy) <- s
rownames(accuracy) <- C
colnames(bus) <- s
rownames(bus) <- C
colnames(opel) <- s
rownames(opel) <- C
colnames(saab) <- s
rownames(saab) <- C
colnames(van) <- s
rownames(van) <- C

library(gplots)
heatmap.2(accuracy,col = redgreen(256),dendrogram="none",Rowv = NA,Colv = NA,
          xlab="sigma",ylab="Cost parameter C",main="Accuracy")
op.row <- which(accuracy==max(accuracy),arr.ind=TRUE)[1]
op.col <- which(accuracy==max(accuracy),arr.ind=TRUE)[2]
op.C <- as.numeric(rownames(accuracy)[op.row])
op.sigma <- as.numeric(colnames(accuracy)[op.col])
#Choose C=100, sigma=0.01 to maximize accuracy

heatmap.2(bus,col = redgreen(256),dendrogram="none",Rowv = NA,Colv = NA,
          xlab="sigma",ylab="Cost parameter C",main="Accuracy of prediction of bus")
heatmap.2(opel,col = redgreen(256),dendrogram="none",Rowv = NA,Colv = NA,
          xlab="sigma",ylab="Cost parameter C",main="Accuracy of prediction of opel")
heatmap.2(saab,col = redgreen(256),dendrogram="none",Rowv = NA,Colv = NA,
          xlab="sigma",ylab="Cost parameter C",main="Accuracy of prediction of saab")
heatmap.2(van,col = redgreen(256),dendrogram="none",Rowv = NA,Colv = NA,
          xlab="sigma",ylab="Cost parameter C",main="Accuracy of prediction of van")

####3-5 Set C=100 and sigma =0.01 and conduct SVM analysis####
rbfsvm <- ksvm(train.x,train.y, #Data
               type="C-svc",    #C-svc
               kernel="rbfdot", #Kernel function(rbfdot means Gaussian Kernel)
               scale="FALSE",   #When data is sparse set "scale" FALSE
               C=op.C,           #Cost parameter
               kpar=list(sigma=op.sigma)) #Standard deviation of Kernel

###Accuracy analysis###
#(A) Prediction based on test data
predicted <- predict(rbfsvm,test.x,type="response")

#(B) Mixing matrix
(cm <- table(test.y, predicted))

#Calucaltion of accracy of the model
(accuracyTraining <- sum(diag(cm))/sum(cm)) #Accuracy

## (E) R-squared
library(polycor)
predicted.p_class <- predict(rbfsvm, newdata=test.x, type="response")

#Calculate correlation coefficient among categorical variables by using polychor function
R <- polychor(predicted.p_class, test.y, ML=TRUE)
#ML=maximally likelyfood
R_sq <- R^2 #CALCULATE R-SQUARED
sprintf("R: %4.3f, R^2: %4.3f", R, R_sq)  #PRINT R AND R-SQUARED
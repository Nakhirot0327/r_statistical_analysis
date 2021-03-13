#Homework
#Design of Experiment -Apartment Selling-

#Practice 2-1
#SET CURRENT DIRECTORY
#setwd("/work/R/140524")
#getwd()

#INSTALL CONJOINT PACKAGE
install.packages("conjoint")
library(conjoint)

#SET APARTMENT FACTORS
apartmentFactors <- expand.grid(
    layout = c("1LDK", "2LDK", "3K"),
    floor = c("low", "high"),
    direction = c("south", "east", "west"),
    wayo = c("wa", "yo")
)

#CREATE ORTHOGONAL DESIGN
design_ort <- caFactorialDesign(data=apartmentFactors, type="orthogonal")
design_ort

#TRANSFER LEVELS TO NUMBERS
edo <- caEncodedDesign(design_ort)
edo

#CONFIRM ORTHOGONALITY
cor(edo)

#CREATE ORTHOGONAL DESIGN TABLE (FOR HARD CODING) 
A <- paste("A", c(2,3,1,3,1,2,1,2,3), sep="")
B <- paste("B", c(2,2,1,1,2,1,2,2,2), sep="")
C <- paste("C", c(1,2,3,1,1,2,2,3,3), sep="")
D <- paste("D", c(1,1,1,2,2,2,2,2,2), sep="")

#INPUT POINTS IN TABLE
V <- c(117,67,49,92,79,100,65,84,71)

#RUN ANOVA AND SHOW RESULT
data <- data.frame(A=A, B=B, C=C, D=D, V=V)
result <- summary(aov(V~A+B+C+D, data=data))
result

#CONFIRM USING BOXPLOT
boxplot(V~A, names=c("1LDK", "2LDK", "3K"), xlab="layout", ylab="point")
boxplot(V~B, names=c("low", "high"), xlab="floor", ylab="point")
boxplot(V~C, names=c("south", "east", "west"), xlab="direction", ylab="point")
boxplot(V~D, names=c("wa", "yo"), xlab="wayo", ylab="point")

######2.ŽÀŒ±Œv‰æ–@######
#Practice 1
#Design of Experiment ?Credit Card for Developing Country?

#SET CURRENT DIRECTORY
#setwd("/Users/Hirotoshi/Documents/100_OTL/110_DSL/140524_DSL_Seminer/")

#INSTALL CONJOINT PACKAGE
install.packages("conjoint")
install.packages("AlgDesign")
library(conjoint)

#SET CARD FACTORS
cardFactors <- expand.grid(
  type = c("partner", "original"),
  point = c("low", "mid", "high"),
  brand = c("amex", "other"),
  annual_fee = c("expense", "free")
)

#CREATE ORTHOGONAL DESIGN
design_ort <- caFactorialDesign(data=cardFactors, type="orthogonal")
design_ort
#TRANSFER LEVELS TO NUMBERS
edo <- caEncodedDesign(design_ort)
edo

#CONFIRM
cor(edo)

A <- paste("A", edo[,1], sep="")
B <- paste("B", edo[,2], sep="")
C <- paste("C", edo[,3], sep="")
D <- paste("D", edo[,4], sep="")

V <- c(68,90,60,83,69,26,95,51)

data <- data.frame(A=A, B=B, C=C, D=D, V=V)
result <- summary(aov(V~A+B+C+D, data=data))
result


## 3 Statistic Test - Practice 12 Experiment Design##
## Edited by DSL https://www.facebook.com/dsl.statclass/ ##

#Firstly set your working directory
setwd("C:/Users/Hirotoshi/Documents/100_OTL/110_DSL/DSL2016/160723_B_Statistics_Test/Data/")

####0. Package Install####
if("conjoint" %in% rownames(installed.packages()) == FALSE) {install.packages("conjoint")}
library(conjoint)
if("AlgDesign" %in% rownames(installed.packages()) == FALSE) {install.packages("AlgDesign")}
library(AlgDesign)

####1. Data Preparation####
#Create list of partners
cardFactors <- expand.grid(
  type = c("partner", "original"),
  point = c("low", "mid", "high"),
  brand = c("amex", "other"),
  annual_fee = c("expense", "free")
)
cardFactors

#Create orthogonal table
design_ort <- caFactorialDesign(data=cardFactors, type="orthogonal")
design_ort

#Create dataframe for anova
V <- c(68,90,60,83,69,26,95,51)
dat <- cbind(design_ort,V)

####2. Four-way Anova####
result <- aov(V~., data=dat)
str(result)
summary(result)
#A = Partner
#B = Point rate
#C = Card Brand name
#D = Annual charge

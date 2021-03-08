## Practice1 ##

## Preparing ##
#GET AND INSTALL PACKAGES

#SET CURRENT DIRECTORY
setwd("C:/work/R/140823")
getwd()


##
usair<-source("c:/work/R/140823/chap3usair.dat")$value
attach(usair)

head(usair)

##DATA  DESCRIPTION
#-------------------------------------------------------------
#SO2      大気中の二酸化硫黄の含有量(μg/m^3)
#Temp     年間平均気温
#Manuf    20人以上を雇用する製造業者の数
#Pop      住民数(1000人)
#Wind     年間平均風速
#Precip   年間平均降水量
#Days     降水のあった日数の年間平均

#
pairs(usair)
#CHECK CORRELATION
cor(usair)

#
names <- abbreviate(row.names(usair))
plot(SO2,  Manuf, pch=1, lwd=2, type="n",
     xlim=c(min(SO2), max(SO2)), ylim=c(min(Manuf), max(Manuf)))
text(SO2, Manuf, labels=names, lwd="2")


## linear regression ## 
## 
lm_fit <- lm(SO2 ~ Neg.Temp + Manuf + Pop + Wind + Precip + Days)
summary(lm_fit)

##
lm_aic <- step(lm_fit)
summary(lm_aic)

#
par(fig=c(0, 0.75, 0, 0.75))
plot(SO2,  Manuf, pch=1, lwd=2,
     xlim=c(min(SO2), max(SO2)), ylim=c(min(Manuf), max(Manuf)))
#symbols(SO2, Manuf, circles=Pop, inches=0.4,
#        add = TRUE, lwd=2)
abline(lm(Manuf~SO2), lwd=2)
#
par(fig=c(0, 0.75, 0.65, 1), new=TRUE)
hist(SO2, lwd=1)
#
par(fig=c(0.7, 1, 0, 0.75), new=TRUE)
boxplot(Manuf, lwd=2)



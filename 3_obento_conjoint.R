## PRACTICE 3 ###
## 3_0 PREPARING ##
# INSTALL PACKAGE
install.packages("conjoint")
library(conjoint)

#SET CURRENT DIRECTORY
setwd("/work/R/140823")
getwd()


## 3_1 READ AND CHECK DATA ## 
#READ DATA (USER EVALUATION OF OBENTO)
dat <- read.csv("obento_data.csv", header=TRUE)
head(dat)
dat_all <- dat[,c(-1:-4)]   #DELETE 1ST-4TH COLUMN (ID, Sex, Age, Frequency)
head(dat_all)

#READ L18 TABLE
L18 <- read.csv("L18_table.csv", header=TRUE)
#L18 <- read.csv("L18.csv", header=TRUE)
head(L18)
cor(L18)    #CONFIRM ORTHOGONALITY

#READ OBENTO INFORMATION
attr <- read.csv("levels.csv", header=TRUE)
head(attr)

#FACTOR: (LEVEL)
#Staple: rice(1), noodle(2)
#Main Dish: beef(4), fish(5), chichen(6)
#Side Dish: boiled egg(7), shrimp(8), sausage(9)
#Vegetable: onion(10), carrot(11), potato(12)
#Vegetable Size: large(13), regular(14), small(15)
#Garnish: cheese(16), corn(17), olive(18)
#Rice Size: small(19), regular(20), large(21)
#Price: yen350(22), yen450(23), yen550(24)


## 3_2 CONJOINT ANALYSIS (ALL DATA) ##
#CONJOINT ANALYSIS
#par(mfrow=c(3,3), mar=c(5,3,4,3), ps=12)   #UNCOMMENT TO DISPLAY GRAPHS AT A GLANCE
par(ps=12)  #FONT SIZE
cj <- Conjoint(dat_all, L18, attr)    #CONJOINT ANALYSIS
title(main="Importance of Factors (all data)")
par(mfrow=c(1,1), mar=c(5,3,3,3)) 

#CONFIMR PREFERABLE COMBINATION
#Staple: rice(1)
#Main Dish: beef(4)
#Side Dish: boiled egg(7)
#Vegetable: carrot(11)
#Vegetable Size: large(13)
#Garnish: cheese(16)
#Rice Size: regular(20)
#Price: yen350(22)

#CALCULATE PREDICTIVE MAX POINT (PREFERABLE FACTOR COMBINATION)
#y = b + a1x1 + a2x2 + a3x3 + a4x4 + ... 
b = 5.1405
a = c(0.0752, 0.2908, 0.281, 0.3203, 0.4477, 0.1536, 0.3301, 1.4575)
(max_point = b + sum(a))

## 3_3 CONJOINT ANALYSIS (SEGMENTATION) ##
#FACTOR = Sex
sexs = c("Male", "Female")
for (i in 1:max(dat$Sex)) {
    dat_p <- subset(dat, Sex==i)
    if (nrow(dat_p) < 20) next  #IF NUMBER OF ROWS  IS UNDER 20, GO NEXT LOOP
    dat_p <- dat_p[,c(-1:-4)] #DELETE 1ST-4TH COLUMN (ID, Sex, Age, Frequency)
    
    par(mfrow=c(3,3), mar=c(4,3,5,3), ps=12)
    Conjoint(dat_p, L18, attr)  #CONJOINT ANALYSIS
    
    titleName = paste("Importance of Factors (", sexs[i], ")")
    title(main=titleName)
}

#FACTOR = Age
ages = c("Teenager", "Twenties", "Thirties", "Forties", "Fifties", "Sixties")
for (i in 1:max(dat$Age)) {
    dat_p <- subset(dat, Age==i)
    if (nrow(dat_p) < 20) next  #IF NUMBER OF ROWS  IS UNDER 20, GO NEXT LOOP
    dat_p <- dat_p[,c(-1:-4)] #DELETE 1ST-4TH COLUMN (ID, Sex, Age, Frequency)
    
    par(mfrow=c(3,3), mar=c(4,3,5,3), ps=12)
    Conjoint(dat_p, L18, attr)  #CONJOINT ANALYSIS
    
    titleName = paste("Importance of Factors (", ages[i], ")")
    title(main=titleName)
}

#FACTOR = Frequency
freqs = c("everyday", "4?5 in a week", "2?3 in a week", "1 in a week", "2?3 in a month",
          "1 in a month", "under 1 in a month")
for (i in 1:max(dat$Frequency)) {
    dat_p <- subset(dat, Frequency==i)
    if (nrow(dat_p) < 20) next  #IF NUMBER OF ROWS  IS UNDER 20, GO NEXT LOOP
    dat_p <- dat_p[,c(-1:-4)] #DELETE 1ST-4TH COLUMN (ID, Sex, Age, Frequency)
    
    par(mfrow=c(3,3), mar=c(4,3,5,3), ps=12)
    Conjoint(dat_p, L18, attr)  #CONJOINT ANALYSIS
    
    titleName = paste("Importance of Factors (", freqs[i], ")")
    title(main=titleName)
}


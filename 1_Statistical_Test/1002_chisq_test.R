#####Chisq-test#####
#set working directory
#setwd("your folder")

#read data
rikon <- read.csv("Rikon.csv",row.names=1)
rikon

#calculate expected number of divorces
couple <- sum(rikon[,2]) #total number of divorces
couple_prc <- rikon[,2]/couple #proportion of each prefecture's number of couples
ex_div <- sum(rikon[,1])*couple_prc #expected number of divorces

#chisquare-test
chisq.test(x=rikon[,1],p=ex_div,rescale.p=TRUE)
chisq.test(x=rikon[,1],p=couple_prc)

#visualize the result
div_per100 <- rikon[,1]/rikon[,2]*100
barplot(sort(div_per100),name=row.names(rikon),axes=TRUE,las=2,
        main="number of divorces per 100 couples")

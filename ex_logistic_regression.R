##Input data
math <- c(60,80,50,60,20,40)
eng <- c(70,20,60,40,80,70)
y <- c(1,1,0,0,0,1)

dat <- cbind(math,eng,y)
dat <- as.data.frame(dat)

dat
##Logistic regression
glm_a <- glm(y~., data=dat, family=binomial)

##Confirmation of the result
summary(glm_a) #print the result
attributes(glm_a) #confrim of components of the result

glm_a$coefficients  #Coefficients of linear regression of z
glm_a$linear.predictors #Value of z
glm_a$fitted.values #Pass rate of each person

##Plot the result of the analysis
##Make sigmoid function of the result
xmin <- -max(abs(z),max(z))
modelz <- seq(round(xmin,2),round(-xmin,2),by=0.01)
modely <- exp(modelz)/(exp(modelz)+1)

##Plot the result of the analysis
par(ps=20) #change the font size
plot(modelz,modely,xlim=c(xmin,-xmin),axes=F,ann=F,type="l",col=2)
par(new=T)
plot(z,glm_a$fitted.values,xlim=c(xmin,-xmin),axes=F,ann=F,pch=19,col=2,cex=2)
par(new=T)
plot(z,y,pch=19,col=5,cex=2,xlim=c(xmin,-xmin),ylab="Pass rate",
     xlab=paste("z =",round(glm_a$coefficients[1],2),"+ ",
                round(glm_a$coefficients[2],2),"* math +",
                round(glm_a$coefficients[3],2),"* eng"),
     main="Logistic Regression")
legend("bottomright",legend=c("Acutal value","Predictive value"),
       pch=19,col=c(5,2),bty="n",cex=1)
par(ps=10) #change the font size
text(x=glm_a$linear.predictors,y-0.01,
     labels=paste("Math =",dat[,"math"],"\nEng =",dat[,"eng"]))
arrows(x0=glm_a$linear.predictors,y0=y,
       x1=glm_a$linear.predictors,y1=glm_a$fitted.values,lty=2)
arrows(x0=glm_a$linear.predictors,y0=glm_a$fitted.values,
       x1=xmin-1,y1=glm_a$fitted.values,lty=2)
text(x=xmin,y=glm_a$fitted.values+0.03,
     labels=paste(round(glm_a$fitted.values,2)*100,"%"))
par(ps=15) #change the font size
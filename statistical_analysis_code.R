#Preparing Data
df_GoogL=read.csv('C:/Users/sande/Desktop/ST501/GOOGL.csv')
df_AmzN=read.csv('C:/Users/sande/Desktop/ST501/AMZN.csv')
df_GoogL=data.frame(df_GoogL$Date,df_GoogL$Adj.Close)
df_AmzN=data.frame(df_AmzN$Date,df_AmzN$Adj.Close)
df_GoogL<- setNames(df_GoogL, c('Date','Price'))
df_AmzN<- setNames(df_AmzN, c('Date','Price'))
df_GoogL$LR<-c(0,diff(log(df_GoogL$Price)))
df_AmzN$LR<-c(0,diff(log(df_AmzN$Price)))

#Data Visualization and Descriptive Statistics Question 1 (b) and 2(b)
#install.packages("fitdistrplus")
library(fitdistrplus)
plotdist(df_GoogL$LR[2:251],histo = TRUE,demp = TRUE)
plotdist(df_AmzN$LR[2:251],histo = TRUE,demp = TRUE)
descdist(df_GoogL$LR[2:251])
descdist(df_AmzN$LR[2:251])

#Question 1: GoogL data  fitting and Expected Shortfall calculation
#install.packages("SuppDists")
#install.packages("goftest")
library(SuppDists)
library(goftest)
#Expected Shortfall for GOOGL is -4.22%
quantile(df_GoogL$LR[2:251],probs = 0.05)
GoogL_ES=mean(subset(df_GoogL$LR[2:251],df_GoogL$LR[2:251]<quantile(df_GoogL$LR[2:251],probs = 0.05)))
# Given the negative skweness and kurtosis of 4.53 we tried to fit a logistic distribution
Fit_model_GoogL<-fitdist(df_GoogL$LR[2:251],"logis")
summary(Fit_model_GoogL)

#We adjusted the location and scale parameters slightly to check for an increase in fit.
mew=Fit_model_GoogL$estimate[1]
sca=Fit_model_GoogL$estimate[2]-0.5*Fit_model_GoogL$sd[2]
# Ad test using computed estimates of Location and Scale pvalue = 0.4183
ad.test(df_GoogL$LR[2:251],null="plogis",location=mew,scale=sca)

#Solving for c using qlogis and estimate ES using dlogis. Expected Shortfall from Logistic = -3.277%
c=qlogis(0.05,location = mew,scale = sca)
xfx=function(x){x*dlogis(x,location = mew,scale = sca)}
GoogL_Logis_ES_Estimate=integrate(xfx,lower = -Inf,upper = c)
GoogL_Logis_ES_Estimate$value*20

# Given the negative skweness and kurtosis of 4.53 we tried to fit a Johnson distribution (method='quant')
Fit_model_GoogL2<-JohnsonFit(df_GoogL$LR[2:251])
Fit_model_GoogL2

# Ad test using computed estimates of JohnsonFit pvalue= 0.4139
ad.test(df_GoogL$LR[2:251],null="pJohnson",parms=Fit_model_GoogL2)

#Solving for c using qJohnson and estimate ES using dJohnson.Expected Shortfall from SU = -3.787% 
c=qJohnson(0.05,parms=Fit_model_GoogL2)
xfx=function(x){x*dJohnson(x,parms = Fit_model_GoogL2)}
GoogL_SU_ES_Estimate=integrate(xfx,lower = -Inf,upper = c)
GoogL_SU_ES_Estimate$value*20

#Comparing distribution fit on data
hist(df_GoogL$LR[2:251],probability = TRUE,breaks = 25,xlab = "GOOGL Log Returns",main = "GOOGL Distribution Fit")
curve(dJohnson(x,parms = Fit_model_GoogL2),from = min(df_GoogL$LR[2:251]),to=max(df_GoogL$LR[2:251]),col="green",add = TRUE,type = 'b',pch=1,lwd=2)
curve(dlogis(x,location = Fit_model_GoogL$estimate[1],scale = Fit_model_GoogL$estimate[2]-0.5*Fit_model_GoogL$sd[2]),from = min(df_GoogL$LR[2:251]),to=max(df_GoogL$LR[2:251]),add = TRUE,type='b',pch=2,lwd=1)
legend( x="topleft", legend=c("Johnson","Logistic"),col=c("green","black"), lwd=1, pch=c(1,2), bty='n' )

#Question 2: AMZN data  fitting and Expected Shortfall calculation
#install.packages("SuppDists")
#install.packages("goftest")
library(SuppDists)
library(goftest)
#Expected Shortfall for AMZN is -4.896%
quantile(df_AmzN$LR[2:251],probs = 0.05)
AmzN_ES=mean(subset(df_AmzN$LR[2:251],df_AmzN$LR[2:251]<quantile(df_AmzN$LR[2:251],probs = 0.05)))
# Given the negative skweness and kurtosis of 5.98 we tried to fit a logistic distribution
Fit_model_AmzN<-fitdist(df_AmzN$LR[2:251],"logis")
summary(Fit_model_AmzN)

#We adjusted the location and scale parameters slightly to check for an increase in fit.
mew=Fit_model_AmzN$estimate[1]
sca=Fit_model_AmzN$estimate[2]-1.2*Fit_model_AmzN$sd[2]

# Ad test using computed estimates of Location and Scale pvalue = 0.2743
ad.test(df_AmzN$LR[2:251],null="plogis",location=mew,scale=sca)

#Solving for c using qlogis and estimate ES using dlogis. Expected Shortfall from Logistic = -3.428%
c=qlogis(0.05,location = mew,scale = sca)
xfx=function(x){x*dlogis(x,location = mew,scale = sca)}
AmzN_Logis_ES_Estimate=integrate(xfx,lower = -Inf,upper = c)
AmzN_Logis_ES_Estimate$value*20

# Given the negative skweness and kurtosis of 5.98 we tried to fit a Johnson distribution (method='quant')
Fit_model_AmzN2<-JohnsonFit(df_AmzN$LR[2:251])
Fit_model_AmzN2
# Ad test using computed estimates of JohnsonFit pvalue= 0.992
ad.test(df_AmzN$LR[2:251],null="pJohnson",parms=Fit_model_AmzN2)

#Solving for c using qJohnson and estimate ES using dJohnson.Expected Shortfall from SU = -6.072% 
c=qJohnson(0.05,parms=Fit_model_AmzN2)
xfx=function(x){x*dJohnson(x,parms = Fit_model_AmzN2)}
AmzN_SU_ES_Estimate=integrate(xfx,lower = -Inf,upper = c)
AmzN_SU_ES_Estimate$value*20

#Comparing Distribution fit on AMZN data
hist(df_AmzN$LR[2:251],probability = TRUE,breaks = 25,xlab = "AMZN Log Returns",main = "AMZN Distribution Fit")
curve(dlogis(x,location = Fit_model_AmzN$estimate[1],scale = Fit_model_AmzN$estimate[2]-1.2*Fit_model_AmzN$sd[2]),from = min(df_AmzN$LR[2:251]),to=max(df_AmzN$LR[2:251]),add = TRUE,pch=2,type='b')
curve(dJohnson(x,parms = Fit_model_AmzN2),from = min(df_AmzN$LR[2:251]),to=max(df_AmzN$LR[2:251]),add = TRUE,lwd=2,col='green',type='b',pch=1)
legend( x="topleft", legend=c("Johnson","Logistic"),col=c("green","black"), lwd=1, pch=c(1,2), bty='n')

#Question3 : Portfolio Construction and Expected Shortfall Calculation
#Preparing Portfolio Prices
w=seq(from =0 ,to=1,by=0.02)
l=c("GooG_Price","AmzN_Price",as.character(w))
df_PorT=data.frame(matrix(NA,nrow = 251))
df_PorT=cbind(df_PorT,df_GoogL$Price,df_AmzN$Price)
for (x in w) {
  df_PorT=cbind(df_PorT,x*df_GoogL$Price+(1-x)*df_AmzN$Price)
}
df_PorT$matrix.NA..nrow...251.<-NULL
colnames(df_PorT)<-l

#Calculating Portfolio Log Returns
df_PorT_Log=data.frame(matrix(NA, nrow = 250, ncol = 0))
for (i in seq(1,53)) {
  dummyLR=diff(log(df_PorT[[i]]))
  df_PorT_Log<-cbind(df_PorT_Log,dummyLR)
}
l=c("GooG_LR","AmzN_LR",as.character(w))
colnames(df_PorT_Log)<-l

#Calculating Expected Shortfall for each value of w
ES=c()
Q=c()
for (i in seq(3,53)) {
  Q=c(Q,quantile(df_PorT_Log[[i]],probs = 0.05))
  ES=c(ES,mean(subset(df_PorT_Log[[i]],df_PorT_Log[[i]]<quantile(df_PorT_Log[[i]],probs = 0.05))))
}
#Portfolio 0.05 Quantile versus w
plot(w,Q,ylab = "Portfolio 0.05 Quantile",xlab = "GOOGL weightage in each Portfolio",main = "Q0.05 vs w")
grid(nx = 54, ny = 54, col = "lightgray", lty = "dotted")
#Plotting Expected Shortfall of Portfolio versus w  
plot(w,ES,ylab = "Portfolio Expected Shortfall",xlab = "GOOGL weightage in each Portfolio",main = "ES vs w")
grid(nx = 54, ny = 54, col = "lightgray", lty = "dotted")

# Minimum and maximum Expected Shortfall and corresponding w
ES_Min=min(ES)
w[which.min(ES)]
ES_Max=max(ES)
w[which.max(ES)]




f<-read.csv(choose.files())
attach(f)
summary(f)
class(f$State)
sum(is.na(f))
#checking correlation matrix without states, because cor() accept only numeric data
cor(f[-4])

#boxplot(Administration)$out
#boxplot(R.D.Spend)$out
#boxplot(Marketing.Spend)$out

pairs(f)    #plotting pairs of graph with each other
#install.packages("corpcor")
library(corpcor)  
cor2pcor(cor(f[-4]))    #gives pure correlation matrix with each other

#cor(Profit,Administration)

model1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = f)
summary(model1)

#checking different combinations for checking their influence
model_with_admin<-lm(Profit~Administration,data = f)
summary(model_with_admin)
model_with_marketing<-lm(Profit~Marketing.Spend,data = f)
summary(model_with_marketing)
model_with_markandadmin<-lm(Profit~Marketing.Spend+Administration,data = f)
summary(model_with_markandadmin)
model_with_RDandadmin<-lm(Profit~R.D.Spend+Administration,data = f)
summary(model_with_RDandadmin)
model_with_markandRD<-lm(Profit~Marketing.Spend+R.D.Spend,data = f)
summary(model_with_markandRD)


#checking variance inflation factor and plotting added variable plot for checking correlation between salary and other variables
library(car)
vif(model)
avPlots(model)


model_with_admin<-lm(Profit~Administration,data = f[1:48,1:5])
summary(model_with_admin)
model_with_marketing<-lm(Profit~Marketing.Spend,data = f[1:48,1:5])
summary(model_with_marketing)
model_with_markandadmin<-lm(Profit~Marketing.Spend+Administration,data = f[1:48,1:5])
summary(model_with_markandadmin)
model_with_RDandadmin<-lm(Profit~R.D.Spend+Administration,data = f[1:48,1:5])
summary(model_with_RDandadmin)
model_with_markandRD<-lm(Profit~Marketing.Spend+R.D.Spend,data = f[1:48,1:5])
summary(model_with_markandRD)


#finding influencing data points
influence.measures(model1)
influenceIndexPlot(model1)
influencePlot(model1)

#final model without 49th and 50th row, it gives the best R SQUARED value 
final_model<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = f[1:48,1:5])
summary(final_model)
predicted<-predict(final_model)
RMSE<-sqrt(sum(final_model$residuals^2)/50)
plot(final_model)
#hist(residuals(model))

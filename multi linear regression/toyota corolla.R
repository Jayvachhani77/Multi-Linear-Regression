corolla<-read.csv(choose.files())
corollaData<-corolla[1:1436,3:38]
attach(corollaData)

class(corollaData$Fuel_Type)
class(Color)
unique(Fuel_Type)
unique(Color)
corollaData$Fuel_Type<-as.character(corollaData$Fuel_Type)
corollaData$Fuel_Type<-ifelse(corollaData$Fuel_Type=="Diesel",0,ifelse(corollaData$Fuel_Type=="Petrol",1,2))
corollaData$Fuel_Type<-as.integer(corollaData$Fuel_Type)
corollaData<-corollaData[1:1436,-c(3,4)]
sum(ifelse(corollaData$Cylinders!=4,1,0))
corollaData<-corollaData[1:1436,-c(11)]
corollaData<-corollaData[1:1436,-c(32)]

str(corollaData)
sum(is.na(corollaData))

#pairs(corollaData[-7])
cor(corollaData[-7])


model0<-lm(Price~.,data = corollaData)
summary(model0)

library(car)
vif(model0)
#avPlots(model0)

influencePlot(model0)
influence.measures(model0)
influenceIndexPlot(model0)

model1<-lm(Price~.,data = corollaData[-c(81),1:32])
summary(model1)
plot(model1)

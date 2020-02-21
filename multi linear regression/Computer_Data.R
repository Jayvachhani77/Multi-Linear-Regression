computer<-read.csv(choose.files())
str(computer)
attach(computer)
computer$cd<-as.character(computer$cd)
computer$multi<-as.character(computer$multi)
computer$premium<-as.character(computer$premium)
computer$cd<-ifelse(computer$cd=="yes",1,0)
computer$multi<-ifelse(computer$multi=="yes",1,0)
computer$premium<-ifelse(computer$premium=="yes",1,0)
computer<-computer[,2:11]
computer$cd<-as.integer(computer$cd)
computer$multi<-as.integer(computer$multi)
computer$premium<-as.integer(computer$premium)
str(computer)
sum(is.na(computer))

cor(computer)
modelc0<-lm(computer$price~.,data = computer)
summary(modelc0)

vif(modelc0)


influencePlot(modelc0)
influenceIndexPlot(modelc0)
#modelc1<-lm(sqrt(price)~.,data = computer[-c(1441,1701),])
#summary(modelc1)



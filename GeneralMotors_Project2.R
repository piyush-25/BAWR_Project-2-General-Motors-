## Project-2  ##
## General Motors DataSet ##

#Data Importing and Summary
gmds=read.csv("data.csv")
View(gmds)
summary(gmds)
str(gmds)

#Intial Modeling to check relations
mod1=lm(Price~.,gmds)
summary(mod1)

# Changing Categorical variables To factors
gmds$Cylinder=as.factor(gmds$Cylinder)
gmds$Cruise=as.factor(gmds$Cruise)
gmds$Sound=as.factor(gmds$Sound)
gmds$Leather=as.factor(gmds$Leather)

#Making Dummy variables for Make Column
gmds$IsChevrolet=ifelse(gmds$Make=="Chevrolet",1,0)
gmds$IsCadillac=ifelse(gmds$Make=="Cadillac",1,0)

#Making Dummy variables For Cylinder Column
gmds$Is4stroke=ifelse(gmds$Cylinder==4,1,0)
gmds$Is6stroke=ifelse(gmds$Cylinder==6,1,0)

#Data After Exploration
gmds.new=gmds[,c(-3,-4)]
View(gmds.new)

#Model Accuracy Check
mod2=lm(Price~.,gmds.new)
summary(mod2)
gmds.fitted=data.frame(gmds.new,fitted=fitted(mod2),residual=resid(mod2))
View(gmds.fitted)

#Spliting Of data in train and test dataset
set.seed(12)
split=sample(nrow(gmds),nrow(gmds)*0.7)
gm.train=gmds.new[split,]
gm.test=gmds.new[-split,]

#Model Using training set
model=lm(Price~.,gm.train)
summary(model)

#Final Prediction of price On Test Dataset
pred=predict(model,gm.test)
gmpred=data.frame(gm.test,"Price"=pred,"Difference"=gm.test$Price-pred)

# Predicted Prices 
gm.final=gmpred[,c(11,2:9)]
View(gm.final)


traindataset=read.csv("train.csv",stringsAsFactors = FALSE, header=TRUE)
traindataset

tail(traindataset)
testDataset=read.csv("test.csv",stringsAsFactors =FALSE,header=T)
tail(testDataset)

#NEW VARIABLE
traindataset$IsTrainSet<-T
testDataset$IsTrainSet<-F
testDataset$Survived <- NA
ncol(testDataset)
ncol(traindataset)


#COMBINE WHOLE DATA
TotalDataset= rbind(traindataset,testDataset)
head(TotalDataset)


#FILL EMPTY 
table(TotalDataset$Embarked)
TotalDataset[TotalDataset$Embarked=='',"Embarked"]<-'S'
table(TotalDataset$Embarked)
ncol(TotalDataset)

## fill empty with MEDIANS

###  AGE medians
age.median=median(TotalDataset$Age,na.rm = T)
age.median
TotalDataset[is.na(TotalDataset$Age),"Age"]<-age.median
table(is.na(TotalDataset$Age))
table(is.na(traindataset$Age))

#Fare
table(is.na(TotalDataset$Fare))
fare.median=median(TotalDataset$Fare,na.rm = T)
fare.median
TotalDataset[is.na(TotalDataset$Fare),"Fare"] <-fare.median
table(is.na(TotalDataset$Fare))


##convert to factor
TotalDataset$Pclass<-as.factor(TotalDataset$Pclass)
TotalDataset$Sex<-as.factor(TotalDataset$Sex)
TotalDataset$Embarked<-as.factor(TotalDataset$Embarked)
##



##train and test dataset
traindataset<-TotalDataset[TotalDataset$IsTrainSet==T,]
testDataset<-TotalDataset[TotalDataset$IsTrainSet==F,]

##convert trainig dataset to binary
traindataset$Survived<-as.factor(traindataset$Survived)
names(traindataset)

##formula
formula=as.formula("Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked ")

#make model for prediction
model=randomForest(formula,data=traindataset)

##Predicted values
Predicted<-predict(model,newdata = testDataset)

##output
PassengerID<-testDataset$PassengerId
output<-as.data.frame(PassengerID)
output$Survived<-Predicted

##output to csv file
write=write.csv(output,file="Titanic.csv",row.names = FALSE)


save.image("titanic.Rdata")

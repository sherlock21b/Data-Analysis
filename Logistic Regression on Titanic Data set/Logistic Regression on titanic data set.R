library(readr)
titanic <- read_csv("C:/Users/Admin/Documents/Logistic Regression on Titanic Data set/train.csv")
#lets clean the data first so that there are na values
colSums(is.na(titanic))
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age,na.rm=TRUE)
titanic$Embarked[is.na(titanic$Embarked)]="C"
titanic$Cabin[is.na(titanic$Cabin)]=""
#it is needed to change some values into factors
str(titanic)
apply(titanic,2, function(x) length(unique(x)))
titanic$Survived<-as.factor(titanic$Survived)
titanic$Sex<-as.factor(titanic$Sex)
titanic$Pclass<-as.factor(titanic$Pclass)
titanic$Embarked<-as.factor(titanic$Embarked)
#after cleaning the data lets make some plots to get the idea of survival rate through different categories
library(ggplot2)
#survival rate based on sex
ggplot(data=titanic,aes(x=Sex,fill=Survived))+geom_bar()
#it is clear by the graph that females are much likely to have been survived

#survival based on Embarked
ggplot(data=titanic,aes(x=Embarked,fill=Survived))+geom_bar(position = "fill")
table<-table(titanic$Embarked,titanic$Survived)
#the table doesnt give us a proper idea but if we convert it into percentage it would give a good idea
for(i in dim(table)){
  table[i,]<-table[i,]/sum(table[i,])*100
}
#the table makes it clear that the survival rate of people embarking in C is highest
#survival based on Pclass
ggplot(data=titanic,aes(x=Pclass,fill=Survived))+geom_bar(position="fill")
#we can see that the plot of Pclass and Embarked is quite similar hence there lies a relation
ggplot(data=titanic,aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+facet_wrap(~Pclass)
#By looking at the plot we can say that the relation between embarked and Pclass is not clear
#survival based on Parch
ggplot(data=titanic,aes(x=Parch,fill=Survived))+geom_bar()
#Survival based on SibSp
ggplot(data=titanic,aes(x=SibSp,fill=Survived))+geom_bar()
#we can see that the plots of Parch an SibSp are quite similar lets check for a relation
#histogram is used because we can see that the previous plot was continuous
titanic$full<-titanic$Parch+titanic$SibSp
ggplot(data=titanic,aes(x=full,fill=Survived))+geom_histogram(binwidth=1,position="fill")
#we can see that family with size greater or equal to 2 and less than 5 have a greater chance of survival
#survival based on Age
ggplot(data=titanic,aes(x=Age,fill=Survived))+geom_histogram(binwidth=3,position="fill")
#we can say that people with age upto 18 and people with age 80 and above have a great chance of survival
#Survival based on Fare
ggplot(data=titanic,aes(x=Fare,fill=Survived))+geom_histogram(binwidth=18,position="fill")
#who have paid fair more than 90 have a good chance of survival



#Now lets start with the Prediction
nams<-titanic
nams$Name=NULL
nams$Ticket=NULL

nams$Cabin=NULL
nams$full=NULL

library(caTools)
split<-sample.split(nams,SplitRatio = 0.75)
training<-subset(nams,split=="TRUE")
testing<-subset(nams,split=="FALSE")
rt<-testing
training$PassengerId=NULL
testing$PassengerId=NULL
model<-glm(Survived ~.,family=binomial,data=training)
summary(model)
res<-predict(model,testing)
#now before we check for the accuracy we need to find the threshold
ges<-predict(model,training)
library(ROCR)
Pred<-prediction(ges,training$Survived)
Perf<-performance(Pred,"tpr","fpr")
plot(Perf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
#the graph makes it known that the threshold should be 0.1


#for finding the accuracy we need to construct a confused matrix
t<-table(ActualValue=testing$Survived,PredictedValue=res>0.1)
(159+84)/(159+84+18+36)
#the accuracy is 81% which is quite good 
#hence the survival rate is checked on the titanic data set using logistic regression
#now we are writing the predicted survival rate with the PassengerId on survive.csv
pred.test <- ifelse(res > 0.5,1,0)
data<-data.frame(PassengerId=rt$PassengerId,Survived=pred.test)
row.names(data)=NULL
write.csv(data,"survive.csv")
#hence the resultant data has been updated
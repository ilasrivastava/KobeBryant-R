library(dplyr)
library(ggplot2)

#Import the data, extract month from column 'game_month' generating a new column called 'game_month', add Kobe's age data of each season.
kobe=read.csv('C:/Users/Gege Tao/Desktop/R/Project/bryant data set.csv',sep=',',header=TRUE)
#install.packages("lubridate")
library(lubridate)
kobe<-kobe%>%mutate(game_month=month(as.POSIXlt(kobe$game_date, format="%m/%d/%Y")))
body_age<-data.frame(season=c('1996-97','1997-98','1998-99','1999-00','2000-01','2001-02','2002-03','2003-04','2004-05','2005-06','2006-07','2007-08','2008-09','2009-10','2010-11','2011-12','2012-13','2013-14','2014-15','2015-16'),age=c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37))
kobe<-merge(kobe,body_age,by.x ='season',by.y = 'season')

#Basic analysis: check whether there is missing value, what datatype of each column, calculate Kobe's general shot accuracy.
str(kobe)
summary(kobe)
accuracy=mean(kobe$shot_made_flag)
#accuracy=0.446161



#EDA Analysis
##Kobe himself analysis
###Analyze Kobe's body age and its relationship with his shot accuracy
ggplot(data=kobe,aes(x=age))+geom_histogram(stat='count')+labs(y='Number of shots')
ggplot(data=kobe,aes(x=age,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')

###Analyze the relationship between month and Kobe's shot count, accuracy; Kobe's age as control variable. This is to answer when Kobe was xx years old, he played better in which month.
ggplot(data=kobe,aes(x=as.factor(game_month)))+geom_histogram(stat='count')+facet_wrap(~ age)+labs(x='game_month',y='Number of shots')
ggplot(data=kobe,aes(x=as.factor(game_month),fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+facet_wrap(~ age)+guides(fill=guide_legend(title=NULL))+labs(x='game_month',y='Accuracy')
#kobe%>%group_by(game_month,age)%>%summarize(age_accuracy=mean(shot_made_flag,na.rm=T))

###Kobe's shot accuracy of different action_type
df1<-data.frame(c(kobe%>%group_by(action_type)%>%summarize(action_accuracy=mean(shot_made_flag,na.rm=T))))
#kobe%>%group_by(action_type)%>%summarize(action_accuracy=mean(shot_made_flag,na.rm=T))
ggplot(df1, aes(x = action_type, y = 1)) +
  geom_point(aes(y = sort(df1$action_accuracy)), size = 3, color = "dark blue", stat = "identity") +
  coord_flip() 

###Analyze Kobe's combined_shot_type and its relationship with his shot accuracy.
ggplot(data=kobe,aes(x=combined_shot_type))+geom_histogram(stat='count')+labs(y='Number of shots')
ggplot(data=kobe,aes(x=combined_shot_type,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='accuracy')

###Analyze Kobe's 2pt/3pt shot and its relationship with his shot accuracy.
ggplot(data=kobe,aes(x=shot_type))+geom_histogram(stat='count')+labs(y='Number of shots')
ggplot(data=kobe,aes(x=shot_type,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')

###Analyze Kobe's shot distance and its relationship with his shot accuracy.
ggplot(data=kobe,aes(x=shot_distance))+geom_histogram(stat='count')+labs(y='Number of shots')
ggplot(data=kobe,aes(x=shot_distance,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')


##Basketball court influence
###Analyze shot_made_flag by position
ggplot(data=kobe, aes(x = loc_x, y = loc_y)) +
  geom_point(aes(color = shot_made_flag),alpha = 0.5, size = 0.5) +
  facet_wrap(~ shot_made_flag) +
  labs(title = "Shots Made(Blue) vs. Shots Missed(Navy)")+theme(legend.position="none")

###Analyze in which shot area, Kobe performed better.
ggplot(data=kobe,aes(x = loc_x, y = loc_y,col=shot_zone_area))+geom_point()
ggplot(data=kobe,aes(x=shot_zone_area))+geom_histogram(stat='count')+labs(y='Number of shots')+coord_flip()
ggplot(data=kobe,aes(x=shot_zone_area,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')+coord_flip()

###Analyze in which court area, Kobe performed better.
ggplot(data=kobe,aes(x = loc_x, y = loc_y,col=shot_zone_basic))+geom_point()
ggplot(data=kobe,aes(x=shot_zone_basic))+geom_histogram(stat='count')+labs(y='Number of shots')+coord_flip()
ggplot(data=kobe,aes(x=shot_zone_basic,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')+coord_flip()

###Analyze in which shot distance, Kobe performed better.
ggplot(data=kobe,aes(x = loc_x, y = loc_y,col=shot_zone_range))+geom_point()
ggplot(data=kobe,aes(x=shot_zone_range))+geom_histogram(stat='count')+labs(y='Number of shots')+coord_flip()
ggplot(data=kobe,aes(x=shot_zone_range,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')+coord_flip()


##Competition condition analysis
###Analyze in which season, Kobe performed better.
ggplot(data=kobe,aes(x=season))+geom_histogram(stat='count')+labs(y='Number of shots')+coord_flip()
ggplot(data=kobe,aes(x=season,fill=as.factor(shot_made_flag)))+geom_bar(position = 'fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')+coord_flip()

###Analyze in which month, Kobe performed better.
ggplot(data=kobe,aes(x=as.factor(game_month)))+geom_histogram(stat='count')+labs(x='game_month',y='Number of shots')
ggplot(data=kobe,aes(x=as.factor(game_month),fill=as.factor(shot_made_flag)))+geom_bar(position = 'fill')+guides(fill=guide_legend(title=NULL))+labs(x='game_month',y='Accuracy')

###Analyze when played with which opponent, Kobe performed better.
ggplot(data=kobe,aes(x=opponent,fill=as.factor(shot_made_flag)))+geom_bar(position = 'fill')+guides(fill=guide_legend(title=NULL))+labs(y='Accuracy')+coord_flip()

###Analyze Kobe's performance in regular season and playoff.
ggplot(data=kobe,aes(x=as.factor(playoffs)))+geom_histogram(stat='count')+labs(x='playoffs',y='Number of shots')
ggplot(data=kobe,aes(x=as.factor(playoffs),fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(x='playoffs',y='Accuracy')

###Analyze Kobe's performance in different periods
ggplot(data=kobe,aes(x=as.factor(period)))+geom_histogram(stat='count')+labs(x='period',y='Number of counts')
ggplot(data=kobe,aes(x=as.factor(period),fill=as.factor(shot_made_flag)))+geom_bar(position = 'fill')+guides(fill=guide_legend(title=NULL))+labs(x='periods',y='Accuracy')

###Analyze Kobe's performance in different number of remaining minutes; also analyze in different periods, Kobe's performance of differenct remaining seconds
ggplot(data=kobe,aes(x=seconds_remaining))+geom_histogram(stat='count')+labs(y='Number of shots')
ggplot(data=kobe,aes(x=seconds_remaining,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+guides(fill=guide_legend(title=NULL))+labs(y='accuracy')
ggplot(data=kobe,aes(x=seconds_remaining,fill=as.factor(shot_made_flag)))+geom_bar(position='fill')+facet_wrap(~as.factor(period))+guides(fill=guide_legend(title=NULL))+labs(y='accuracy')




#Building model

##Change id, name and date column into NULL, because we do not need these columns. Change the data type of shot_made_flag into factor since we should do a classification problem
library(caret)
library(glmnet)
library(car)


kobe$game_event_id=NULL
kobe$game_id=NULL
kobe$team_id=NULL
kobe$team_name=NULL
kobe$matchup=NULL
kobe$game_date=NULL
kobe$shot_id=NULL
kobe$shot_made_flag<-as.factor(kobe$shot_made_flag)
str(kobe)


###Ridge and Lasso Logistic Regression

###Splitting into training and testing data
kobe.matrix<- model.matrix(kobe$shot_made_flag ~.,data=kobe)[, -1]
set.seed(1234)
data.split<- createDataPartition(kobe$shot_made_flag, p = .7, list = F)
training1<- kobe[data.split,]
testing1<- kobe[-data.split,]
training.matrix<- kobe.matrix[data.split,]
testing.matrix<- kobe.matrix[-data.split,]

###Ridge regression
set.seed(1234)
kobe.ridge<- cv.glmnet(x = training.matrix, y = training1$shot_made_flag,family="binomial",alpha=0, nfolds = 10,type.measure = "deviance")

#we can plot the values of lambda to identify which values lead to smallest mse
plot(kobe.ridge)
#we can also plot out how the coefficients shrink over different values of lambda
plot(kobe.ridge$glmnet.fit, xvar = "lambda", label = T)

#we can identify coefficients of the lambda that produces the smallest mse
coef(kobe.ridge, s="lambda.min")
#or the one that is the most parsimonious while being 1 standard error away from the min mse
coef(kobe.ridge, s="lambda.1se")

###Check the performance of this ridge logistic regression model.
###Testing
prediction<-predict(kobe.ridge,s=kobe.ridge$lambda.1se,newx=testing.matrix,type='response')
predict.class<-round(prediction,digits=0)
confusionMatrix(data=as.factor(predict.class),reference = testing1$shot_made_flag,positive="1")
###Overfitting check
prediction_training<-predict(kobe.ridge,s=kobe.ridge$lambda.1se,newx=training.matrix,type='response')
predict.class_training<-round(prediction_training,digits=0)
mean(predict.class_training==training1$shot_made_flag)

#### Lasso Regression ####
set.seed(1234)
kobe.lasso<- cv.glmnet(x = training.matrix, y =training1$shot_made_flag, alpha=1, family="binomial", nfolds = 10,type.measure = "deviance")

#we can plot the values of lambda to identify which values lead to smallest mse
plot(kobe.lasso)
#we can also plot out how the coefficients shrink over different values of lambda
plot(kobe.lasso$glmnet.fit, xvar = "lambda", label = T)

#we can identify coefficients of the lambda that produces the smallest mse
coef(kobe.lasso, s="lambda.min")
#or the one that is the most parsimonious while being 1 standard error away from the min mse
coef(kobe.lasso, s="lambda.1se")

###Check the performance of this lasso logistic regression model.
###Testing
prediction<-predict(kobe.lasso,s=kobe.lasso$lambda.1se,newx=testing.matrix,type='response')
predict.class<-round(prediction,digits=0)
confusionMatrix(data=as.factor(predict.class),reference = testing1$shot_made_flag,positive="1")

###Overfitting check
prediction_training<-predict(kobe.lasso,s=kobe.lasso$lambda.1se,newx=training.matrix,type='response')
predict.class_training<-round(prediction_training,digits=0)
mean(predict.class_training==training1$shot_made_flag)







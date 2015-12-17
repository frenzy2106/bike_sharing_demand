train <- read.csv("train.csv")
test <- read.csv("test.csv")

test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)

par(mfrow=c(4,2))
par(mar = rep(2, 4))
#hist(data$season)
#hist(data$weather)
#hist(data$humidity)
#hist(data$holiday)
#hist(data$workingday)
#hist(data$temp)
#hist(data$atemp)
#hist(data$windspeed)

#prop.table(table(data$weather))

data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

data$hour=substr(data$datetime,12,13)
data$hour=as.factor(data$hour)
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

#boxplot(log(train$count)~train$hour,xlab="hour", ylab="log(count)")

#par(mfrow=c(1,2))

#boxplot(train$casual~train$hour,xlab="hour", ylab="count of users")

#boxplot(train$registered~train$hour,xlab="hour", ylab="count of users")

date=substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day=days

train <- data[1:10886, ]



par(mfrow=c(1,2))

#boxplot(train$casual~train$day,xlab="day", ylab="casual")

#boxplot(train$registered~train$day,xlab="day", ylab="registered")


sub=data.frame(train$registered,train$casual,train$count,train$temp,train$humidity,train$atemp,train$windspeed)

corr <- cor(sub)

data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]
boxplot(train$count~train$year,xlab="year", ylab="count")

train$hour=as.integer(train$hour) # convert hour to integer
test$hour=as.integer(test$hour) # modifying in both train and test data set

library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(RColorBrewer)

#d=rpart(registered~hour,data=train)
#fancyRpartPlot(d)

#e=rpart(registered~temp,data=train)
#fancyRpartPlot(e)

#f=rpart(casual~temp,data=train)
#fancyRpartPlot(f)

#g=rpart(casual~hour,data=train)
#fancyRpartPlot(g)

data=rbind(train,test)

data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>=22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==19 | data$hour==18]=7

data$dp_cas=0
data$dp_cas[data$hour<=8.5]=1
data$dp_cas[data$hour>8.5 & data$hour<22]=3
data$dp_cas[data$hour>=22]=2




data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

data$temp_reg=0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>13 & data$temp<23]=2
data$temp_reg[data$temp>23 & data$temp<30]=3
data$temp_reg[data$temp>30]=4

data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>15 & data$temp<23]=2
data$temp_cas[data$temp>23 & data$temp<30]=3
data$temp_cas[data$temp>30]=4

data$day_type=""
data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"


data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1

train <- data[1:10886, ]
test <- data[10886:17379, ]

train$hour=as.factor(train$hour)
test$hour=as.factor(test$hour)

train$weather=as.factor(train$weather)
test$weather=as.factor(test$weather)

train$season=as.factor(train$season)
test$season=as.factor(test$season)

train$holiday=as.factor(train$holiday)
test$holiday=as.factor(test$holiday)

train$workingday=as.factor(train$workingday)
test$workingday=as.factor(test$workingday)

train$day=as.factor(train$day)
test$day=as.factor(test$day)

train$day_type=as.factor(train$day_type)
test$day_type=as.factor(test$day_type)

logreg = log(train$registered+1)
logcas = log(train$casual+1)



library(randomForest)
#predicting the log of registered users.
set.seed(415)
#fit1 <- randomForest(logreg ~ hour +workingday+day+holiday+ day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
fit1 <- randomForest(logreg ~ hour +workingday+day+holiday + day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
test$logreg=pred1

#predicting the log of casual users.
set.seed(415)
fit2 <- randomForest(logcas ~hour + day_type+day+humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+dp_cas+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
pred2=predict(fit2,test)
test$logcas=pred2

test$registered=exp(test$logreg)-1
test$casual=exp(test$logcas)-1
test$count=test$casual+test$registered
s<-data.frame(datetime=test$datetime,count=test$count)
write.csv(s,file="submit.csv",row.names=FALSE)

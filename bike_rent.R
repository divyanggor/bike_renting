rm(list = ls())
setwd("/Users/divyanggor/Documents/Study/Online_Course/Edwisor/Project/project_2/")

# #loading Libraries
x = c("plyr","ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart",'MASS','xgboost','stats','gdistance', 'Imap', 'car',"Metrics")
#load Packages
lapply(x, require, character.only = TRUE)
rm(x)


bike_rent= read.csv("day.csv")
summary(bike_rent)
head(bike_rent,5)
#########Changing coulmn Names#########
colnames(bike_rent) = c("instant","date","season","year","month","holiday","weekday","workingday","weather_condition","temprature","feeling_temprature","humidity","windspeed","casual_count","registered_count","total_count")

######Missing Value Analysis######
apply(bike_rent,2,function(x){sum(is.na(x))})

#Ther is no Missing Value in data.

plot_bike_rent = bike_rent

#plot_bike_rent$season[plot_bike_rent$season==1]="springer"
#plot_bike_rent$season[plot_bike_rent$season==2]="summer"
#plot_bike_rent$season[plot_bike_rent$season==3]="fall"
#plot_bike_rent$season[plot_bike_rent$season==4]="winter"
#plot_bike_rent$year[plot_bike_rent$year==0]=2011
#plot_bike_rent$year[plot_bike_rent$year==1]=2012
#head(plot_bike_rent,5)

############Feature Engineering##########

str(bike_rent)
cols = c('season','year','month','holiday','weekday','workingday','weather_condition')
bike_rent[, cols] = lapply(bike_rent[, cols], factor)
str(bike_rent)

#########Outliner Analysis#################
# Boxplot for total_count variable
pl1 = ggplot(bike_rent,aes(y = total_count))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

boxplot(bike_rent[,"total_count"])
boxplot(bike_rent[,c('temprature','feeling_temprature','humidity','windspeed')])

values = bike_rent[,'windspeed'] %in% boxplot.stats(bike_rent[,'windspeed'])$out
bike_rent[which(values),'windspeed'] = NA

values = bike_rent[,'humidity'] %in% boxplot.stats(bike_rent[,'humidity'])$out
bike_rent[which(values),'humidity'] = NA

apply(bike_rent,2,function(x){sum(is.na(x))})
#here very less number of missing values so we can drop those values.

bike_rent = na.omit(bike_rent)

################  Feature selection                 ###############
numeric = sapply(bike_rent,is.numeric) #selecting numeric variables
numeric_data = bike_rent[,numeric]
cnames = colnames(numeric_data)
#Correlation analysis for numeric variables
cor(numeric_data)
corrgram(bike_rent[,numeric],upper.panel=panel.pie, main = "Correlation Plot")
#Drop unnacessary variables 
bike_rent = subset(bike_rent,select=-c(date,instant, casual_count,registered_count))
#Anova Test
aov_results = aov(total_count ~ season + year + month + holiday + workingday+ weekday + weather_condition,data = bike_rent)
summary(aov_results)

# workingday has p value greater than 0.05 
bike_rent = subset(bike_rent,select=-workingday)
#################### Splitting train into train and validation subsets ###################
set.seed(42)
tr = createDataPartition(bike_rent$total_count,p=0.80,list = FALSE) # 80% in trainin and 20% in Test Datasets
train_bike_rent = bike_rent[tr,]
test_bike_rent = bike_rent[-tr,]


#############            Linear regression               #################
lm_model = lm(total_count ~.,data=bike_rent)

summary(lm_model)
plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")

lm_predictions = predict(lm_model,test_bike_rent[,1:10])
qplot(x = test_bike_rent[,11], y = lm_predictions, data = test_bike_rent, color = I("blue"), geom = "point")
regr.eval(test_bike_rent[,11],lm_predictions)
library(Metrics)
rmsle(lm_predictions,test_bike_rent[,11])

#############                             Decision Tree            #####################

Dt_model = rpart(total_count ~.,data=bike_rent, method = "anova")
summary(Dt_model)
#Predict for new test cases
  predictions_DT = predict(Dt_model, test_bike_rent[,1:10])
qplot(x = test_bike_rent[,11], y = predictions_DT, data = test_bike_rent, color = I("blue"), geom = "point")
regr.eval(test_bike_rent[,11],predictions_DT)
rmsle(predictions_DT,test_bike_rent[,11])

#############                             Random forest            #####################
rf_model = randomForest(total_count ~.,data=bike_rent)
summary(rf_model)
rf_predictions = predict(rf_model,test_bike_rent[,1:10])
qplot(x = test_bike_rent[,11], y = rf_predictions, data = test_bike_rent, color = I("blue"), geom = "point")
regr.eval(test_bike_rent[,11],rf_predictions)
rmsle(rf_predictions,test_bike_rent[,11])

############          Improving Accuracy by using Ensemble technique ---- XGBOOST             ###########################
train_data_matrix = as.matrix(sapply(train_bike_rent[-11],as.numeric))
test_data_data_matrix = as.matrix(sapply(test_bike_rent[-11],as.numeric))
xgboost_model = xgboost(data = train_data_matrix,label = train_bike_rent$total_count,nrounds = 50,verbose = FALSE)
summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)
qplot(x = test_bike_rent[,11], y = xgb_predictions, data = test_bike_rent, color = I("blue"), geom = "point")
regr.eval(test_bike_rent[,11],xgb_predictions)
rmsle(xgb_predictions,test_bike_rent[,11])









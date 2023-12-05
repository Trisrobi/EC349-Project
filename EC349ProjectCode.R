#Clear
cat("\014")  #Clean screen
rm(list=ls()) #Clean Memory

#Load Libraries
library(glmnet)
library(ggplot2)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)

#New Libraries for this week
library(adabag) #AdaBoost
library(caret)  #create data partition/training dataset
library(randomForest) #randomforest

#loading the different datasets

load(file="business_data.Rda")
load(file="yelp_review_small.Rda")
load(file="yelp_user_small.Rda")
load(file="checkin_data.Rda")
load(file="tip_data.Rda")

#exploratory data analysis
summary(business_data)#interesting, will use some of the attributes data, some of its variables has a lot of missing data however
summary(review_data_small)#quite interesting, will be the main dataset to use
summary(user_data_small)#does not seem very useful
summary(tip_data)#mainly compliment_count could be interesting
summary(checkin_data)#does not seem very useful

table(review_data_small$stars)#quite heavily skewed towards 5

#creating our own dataset combining the different datasets

data<-inner_join(review_data_small,business_data, by="business_id")
data2<-subset(tip_data, select=c(business_id,compliment_count))
#put all data frames into list
data <- inner_join(data, data2, multiple="any")
data<- unnest(data, cols=c(attributes))#unnesting the dataframe attributes

data<-subset(data,select=c(state,is_open,ByAppointmentOnly,BikeParking,RestaurantsPriceRange2,CoatCheck,RestaurantsTakeOut,RestaurantsDelivery,Caters,WheelchairAccessible,HappyHour,OutdoorSeating,HasTV,RestaurantsReservations,DogsAllowed,Alcohol,GoodForKids,RestaurantsAttire,RestaurantsTableService,RestaurantsGoodForGroups,DriveThru,NoiseLevel,BusinessAcceptsBitcoin,AcceptsInsurance,BYOB,Corkage,BYOBCorkage,stars.x,WiFi,review_count,funny,cool,useful,compliment_count), drop=TRUE)
#data<-subset(data,select=c(review_count,state,useful,cool,WiFi,funny,Alcohol,stars.x)) relevant for model 2 in which only include some of the values, get lower accuracy
#data<- data$Music(drop=TRUE)

#turning data into factor variables
data$stars.x<-as.factor(data$stars.x)#dependent variable

data$state<-as.factor(data$state)
data$Alcohol<-as.factor(data$Alcohol)
data$WiFi<-as.factor(data$WiFi)
data$is_open<-as.factor(data$is_open)
data$ByAppointmentOnly<-as.factor(data$ByAppointmentOnly)
data$BikeParking<-as.factor(data$BikeParking)
data$RestaurantsPriceRange2<-as.factor(data$RestaurantsPriceRange2)
data$CoatCheck<-as.factor(data$CoatCheck)
data$RestaurantsTakeOut<-as.factor(data$RestaurantsTakeOut)
data$RestaurantsDelivery<-as.factor(data$RestaurantsDelivery)
data$Caters<-as.factor(data$Caters)
data$WheelchairAccessible<-as.factor(data$WheelchairAccessible)
data$HappyHour<-as.factor(data$HappyHour)
data$OutdoorSeating<-as.factor(data$OutdoorSeating)
data$HasTV<-as.factor(data$HasTV)
data$RestaurantsReservations<-as.factor(data$RestaurantsReservations)
data$DogsAllowed<-as.factor(data$DogsAllowed)
data$GoodForKids<-as.factor(data$GoodForKids)
data$RestaurantsAttire<-as.factor(data$RestaurantsAttire)
data$RestaurantsTableService<-as.factor(data$RestaurantsTableService)
data$RestaurantsGoodForGroups<-as.factor(data$RestaurantsGoodForGroups)
data$DriveThru<-as.factor(data$DriveThru)
data$NoiseLevel<-as.factor(data$NoiseLevel)
data$BusinessAcceptsBitcoin<-as.factor(data$BusinessAcceptsBitcoin)
data$AcceptsInsurance<-as.factor(data$AcceptsInsurance)
data$BYOB<-as.factor(data$BYOB)
data$Corkage<-as.factor(data$Corkage)
data$BYOBCorkage<-as.factor(data$BYOBCorkage)


####data taken out in order to increase accuracy####
#data$BusinessAcceptsCreditCards<-as.factor(data$BusinessAcceptsCreditCards)
#data$Smoking<-as.factor(data$Smoking)
#data$GoodForDancing<-as.factor(data$GoodForDancing)


#need to add this code to ensure NA is taken as another factor
data$WiFi<-addNA(data$WiFi)
data$Alcohol<-addNA(data$Alcohol)
data$DogsAllowed<-addNA(data$DogsAllowed)
data$WheelchairAccessible<-addNA(data$WheelchairAccessible)
data$Caters<-addNA(data$Caters)
data$RestaurantsDelivery<-addNA(data$RestaurantsDelivery)
data$RestaurantsTakeOut<-addNA(data$RestaurantsTakeOut)
data$BikeParking<-addNA(data$BikeParking)
data$RestaurantsPriceRange2<-addNA(data$RestaurantsPriceRange2)
data$ByAppointmentOnly<-addNA(data$ByAppointmentOnly)
data$NoiseLevel<-addNA(data$NoiseLevel)
data$is_open<-addNA(data$is_open)
data$state<-addNA(data$state)
data$CoatCheck<-addNA(data$CoatCheck)
data$HappyHour<-addNA(data$HappyHour)
data$OutdoorSeating<-addNA(data$OutdoorSeating)
data$HasTV<-addNA(data$HasTV)
data$RestaurantsReservations<-addNA(data$RestaurantsReservations)
data$GoodForKids<-addNA(data$GoodForKids)
data$RestaurantsAttire<-addNA(data$RestaurantsAttire)
data$RestaurantsTableService<-addNA(data$RestaurantsTableService)
data$RestaurantsGoodForGroups<-addNA(data$RestaurantsGoodForGroups)
data$DriveThru<-addNA(data$DriveThru)
data$BusinessAcceptsBitcoin<-addNA(data$BusinessAcceptsBitcoin)
data$AcceptsInsurance<-addNA(data$AcceptsInsurance)
data$BYOB<-addNA(data$BYOB)
data$Corkage<-addNA(data$Corkage)
data$BYOBCorkage<-addNA(data$BYOBCorkage)

##taken out to increase accuracy

#data$BusinessAcceptsCreditCards<-addNA(data$BusinessAcceptsCreditCards)
#data$Smoking<-addNA(data$Smoking)
#data$GoodForDancing<-addNA(data$GoodForDancing)

#adapting data to make sure it is of numeric type
data$review_count<-as.numeric(data$review_count)
data$funny<-as.numeric(data$funny)
data$cool<-as.numeric(data$cool)
data$useful<-as.numeric(data$useful)
data$compliment_count<-as.numeric(data$compliment_count)

#due to a bug in the RandomForest code, need to rename those variables as NA
levels(data$state)[is.na(levels(data$state))] <- "NA"
levels(data$WiFi)[is.na(levels(data$WiFi))] <- "NA"
levels(data$Alcohol)[is.na(levels(data$Alcohol))] <- "NA"
levels(data$is_open)[is.na(levels(data$is_open))] <- "NA"
levels(data$ByAppointmentOnly)[is.na(levels(data$ByAppointmentOnly))] <- "NA"
levels(data$BikeParking)[is.na(levels(data$BikeParking))] <- "NA"
levels(data$RestaurantsPriceRange2)[is.na(levels(data$RestaurantsPriceRange2))] <- "NA"
levels(data$CoatCheck)[is.na(levels(data$CoatCheck))] <- "NA"
levels(data$RestaurantsTakeOut)[is.na(levels(data$RestaurantsTakeOut))] <- "NA"
levels(data$RestaurantsDelivery)[is.na(levels(data$RestaurantsDelivery))] <- "NA"
levels(data$Caters)[is.na(levels(data$Caters))] <- "NA"
levels(data$WheelchairAccessible)[is.na(levels(data$WheelchairAccessible))] <- "NA"
levels(data$HappyHour)[is.na(levels(data$HappyHour))] <- "NA"
levels(data$OutdoorSeating)[is.na(levels(data$OutdoorSeating))] <- "NA"
levels(data$HasTV)[is.na(levels(data$HasTV))] <- "NA"
levels(data$RestaurantsReservations)[is.na(levels(data$RestaurantsReservations))] <- "NA"
levels(data$DogsAllowed)[is.na(levels(data$DogsAllowed))] <- "NA"
levels(data$GoodForKids)[is.na(levels(data$GoodForKids))] <- "NA"
levels(data$RestaurantsAttire)[is.na(levels(data$RestaurantsAttire))] <- "NA"
levels(data$RestaurantsTableService)[is.na(levels(data$RestaurantsTableService))] <- "NA"
levels(data$RestaurantsGoodForGroups)[is.na(levels(data$RestaurantsGoodForGroups))] <- "NA"
levels(data$DriveThru)[is.na(levels(data$DriveThru))] <- "NA"
levels(data$NoiseLevel)[is.na(levels(data$NoiseLevel))] <- "NA"
levels(data$BusinessAcceptsBitcoin)[is.na(levels(data$BusinessAcceptsBitcoin))] <- "NA"
levels(data$AcceptsInsurance)[is.na(levels(data$AcceptsInsurance))] <- "NA"
levels(data$BYOB)[is.na(levels(data$BYOB))] <- "NA"
levels(data$Corkage)[is.na(levels(data$Corkage))] <- "NA"
levels(data$BYOBCorkage)[is.na(levels(data$BYOBCorkage))] <- "NA"

#taken out to increase accuracy
#levels(data$BusinessAcceptsCreditCards)[is.na(levels(data$BusinessAcceptsCreditCards))] <- "NA"
#levels(data$Smoking)[is.na(levels(data$Smoking))] <- "NA"
#levels(data$GoodForDancing)[is.na(levels(data$GoodForDancing))] <- "NA"

# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (90%) and testing set (10%)
parts = createDataPartition(data$stars.x, p = 0.9, list = F)
train = data[parts, ]
test = data[-parts, ]

### Random Forest ###
set.seed(1312)
model_RF<-randomForest(stars.x~.,data=train, ntree=100, type="prob")
print(model_RF$importance)
print(model_RF$confusion)
varImpPlot(model_RF,n.var=15 ,main="importance of variables graph")
pred_RF_test = predict(model_RF, test)
confusionMatrix(pred_RF_test, test$stars.x)
mean(model_RF[["err.rate"]])
print(model_RF)#interesting to see that really bad at predicting extremes, with an emphasis on lower numbers such as 1 and 2, really good at predicting middle numbers such as 3-4.5

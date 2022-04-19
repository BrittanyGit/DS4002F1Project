#Connor Nickol

#Load packages
library(tidyverse)
library(tidymodels)
library(modelr)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(gbm)

#Read in data sets
circuits<-read.csv("circuits.csv")
con_results<-read.csv("constructor_results.csv")
con_standings<-read.csv("constructor_standings.csv")
driver_standings<-read.csv("driver_standings.csv")
drivers<-read.csv("drivers.csv")
lap_times<-read.csv("lap_times.csv")
pit_stops<-read.csv("PIT_STOPS.csv")
qualifying<-read.csv("qualifying.csv")
races<-read.csv("RACES.csv")
results<-read.csv("results.csv")
seasons<-read.csv("seasons.csv")
status<-read.csv("status.csv")
safety_cars<-read.csv("SAFETY_CAR.csv")

#Remove columns from certain sets
circuits2<-circuits %>% select(-c(location,url))
drivers2<-drivers %>% select(-c(driverRef,number,dob,url))
races2<-races %>% select(-c(ROUND,F1DATE,TIME,URL,SCORE,
                            DNF_COUNT,DNF_DUE_TO_ACCIDENT_COUNT,WEATHER,
                            YEAR_C,RACE_COUNT,NAME_YEAR,OVERTAKEN_POSITIONS_TOTAL))
results2<-results %>% select(-c(constructorId,number,positionText,
                                points,laps,time,milliseconds,
                                fastestLap,fastestLapTime,fastestLapSpeed,
                                rank))

#Remove columns and filter for qualifying position to be 4 or less
qualifying2<- qualifying %>% select(-c(qualifyId,constructorId,number)) %>% filter(position <= 4)

#Fix qualifying times and convert to be 0 if not present
qualifying2$q1<-gsub('[\\N]'," ",qualifying2$q1)
qualifying2$q2<-gsub('[\\N]'," ",qualifying2$q2)
qualifying2$q3<-gsub('[\\N]'," ",qualifying2$q3)
qualifying2$q1[qualifying2$q1=="  "]<-0
qualifying2$q2[qualifying2$q2=="  "]<-0
qualifying2$q3[qualifying2$q3=="  "]<-0
qualifying2$q1[qualifying2$q1==""]<-0
qualifying2$q2[qualifying2$q2==""]<-0
qualifying2$q3[qualifying2$q3==""]<-0

#Put all qualifying times into q1
for (i in 1:nrow(qualifying2)){
  if(qualifying2$q3[i]==0){
    qualifying2$q3[i]<-qualifying2$q1[i]
  }
}

#Remove q2 and q3 columns 
qualifying2<-qualifying2 %>% filter(q3 != 0) %>% select(-c(q1,q2))

#Rename some variables
qualifying2<-rename(qualifying2, qual_time=q3)
qualifying2<-rename(qualifying2, qual_positon=position)
results2<-rename(results2, finish_position=position)
circuits2<-rename(circuits2, circuit=name)

# #Get number of pit stops for each driver each racce
# pit_stops2<-pit_stops %>% group_by(raceId,driverId) %>% 
#   arrange(desc(stop)) %>% slice(1)
# 
# #Rename column
# pit_stops2<-rename(pit_stops2, num_stops=stop)
# 
# #Remove extra columns
# pit_stops2<-pit_stops2 %>% select(-c(lap,time,duration,milliseconds))

#Join together filtered data sets
formula_1_data<-qualifying2 %>% left_join(results2,by=c("raceId","driverId")) 

formula_1_data<-formula_1_data %>% left_join(races2,by=c("raceId"="RACEID")) 

formula_1_data<-formula_1_data %>% left_join(circuits2,by=c("CIRCUITREF"="circuitRef"))

formula_1_data<-formula_1_data %>% left_join(drivers2,by=c("driverId"))

write.csv(formula_1_data,"f1_data.csv",row.names = FALSE)

#Create qual qap and won race dummy variables
formula_1_data<-formula_1_data %>% mutate(qual_gap=0,won_race=0) %>% na.omit()

# Set won race to be a 1 if driver won the race
for(i in 1:nrow(formula_1_data)){
  if (formula_1_data$finish_position[i]==1){
    formula_1_data$won_race[i]<-1
  }
}

#Remove colons from qual time
formula_1_data$qual_time <- gsub("[:]", "" ,formula_1_data$qual_time, perl=TRUE)

#Make qual time a numeric
formula_1_data<-formula_1_data %>% mutate(qual_time=as.numeric(qual_time))

#Convert qual times to be in seconds
for (i in 1:nrow(formula_1_data)){
  if (formula_1_data$qual_time[i] >=100 & formula_1_data$qual_time[i] < 200){
    formula_1_data$qual_time[i]<-60+(formula_1_data$qual_time[i]-100)
  }
  else if (formula_1_data$qual_time[i] >=200 & formula_1_data$qual_time[i] < 300){
    formula_1_data$qual_time[i]<-120+(formula_1_data$qual_time[i]-200)
  }
  else{
    formula_1_data$qual_time[i]<-formula_1_data$qual_time[i]
  }
}

formula_1_data<-formula_1_data  %>% 
  filter(grid<4) %>% 
  group_by(raceId) %>% 
  mutate(count=sum(n())) %>% 
  filter(count==3)

#Arrange by raceid least to greatest
formula_1_data<-formula_1_data %>% arrange(raceId)

#create time global variable
time<-0

#Create qual gap times 
for (i in 1:nrow(formula_1_data)){
  ifelse(formula_1_data$qual_positon[i]==1,
         time<-formula_1_data$qual_time[i],
         formula_1_data$qual_gap[i]<-formula_1_data$qual_time[i]-time) 
}

formula_1_data<-formula_1_data %>% filter(qual_gap>=0 & qual_gap < 4)

test<-formula_1_data %>% group_by(circuit) %>% 
  summarise(total_races=n()) %>% 
  filter(total_races > 9)

circuits<-test$CIRCUITREF

formula_1_data_filtered<-formula_1_data %>% filter(CIRCUITREF %in% circuits) %>% filter(grid < 4)

plot_circuits<-ggplot(formula_1_data_filtered,aes(x=grid,y=won_race)) +
  geom_bar(stat="identity") + facet_wrap(~CIRCUITREF) + theme_minimal()

plot_circuits

plot_wins_position<-ggplot(formula_1_data_filtered,aes(x=grid,y=won_race))+
  geom_bar(stat = "identity")+ labs(title = "All time wins by starting position",
                                    x="Starting Position",
                                    y="Total Wins") + theme_minimal()

plot_wins_position

formula_1_data<-formula_1_data %>% mutate(finish_position=as.numeric(finish_position))

plot_qual_data<-formula_1_data %>% filter(qual_gap>0) 

plot_qual<-ggplot(plot_qual_data,
      aes(x=finish_position,y=qual_gap))+
  geom_point()+theme_minimal()

plot_qual

data_weather<-formula_1_data %>% filter(WEATHER_WET=="Y")

plot_weather<-ggplot(data_weather,aes(x=grid,y=won_race))+
  geom_bar(stat = "identity")+ labs(title = "All time wins by starting position when raining",
                                    x="Starting Position",
                                    y="Total Wins") + theme_minimal()
plot_weather

races_won_nationality<-ggplot(formula_1_data,aes(x=grid,y=won_race))+
  geom_bar(stat = "identity")+facet_grid(~nationality)+ 
  labs(title = "All time wins by starting position by nationality",
                                    x="Starting Position",
                                    y="Total Wins") + theme_minimal()
races_won_nationality

f1_2021<-formula_1_data %>% filter(YEAR==2021)

tracks_2021<-f1_2021$circuitId

formula_1_data_filtered_2<-formula_1_data %>% filter(circuitId %in% tracks_2021)

plot_circuits<-ggplot(formula_1_data_filtered_2,aes(x=grid,y=won_race)) +
  geom_bar(stat="identity") + facet_wrap(~CIRCUITREF) + theme_minimal()

plot_circuits

## Changing to look at time compared to fourth place driver

#Load packages
library(tidyverse)

#Read in data sets
circuits<-read.csv("circuits.csv")
con_results<-read.csv("constructor_results.csv")
con_standings<-read.csv("constructor_standings.csv")
driver_standings<-read.csv("driver_standings.csv")
drivers<-read.csv("drivers.csv")
lap_times<-read.csv("lap_times.csv")
pit_stops<-read.csv("PIT_STOPS.csv")
qualifying<-read.csv("qualifying.csv")
races<-read.csv("RACES.csv")
results<-read.csv("results.csv")
seasons<-read.csv("seasons.csv")
status<-read.csv("status.csv")
safety_cars<-read.csv("SAFETY_CAR.csv")

#Remove columns from certain sets
circuits2<-circuits %>% select(-c(location,url))
drivers2<-drivers %>% select(-c(driverRef,number,dob,url))
races2<-races %>% select(-c(ROUND,F1DATE,TIME,URL,SCORE,
                            DNF_COUNT,DNF_DUE_TO_ACCIDENT_COUNT,WEATHER,
                            YEAR_C,RACE_COUNT,NAME_YEAR,OVERTAKEN_POSITIONS_TOTAL))
results2<-results %>% select(-c(constructorId,number,positionText,
                                points,laps,time,milliseconds,
                                fastestLap,fastestLapTime,fastestLapSpeed,
                                rank))

#Remove columns and filter for qualifying position to be 4 or less
qualifying2<- qualifying %>% select(-c(qualifyId,constructorId,number)) %>% filter(position <= 4)

#Fix qualifying times and convert to be 0 if not present
qualifying2$q1<-gsub('[\\N]'," ",qualifying2$q1)
qualifying2$q2<-gsub('[\\N]'," ",qualifying2$q2)
qualifying2$q3<-gsub('[\\N]'," ",qualifying2$q3)
qualifying2$q1[qualifying2$q1=="  "]<-0
qualifying2$q2[qualifying2$q2=="  "]<-0
qualifying2$q3[qualifying2$q3=="  "]<-0
qualifying2$q1[qualifying2$q1==""]<-0
qualifying2$q2[qualifying2$q2==""]<-0
qualifying2$q3[qualifying2$q3==""]<-0

#Put all qualifying times into q1
for (i in 1:nrow(qualifying2)){
  if(qualifying2$q3[i]==0){
    qualifying2$q3[i]<-qualifying2$q1[i]
  }
}

#Remove q2 and q3 columns 
qualifying2<-qualifying2 %>% filter(q3 != 0) %>% select(-c(q1,q2))

#Rename some variables
qualifying2<-rename(qualifying2, qual_time=q3)
qualifying2<-rename(qualifying2, qual_positon=position)
results2<-rename(results2, finish_position=position)
circuits2<-rename(circuits2, circuit=name)

# #Get number of pit stops for each driver each racce
# pit_stops2<-pit_stops %>% group_by(raceId,driverId) %>% 
#   arrange(desc(stop)) %>% slice(1)
# 
# #Rename column
# pit_stops2<-rename(pit_stops2, num_stops=stop)
# 
# #Remove extra columns
# pit_stops2<-pit_stops2 %>% select(-c(lap,time,duration,milliseconds))

#Join together filtered data sets
formula_1_data<-qualifying2 %>% left_join(results2,by=c("raceId","driverId")) 

formula_1_data<-formula_1_data %>% left_join(races2,by=c("raceId"="RACEID")) 

formula_1_data<-formula_1_data %>% left_join(circuits2,by=c("CIRCUITREF"="circuitRef"))

formula_1_data<-formula_1_data %>% left_join(drivers2,by=c("driverId"))

#Create qual qap and won race dummy variables
formula_1_data<-formula_1_data %>% mutate(qual_gap=0,won_race=0) %>% na.omit()

# Set won race to be a 1 if driver won the race
for(i in 1:nrow(formula_1_data)){
  if (formula_1_data$finish_position[i]==1){
    formula_1_data$won_race[i]<-1
  }
}

#Remove colons from qual time
formula_1_data$qual_time <- gsub("[:]", "" ,formula_1_data$qual_time, perl=TRUE)

#Make qual time a numeric
formula_1_data<-formula_1_data %>% mutate(qual_time=as.numeric(qual_time))

#Convert qual times to be in seconds
for (i in 1:nrow(formula_1_data)){
  if (formula_1_data$qual_time[i] >=100 & formula_1_data$qual_time[i] < 200){
    formula_1_data$qual_time[i]<-60+(formula_1_data$qual_time[i]-100)
  }
  else if (formula_1_data$qual_time[i] >=200 & formula_1_data$qual_time[i] < 300){
    formula_1_data$qual_time[i]<-120+(formula_1_data$qual_time[i]-200)
  }
  else{
    formula_1_data$qual_time[i]<-formula_1_data$qual_time[i]
  }
}

f1_data<-formula_1_data %>% 
  group_by(raceId) %>% 
  arrange(raceId,desc(qual_positon))

time<-0

#Create qual gap times 
for (i in 1:nrow(f1_data)){
  ifelse(f1_data$qual_positon[i]==4,
         time<-f1_data$qual_time[i],
         f1_data$qual_gap[i]<-f1_data$qual_time[i]-time) 
}

#Filter out 4th place and drivers who did not finish
f1_data2<-f1_data %>% 
  filter(finish_position != "\\N",grid<4) %>% 
  filter(qual_gap <= 0) %>% 
  mutate(race_21=0)

#Create a vector with all of the 2021 circuits
circuits_2021<-c("Bahrain International Circuit","Autodromo Enzo e Dino Ferrari",
                 "Autódromo Internacional do Algarve","Circuit de Barcelona-Catalunya",
                 "Circuit de Monaco","Baku City Circuit",
                 "Circuit Paul Ricard","Hungaroring","Silverstone Circuit",
                 "Red Bull Ring","Circuit de Spa-Francorchamps",
                 "Circuit Park Zandvoort","Autodromo Nazionale di Monza",
                 "Sochi Autodrom","Istanbul Park","Circuit of the Americas",
                 "Autódromo Hermanos Rodríguez","Autódromo José Carlos Pace",
                 "Losail International Circuit","Jeddah Street Circuit",
                 "Yas Marina Circuit")

#Filter to all the 2021 circuits
all_years_2021_circuits<-f1_data2 %>% 
  filter(circuit %in% circuits_2021) 

#True/False if circuit was in 2021
f1_data2$race_21 <- f1_data2$circuit %in% circuits_2021

#Change to be 1s and 0s
f1_data2$race_21<-ifelse(f1_data2$race_21==TRUE,1,0)

#Make circuit id a 0 if not raced in 2021
for(i in 1:nrow(f1_data2)){
  if(f1_data2$race_21[i]==0){
    f1_data2$circuitId[i]<-0
  }
}

#Make races from 2021 that were new 0s
for(i in 1:nrow(f1_data2)){
  if(f1_data2$circuitId[i]==39 | 
     f1_data2$circuitId[i]==77 |
     f1_data2$circuitId[i]==78){
    f1_data2$circuitId[i]<-0
  }
}


table(f1_data2$circuitId)

#Change nationality to be a factor
f1_data2<-f1_data2 %>% mutate(nationality=as.factor(nationality))

write.csv(f1_data2,"f1_model_data.csv",row.names = FALSE)

# #Create test and training data
# test_2021<-f1_data2 %>% filter(YEAR == 2021) %>% mutate(won_race=as.factor(won_race))
# train_f1<-f1_data2 %>% filter(YEAR != 2021) %>% mutate(won_race=as.factor(won_race))
# 
# #Create a logistic regression model
# LogReg.mod1 <- glm(won_race ~ qual_gap + grid + WEATHER_WET +
#                      circuitId + lat + lng + nationality, 
#                    data = train_f1, family = "binomial")
# summary(LogReg.mod1)
# 
# #Run predictions for log reg model
# test_2021$predict<-round(predict(LogReg.mod1,newdata = test_2021,type = "response"),digits = 5)
# 
# #Creating the metrics to test the accuracy of our model
# rates<-ROCR::prediction(test_2021$predict,test_2021$won_race)
# roc_result<-ROCR::performance(rates,measure = "tpr",x.measure = "fpr")
# plot(roc_result,main="ROC Curve")
# lines(x=c(0,1),y=c(0,1),col="red")
# 
# #Auc
# auc<-ROCR::performance(rates,measure = "auc")
# auc@y.values
# 
# #Confusion matrix
# confusion.mat<-table(test_2021$won_race,test_2021$predict>0.5)
# confusion.mat
# 
# #Recursive Binary Splitting 
# DT.mod1<-rpart(won_race ~ qual_gap + grid + WEATHER_WET +
#                                circuitId + lat + lng + nationality, 
#                              data = train_f1)
# 
# #Plot of DT 1
# DT.mod1 %>%
#   rpart.plot(type = 4, nn = TRUE)
# 
# summary(DT.mod1)
# 
# DT.add <- test_2021 %>%
#   gather_predictions(DT.mod1, type = "class") %>%
#   rename(pred_won_race = pred) %>%
#   mutate(pred_won_race= as.factor( pred_won_race ),
#          won_race = as.factor( won_race ))
# 
# all_metrics <- metric_set(accuracy, precision, recall)
# 
# #Look at metrics for DT
# DT.add %>%
#   all_metrics(truth = won_race, estimate = pred_won_race)
# 
# # Random Forest
# RF.mod1 <- randomForest(won_race ~ qual_gap + grid + WEATHER_WET +
#                           circuitId + lat + lng + nationality, 
#                         data = train_f1,
#                         mtry = 3, importance = TRUE)
# 
# RF.mod1
# 
# importance(RF.mod1)
# varImpPlot(RF.mod1)
# 
# # RF.add <- test_2021 %>%
# #   gather_predictions(RF.mod1, type = "prob") %>%
# #   rename(pred_won_race = pred)  %>%
# #   mutate(pred_won_race = as.factor( pred_won_race))
# 
# test_2021$predict<-predict(RF.mod1,new_data=test_2021)
# 
# RF.add %>%
#   all_metrics(truth = won_race, estimate = pred_won_race)

model_f1_data<-read.csv("f1_model_data.csv")

model_f1_data<-model_f1_data %>% mutate(won_race=as.factor(won_race),
                                        WEATHER_WET=as.factor(WEATHER_WET),
                                        nationality=as.factor(nationality))

train<-model_f1_data %>% filter(YEAR!=2021)
test<-model_f1_data %>% filter(YEAR==2021)

pred.test<-test[,"won_race"]

##Random Forest
set.seed(2222)
rf.class<-randomForest::randomForest(won_race ~ qual_gap + grid + WEATHER_WET +
                                       circuitId + lat + lng + nationality, 
                                     data=train, mtry=3,importance=TRUE)
rf.class

importance(rf.class)
varImpPlot(rf.class)

##test accuracy with Random Forest
pred.rf<-predict(rf.class, newdata=test)
mean(pred.rf==pred.test) ## 67%

table(pred.test,pred.rf)

# Bagging
set.seed(222)
##bagging is special case of random forest when mtry = number of predictors
bag.class<-randomForest::randomForest(won_race ~ qual_gap + grid + WEATHER_WET +
                                        circuitId + lat + lng + nationality, 
                                      data=train, mtry=7, importance=TRUE)
bag.class ##note with classification tree OOB estimates are provided

##importance measures of predictors
randomForest::importance(bag.class)
##graphical version
randomForest::varImpPlot(bag.class)

##test accuracy with bagging
pred.bag<-predict(bag.class, newdata=test)
##confusion matrix for test data
table(pred.test, pred.bag)
mean(pred.bag==pred.test) ## 72%

# ## Boosting
# set.seed(22222)
# boost.class<-gbm::gbm(won_race ~ qual_gap + grid + WEATHER_WET +
#                         circuitId + lat + lng + nationality, 
#                       data=train, distribution="bernoulli", n.trees=500)
# summary(boost.class)
# 
# plot(boost.class,i="rm")
# plot(boost.class,i="lstat")
# 
# ##this gives predicted probabilities, not predicted class. This is because the response is using 0/1 dummy codes with gbm()
# pred.boost<-predict(boost.class, newdata=test, n.trees=500, type = "response")
# 
# ##confusion matrix
# boost.tab<-table(pred.test, pred.boost>0.5)
# boost.tab
# ##test accuracy with boosting
# (boost.tab[1,1]+boost.tab[2,2])/sum(boost.tab) ##0.905. 

## Recursive Binary Splitting

##Use recursive binary splitting on training data
tree.class.train<-tree::tree(won_race ~ qual_gap + grid + WEATHER_WET +
                               circuitId + lat + lng + nationality, data=train)
summary(tree.class.train) ##16 terminal nodes! difficult to interpret

##plot tree
plot(tree.class.train)
text(tree.class.train, cex=0.6, pretty=0) ##Note: If there are categorical predictors, should have an additional argument: pretty=0 so R will use the category names in the tree

##find predicted classes for test data
tree.pred.test<-predict(tree.class.train, newdata=test, type="class") ##type="class" to get predicted class based on threshold of 0.5
head(tree.pred.test)

##find predicted probabilities for test data
pred.probs<-predict(tree.class.train, newdata=test)
head(pred.probs)

##confusion matrix for test data
table(pred.test, tree.pred.test) ##actual classes in rows, predicted classes in columns

##overall accuracy
mean(tree.pred.test==pred.test) ## 75%

##Prune tree##

##use CV
set.seed(2)
cv.class<-tree::cv.tree(tree.class.train, K=10, FUN=prune.misclass) ##FUN=prune.misclass so error rate is used to guide CV and pruning, rather than the deviance which is the default (and should not be used in classification).
cv.class

##plot of dev against size
plot(cv.class$size, cv.class$dev,type='b')

##size of tree chosen by pruning
trees.num.class<-cv.class$size[which.min(cv.class$dev)]
trees.num.class ##4 terminal nodes. A lot smaller than recursive binary splitting

##fit tree with size chosen by pruning
prune.class<-tree::prune.misclass(tree.class.train, best=trees.num.class)
prune.class

##plot pruned tree
plot(prune.class)
text(prune.class, cex=0.75, pretty=0)

##prediction based on pruned tree for test data
tree.pred.prune<-predict(prune.class, newdata=test, type="class")
##confusion matrix for test data
table(pred.test, tree.pred.prune)

##overall accuracy
mean(tree.pred.prune==pred.test) #75%

#Logistic Regression
LogReg.mod1 <- glm(won_race ~ qual_gap + grid + WEATHER_WET +
                     circuitId + lat + lng + nationality,
                   data = train, family = "binomial")
summary(LogReg.mod1)

##predicted survival rate for test data based on training data
preds<-predict(LogReg.mod1,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-ROCR::prediction(preds, test$won_race)
rates

##store the true positive and false postive rates
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values

##confusion matrix when threshold is 0.5
confusion.mat<-table(test$won_race,preds > 0.5)
confusion.mat

acc<-46/57
acc

# Accuracy of 81%

## Looking at the top 5 most raced at tracks 
top_5<-c("silverstone","hungaroring","monza","spa","monaco")

top_tracks<-model_f1_data %>% filter(CIRCUITREF %in% top_5)

top_tracks<-top_tracks %>% select(won_race,qual_gap,grid,
                                  WEATHER_WET,circuitId,
                                  lat,lng,YEAR)

top_tracks<-top_tracks %>% mutate(grid=as.factor(grid),
                                  circuitId=as.factor(circuitId))

train<-top_tracks %>% filter(YEAR<=2014) %>% select(-YEAR)
test<-top_tracks %>% filter(YEAR > 2014) %>% select(-YEAR)

#Logistic Regression
LogReg.mod2 <- glm(won_race ~.,
                   data = train, family = "binomial")
summary(LogReg.mod1)

##predicted survival rate for test data based on training data
preds<-predict(LogReg.mod2,newdata=test, type="response")

test$predict<-preds

##produce the numbers associated with classification table
rates<-ROCR::prediction(preds, test$won_race)
rates

##store the true positive and false postive rates
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values

##confusion matrix when threshold is 0.5
confusion.mat<-table(test$won_race,preds > 0.5)
confusion.mat

acc<-67/86
acc

circuit<-circuits %>% mutate(circuitId=as.factor(circuitId)) %>% 
  select(circuitId,name,country,location)

prob_summary<-test %>% filter(grid==1) %>% 
  group_by(circuitId) %>% summarise(avg_prob=sum(predict)/n()) %>% 
  left_join(circuit)

##MODEL 3

#Get number of pit stops for each driver each race
pit_stops2<-pit_stops %>% group_by(RACEID,DRIVERID) %>%
  arrange(desc(STOP)) %>% slice(1)

#Rename column
pit_stops2<-rename(pit_stops2, num_stops=STOP)

#Remove extra columns
pit_stops2<-pit_stops2 %>% select(-c(LAP,TIME,DURATION,MILLISECONDS))

top_tracks_pit <- model_f1_data %>% 
  left_join(pit_stops2,by=c("raceId"="RACEID","driverId"="DRIVERID")) %>% na.omit()

safety_cars<-rename(safety_cars, NAME=RACE)

top_tracks_pit_safe<-top_tracks_pit %>% left_join(safety_cars,by=c("YEAR","NAME"))

top_tracks_pit_safe[is.na(top_tracks_pit_safe)]<-0

top_tracks_pit_safe<-top_tracks_pit_safe %>% select(won_race,qual_gap,grid,
                                  WEATHER_WET,circuitId,
                                  lat,lng,YEAR,COUNT,num_stops)

top_tracks_pit_safe<-top_tracks_pit_safe %>% mutate(grid=as.factor(grid),
                                  circuitId=as.factor(circuitId),
                                  COUNT=as.factor(COUNT),
                                  num_stops=as.factor(num_stops))

train<-top_tracks_pit_safe %>% filter(YEAR != 2021) %>% select(-YEAR)
test<-top_tracks_pit_safe %>% filter(YEAR == 2021) %>% select(-YEAR)

#Logistic Regression
LogReg.mod3 <- glm(won_race ~.,
                   data = train, family = "binomial")
summary(LogReg.mod3)

##predicted survival rate for test data based on training data
preds<-predict(LogReg.mod3,newdata=test, type="response")

test$predict<-preds

##produce the numbers associated with classification table
rates<-ROCR::prediction(preds, test$won_race)
rates

##store the true positive and false postive rates
roc_result<-ROCR::performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-ROCR::performance(rates, measure = "auc")
auc@y.values

##confusion matrix when threshold is 0.5
confusion.mat<-table(test$won_race,preds > 0.5)
confusion.mat

acc<-41/54
acc

##Use recursive binary splitting on training data
tree.class.train<-tree::tree(won_race ~ ., data=train)
summary(tree.class.train) ##16 terminal nodes! difficult to interpret

##plot tree
plot(tree.class.train)
text(tree.class.train, cex=0.6, pretty=0) ##Note: If there are categorical predictors, should have an additional argument: pretty=0 so R will use the category names in the tree

##find predicted classes for test data
tree.pred.test<-predict(tree.class.train, newdata=test, type="class") ##type="class" to get predicted class based on threshold of 0.5
head(tree.pred.test)

##find predicted probabilities for test data
pred.probs<-predict(tree.class.train, newdata=test)
test$predict<-pred.probs
head(pred.probs)

pred.test<-test[,"won_race"]

##confusion matrix for test data
table(pred.test, tree.pred.test) ##actual classes in rows, predicted classes in columns

##overall accuracy
mean(tree.pred.test==pred.test) #78%

# Bagging
set.seed(222)
##bagging is special case of random forest when mtry = number of predictors
bag.class<-randomForest::randomForest(won_race ~ ., 
                                      data=train, mtry=8, importance=TRUE)
bag.class ##note with classification tree OOB estimates are provided

##importance measures of predictors
randomForest::importance(bag.class)
##graphical version
randomForest::varImpPlot(bag.class)

##test accuracy with bagging
pred.bag<-predict(bag.class, newdata=test)
##confusion matrix for test data
table(pred.test, pred.bag)
mean(pred.bag==pred.test) #69%

##Random Forest
set.seed(2222)
rf.class<-randomForest::randomForest(won_race ~ ., 
                                     data=train, mtry=3,importance=TRUE)
rf.class

importance(rf.class)
varImpPlot(rf.class)

##test accuracy with Random Forest
pred.rf<-predict(rf.class, newdata=test)
mean(pred.rf==pred.test) #70%

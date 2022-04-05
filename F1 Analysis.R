#Connor Nickol

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

f1_data2<-f1_data %>% filter(finish_position != "\\N",grid<4)

write.csv(f1_data2,"f1_data.csv",row.names=FALSE)

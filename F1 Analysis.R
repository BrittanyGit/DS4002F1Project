library(tidyverse)

circuits<-read.csv("circuits.csv")
con_results<-read.csv("constructor_results.csv")
con_standings<-read.csv("constructor_standings.csv")
driver_standings<-read.csv("driver_standings.csv")
drivers<-read.csv("drivers.csv")
lap_times<-read.csv("lap_times.csv")
pit_stops<-read.csv("pit_stops.csv")
qualifying<-read.csv("qualifying.csv")
races<-read.csv("races.csv")
results<-read.csv("results.csv")
seasons<-read.csv("seasons.csv")
status<-read.csv("status.csv")

circuits2<-circuits %>% select(-c(location,circuitRef,url))
drivers2<-drivers %>% select(-c(driverRef,number,dob,url))
races2<-races %>% select(-c(date,time,url))
results2<-results %>% select(-c(constructorId,number,positionText,
                                points,laps,time,milliseconds,
                                fastestLap,fastestLapTime,fastestLapSpeed,
                                rank))
qualifying2<- qualifying %>% select(-c(qualifyId,constructorId,number)) %>% filter(position <= 3)

qualifying2$q1<-gsub('[\\N]'," ",qualifying2$q1)
qualifying2$q2<-gsub('[\\N]'," ",qualifying2$q2)
qualifying2$q3<-gsub('[\\N]'," ",qualifying2$q3)
qualifying2$q1[qualifying2$q1=="  "]<-0
qualifying2$q2[qualifying2$q2=="  "]<-0
qualifying2$q3[qualifying2$q3=="  "]<-0
qualifying2$q1[qualifying2$q1==""]<-0
qualifying2$q2[qualifying2$q2==""]<-0
qualifying2$q3[qualifying2$q3==""]<-0

for (i in 1:nrow(qualifying2)){
  if(qualifying2$q3[i]==0){
    qualifying2$q3[i]<-qualifying2$q1[i]
  }
}

qualifying2<-qualifying2 %>% filter(q3 != 0) %>% select(-c(q1,q2))

qualifying2<-rename(qualifying2, qual_time=q3)
qualifying2<-rename(qualifying2, qual_positon=position)
results2<-rename(results2, finish_position=position)
circuits2<-rename(circuits2, circuit=name)

pit_stops2<-pit_stops %>% group_by(raceId,driverId) %>% 
  arrange(desc(stop)) %>% slice(1)

pit_stops2<-rename(pit_stops2, num_stops=stop)

pit_stops2<-pit_stops2 %>% select(-c(lap,time,duration,milliseconds))

formula_1_data<-qualifying2 %>% left_join(results2,by=c("raceId","driverId")) 

formula_1_data<-formula_1_data %>% filter(finish_position != "\\N")

formula_1_data<-formula_1_data %>% left_join(races2,by=c("raceId")) 

formula_1_data<-formula_1_data %>% left_join(circuits2,by=c("circuitId"))

formula_1_data<-formula_1_data %>% left_join(drivers2,by=c("driverId"))



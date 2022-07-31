compare_df_cols(a_2020 , b_2019_q4 , c_2019_q1 , return = "mismatch")
b_2019_q4 <- rename(b_2019_q4
                    ,ride_id = trip_id
                    ,rideable_type = bikeid 
                    ,started_at = start_time  
                    ,ended_at = end_time  
                    ,start_station_name = from_station_name 
                    ,start_station_id = from_station_id 
                    ,end_station_name = to_station_name 
                    ,end_station_id = to_station_id 
                    ,member_casual = usertype)


c_2019_q1 <- rename(c_2019_q1
                    ,,ride_id = trip_id
                    ,rideable_type = bikeid 
                    ,started_at = start_time  
                    ,ended_at = end_time  
                    ,start_station_name = from_station_name 
                    ,start_station_id = from_station_id 
                    ,end_station_name = to_station_name 
                    ,end_station_id = to_station_id 
                    ,member_casual = usertype)
compare_df_cols(a_2020 , b_2019_q4 , c_2019_q1 , return = "mismatch")
b_2019_q4 <- mutate(b_2019_q4 , ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type))
c_2019_q1 <- mutate(c_2019_q1 , ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type))
compare_df_cols(a_2020, b_2019_q4 , c_2019_q1  , return = "mismatch")
all_trips <- bind_rows(a_2020 , b_2019_q4 , c_2019_q1)
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender))
colnames(all_trips)
dim(all_trips)
head(all_trips)
summary(all_trips)
table(all_trips$member_casual)
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual , 
                                "Subscriber" = "member" , 
                                "Customer" = "casual"))
table(all_trips$member_casual)
all_trips$Date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$Date), "%m")
all_trips$day<- format(as.Date(all_trips$Date), "%d")
all_trips$year <- format(as.Date(all_trips$Date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$Date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at , all_trips$started_at)
str(all_trips)
is.factor(all_trips$ride_length)
is.numeric(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual , FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual , FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual , FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual , FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week , FUN = mean)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") 


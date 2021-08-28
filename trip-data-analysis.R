library('tidyverse')
library('lubridate')
library('hms')

# Store all the file names in a list
filenames <-
  list.files(path = "~/Projects/current-projects/data-analytics/case-study-1/data/raw-data-copy/",
             pattern = '*.csv',
             full.names = TRUE)

# Read all the files &
# store them in a list of data-frames
df_list <- lapply(filenames, read.csv)

# delete unnecessary columns from each data-frame
for (i in 1:length(df_list)) {
  df_list[[i]] <- df_list[[i]] %>%
    select(
      -c(
        ride_id,
        rideable_type,
        start_station_name,
        start_station_id,
        end_station_name,
        end_station_id,
        start_lat,
        start_lng,
        end_lat,
        end_lng
      )
    )
}

# merge all data-frames into 'year_data'
year_data <- Reduce(function(...)
  merge(..., all = TRUE), df_list)

# rename columns
year_data <-
  rename(
    year_data,
    start_time = started_at,
    end_time = ended_at,
    user_type = member_casual
  )

# convert column types
# from 'character' to 'datetime'
year_data <-
  transform(
    year_data,
    start_time = parse_date_time(start_time, 'ymd HMS'),
    end_time = parse_date_time(end_time, 'ymd HMS')
  )

# create a new column named `trip_duration` & `weekday`
year_data <-
  mutate(year_data, trip_duration = difftime(end_time, start_time), weekday = weekdays(start_time))

# remove observations with negative trip_duration
year_data <- year_data[!year_data$trip_duration < 0, ]

# Analysis

# visualize yearly trip count by user type
year_data %>% 
  group_by(user_type) %>% 
  summarise(trip_count = paste(round(n() / 1e6, 1), "M")) %>% 
  ggplot(aes(x = user_type, y = trip_count, fill = user_type)) + 
  geom_col(position = "dodge") +
  labs(title = 'Yearly trip count by user types') +
  xlab('User Types') +
  ylab('Total Trips')

# Visualize average trip count by user type for each week day
year_data %>%
  group_by(user_type, weekday) %>%
  summarise(trip_count = paste(round(n() / 1e3, 1), "K")) %>%
  arrange(user_type, weekday)  %>%
  ggplot(aes(x = weekday, y = trip_count, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(title = 'Average trip count by user type for each weekday') +
  xlab('Weekdays') +
  ylab('Trip Count')

# Visualize average trip duration by user type
year_data %>% 
  group_by(user_type) %>% 
  summarise(mean_trip_duration = mean(trip_duration))  %>%  
  ggplot(aes(x = user_type, y = as_hms(mean_trip_duration), fill = user_type)) + 
  geom_col(position = "dodge") +
  labs(title = 'Average trip duration by user types') +
  xlab('User Types') +
  ylab('Trip Duration')

# Visualize average trip duration by user type for each week day
year_data %>%
  group_by(user_type, weekday) %>%
  summarise(mean_trip_duration = mean(trip_duration)) %>%
  arrange(user_type, weekday)  %>%
  ggplot(aes(x = weekday, y = as_hms(mean_trip_duration), fill = user_type)) +
  geom_col(position = "dodge") +
  labs(title = 'Average trip duration by user type for each weekday') +
  xlab('Weekdays') +
  ylab('Trip Duration')











library(here)
library(tidyverse)
library(janitor)

ice_cover <- read_csv(here("data","ntl33_v7.csv"), na = "-999")

meteo_data <- read_csv(here("data","ntl20_v6.csv"))

## Ice duration across different lakes in Madison Lake area
#Lake id and ice duration

ggplot(ice_cover, aes(x = lakeid, y = ice_duration)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2)

#Find the mean ice cover duration in Madison Lake Area by year 

 mean_ice <- ice_cover %>% group_by(year4) %>% 
  summarize(mean_ice_cover = mean(ice_duration, na.rm = TRUE))
 # Create an exploratory visualization of mean ice cover duration by year for the Madison Lake Area. Add a sentence or two below this exploratory graph describing the overall trend(s) you observe.
 
 ggplot(mean_year,aes(x = year4, y = mean_ice_cover)) + geom_line()
 
 #Find the mean air temperature (using the daily average air temperature - adjusted) in Madison Lake Area by year using only observations from winter months (December, January, February)
 
 mean_airtemp_year <- meteo_data %>% 
   filter(month %in% c(12,1,2)) %>% 
   group_by(year4) %>% 
   summarize(mean_air_temp = mean(ave_air_temp_adjusted, na.rm =TRUE))
#Create an exploratory visualization of mean winter temperatures for the Madison Lake Area. Add a sentence or two below this exploratory graph describing the overall trend(s) you observe.
 
 ggplot(mean_airtemp_year, aes(x = year4, y = mean_air_temp)) + geom_line()
#Join the mean winter air temperatures to the mean ice cover duration data you found above
 
joined_data <- full_join(mean_ice,mean_airtemp_year, by = "year4") 

#Create an exploratory scatterplot of mean winter temperatures versus mean ice duration. Add a sentence or two below this exploratory graph describing the overall trend(s) you observe.

ggplot(joined_data, aes(x = mean_air_temp, y = mean_ice_cover)) + geom_point()

#Explore thaw dates since 1970. Create a subset of the ice cover duration data since 1970 (ok to include 1970), then convert the ice_off column to a Date. Use lubridate::yday() to pull the numeric day of the year from that Date. Create an exploratory plot of the ice off day (numeric day-of-the-year) from 1970 - present. Add a sentence or two below this exploratory graph describing the overall trend(s) you observe. 

ice_1970 <- ice_cover %>%
  filter(year4 >= "1970") %>% 
  mutate(ice_off = lubridate::ymd(ice_off)) %>% 
  mutate(day_ice_off = lubridate::yday(ice_off))

ggplot(data = ice_1970, aes(x = year4, y = day_ice_off)) +
  geom_point()
  
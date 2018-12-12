# plot the images
rm(list = ls())
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(lubridate)
library(here)
library(tidyr)
library(scales)
# set to
bb_id = "2586235"

accel = read_rds("accel.rds")
accel = accel %>% 
  rename(biobank_id = eid) %>% 
  select(biobank_id, 
         starts_with("nowear"), 
         starts_with("non_wear"),
         starts_with("average"))


four_hr = scale_x_datetime(
  date_breaks = "4 hours",
  labels = date_format("%I %p", tz = "UTC"))

transparent_legend =  theme(
  legend.background = element_rect(
    fill = "transparent"),
  legend.key = element_rect(
    fill = "transparent", 
    color = "transparent")
)

long = read_rds("bad_data_1440_goodQA.rds")

accel = accel %>% 
  filter(biobank_id %in% unique(long$biobank_id))

long = long %>% 
  select(biobank_id, day, `0`:`1439`, good) %>% 
  gather(time, value, `0`:`1439`)
long = long %>% 
  mutate(time = as.numeric(time),
         time_of_day = biobankr::min_to_time(time))
tz(long$time_of_day) = "UTC" 

df = long %>% 
  filter(biobank_id %in% bb_id) %>% 
  mutate(dd = paste0("Day ", day))


g = df %>% 
  ggplot(aes_string(x = "time_of_day", y = "value")) + 
  four_hr + 
  xlab("Time of Day") +
  ylab("Acceleration (milli-g)") + 
  theme(text = element_text(size = 16))  +
  geom_line()
gfacet = g + facet_wrap(~ dd, scales = "free_y")
gfacet

mn = 

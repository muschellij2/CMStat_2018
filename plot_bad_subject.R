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
library(matrixStats)
# set to

id_file = here("ids_with_good_qa.txt")

biobank_ids = readLines(id_file)
biobank_ids = as.integer(biobank_ids)

# need first_day.rds
# 

accel = read_rds("accel.rds")
accel = accel %>% 
  rename(biobank_id = eid) %>% 
  select(biobank_id, 
         starts_with("nowear"), 
         starts_with("non_wear"),
         starts_with("average"),
         contains("calibrated"))
accel = accel %>% 
  filter(biobank_id %in% biobank_ids)
accel = accel %>% 
  mutate(potentially_bad = 
           nowear_time_bias_adjusted_average_acceleration > 75 |
           nowear_time_bias_adjusted_acceleration_maximum > 5000)
all_accel = accel


four_hr = scale_x_datetime(
  date_breaks = "4 hours",
  labels = date_format("%I %p", tz = "UTC"))
# theme(axis.text.x = element_text(angle = 90, hjust = 1))
four_hr2 = scale_x_datetime(
  date_breaks = "4 hours",
  labels = date_format("%H", tz = "UTC"))

transparent_legend =  theme(
  legend.background = element_rect(
    fill = "transparent"),
  legend.key = element_rect(
    fill = "transparent", 
    color = "transparent")
)

long = read_rds("bad_data_1440_goodQA.rds")

xx = rowMaxs(as.matrix(long[, as.character(0:1439)]), na.rm = TRUE)
rs = rowSums(as.matrix(long[, as.character(0:1439)]) > 4000, na.rm = TRUE)

# 5 minutes
plot_ids = unique(long$biobank_id[ rs > 10])


accel = accel %>% 
  filter(biobank_id %in% unique(long$biobank_id))

long = long %>% 
  select(biobank_id, day, `0`:`1439`, good) %>% 
  gather(time, value, `0`:`1439`)
long = long %>% 
  mutate(time = as.numeric(time),
         time_of_day = biobankr::min_to_time(time))
tz(long$time_of_day) = "UTC" 

long = long %>% 
  mutate(dd = paste0("Day ", day))

check_ids = accel$potentially_bad & 
  accel$biobank_id %in% biobank_ids

# bb_id = "2586235"
# bb_id = "1224022"
bb_id = sample(plot_ids, size = 1)
# 3462861_daily_plot - own
# 2586235_daily_plot - else
for (bb_id in plot_ids) {
  df = long %>% 
    filter(biobank_id %in% bb_id) 
  
  accel_subj = accel %>% 
    filter(biobank_id %in% bb_id) %>% 
    mutate(calib = paste0("Calibrated on ", 
                          ifelse(data_quality_calibrated_on_own_data == "Yes", 
                                 "Own Data",
                                 "Someone Else's Data")))  
  
  table(all_accel$data_quality_calibrated_on_own_data)
  
  g = df %>% 
    ggplot(aes_string(x = "time_of_day", y = "value")) + 
    four_hr2 + 
    xlab("Time of Day") +
    ylab("Acceleration (milli-g)") + 
    theme(text = element_text(size = 16))  +
    geom_line()
  gfacet_free = g + facet_wrap(~ dd, scales = "free_y")
  gfacet = g + facet_wrap(~ dd)
  
  
  pngname = here("images", paste0(bb_id, "_daily_plot.png"))
  png(pngname, height = 5, width = 10, res = 300, units = "in")
  print(gfacet + ggtitle(paste0("Participant ", bb_id, " ", accel_subj$calib)))
  dev.off()
  
  pngname = here("images", paste0(bb_id, "_daily_plot_free.png"))
  png(pngname, height = 5, width = 10, res = 300, units = "in")
  print(gfacet_free  + ggtitle(paste0("Participant ", bb_id, " ", accel_subj$calib)))
  dev.off()
  
}




gbase = all_accel %>% 
  ggplot() + 
  ylab("Number of Subjects") 
gmax = gbase +
  geom_histogram(aes(x = nowear_time_bias_adjusted_acceleration_maximum)) +
  xlab("Max Acceleration (milli-g) while 'Wearing Device'")

gcal = all_accel %>% 
  ggplot() + 
  ylab("Number of Subjects")  +
  geom_histogram(aes(x = nowear_time_bias_adjusted_acceleration_maximum))+
  facet_wrap(~ data_quality_calibrated_on_own_data,
             ncol = 1, scales = "free_y")
gcal

all_accel = all_accel %>% 
  mutate(calib = paste0("Calibrated on ", 
                        ifelse(data_quality_calibrated_on_own_data == "Yes", 
                               "Own Data",
                               "Someone Else's Data")))
gcalib = all_accel %>% 
  ggplot() + 
  ylab("Number of Subjects")  +
  facet_wrap(~ calib,
             ncol = 1, scales = "free_y")

pngname = here("images", "nowear_average_by_calibration.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")

calib_avg = gcalib + 
  geom_histogram(aes(x = nowear_time_bias_adjusted_average_acceleration)) +
  xlab("Mean Acceleration (milli-g) while 'Wearing Device'")
print(calib_avg)
dev.off()
pngname = here("images", "nowear_max_by_calibration.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
calib_max = gcalib + 
  geom_histogram(aes(x = nowear_time_bias_adjusted_acceleration_maximum)) +
  xlab("Max Acceleration (milli-g) while 'Wearing Device'")  
print(calib_max)
dev.off()



table(all_accel$potentially_bad, all_accel$data_quality_calibrated_on_own_data)


table(all_accel$data_quality_calibrated_on_own_data)

gmax

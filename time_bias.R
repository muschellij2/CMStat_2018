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

accel = read_rds("accel.rds")

accel = accel %>% 
  rename(biobank_id = eid)
stopifnot(all(biobank_ids %in% accel$biobank_id))
accel = accel %>% 
  filter(biobank_id %in% biobank_ids) %>% 
  select(-dob)


# need first_day.rds
first_file = "first_day.rds"
first_day = readRDS(first_file)

stopifnot(all(biobank_ids %in% first_day$biobank_id))

first_day = first_day[ 
  first_day$biobank_id %in% biobank_ids,]
first_day = first_day %>% 
  mutate(accel_date = ymd(accel_date))


demog = read_csv("demographics_reformat_last_visit.csv.gz")
stopifnot(all(biobank_ids %in% accel$biobank_id))
demog = demog %>% 
  filter(biobank_id %in% biobank_ids)

df = full_join(demog, 
               accel %>% 
                 select(biobank_id, start_time_of_wear, 
                        date_of_attending_assessment_centre))
# rm(demog)
# rm(accel)

df = left_join(
  df, 
  first_day, 
  by = "biobank_id")

df = df %>% 
  mutate(
    diff_days = difftime(accel_date, date_of_attending_assessment_centre,
                         units = "days"),
    diff_years = as.numeric(diff_days/365.25),
    diff_days_wear = difftime(start_time_of_wear, 
                              date_of_attending_assessment_centre,
                         units = "days"),
    diff_years_wear = as.numeric(diff_days_wear/365.25),
    off = difftime(start_time_of_wear, 
                   accel_date,
                   units = "days")
  )
stopifnot(!any(abs(as.numeric(df$off)) > 1))

mn_time = mean(df$diff_years)

pngname = here("images", "time_diff.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
g = df %>% 
  ggplot(aes(x = diff_years)) + 
  geom_histogram() + 
  ylab("Number of Participants") +
  xlab("Assessment to Accelerometry (Years)") +
  theme(text = element_text(size = 20)) +
  geom_vline(aes(xintercept = mn_time), colour = "red",
             size = 2)
print(g)
dev.off()  


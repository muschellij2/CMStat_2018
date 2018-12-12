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

id_file = here("ids_with_good_qa.txt")

biobank_ids = readLines(id_file)
biobank_ids = as.integer(biobank_ids)

outfile = here("all_long_summary_data.rds")
fdf = read_rds(outfile)
fdf = as_data_frame(fdf)
fdf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good, nobad) %>% 
  summarize(max(value))

outfile = here("all_long_full_data.rds")
long = read_rds(outfile)

# drop_bad = TRUE
# app = ""
# if (drop_bad) {
#   app = "nobad_"
# }
# 
# out_pdf = here(paste0(app, "test_plots.pdf") )

# pdf(out_pdf)
start_date = biobankr::min_to_time(600)
tz(start_date) = "UTC"
fdf = fdf %>% 
  mutate(dd = paste0("Day ", day),
         measure = recode(measure,
                          q50 = "median"))

long = mapply(
  function(x, y) {
    colnames(x)[1] = y
    x = x %>% 
      mutate(dd = paste0("Day ", day),
             measure = recode(measure,
                              q50 = "median"))
  }, long, names(long), SIMPLIFY = FALSE)

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

######################################
# Start the Plots
######################################
run_measure = "Mean"
mn = fdf %>% 
  filter(measure %in% tolower(run_measure)) %>% 
  arrange(day, time, measure, nobad)

set_g = function(data, colour = NULL) {
  data %>% 
    ggplot(aes_string(x = "time_of_day", y = "value", colour = colour)) + 
    four_hr + 
    xlab("Time of Day") +
    ylab(paste0(run_measure, " Acceleration (milli-g)")) + 
    theme(text = element_text(size = 16))  +
    geom_line()
}
g = mn %>% set_g(colour = "nobad")
gfacet = g + facet_wrap(~ dd, scales = "free_y")
gfacet
day0 = mn %>% 
  filter( day == 0 | day == 7)
(gfacet + 
    geom_vline(aes(xintercept = start_date), 
               color = "red", 
               size = 2)) %+% day0
mn = mn %>% 
  filter( day %in% 1:6)


varname = "sex"
l = long[[varname]] %>% 
  filter(measure %in% tolower(run_measure),
         day %in% 1:6)
s = l %>% set_g(colour = varname)
sfacet = s + facet_wrap(~ dd, scales = "free_y")

mn = mn %>% 
  filter( !(time_of_day < start_date & day == 0))
long = lapply(long, function(x) {
  x = x %>% 
    filter( !(time_of_day < start_date & day == 0))
  x
})
gfacet %+% mn
sfacet %+% l


# dev.off()

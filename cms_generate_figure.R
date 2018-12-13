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
                          q50 = "median"),
         Group = ifelse(nobad,
                            "Restrictive Maximum Group",
                            "Full Group"))

long = mapply(
  function(x, y) {
    colnames(x)[1] = y
    x = x %>% 
      mutate(dd = paste0("Day ", day),
             measure = recode(measure,
                              q50 = "median"),
             Group = ifelse(nobad,
                                "Restrictive Maximum Group",
                                "Full Group"))
  }, long, names(long), SIMPLIFY = FALSE)

four_hr = scale_x_datetime(
  date_breaks = "4 hours",
  labels = date_format("%H:%M", tz = "UTC"))

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
# nobad is removing the "bad_ids"
# sub_good is 1368 minute cutoff
# how the f does day 0 have data with nogood?

run_measure = "Mean"
mn = fdf %>% 
  filter(measure %in% tolower(run_measure)) %>% 
  arrange(day, time, measure, sub_good, nobad)


# sub_good has no discernable effect
# other than take out days 0 and 7!
# but nobad does
# nobad is removing the "bad_ids"
# sub_good is 1368 minute cutoff
# how the f does day 0 have data with nogood?
mn = mn %>% 
  filter(!sub_good)

set_g = function(data, colour = NULL) {
  data %>% 
    ggplot(aes_string(x = "time_of_day", y = "value", colour = colour)) + 
    four_hr + 
    xlab("Time of Day") +
    ylab(paste0(run_measure, " Acceleration (milli-g)")) + 
    theme(text = element_text(size = 16))  +
    geom_line()
}
# g = mn %>% set_g(colour = "nobad")
g = mn %>% 
  filter(!nobad) %>% 
  set_g() +
  transparent_legend 
gfacet = g + facet_wrap(~ dd, scales = "free_y")

pngname = here("images", "daily_plot.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
print(gfacet)
dev.off()

pngname = here("images", "day1_plot.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
print(gfacet)
dev.off()

day0 = mn %>% 
  filter( day == 0 | day == 7)
(g + facet_wrap(~ dd, scales = "free_y", ncol = 1) + 
    geom_vline(aes(xintercept = start_date), 
               color = "red", 
               size = 1.5)) %+% day0


gres = mn %>% 
  set_g(colour = "Group") +
  transparent_legend + 
  theme(legend.position = c(0.5, 0.8))  + 
  guides(colour = guide_legend(title = "", direction = "horizontal"))




gfacet = gres + facet_wrap(~ dd, scales = "free_y")
gfacet
mn = mn %>% 
  filter( day %in% 1:6)
g = mn %>% 
  filter(!nobad) %>% 
  set_g()
gfacet = g + facet_wrap(~ dd, scales = "free_y")

g = mn %>% 
  filter(nobad) %>% 
  set_g()
gfacet = g + facet_wrap(~ dd, scales = "free_y")


g = mn %>% 
  filter(!sub_good) %>% 
  set_g()
gfacet = g + facet_wrap(~ dd, scales = "free_y")

g = mn %>% 
  filter(sub_good) %>% 
  set_g()
gfacet = g + facet_wrap(~ dd, scales = "free_y")



varname = "age_accel"
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

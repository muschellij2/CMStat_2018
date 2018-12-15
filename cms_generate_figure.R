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

outfile = here("all_long_overall_data.rds")
odf = read_rds(outfile)
odf = as_data_frame(odf)
stopifnot(!"day" %in% colnames(odf))
odf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good, nobad) %>% 
  summarize(max(value))


no7 = here("nobad_overall_noday7_1440_goodQA.rds")
no7 = read_rds(no7)
no7  = as_data_frame(no7)
no7 = no7 %>% 
  gather(measure, value,
         q0,q25,q50,q75,q100,sd,mean,min,max,n)
no7 = no7 %>% 
  mutate(time_of_day = biobankr::min_to_time(time, tz = ""))
tz(no7$time_of_day) = "UTC"    
no7$nobad = TRUE
no7 = no7 %>% 
  mutate(measure = recode(measure,
                          q50 = "median"),
         Group = ifelse(nobad,
                        "Restrictive Maximum Group",
                        "Full Group"))

outfile = here("all_long_summary_data.rds")
fdf = read_rds(outfile)
fdf = as_data_frame(fdf)
fdf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good, nobad) %>% 
  summarize(max(value))

outfile = here("all_long_full_data.rds")
long = read_rds(outfile)

outfile = here("longoverall_full_data.rds")
olong = read_rds(outfile)

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

odf = odf %>% 
  mutate( measure = recode(measure,
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

omn = odf %>% 
  filter(measure %in% tolower(run_measure)) %>% 
  arrange(time, measure, sub_good, nobad)

ono7 = no7 %>% 
  filter(measure %in% tolower(run_measure))  

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
    four_hr2 +  
    xlab("Hour of Day") +
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

o = omn %>% 
  filter(nobad) %>% 
  filter(!sub_good)
tz(o$time_of_day) = "UTC"

sd = biobankr::min_to_time(1) 
tz(sd) = "UTC"
sd = sd - 55000

ed = sd + 8000

pngname = here("images", paste0(run_measure, "_overall_plot.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
print({
  o %>% set_g() 
})
dev.off()

o = omn %>% 
  filter(nobad) %>% 
  mutate(keep = ifelse(
    sub_good,
    "Days with ≥ 95% good minutes",
    "All days"
  ))


pngname = here("images", paste0(run_measure, "_overall_plot_good.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
gg = o %>% set_g(colour = "keep")
gg =  gg + transparent_legend + 
  theme(legend.position = c(0.6, 0.35))  + 
  guides(colour = guide_legend(title = "")) +
  scale_colour_manual(values = c("red", "blue")) 
# gg = gg +     annotate(
#       "rect", 
#       xmin = sd,
#       xmax = ed,
#       ymin  = 45 , 
#       ymax = 52, 
#       alpha = 0.2, 
#       color = "black")
print(gg)
dev.off()


pngname = here("images", paste0(run_measure, "_daily_plot.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
gg = gfacet +  annotate(
  "rect", 
  xmin = sd,
  xmax = ed,
  ymin  = 45 , 
  ymax = 52, 
  alpha = 0.2, 
  color = "black")
print(gg)
dev.off()

pngname = here("images", "day1_plot.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
day0 = mn %>% 
  filter( day == 0 | day == 7)
gg = (g + facet_wrap(~ dd, scales = "free_y", ncol = 1) + 
    geom_vline(aes(xintercept = start_date), 
               color = "red", 
               size = 1.5)) %+% day0
gg = gg + annotate(
  "rect", 
  xmin = sd,
  xmax = ed,
  ymin  = 45 , 
  ymax = 52, 
  alpha = 0.2, 
  color = "black")
print(gg)
dev.off()

mn = mn %>% 
  filter( !(time < 600 & day == 0))

pngname = here("images", "daily_plot_correct.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
gg = gfacet %+% mn
gg = gg +  annotate(
  "rect", 
  xmin = sd,
  xmax = ed,
  ymin  = 45 , 
  ymax = 52, 
  alpha = 0.2, 
  color = "black")
print(gg)
dev.off()




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


levs = c( "[38,44]", "(44,54]", "(54,64]", "(64,74]", "(74,84]")
cols = c( "#FFFFBF", "#D7191C", "#FDAE61",  "#ABD9E9", "#2C7BB6")
names(cols) = levs
varname = "age_accel"
l = long[[varname]] %>% 
  filter(measure %in% tolower(run_measure),
         day %in% 1:6)
l$age_accel = factor(l$age_accel, levels = levs)
s = l %>% set_g(colour = varname)
s = s + scale_colour_manual(values = cols, drop = FALSE) + ylim(0, 65)
sfacet = s + facet_wrap(~ dd, scales = "free_y")

pngname = here("images", paste0(varname, "_daily_plot_1_6.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
ss = sfacet +  transparent_legend + 
  theme(legend.position = "top")  + 
  guides(colour = guide_legend(title = "Age at Accelerometry",
                               direction = "horizontal",
                               nrow = 2)) 

print(ss)
dev.off()

l = l %>% 
  filter(!age_accel %in% "[38,44]")

pngname = here("images", paste0(varname, "_daily_plot_1_6_drop_age.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
print({
  ss %+% l
})
dev.off()



varname = "age_assessment"
l = long[[varname]] %>% 
  filter(measure %in% tolower(run_measure),
         day %in% 1:6)
l$age_assessment = factor(l$age_assessment, levels = levs)
stopifnot(!any(is.na(l$age_assessment)))
s = l %>% set_g(colour = varname)
s = s + scale_colour_manual(values = cols, drop = FALSE) + ylim(0, 65)
sfacet = s + facet_wrap(~ dd, scales = "free_y")

pngname = here("images", paste0(varname, "_daily_plot_1_6.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
srun = sfacet +  transparent_legend + 
  theme(legend.position = "top")  + 
  guides(colour = guide_legend(title = "Age at Assessment",
                               direction = "horizontal",
                               nrow = 2)) 
print(srun)
dev.off()


o = ono7 %>% 
  filter(nobad) 
tz(o$time_of_day) = "UTC"

pngname = here("images", paste0(run_measure, "_overall_noday7_plot.png"))
png(pngname, height = 5, width = 10, res = 300, units = "in")
print({
  o %>% set_g() 
})
dev.off()


xx = no7 %>% 
  filter(measure %in% c("mean", "median"))

pngname = here("images", "overall_noday7_plot.png")
png(pngname, height = 5, width = 10, res = 300, units = "in")
print({
  xx %>% set_g("measure") + transparent_legend + 
    theme(legend.position = c(0.6, 0.25))  + 
    guides(colour = guide_legend(title = "Statistic")) +
    scale_colour_manual(values = c("red", "blue"))  
})
dev.off()


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

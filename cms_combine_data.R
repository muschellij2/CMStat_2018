# combine data
rm(list = ls())
library(dplyr)
library(here)
library(purrr)
library(readr)


drop_bad = TRUE
app = ""
if (drop_bad) {
  app = "nobad_"
}
print(app)
outfile = here(paste0(app, "long_summary_data.rds"))
bad_fdf = read_rds(outfile)
bad_fdf = as_data_frame(bad_fdf)
bad_fdf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good) %>% 
  summarize(max(value))

outfile = here(paste0(app, "long_overall_data.rds"))
bad_odf = read_rds(outfile)
bad_odf = as_data_frame(bad_odf)
bad_odf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good) %>% 
  summarize(max(value))

outfile = here(paste0(app, "long_full_data.rds"))
bad_long = read_rds(outfile)
bad_maxs = lapply(bad_long, function(xx) {
  cn = colnames(xx)
  x = xx %>% 
    filter(measure == "max") %>% 
    group_by_("sub_good", cn[1]) %>% 
    summarize(max(value))
  print(x)
  x
})

drop_bad = FALSE
app = ""
if (drop_bad) {
  app = "nobad_"
}
print(app)
outfile = here(paste0(app, "long_summary_data.rds"))
fdf = read_rds(outfile)
fdf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good) %>% 
  summarize(max(value))
fdf = full_join(fdf, bad_fdf)
outfile = here("all_long_summary_data.rds")
write_rds(fdf, outfile, compress = "xz")

outfile = here(paste0(app, "long_overall_data.rds"))
odf = read_rds(outfile)
odf = as_data_frame(odf)
odf %>% 
  filter(measure == "max") %>% 
  group_by(sub_good) %>% 
  summarize(max(value))
odf = full_join(odf, bad_odf)
outfile = here("all_long_overall_data.rds")
write_rds(odf, outfile, compress = "xz")

outfile = here(paste0(app, "long_full_data.rds"))
long = read_rds(outfile)
good_maxs = lapply(long, function(xx) {
  cn = colnames(xx)
  x = xx %>% 
    filter(measure == "max") %>% 
    group_by_("sub_good", cn[1]) %>% 
    summarize(max(value))
  print(x)
  x
})

long = mapply(function(x, y) {
  full_join(x, y)
}, long, bad_long, SIMPLIFY = FALSE)
lapply(long, function(xx) {
  cn = colnames(xx)
  x = xx %>% 
    filter(measure == "max") %>% 
    group_by_("sub_good", "nobad", cn[1]) %>% 
    summarize(max(value))
  print(x)
  x
})

outfile = here("all_long_full_data.rds")
write_rds(long, outfile, compress = "xz")


# combined_file = here(paste0(app, "threshold_summary_1440_goodQA.rds"))
# com = read_rds(combined_file)
# com$nobad = FALSE
# com = full_join(com, bad_com)
# x = com %>% 
#   filter(measure == "max") %>% 
#   group_by_("sub_good", "nobad", cn[1]) %>% 
#   summarize(max(value))
# print(x)
# 
# outfile = here("all_threshold_data.rds")
# write_rds(com, outfile, compress = "xz")

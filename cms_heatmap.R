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

df = read_rds("sample_200_goodQA.rds")
ss = split(df[ as.character(0:1439)], df$biobank_id)
bb_mat = t(sapply(ss, colMeans, na.rm = TRUE))
rownames(bb_mat) = names(ss)

day_mats = split(df[ as.character(0:1439)], df$day)
day_mats = lapply(day_mats, as.matrix)



long = df %>% 
  gather(time, value, `0`:`1439`) %>%
  mutate(time = as.numeric(time)) 
avg = long %>% 
  group_by(day, time) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  mutate(time_of_day = biobankr::min_to_time(time))  
tz(avg$time_of_day) = "UTC"

avg_bb = long %>%   
  group_by(biobank_id, time) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  mutate(time_of_day = biobankr::min_to_time(time))  
tz(avg_bb$time_of_day) = "UTC"


long = long %>% 
  mutate(time_of_day = biobankr::min_to_time(time))
tz(long$time_of_day) = "UTC"


g = avg %>% 
  ggplot(aes(x = time_of_day, y = value)) +
  geom_line() + 
  guides(colour = FALSE) + facet_wrap(~ day)
g


g = long %>% 
  filter(day == 1) %>% 
  ggplot(aes(x = time_of_day, y = value, colour = factor(biobank_id))) +
  geom_line() + 
  guides(colour = FALSE)
g


mat = day_mats[[which(names(day_mats) == 2)]]
ord = rowCumsums( (mat > 50) * 1)
ord = apply(ord, 1, function(x) {
  which(x > 100)[1]
})
ord = order(ord)
mat = mat[ord,]
mat = mat[, 1200:1440]
pheatmap::pheatmap(cluster_rows = FALSE, cluster_cols = FALSE, log10(mat + 1))

pheatmap::pheatmap(cluster_rows = FALSE, cluster_cols = FALSE, log10(bb_mat+1))

library(biobankr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(here)
library(tidyr)
library(gridExtra)
library(grid)

options(digits.secs = 2)
x = read_cwa(file = "1234520_90001_0_0.cwa.gz", 
             end = 30000, tz = "UTC") 
x = x$data
x = x %>% 
  select(time, x, y, z)
x = x %>% 
  mutate(norm = (sqrt(x^2 + y^2 + z^2) - 1) * 1000)

transparent_legend =  theme(
  legend.background = element_rect(
    fill = "transparent"),
  legend.key = element_rect(
    fill = "transparent", 
    color = "transparent")
)

x$sec = lubridate::floor_date(x$time, unit = "second")
x$five = lubridate::floor_date(x$time, unit = "5 second")

usec = unique(x$sec)
usec = sort(usec)
# 10 seconds 
# 2 minutes
vals = usec[56:(56 + 120 - 1)]
take_sec = range(vals)
df = x %>% 
  filter( sec <= take_sec[2] & sec >= take_sec[1])
sec = df %>%
  group_by(sec) %>%
  summarise_at(.vars = vars(norm),
               .funs = list(mean = mean, median = median),
               na.rm = TRUE)
five = df %>%
  group_by(five) %>%
  summarise_at(.vars = vars(norm),
               .funs = list(mean = mean, median = median),
               na.rm = TRUE)

five_sec = sec %>%
  mutate(five = lubridate::floor_date(sec, unit = "5 second") ) %>% 
  group_by(five) %>%
  rename(mn = mean, med = median) %>% 
  summarise_at(.vars = vars(mn, med),
               .funs = list(mean = mean, median = median),
               na.rm = TRUE)

min = sec %>%
  mutate(min = lubridate::floor_date(sec, unit = "1 minute")) %>% 
  group_by(min) %>%
  rename(mn = mean, med = median) %>% 
  summarise_at(.vars = vars(mn, med),
               .funs = list(mean = mean, median = median),
               na.rm = TRUE)


add_labs = function(x) {
  x = x + 
    ylab(NULL) + 
    ylim(80, 110) + 
    theme(text = element_text(size = 24)) +
    theme(axis.text.x = element_blank())
  x
}

gg = ggplot(df, aes(x = factor(sec), y = norm)) + 
  geom_boxplot() + xlab("Second-level") 
gg = gg %>% 
  add_labs
gall = gg 


long = sec %>% 
  gather(var, value, mean, median)
gsec = ggplot(long, aes(x = factor(sec), 
                        y = value, 
                        colour = var)) + 
  geom_point() + 
  transparent_legend + 
  guides(colour = guide_legend(title = "Statistic")) + 
  theme(legend.position = c(0.65, 0.35)) + 
  xlab("Second-level") 
gsec = gsec %>% add_labs()


g5 = ggplot(five_sec, aes(x = factor(five), y = mn_mean)) + 
  geom_point() + 
  theme(axis.text.x = element_blank()) + 
  xlab("5 second-level") 
g5 = g5 %>% add_labs()

gmin = ggplot(min, aes(x = factor(min), y = mn_mean)) + 
  geom_point() + 
  theme(axis.text.x = element_blank()) + 
  xlab("Minute-level")   
gmin = gmin %>% add_labs()

gres = grid.arrange(gall,
                    gsec,
                    g5, gmin, 
                    left = textGrob("Acceleration (milli-g)", 
                                    rot = 90, vjust = 1,
                                    gp = gpar(fontsize = 24)),
                    ncol = 1)


pngname = here("images", "varying_levels.png")
png(pngname, height = 10, width = 7, res = 300, units = "in")
grid.draw(gres)
dev.off()


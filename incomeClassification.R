options(warn = -1)
options(scipen = 10000)
options(digits = 3)
options(repr.plot.width = 14, repr.plot.height = 9.5)


library(tidytuesdayR)
library(lubridate)
library(tidyverse)
library(skimr)
library(scales)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(devtools)
library(factoextra)
library(viridis)
library(ggrepel)
library(lubridate)

theme_Jungwon = theme(legend.position = "bottom",
                      legend.direction = "horizontal",
                      legend.background = element_rect(fill = "gray91"),
                      legend.key = element_rect(fill = "gray91"),
                      legend.title = element_text(size = 12.5, colour = "gray5", family = "NanumGothic"),
                      legend.text = element_text(size = 12, colour = "gray11", family = "NanumGothic"), 
                      
                      
                      axis.text = element_text(size = 12, colour = "gray15", family = "NanumGothic"),
                      axis.title = element_text(size = 12.5, colour = "gray15", family = "NanumGothic"),
                      axis.line = element_line(size = 0.3, colour = "gray25"),
                      
                      plot.caption = element_text(color = "gray70", size = 11.5),
                      plot.background = element_rect(fill = "gray91"), 
                      plot.title = element_text(size = 21, colour = "gray15", family = "NanumGothic"),
                      plot.subtitle = element_text(size = 16, colour = "gray42", family = "NanumGothic"),
                      
                      strip.background = element_rect(fill = "gray75"),
                      strip.text = element_text(size = 13, colour = "gray25", face = "bold", family = "NanumGothic"),
                      
                      panel.grid.major = element_line(colour = "gray95"),
                      panel.background = element_rect(fill = "gray91"))

theme_set(theme_grey(base_family='NanumGothic'))



setwd('/Users/jungwonwoo/Desktop/program_file')
tuesdata <- tidytuesdayR::tt_load('2021-02-09')

home_owner = tuesdata$home_owner
income_aggregate = tuesdata$income_aggregate
income_distribution = tuesdata$income_distribution
income_limits = tuesdata$income_limits
income_mean = tuesdata$income_mean
income_time = tuesdata$income_time
lifetime_wealth = tuesdata$lifetime_wealth
lifetime_earn = tuesdata$lifetime_earn
race_wealth = tuesdata$race_wealth
retirement = tuesdata$retirement
student_debt = tuesdata$student_debt

# income mean first.
income_mean %>% head()

income_mean %>%
  select(year) %>% range() # 1967 to 2019

income_mean %>%
  group_by(year, income_quintile) %>%
  summarise(meanIncome = mean(income_dollars)) %>%
  ggplot(aes(x = year, y = log(meanIncome), group = income_quintile, color = income_quintile)) +
  geom_line()+
  facet_wrap(~income_quintile, scales = 'free') +
  theme_Jungwon


income_mean %>%
  select(year, dollar_type, income_dollars) %>%
  pivot_wider(values_from = 'income_dollars',
              names_from  = 'dollar_type',
              values_fn = mean) %>%
  mutate(valueRatio = round(`2019 Dollars`/`Current Dollars`,2)) %>%
  as.data.frame() %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = `Current Dollars`, color = 'Blue')) +
  geom_line(aes(y = `2019 Dollars`, color = 'Red')) +
  geom_ribbon(aes(ymin = `Current Dollars`, ymax = `2019 Dollars`), fill = "grey", alpha=0.5) +
  theme_Jungwon
  

income_mean %>%
  select(year, dollar_type, income_dollars) %>%
  pivot_wider(values_from = 'income_dollars',
              names_from  = 'dollar_type',
              values_fn = mean) %>%
  mutate(valueRatio = round(`2019 Dollars`/`Current Dollars`,2)) %>%
  as.data.frame() %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = valueRatio)) +
  theme_Jungwon


income_mean %>%
  select(year, dollar_type, income_dollars) %>%
  pivot_wider(values_from = 'income_dollars',
              names_from  = 'dollar_type',
              values_fn = mean) %>%
  select(year, `2019 Dollars`) %>%
  as.data.frame()


income_distribution %>%
  as.data.frame()

library(tidyverse)
library(skimr)
library(scales)

getmode = function(vec) {
  uniq = unique(vec)
  uniq[which.max(tabulate(match(vec, uniq)))]
}

AirTrain = read.csv("/Users/jungwonwoo/Desktop/program_file/AirBnBPrice/train.csv")
AirTest = read.csv("/Users/jungwonwoo/Desktop/program_file/AirBnBPrice/test.csv")
submission = read.csv("/Users/jungwonwoo/Desktop/program_file/AirBnBPrice/sample_submission.csv")



neighbourhood_summary = AirTrain %>%
  select(host_id, neighbourhood, room_type, price, number_of_reviews, availability_365) %>%
  group_by(neighbourhood) %>%
  summarise(meanPrice = mean(price),
            varPrice = (sd(price)^2),
            meanReviewNum = mean(number_of_reviews),
            varReviewNum = (sd(number_of_reviews)^2),
            meanAvailability = mean(availability_365),
            varAvailability = (sd(availability_365)^2),
            modeRoomType = getmode(room_type),
            n = n()) %>%
  arrange(desc(meanPrice)) %>%
  filter(n >= 500) %>%
  as.data.frame()

neighbourhood_summary 
  
AirTrain %>%
  ggplot(aes(price, fill = neighbourhood_group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(fill = NULL, x = "price per night")

AirTrain %>%
  ggplot(aes(longitude, latitude, color = log(price))) +
  geom_point(alpha = 0.15) +
  scale_color_viridis_c()

AirTrain %>%
  ggplot(aes(longitude, latitude, z = log(price))) +
  stat_summary_hex(alpha = 0.8, bins = 70) +
  scale_fill_viridis_c() +
  labs(fill = "Mean price (log)")

AirTrain %>% colnames()
AirTest %>% colnames()

target = AirTrain %>% select(price)
AirTrain = AirTrain %>% select(-price)

combined = AirTrain %>%
  bind_rows(AirTest) %>%
  mutate(idx = seq(1, dim(combined)[1]))

combined %>%
  filter(number_of_reviews == 0) %>%
  mutate(last_review = 0,
         reviews_per_month = 0) %>%
  bind_rows(combined %>%
              filter(number_of_reviews != 0) %>%
              mutate(last_review = as.numeric(as.Date(last_review, origin = '1900-01-01')),
                     last_review = scales::rescale(last_review, to = c(1, 10)))) %>%
  arrange(idx)

AirTrain %>%
  select(neighbourhood_group, latitude, longitude) %>%
  mutate(price = target$price,
         latitude = scales::rescale(latitude, to = c(1, 10)),
         longitude = scales::rescale(longitude, to = c(1, 10))) %>% 
  group_by(neighbourhood_group) %>%
  summarise(meanLat = mean(latitude),
            meanLon = mean(longitude),
            meanPrice = mean(price)) %>% as.data.frame() %>%
  mutate(EucDist = round((6.727255 - meanLat)^2 + (5.591585 -  meanLon)^2, 5))
  
# as expected Manhattan's mean price was the highest among all neighborhood. 
# the assumption is if the further distance between the main point(Manhattan) is, the less meanPrice would be detected.

distance_price = AirTrain %>%
  select(neighbourhood, latitude, longitude) %>%
  mutate(price = target$price,
         latitude = scales::rescale(latitude, to = c(1, 10)),
         longitude = scales::rescale(longitude, to = c(1, 10))) %>% 
  group_by(neighbourhood) %>%
  summarise(meanLat = mean(latitude),
            meanLon = mean(longitude),
            meanPrice = mean(price),
            n = n()) %>%
  as.data.frame() %>%
  mutate(EucDist = ((5.681137 - meanLat)^2 + (5.030463 -  meanLon)^2))


summary(lm(distance_price$meanPrice ~ distance_price$EucDist)) # actually the p value is significant.
# Tribeca has the highest. 5.681137 5.030463

library(broom)

AirTrain %>%
  select(neighbourhood, latitude, longitude) %>%
  mutate(price = target$price,
         latitude = scales::rescale(latitude, to = c(1, 10)),
         longitude = scales::rescale(longitude, to = c(1, 10))) %>% 
  mutate(EucDist = ((5.681137 - latitude)^2 + (5.030463 -  longitude)^2)) %>%
  group_by(neighbourhood) %>%
  do(fitneighbourhood = tidy(lm(price ~ EucDist, data = .))) %>%
  unnest(fitneighbourhood) %>%
  filter(term != '(Intercept)',
         p.value <= 0.05) %>% 
  as.data.frame()

# load libraries
library(tidyverse)
library(rio)

# import data
df <- import("The Knot Atlanta Competition.csv")

# how many have five stars? 61 or 76.3%
five_stars <- df %>%
  filter(star_count == 5)

five_stars_count <- nrow(five_stars)
five_stars_perc <- nrow(five_stars)/nrow(df)

# average rating
mean_rating <- mean(df$star_count)
median_rating <- median(df$star_count)

hist(df$star_count, labels = TRUE, ylim = c(0, 90))

# how many are on the first page? 30 or 37.5%
first_page <- df %>%
  filter(page_num == 1)

# count by price category
price_cats <- df %>%
  group_by(price_category) %>%
  summarize(count = n())

price_cats <- price_cats %>%
  mutate(percent = count/80*100)

# histogram of number of reviews
hist(df$review_count, labels = TRUE, ylim = c(0, 60), 
     breaks = seq(min(df$review_count), max(df$review_count), length.out = 11))

plot(cut(df$review_count, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90,
                                     100, 110, 120, 130, 140, 150, 160, 170,
                                     180, 190, 200, 210, 220)))
review_bar <- df %>%
  mutate(bin = case_when(review_count <= 10 ~ "0-10",
                         review_count > 10 & review_count <= 20 ~ "11-20",
                         review_count > 20 & review_count <= 30 ~ "21-30",
                         review_count > 30 & review_count <= 40 ~ "31-40",
                         review_count > 40 & review_count <= 50 ~ "41-50",
                         review_count > 50 & review_count <= 60 ~ "51-60",
                         review_count > 60 & review_count <= 70 ~ "61-70",
                         review_count > 70 & review_count <= 80 ~ "71-80",
                         review_count > 80 & review_count <= 90 ~ "81-90",
                         review_count > 90 & review_count <= 100 ~ "91-100",
                         review_count > 100 & review_count <= 110 ~ "101-110",
                         review_count > 110 & review_count <= 120 ~ "111-120",
                         review_count > 120 & review_count <= 130 ~ "121-130",
                         review_count > 130 & review_count <= 140 ~ "131-140",
                         review_count > 140 & review_count <= 150 ~ "141-150",
                         review_count > 150 & review_count <= 160 ~ "151-160",
                         review_count > 160 & review_count <= 170 ~ "161-170",
                         review_count > 170 & review_count <= 180 ~ "171-180",
                         review_count > 180 & review_count <= 190 ~ "181-190",
                         review_count > 190 & review_count <= 200 ~ "191-200",
                         review_count > 200 ~ "201 and above")) %>%
  group_by(bin) %>%
  summarize(count = n()) %>%
  mutate(bin = factor(bin,
                      levels = c("0-10",
                                 "11-20",
                                 "21-30",
                                 "31-40",
                                 "41-50",
                                 "51-60",
                                 "61-70",
                                 "71-80",
                                 "81-90",
                                 "91-100",
                                 "101-110",
                                 "111-120",
                                 "121-130",
                                 "131-140",
                                 "141-150",
                                 "151-160",
                                 "161-170",
                                 "171-180",
                                 "181-190",
                                 "191-200",
                                 "201 and above"))) %>%
  arrange(bin)

ggplot(review_bar) +
  geom_col(aes(x = bin, y = count)) +
  theme_classic()








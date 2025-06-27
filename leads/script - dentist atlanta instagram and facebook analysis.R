# load libraries
library(tidyverse)
library(rio)
library(rjson)
library(httr)

# import data
df <- import("dentist atlanta website analysis.csv")

# we only need instagram and facebook
df <- df %>%
  select(index, name, insta, fb)

# remove from the list if they don't have an instagram or fb
df <- df %>%
  filter(insta != "" & fb != "")

# add https://www. to instagram and facebook links
df <- df %>%
  mutate(insta = paste0("https://www.", insta),
         fb = paste0("https://", fb))



header <- c("User-Agent" = "Mozilla/5.0")


check <- GET("https://i.instagram.com/api/v1/users/web_profile_info/?username=google", headers = header)
check <- content(check, as = "text")
export(data.frame(check), "check.txt")

check <- GET("https://www.instagram.com/colonysquaredental/", headers = header)
check <- content(check, as = "text")
export(data.frame(check), "check.txt")












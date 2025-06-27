# load libraries
library(tidyverse)
library(rio)
library(rvest)

# create list of urls
urls <- c("https://www.theknot.com/marketplace/wedding-videographers-atlanta-ga?sort=featured",
          "https://www.theknot.com/marketplace/wedding-videographers-atlanta-ga?page=2&sort=featured",
          "https://www.theknot.com/marketplace/wedding-videographers-atlanta-ga?page=3&sort=featured")

# get html
html_list <- lapply(urls, read_html)

# get list of html classes on page
classes <- html_list[[1]] %>%
  html_nodes("*") %>%
  html_attr("class") %>%
  unique() %>%
  data.frame()

# get vendor names, locations, price category, rating, and number of review
node_list <- lapply(html_list, html_nodes, ".vendor-name--47aa1, 
                    .location-text--7c71e, 
                    .star-count--18678, 
                    .review-count--2d8bd, 
                    .secondary-info--22778")

text_list <- lapply(node_list, as.character)

df_list <- lapply(text_list, data.frame)

# create data_type column
df_list <- lapply(df_list, mutate, 
                  data_type = case_when(str_detect(`X..i..`, "vendor-name") ~ "vendor",
                                        str_detect(`X..i..`, "location") ~ "location",
                                        str_detect(`X..i..`, "star-count") ~ "star_count",
                                        str_detect(`X..i..`, "review-count") ~ "review_count",
                                        str_detect(`X..i..`, "secondary-info") ~ "price_category"))

# create page_num column
pages <- 1:length(df_list)

df_list <- mapply(cbind, df_list, page_num = pages, SIMPLIFY = FALSE)

# combine into one dataframe
df <- bind_rows(df_list)

# create id column
df <- df %>%
  mutate(group_id = case_match(data_type,
                               "location" ~ 1,
                               "star_count" ~ 2,
                               "review_count" ~ 3,
                               "vendor" ~ 4,
                               "price_category" ~ 5))

df <- df %>%
  mutate(vendor_id = case_when(group_id == 1 ~ row_number(),
                               TRUE ~ NA_integer_))

df <- df %>%
  fill(vendor_id, .direction = "down")

df <- df %>%
  select(-group_id)

# create vendor, location, star_count, and review_count columns
df <- df %>%
  pivot_wider(names_from = data_type,
              values_from = `X..i..`)

df <- df %>%
  mutate(vendor_id = row_number())

# clean up vendor column
df <- df %>%
  mutate(vendor = str_replace_all(vendor, "^.*?>", ""),
         vendor = str_replace_all(vendor, "<\\/div>", ""),
         vendor = str_replace_all(vendor, "&amp;", "&"),
         vendor = str_replace_all(vendor, "\\n", ""))

# clean up location column
df <- df %>%
  mutate(location = str_replace_all(location, "^.*?>", ""),
         location = str_replace_all(location, "^<.*?>", ""),
         location = str_replace_all(location, "<.*?>", ""),
         location = str_replace_all(location, "\\n", ""),
         location = str_replace_all(location, "&amp;", "&"))

# clean up star_count column
df <- df %>%
  mutate(star_count = str_extract_all(star_count, ">\\d{1}\\.\\d{1}<|>\\d{1}<"),
         star_count = str_replace_all(star_count, ">|<", ""))

# clean up review_count column
df <- df %>%
  mutate(review_count = str_extract_all(review_count, ">\\d+<"),
         review_count = str_replace_all(review_count, ">|<", ""))

# clean up price category
df <- df %>%
  mutate(price_category = str_replace_all(price_category, "<.*?>", ""),
         price_category = str_replace_all(price_category, " – ", " - "))

# export to Excel
export(df, "The Knot Atlanta Competition.csv")
  
  
  
  
  
  
  










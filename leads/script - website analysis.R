# load libraries
library(tidyverse)
library(rio)
library(rvest)

# input exact search terms
search_terms <- "car detailing atlanta"

# import data
df <- import(paste0(search_terms, ".csv"))

# remove businesses without websites
df <- df %>%
  filter(url != "no website found")

# get emails from each website if there is one
html_list <- lapply(seq_along(df$url), function(i) {
  tryCatch(
    {
      url <- df$url[i]
      x <- read_html(url)
      list(html = x, index = i)
    },
    error = function(e) {
      message(paste0("Error with URL: ", url, " -> ", e$message))
      return(NULL)
    }
  )
})

# remove null values
html_list <- compact(html_list)

# get email if there is one
email_list <- lapply(seq_along(html_list), function(i) {
      x <- html_text(html_list[[i]][[1]])                   # Extract text content from HTML
      x <- str_extract(x, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")  # Use a regex to extract the email address
      list(email = x, index = html_list[[i]][[2]])
})

# remove elements with no email
email_list <- Filter(function(x) !any(is.na(x)), email_list)

# add emails to original data frame
df <- df %>%
  mutate(index = row_number())

emails <- bind_rows(email_list)

df <- full_join(df, emails)

# get instagram link if there is one
insta_list <- lapply(seq_along(html_list), function(i) {
  x <- html_nodes(html_list[[i]][[1]], "a")
  x <- html_attr(x, "href")
  x <- str_extract_all(x, "instagram.com/.*")
  x <- x[lapply(x, length) > 0]
  x <- Filter(function(x) !any(is.na(x)), x)
  x <- paste0(x)
  list(insta = x, index = html_list[[i]][[2]])
})

# remove elements with no instagram link
new_insta_list <- list()

for (i in seq_along(insta_list)) {
  if (length(insta_list[[i]]$insta) != 0) {
    new_insta_list[[i]] <- insta_list[[i]]
  }
}

insta_list <- new_insta_list

rm(new_insta_list)

insta_list <- insta_list[lapply(insta_list, length) > 0]

# create insta data frame
instas <- bind_rows(insta_list)

# remove duplicates
instas <- unique(instas)

# combine with original data frame
df <- full_join(df, instas)

# some websites have multiple insta links, combine those with semi-colon
df <- df %>%
  group_by(index, name, url, description, email) %>%
  summarize(insta = paste0(insta, collapse = "; ")) %>%
  ungroup()

# get facebook link if there is one
fb_list <- lapply(seq_along(html_list), function(i) {
  x <- html_nodes(html_list[[i]][[1]], "a")
  x <- html_attr(x, "href")
  x <- str_extract_all(x, "facebook.com/.*")
  x <- x[lapply(x, length) > 0]
  x <- Filter(function(x) !any(is.na(x)), x)
  x <- paste0(x)
  list(fb = x, index = html_list[[i]][[2]])
})

# remove elements with no facebook link
new_fb_list <- list()

for (i in seq_along(fb_list)) {
  if (length(fb_list[[i]]$fb) != 0) {
    new_fb_list[[i]] <- fb_list[[i]]
  }
}

fb_list <- new_fb_list

rm(new_fb_list)

fb_list <- fb_list[lapply(fb_list, length) > 0]

# create facebook data frame
fbs <- bind_rows(fb_list)

# remove duplicates
fbs <- unique(fbs)

# combine with original data frame
df <- full_join(df, fbs)

# some websites have multiple facebook links, combine those with semi-colon
df <- df %>%
  group_by(index, name, url, description, email, insta) %>%
  summarize(fb = paste0(fb, collapse = "; ")) %>%
  ungroup()

# get video link if there is one
video_list <- lapply(seq_along(html_list), function(i) {
  x <- html_nodes(html_list[[i]][[1]], "*")
  x <- html_attrs(x)
  x <- str_extract_all(x, "https:.*\\.mp4")
  x <- x[lapply(x, length) > 0]
  x <- Filter(function(x) !any(is.na(x)), x)
  x <- paste0(x)
  list(video = x, index = html_list[[i]][[2]])
})

# remove elements with no video link
new_video_list <- list()

for (i in seq_along(video_list)) {
  if (length(video_list[[i]]$video) != 0) {
    new_video_list[[i]] <- video_list[[i]]
  }
}

video_list <- new_video_list

rm(new_video_list)

video_list <- video_list[lapply(video_list, length) > 0]

# create video data frame
videos <- bind_rows(video_list)

# remove duplicates
videos <- unique(videos)

# combine with original data frame
df <- full_join(df, videos)

# move index column all the way to the left
df <- df %>%
  relocate(index)

# some websites have multiple video links, combine those with semi-colon
df <- df %>%
  group_by(index, name, url, description, email, insta, fb) %>%
  summarize(video = paste0(video, collapse = "; ")) %>%
  ungroup()

# export to csv file
export(df, paste0(search_terms, " website analysis.csv"))






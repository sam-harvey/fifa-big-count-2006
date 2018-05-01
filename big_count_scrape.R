library(checkpoint)

checkpoint(snapshotDate = "2018-01-01",
           R.version = "3.3.3",
           scanForPackages = TRUE)

library(tidyverse)
library(rvest)

#https://web.archive.org/web/20160304131953/http://www.fifa.com/worldfootball/bigcount/allplayers.html
html.bc = read_html("https://web.archive.org/web/20160304131953/http://www.fifa.com/worldfootball/bigcount/allplayers.html")

df.big_count = html.bc %>%
  html_nodes("table") %>% 
  html_table(header = TRUE) %>% 
  .[[1]] %>% 
  as.tbl()

names(df.big_count) = gsub(" ",
                           "_",
                           names(df.big_count))

#Where is this from?
df.country_iso = read_csv("country_data.csv")

#Where is this from?
df.confederation = read_csv("country_confederation.csv")

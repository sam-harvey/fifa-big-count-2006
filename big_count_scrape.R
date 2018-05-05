library(checkpoint)

checkpoint(snapshotDate = "2018-01-01",
           R.version = "3.3.3",
           scanForPackages = TRUE)

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(rvest)
library(stringdist)

#https://web.archive.org/web/20160304131953/http://www.fifa.com/worldfootball/bigcount/allplayers.html
html.bc = read_html("https://web.archive.org/web/20160304131953/http://www.fifa.com/worldfootball/bigcount/allplayers.html")

df.big_count = html.bc %>%
  html_nodes("table") %>% 
  html_table(header = TRUE) %>% 
  .[[1]] %>% 
  as.tbl()

names(df.big_count) = gsub(" ", "_", names(df.big_count))
names(df.big_count) = tolower(names(df.big_count))

# #Where is this from?
# df.country_iso = read_csv("country_data.csv")

# https://en.wikipedia.org/wiki/ISO_3166-1
# Have to get this from wiki because ISO charges for their standards, weird.

html.iso = read_html("https://en.wikipedia.org/w/index.php?title=ISO_3166-1&oldid=833704837")

df.iso_country = html.iso %>% 
  html_nodes("table") %>% 
  .[[3]] %>% 
  html_table(header = TRUE) %>% 
  as.tbl 

names(df.iso_country) = c("name",
                          "alpha_2_code",
                          "alpha_3_code",
                          "numeric_code",
                          "link_to_iso_3166_2_subdivision_codes",
                          "independent")

df.iso_country = df.iso_country %>% 
  #Fix names with accents
  mutate(name = case_when(grepl("!", name) ~ str_extract(name, "(?<=!)(.)*"),
                          TRUE ~ name)) %>% 
  #code independent as boolean
  mutate(independent = case_when(independent == "Yes" ~ TRUE,
                                 independent == "No" ~ FALSE))

#There are a lot of unmatched countries
df.iso_country %>% 
  full_join(df.big_count,
            by = c('name' = "association")) %>% 
  filter(is.na(alpha_3_code) |
           is.na(registered_players))

#Use fuzzy string matching rather than doing this manually
vt.to_match = df.big_count %>% 
  anti_join(df.iso_country,
            by = c("association" = "name")) %>% 
  pull(association)

vt.unmatched = df.iso_country %>% 
  anti_join(df.big_count,
            by = c("name" = "association")) %>% 
  pull(name)

vt.lcs_dist = stringdist(vt.to_match[1], vt.unmatched,
           method = "osa")

vt.unmatched[which(vt.lcs_dist == min(vt.lcs_dist))]

#Idea for each country to match (from big count data) find the closest string
#Among all these filter where the same unmatched country (from ISO data) has the minimum distance
#Rinse and repeat with countries to match removed from previous stage

#The Jaro distance seems to handle this best
mat.dist = stringdistmatrix(a = vt.to_match,
                            b = vt.unmatched,
                            method = 'jw')

rownames(mat.dist) = vt.to_match
colnames(mat.dist) = vt.unmatched

df.match_dist = mat.dist %>% 
  as.data.frame() %>% 
  mutate(to_match = rownames(.)) %>% 
  gather(unmatched, distance, -to_match)

df.matched = data_frame()

while(dim(df.match_dist)[1] > 0){
  
  df.new_matches = df.match_dist %>%
    group_by(to_match) %>% 
    filter(distance == min(distance)) %>% 
    group_by(unmatched) %>%
    filter(distance == min(distance))
  
  df.matched = bind_rows(df.matched,
                         df.new_matches)
  
  df.match_dist = df.match_dist %>% 
    filter(!(to_match %in% df.new_matches$to_match)) %>%
    filter(!(unmatched %in% df.new_matches$unmatched))
  
}

#Manual edits to where our fuzzy-match didn't work
df.matched



# #Where is this from?
# df.confederation = read_csv("country_confederation.csv")

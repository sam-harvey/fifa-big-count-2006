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

#Convert all counts to numeric
df.big_count = df.big_count %>% 
  mutate_at(-1,
            function(x){
              as.numeric(
                gsub(",", "", x)
              )
            })

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
  #Remove footnotes
  mutate(name = case_when(grepl("\\[", name) ~ str_extract(name, "(.)*(?=\\[)"),
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

#Manual edits to where our fuzzy matching didn't work
df.matched = df.matched %>% 
  #Careful exact distance ties lead to duplicates
  group_by(to_match) %>% 
  slice(1) %>% 
  ungroup %>% 
  mutate(unmatched = case_when(to_match == "Cape Verde Islands" ~ "Cabo Verde",
                               to_match %in% c("England", 
                                               "Scotland",
                                               "Northern Ireland",
                                               "Wales") ~ "United Kingdom of Great Britain and Northern Ireland",
                               to_match == "Iran" ~ "Iran (Islamic Republic of)",
                               to_match == "Republic of Ireland" ~ "Ireland",
                               to_match == "Netherlands Antilles" ~ "Bonaire, Sint Eustatius and Saba",
                               to_match == "FYR Macedonia" ~ "Macedonia (the former Yugoslav Republic of)",
                               to_match == "Tahiti" ~ "French Polynesia",
                               to_match == "Chinese Taipei" ~ "Taiwan, Province of China",
                               TRUE ~ unmatched)) %>% 
  select(-distance) 

#Player counts at an ISO country level. In effect, other than labels the only change is that british countries are aggregated to the UK
df.iso_big_count = df.big_count %>% 
  left_join(df.matched,
            by = c("association" = "to_match")) %>% 
  mutate(iso_country_name = case_when(is.na(unmatched) ~ association,
                              T ~ unmatched)) %>% 
  inner_join(df.iso_country,
             by = c('iso_country_name' = 'name')) %>% 
  group_by(iso_country_name, association, alpha_2_code, alpha_3_code) %>% 
  summarise_if(is.numeric,
               sum) %>% 
  ungroup

#Get the countries belonging to each association

#AFC
html.afc = read_html("https://en.wikipedia.org/w/index.php?title=Asian_Football_Confederation&oldid=840965559")

df.afc = html.afc %>% 
  html_nodes("table") %>% 
  .[[4]] %>% 
  html_table() %>% 
  setNames(., gsub(" ", "_", tolower(names(.))))

#CAF
html.caf = read_html("https://en.wikipedia.org/w/index.php?title=Confederation_of_African_Football&oldid=835538543")

df.caf = html.caf %>% 
  html_nodes("table") %>% 
  .[[6]] %>% 
  html_table() %>% 
  setNames(., gsub(" ", "_", tolower(names(.))))

#CONCACAF
html.concacaf = read_html("https://en.wikipedia.org/w/index.php?title=CONCACAF&oldid=840709849")

df.concacaf = html.concacaf %>% 
  html_nodes('table') %>% 
  .[[5]] %>% 
  html_table() %>% 
  setNames(., gsub(" ", "_", tolower(names(.))))

#UEFA
html.uefa = read_html("https://en.wikipedia.org/w/index.php?title=UEFA&oldid=840719285")

df.uefa = html.uefa %>% 
  html_nodes('table') %>% 
  .[[5]] %>% 
  html_table() %>% 
  setNames(., gsub(" ", "_", tolower(names(.))))

#CONMEBOL
html.conmebol = read_html("https://en.wikipedia.org/w/index.php?title=CONMEBOL&oldid=840486130")

df.conmebol = html.conmebol %>% 
  html_nodes('table') %>% 
  .[[4]] %>% 
  html_table() %>% 
  setNames(., gsub(" ", "_", tolower(names(.))))

html.ofc = read_html("https://en.wikipedia.org/w/index.php?title=Oceania_Football_Confederation&oldid=838931579")

df.ofc = html.ofc %>% 
  html_nodes('table') %>% 
  .[[4]] %>% 
  html_table() %>% 
  setNames(., gsub(" ", "_", tolower(names(.))))

#Put each confederation into the same format and bind them all
df.afc = df.afc %>% 
  filter(!str_detect(association, "\\(")) %>% 
  mutate(association = str_extract(association, "[^\\[]*")) %>% 
  select(association, code) %>% 
  mutate(confederation = "AFC")

df.caf = df.caf %>% 
  filter(!str_detect(association, "\\(|-")) %>% 
  mutate(association = str_extract(association, "[^0-9]*")) %>% 
  select(association, code) %>% 
  mutate(confederation = "CAF")

df.conmebol = df.conmebol %>% 
  select(association = country,
         code = association) %>% 
  mutate(confederation = "CONMEBOL")

df.uefa = df.uefa %>% 
  select(association,
         code) %>% 
  mutate(confederation = "UEFA")
  
df.ofc = df.ofc %>% 
  mutate(association = str_extract(association, "[^\\[]*")) %>% 
  select(association, code) %>% 
  mutate(confederation = "OFC")

df.concacaf = df.concacaf %>% 
  filter(!str_detect(association, "\\(")) %>%
  mutate(association = str_extract(association, "[^\\[]*")) %>% 
  select(association, code) %>% 
  mutate(confederation = "CONCACAF")

df.confederations = bind_rows(df.afc,
                              df.caf,
                              df.conmebol,
                              df.uefa,
                              df.ofc,
                              df.concacaf)

#Need to do another round of fixing for matching the confederation association and the big count / iso name.

df.confederations %>% 
  full_join(df.iso_big_count,
            by = c("association")) %>% 
  filter(is.na(iso_country_name) |
           is.na(code))

df.confederations %>% 
  full_join(df.iso_big_count,
            by = c("association" = "iso_country_name")) %>% 
  filter(is.na(alpha_3_code) |
           is.na(code))

df.confederations %>% 
  full_join(df.iso_big_count,
            by = c("code" = "alpha_3_code")) %>% 
  filter(is.na(iso_country_name) |
           is.na(confederation))

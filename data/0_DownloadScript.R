library(tidyverse)
library(rvest)

url_data <- read_html("http://www.dailyscript.com/scripts/jurassicpark_script_final_12_92.html")

script <- url_data %>% 
  html_text() %>% 
  str_split("\n")

script_df <- tibble(raw = script[[1]]) %>% 
  mutate(Part = case_when(
    grepl("\t\t\t\t", raw, fixed=TRUE) ~ "SPEAKER",
    grepl("\t\t", raw, fixed=TRUE) ~ "LINE",
    grepl("\t", raw, fixed=TRUE) ~ "DIRECTION",
    TRUE ~ ""
  )) %>% 
  filter(raw != "\r") %>% 
  #Clean up raw
  mutate(raw = gsub("\\n|\\t|\\r", "", raw)) %>% 
  filter(raw != "") %>% 
  #Clean up the few instances where LINE is labled DIRECTION
  mutate(Part = ifelse(lag(Part) == "SPEAKER", "LINE", Part)) %>% 
  mutate(Part = ifelse(lag(raw) == "CUT TO:", "SCENE", Part)) %>% 
  mutate(Part = ifelse(!is.na(as.numeric(substr(raw, 1, 2))) | !is.na(as.numeric(substr(raw, 1, 3))), "SCENE", Part))

saveRDS(script_df, "data/Script.RDS")

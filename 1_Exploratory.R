####
# Jurassic Park EDA
####
library(tidytext)
library(tidyverse)
library(stringr)

script_raw <- readRDS("data/Script.RDS")

script <- script_raw[17:nrow(script_raw),] %>% 
  filter(Part %in% c("SCENE", "SPEAKER", "LINE", "DIRECTION")) %>% 
  mutate(Part = ifelse(raw %in% c("120 MILES WEST OF COSTA RICA", "10,000 VOLTS!\""), "DIRECTION", Part)) %>% 
  #SCENE
  mutate(Scene = ifelse(Part == "SCENE", raw, NA)) %>% 
  fill(Scene, .direction = "down") %>% 
  mutate(Scene_num = ifelse(Part == "SCENE", 1, 0)) %>% 
  mutate(Scene_num = cumsum(Scene_num)) %>% 
  #SPEAKING
  separate(raw, c("Speaker", "Speaker_detail"), sep = "\\(", remove=F) %>% 
  mutate(Speaker = ifelse(Part == "SPEAKER", Speaker, NA),
         Speaker = toupper(trimws(Speaker)),
         Speaker_detail = ifelse(Part == "SPEAKER", gsub(")", "", Speaker_detail, fixed=T), NA)) %>% 
  #--typos
  mutate(Speaker = case_when(
    Speaker == "VOLUNTERR" ~ "VOLUNTEER",
    Speaker == "MALCCOLM" ~ "MALCOLM",
    Speaker == "MALCOLMGENNARO" ~ "GENNARO",
    TRUE ~ Speaker
  )) %>% 
  fill(Speaker, Speaker_detail, .direction = "down") %>% 
  mutate_at(vars(Speaker, Speaker_detail), funs(ifelse(Part %in% c("DIRECTION", "SCENE"), NA, .)))


#Data for SCENE analysis - when i watch the movie

write.csv(unique(script$Speaker), "Characters.csv", row.names = F)

write.csv(unique(script$Scene), "SceneDetail.csv", row.names = F)

#LINES
lines <- script %>% 
  filter(Part == "LINE") %>% 
  unnest_tokens(word, raw, drop=FALSE)

lines_sent <- lines %>%
  anti_join(stop_words) %>% 
  inner_join(sentiments %>% filter(lexicon=="bing") %>% select(word, sentiment))

lines_chart <- lines_sent %>% 
  count(Scene_num, sentiment) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n))

ggplot(lines_chart, aes(x = Scene_num, y = n, fill=sentiment)) + 
  geom_col()

#####
# IDEAS
####

# Map of character paths on island. including dinos and deaths

# feminist comments! "WOman inherits" "we can discuss sexism in survival"
# Lex as the hacker

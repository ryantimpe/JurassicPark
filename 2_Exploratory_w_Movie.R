####
# Jurassic Park EDA
####
library(tidytext)
library(tidyverse)
library(stringr)

script_raw <- readRDS("data/Script.RDS")
movie_raw <- readxl::read_xlsx("data/SceneDetail.xlsx")

##
# Script
##
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

##
# Movie
##

movie <- movie_raw %>% 
  filter(Scene_Dup != 0 | is.na(Scene_Dup)) %>% 
  arrange(T_h, T_m, T_s) %>%
  mutate(T_cums = T_h*60*60 + T_m*60 + T_s) %>% 
  mutate(Scene_length = lead(T_cums) - T_cums)

character_paths <- movie %>% 
  select(MovieScene, Scene_length, dplyr::contains("Location"), 
         dplyr::starts_with("T_"), dplyr::starts_with("C_")) %>% 
  gather(Character, in_scene, dplyr::starts_with("C_")) %>% 
  filter(!is.na(in_scene)) %>% 
  mutate(Character = gsub("C_", "", Character, fixed=TRUE)) %>% 
  group_by(Character) %>% 
  mutate(Location_unique = Location2 != lag(Location2)) %>% 
  ungroup()

sel_chars <- c("LEX", "GRANT", "ELLIE", "HAMMOND", "MALCOLM", 
               "HARDING","GENNARO", 
               "MULDOON", "JOPHREY", "ARNOLD", "NEDRY"
               )

movie_location_id <- movie %>%
  select(Location, Location2) %>% 
  distinct() %>% 
  left_join(select(., Location) %>% distinct() %>% mutate(Location_id = row_number())) %>% 
  left_join(select(., Location2) %>% distinct() %>% mutate(Location2_id = row_number())) %>% 
  mutate(Location_index = Location_id * 100 + Location2_id) %>% 
  select(-dplyr::contains("_id"))

character_paths_chart <- character_paths %>% 
  filter(Character %in% sel_chars) %>% 
  left_join(movie_location_id) %>% 
  mutate(y = factor(paste(Location_index, Location2)))

ggplot(character_paths_chart, aes(x=T_cums, y = y, group = Character, color = Character)) +
  geom_line(size = 2, alpha = 0.5)

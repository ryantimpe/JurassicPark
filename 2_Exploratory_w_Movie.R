####
# Jurassic Park EDA
####
library(tidytext)
library(tidyverse)
library(stringr)

script_raw <- readRDS("data/Script.RDS")
movie_raw <- readxl::read_xlsx("data/SceneDetail.xlsx")

location_map <- read_csv("data/Movie_locations.csv")

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
  filter(Character %in% c("GRANT")) %>% 
  left_join(movie_location_id) %>% 
  mutate(y = factor(paste(Location_index, Location2)))

ggplot(character_paths_chart, aes(x=T_cums, y = y, group = Character, color = Character)) +
  geom_line(size = 2, alpha = 0.5)

character_paths_map <- character_paths %>% 
  select(-Scene_length, -T_h, -T_m, -T_s) %>% 
  left_join(location_map, by = c("Location", "Location2")) %>%
  #Want a complete time for each Character * map
  # group_by(Character, Loc_map) %>% 
  # complete(T_cums = min(T_cums):max(T_cums)) %>% 
  # fill(MovieScene, Location, Location2, in_scene, Location_unique,
  #      dplyr::starts_with("Loc_x"), dplyr::starts_with("Loc_y")) %>% 
  # ungroup() 
  gather(Coord_detailed, coord, 
         dplyr::starts_with("Loc_x"), dplyr::starts_with("Loc_y")) %>% 
  mutate(Loc_map2 = substr(Coord_detailed, 6, 6),
         Coord_dim = substr(Coord_detailed, 5, 5)) %>% 
  select(-Coord_detailed) %>% 
  spread(Coord_dim, coord) %>% 
  filter(!is.na(x)) %>% 
  arrange(Character, Loc_map2, T_cums) %>% 
  group_by(Character,  Loc_map2, x, y) %>% 
  summarize(char_time = max(T_cums)) %>% 
  ungroup() %>% 
  arrange(char_time) %>% 
  group_by(Character, Loc_map2) %>% 
  mutate(diff = lead(char_time) - char_time) %>% 
  mutate(diff = ifelse(is.na(diff), 60, diff))

location_map2 <- location_map %>% 
  gather(Coord_detailed, coord, 
         dplyr::starts_with("Loc_x"), dplyr::starts_with("Loc_y")) %>% 
  mutate(Loc_map2 = substr(Coord_detailed, 6, 6),
         Coord_dim = substr(Coord_detailed, 5, 5)) %>% 
  select(-Coord_detailed) %>% 
  spread(Coord_dim, coord) %>% 
  filter(!is.na(x)) %>% 
  group_by( Loc_map2, x, y) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-Loc_map)

character_paths_map2 <- character_paths_map %>% 
  left_join(location_map2)

ggplot(character_paths_map2 %>% filter(Character == "NEDRY"), 
       aes(x=x, y = y,
            color = char_time)) +
  geom_line(size = 2, alpha = 0.5) +
  geom_point(aes(size = diff), color = "#FF4040") +
  geom_label(aes(label = Location2), alpha = .5) +
  facet_grid(.~Loc_map2) +
  coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10))

###
# Character paths
###


library(tidytext)
library(tidyverse)
library(stringr)


location_map <- read_csv("data/Movie_locations.csv") 
movie_raw <- readxl::read_xlsx("data/SceneDetail.xlsx")

movie <- movie_raw %>% 
  filter(Scene_Dup != 0 | is.na(Scene_Dup)) %>% 
  arrange(T_h, T_m, T_s) %>%
  mutate(T_cums = T_h*60*60 + T_m*60 + T_s) %>% 
  mutate(T_length = lead(T_cums) - T_cums) 

character_paths_map <- movie %>% 
  select(MovieScene, dplyr::contains("Location"), 
         T_cums, T_length, dplyr::starts_with("C_")) %>% 
  gather(Character, in_scene, dplyr::starts_with("C_")) %>% 
  filter(!is.na(in_scene)) %>% 
  mutate(Character = gsub("C_", "", Character, fixed=TRUE)) %>% 
  # filter(Character == "NEDRY") %>% 
  #Coords of each location over 3 maps
  left_join(location_map, by = c("Location", "Location2")) %>% 
  gather(Coord_detailed, coord, 
         dplyr::starts_with("Loc_x"), dplyr::starts_with("Loc_y")) %>% 
  mutate(Loc_map = substr(Coord_detailed, 6, 6),
         Coord_dim = substr(Coord_detailed, 5, 5)) %>% 
  select(-Coord_detailed) %>% 
  spread(Coord_dim, coord) %>% 
  filter(!is.na(x)) %>% 
  #Only need Isla Nublar and Visitor Center on maps 1 and 2
  mutate(Location = case_when(
    Loc_map == 1 & Location == "Visitor's Center" ~ "Isla Nublar",
    Loc_map == 1 ~ Location,
    Loc_map == 2 & Location == "Visitor's Center" ~ "Visitor's Center",
    TRUE ~ Location2
  )) %>% 
  select(-Location2) %>% 
  #Get start and end time for each Char-map-location
  arrange(Character, Loc_map, T_cums) %>% 
  group_by(Character, Loc_map) %>% 
  mutate(Loc_index = ifelse(Location == lag(Location), 0, 1),
         Loc_index = ifelse(is.na(Loc_index), 1, Loc_index)) %>% 
  mutate(Loc_index = cumsum(Loc_index)) %>% 
  ungroup() %>% 
  #Get Time spent at each location/map/id
  group_by(Character, Location, Loc_map, Loc_index, x, y) %>% 
  summarize(T_min = min(T_cums),
            T_max = max(T_cums + T_length)) %>% 
  mutate(T_length = T_max - T_min) %>% 
  ungroup() %>% 
  arrange(Character, T_min) %>% 
  #Better factor for Location
  mutate(Location = paste(Loc_index, Location),
         Loc_map = factor(Loc_map, labels = c("Globe", "Isla Nublar", "Visitor Center"))) %>% 
  mutate(x = jitter(x), y = jitter(y))


ggplot(character_paths_map %>% filter(Character == "GRANT"), 
       aes(x = x, y = y,
           color = T_min)) +
  geom_path(size = 2, color = "white") +
  geom_path(size = 2, alpha = 0.5) +
  geom_point(aes(size = T_length), color = "#FF4040") +
  geom_label(aes(label = Location), color = "black", alpha = .5) +
  facet_grid(.~ Loc_map) +
  coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10)) + 
  theme_dark()

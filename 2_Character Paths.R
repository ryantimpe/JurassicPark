###
# Character paths
###

library(tidytext)
library(tidyverse)
library(stringr)
library(rsvg)

library(emoGG)
library(gganimate)
emoji_search("skull")


location_map <- read_csv("data/Movie_locations.csv") 
movie_raw <- readxl::read_xlsx("data/SceneDetail.xlsx")
char_colors <- read_csv("data/Character_colors.csv")

source("0_Functions.R")

movie <- movie_raw %>% 
  filter(Scene_Dup != 0 | is.na(Scene_Dup)) %>% 
  arrange(T_h, T_m, T_s) %>%
  mutate(T_cums = T_h*60*60 + T_m*60 + T_s) %>% 
  mutate(T_length = lead(T_cums) - T_cums) 

killed <- movie %>%
  filter(!is.na(A_Killed)) %>% 
  pull(A_Killed) %>% 
  toupper()

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
  mutate(Loc_map = factor(Loc_map, labels = c("Globe", "Isla Nublar", "Visitor Center"))) %>% 
  # mutate(x = jitter(x), y = jitter(y)) %>% 
  #prepare chart
  inner_join(char_colors %>% filter(!is.na(Color))) %>% 
  #Additional Detail
  mutate(char_killed = Character %in% killed)

plot_locations <- location_map %>% 
  gather(Coord_detailed, coord, 
         dplyr::starts_with("Loc_x"), dplyr::starts_with("Loc_y")) %>% 
  mutate(Loc_map = substr(Coord_detailed, 6, 6),
         Coord_dim = substr(Coord_detailed, 5, 5)) %>% 
  select(-Coord_detailed) %>% 
  spread(Coord_dim, coord) %>% 
  filter(!is.na(x), !is.na(LocationLabel)) %>% 
  group_by(LocationLabel) %>% 
  top_n(1, Loc_map) %>% 
  ungroup() %>% 
  #Better factor for Location
  mutate(Loc_map = factor(Loc_map, labels = c("Globe", "Isla Nublar", "Visitor Center"))) %>% 
  select(Loc_map, LocationLabel, x, y)
# PLOT ----

#Import maps

isla_nublar <- rsvg("images/IslaNublar.svg")%>% 
  scale_outline(c(250, 300))

#Scale map locations
ad_l1 <- function(a){a*1}
ad_l2 <- function(a){a*11}
ad_l3 <- function(a){a*1}

dat_map <- isla_nublar$Img_scaled

dat_path <- character_paths_map %>% 
  mutate(x = case_when(
    Loc_map == "Globe" ~ ad_l1(x),
    Loc_map == "Isla Nublar" ~ ad_l2(x),
    TRUE ~ ad_l3(x)
  ),
  y = case_when(
    Loc_map == "Globe" ~ ad_l1(y),
    Loc_map == "Isla Nublar" ~ ad_l2(y),
    TRUE ~ ad_l3(y)
  )) %>% 
  #FOr curve
  group_by(Character, Loc_map) %>% 
  mutate(xend = lag(x), yend = lag(y)) %>% 
  ungroup() %>% 
  # filter(Character %in% c("GRANT"), Loc_map == "Isla Nublar") %>% 
  # filter(Character %in% c("GRANT", "ELLIE", "MALCOLM", "NEDRY", "GENNARO", "MULDOON"), Loc_map == "Isla Nublar") %>%
  filter(Loc_map == "Isla Nublar") %>%
  #NUDGE MULTIPLES....
  #... if multiple characters at some place at some time
  mutate(xn = x, yn = y) %>% 
  group_by(T_min, Location) %>% 
  mutate(nudge = row_number() - 1) %>% 
  mutate_at(vars(xn, xend, yn, yend), funs(. - nudge)) %>% 
  ungroup() %>% 
  #Additional Info
  group_by(Character) %>% 
  mutate(die = ifelse(char_killed & row_number() == n(), 10, NA)) %>% 
  ungroup()

dat_locations <- plot_locations %>% 
  mutate(x = case_when(
    Loc_map == "Globe" ~ ad_l1(x),
    Loc_map == "Isla Nublar" ~ ad_l2(x),
    TRUE ~ ad_l3(x)
  ),
  y = case_when(
    Loc_map == "Globe" ~ ad_l1(y),
    Loc_map == "Isla Nublar" ~ ad_l2(y),
    TRUE ~ ad_l3(y)
  ))

dat_map %>% 
  ggplot(aes(x=x, y=y)) +
  geom_raster(fill = "#32CD32", alpha = 0.5) +
  geom_curve(data = dat_path,
             aes(x=xn, y=yn,
                 color = Color, group = Character,
                 alpha = T_min,
                 xend = xend, yend = yend),
             size = 2, curvature = 0.2,
             color = "white", alpha = 1,
             linetype = "solid", show.legend = FALSE) +
  geom_curve(data = dat_path,
             aes(x=xn, y=yn,
                 color = Color, group = Character,
                 alpha = T_min,
                 xend = xend, yend = yend),
             size = 2, curvature = 0.2,
             linetype = "solid", show.legend = FALSE) +
  scale_color_identity() +
  scale_alpha_continuous(range = c(0.5, 1)) +
  geom_label(data = dat_locations %>% filter(Loc_map == "Isla Nublar"),
             aes(label = LocationLabel),
             fill = "#999999",
             color = "white", alpha = 0.75, nudge_y = 10,
            family = "mono", fontface = "bold", size = 6, 
            show.legend = FALSE
            ) +
  geom_point(data = dat_path,
             aes(size = T_length), color = "#e9e9e9",
             show.legend = FALSE) +
  geom_point(data = dat_path %>% filter(!is.na(die)),
             aes(color = Color),
             size = 15, show.legend = FALSE) +
  geom_emoji(data = dat_path %>% filter(!is.na(die)),
             emoji="1f480", size = 0.03) +
  # facet_wrap(.~ Character) +
  coord_fixed() + 
  theme_dark() +
  theme(
    text = element_text(family = "mono"),
    legend.position = "right",
    panel.background = element_rect(fill = "#333333")
  )

#ANimated version


# dat_path2 <- unique(dat_path$Character) %>% 
#   purrr::map_df(function(charact){
#     dat <- dat_path %>% filter(Character == charact)
#       
#       1:nrow(dat) %>% 
#       purrr::map_df(function(iter){dat[1:iter, ] %>% mutate(state = max(T_min))})
#   }
#   ) %>% 
#   complete(Character, state) %>% 
#   group_by(Character) %>% 
#   fill(-state, .direction = "up") %>% 
#   ungroup() %>% 
#   filter(!is.na(Location))

  
  
dat_map %>% 
  ggplot(aes(x=x, y=y)) +
  geom_raster(fill = "white") +
  geom_curve(data = dat_path,
             aes(x=xn, y=yn,
                 color = Color, group = Character,
                 alpha = T_min,
                 xend = xend, yend = yend),
             size = 2, curvature = 0.2,
             color = "white", alpha = 1) +
  geom_curve(data = dat_path,
             aes(x=xn, y=yn,
                 color = Color, group = Character,
                 alpha = T_min,
                 xend = xend, yend = yend),
             size = 2, curvature = 0.2) +
  scale_color_identity() +
  scale_alpha_continuous(range = c(0.3, 0.9)) +
  # geom_label(data = dat_locations %>% filter(Loc_map == "Isla Nublar"),
  #            aes(label = LocationLabel),
  #            color = "black", alpha = .5, nudge_y = 10) +
  # geom_point(data = dat_path,
  #            aes(size = T_length), color = "#e9e9e9") +
  # geom_point(data = dat_path %>% filter(!is.na(die)),
  #            aes(color = Color),
  #            size = 15) +
  # geom_emoji(data = dat_path %>% filter(!is.na(die)),
  #            emoji="1f480", size = 0.03) +
  # transition_reveal(state) +
  transition_states(T_max) +
  shadow_trail() +
  facet_grid(.~ Loc_map) +
  coord_fixed() + 
  theme_dark() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#333333")
  )


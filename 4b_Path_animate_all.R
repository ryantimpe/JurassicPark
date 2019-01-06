###
# Character paths
###

library(rsvg); library(png)
library(ggrepel)

library(emoGG)
library(gganimate)

source("3_Calculate_character_paths.R")

#Import maps

globe <- abs(1-readPNG("images/Globe1.png")[,,3]) %>% 
  scale_outline(300, "Globe")

isla_nublar <- rsvg("images/IslaNublar.svg")[,,4] %>% 
  scale_outline(c(290, 350), "Isla Nublar")

v_center <- abs(1-readPNG("images/VisitorsCenter1.png")[,,3]) %>% 
  scale_outline(300, "Visitor Center")

#Scale map locations
ad_l1 <- function(a){a*10}
ad_l2 <- function(a){a*13}
ad_l3 <- function(a){a*10}

dat_map <- list(globe$Img_scaled, 
                isla_nublar$Img_scaled,
                v_center$Img_scaled) %>% 
  bind_rows()

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
  # filter(Character %in% c("ARNOLD", "GENNARO", "NEDRY", "MULDOON")) %>%
  # filter(Character %in% c("GENNARO", "ARNOLD", "NEDRY", "MULDOON")) %>%
  # filter(Loc_map == "Globe") %>%
  #NUDGE MULTIPLES....
  #... if multiple characters at some place at some time
  mutate(xn = x, yn = y) %>% 
  arrange(Character) %>% 
  group_by(T_min, Location) %>% 
  mutate(nudge = row_number() - 1) %>% 
  mutate_at(vars(xn, xend, yn, yend), funs(. - nudge*2)) %>% 
  ungroup() %>% 
  #Additional Info
  group_by(Character) %>% 
  mutate(die = ifelse(char_killed & row_number() == n(), 10, NA)) %>% 
  ungroup() %>% 
  mutate(Character = ifelse(Character == "LEX", "LEX & TIM", Character))

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

dat_colors <- char_colors %>% 
  drop_na()%>% 
  mutate(Character = ifelse(Character =="LEX", "LEX & TIM", Character))

#Animated version
dat_path2 <- dat_path %>%
  bind_rows(expand.grid(Loc_map = unique(dat_path$Loc_map),
                        Character = unique(dat_path$Character)) %>% 
              mutate(T_min = max(dat_path$T_min*1.2))) %>% 
  complete(Loc_map, Character, T_min) %>%
  group_by(Loc_map, Character) %>%
  fill(3:ncol(.),.direction = "down") %>% 
  ungroup()

options(gganimate.dev_args = list(width = 875, height = 315),
        gganimate.nframes = 125) 

dat_map %>% 
  ggplot(aes(x=x, y=y)) +
  geom_raster(fill = "#32CD32", alpha = 0.5) +
  geom_text(data = dat_locations,
             aes(label = LocationLabel),
             color = "white", alpha = 0.75, nudge_y = 10,
             family = "mono", fontface = "bold", size = 4,
             show.legend = FALSE
  ) +
  geom_path(data = dat_path2,
            aes(x=xn, y=yn,
                color = "white", group = Character),
            size = 1.3, linetype = "solid",
            show.legend = FALSE) +
  geom_path(data = dat_path2,
             aes(x=xn, y=yn,
                 color = Color, group = Character,
                 alpha = T_min),
             size = 1.3, linetype = "solid",
             show.legend = TRUE) +
  guides(alpha = "none") +
  scale_color_identity(name = "Character", guide = "legend", breaks = dat_colors$Color, labels = dat_colors$Character) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  guides(size = "none") +
  geom_point(data = dat_path2 %>% filter(!is.na(die)),
             aes(color = Color),
             size = 6, show.legend = FALSE) +
  geom_emoji(data = dat_path2 %>% filter(!is.na(die)),
             aes(group = Character),
             emoji="1f480", size = 0.05) +
  facet_wrap(.~ Loc_map) +
  coord_fixed() + 
  labs(
    title = "Character paths in Jurassic Park",
    caption = "@ Ryan Timpe .com"
  ) +
  transition_reveal(T_min) +
  # shadow_trail(0.01) +
  theme_dark() +
  theme(
    text = element_text(family = "mono"),
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    legend.title = element_text(color = "white", size = 12, face = "bold"),
    legend.text = element_text(color = "white", size = 10),
    legend.background = element_rect(fill = "#222222"),
    plot.title = element_text(color = "white", size = 14, face = "bold"),
    plot.background = element_rect(fill = "#222222"),
    plot.caption = element_text(color = "white", size = 10),
    panel.background = element_rect(fill = "#333333"),
    panel.grid = element_line(color = "#444444")
  )

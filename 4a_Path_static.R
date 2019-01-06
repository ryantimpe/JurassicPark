###
# Character paths
###

library(rsvg); library(png)
library(ggrepel)

library(emoGG)
library(gganimate)

source("3_Calculate_character_paths.R")

# PLOT ----

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
             linetype = "solid", show.legend = TRUE) +
  guides(alpha = "none") +
  scale_color_identity( guide = "legend", name = "Character",
                        breaks = dat_colors$Color, labels = dat_colors$Character) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  geom_label_repel(data = dat_locations,
                   aes(label = LocationLabel),
                   fill = "#333333",
                   color = "white", alpha = .75,
                   family = "mono",  size = 4.5,
                   show.legend = FALSE
  ) +
  geom_point(data = dat_path,
             aes(size = T_length), color = "#e9e9e9",
             show.legend = FALSE) +
  guides(size = "none") +
  geom_point(data = dat_path %>% filter(!is.na(die)),
             aes(color = Color),
             size = 8, show.legend = FALSE) +
  geom_emoji(data = dat_path %>% filter(!is.na(die)),
             emoji="1f480", size = 0.05) +
  facet_wrap(.~ Loc_map) +
  labs(
    title = "Character paths in Jurassic Park",
    caption = "@ Ryan Timpe .com"
  ) +
  coord_fixed() + 
  theme_dark() +
  theme(
    text = element_text(family = "mono"),
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(color = "white", size = 12, face = "bold"),
    legend.text = element_text(color = "white", size = 10),
    legend.background = element_rect(fill = "#222222"),
    plot.title = element_text(color = "white", size = 13, face = "bold"),
    plot.background = element_rect(fill = "#222222"),
    plot.caption = element_text(color = "white", size = 10),
    panel.background = element_rect(fill = "#333333"),
    panel.grid = element_line(color = "#444444")
  )

ggsave("SavedOutput/All_ThreeMap_static.png", device = "png",
       width = 6*1300/600, height = 6, units = "in")

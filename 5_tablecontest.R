library(tidyverse)
library(rphylopic)

jp_raw <- readxl::read_xlsx("data/SceneDetail.xlsx")

#Clean
jp_1 <- jp_raw %>% 
  drop_na(MovieScene) %>% 
  #Full timestamp
  mutate(T_stamp = T_h*60*60 + T_m*60 + T_s) %>% 
  arrange(T_stamp)  %>% 
  mutate(Scene_length = T_stamp - lag(T_stamp))%>% 
  relocate(T_stamp, Scene_length, .before = T_h)

#Get a summary of dinosaur activities
names(jp_1)
dino_1 <- jp_1 %>% 
  select(starts_with("Scene"),
         T_stamp,
         MovieScene, Location, Location2,
         ToD, Notable,
         starts_with("D_")) %>% 
  select(-D_Embryos) %>% 
  pivot_longer(starts_with("D_"), names_to = "dinosaur") %>% 
  drop_na(value) %>% 
  #Give unique IDs to dino specimens of each type
  mutate(dinosaur_id = as.numeric(stringr::str_extract(dinosaur, "\\d$")),
         dinosaur_detail = str_extract(dinosaur, "fossil|juvenial"),
         dinosaur = stringr::str_remove_all(dinosaur, "\\d$| fossil| juvenial|^D_")) %>% 
  replace_na(list(dinosaur_id = 1,
                  dinosaur_detail = "adult")) 


dino_1$dinosaur %>% unique()

#Collapse the 3 adult Velociraptors into one dinosaur
dino_2 <- dino_1 %>% 
  group_by(across(c(-dinosaur_id, -value))) %>%
  summarize(value = sum(value), .groups = "drop") %>% 
  ungroup() %>% 
  arrange(T_stamp)
  
  
  #Get images from phylopic
  rphylopic::name_search("Brachiosaur")
brach_list <- search_text("Brachiosaur", output = "names")
brach_out <- search_images(brach_list[,1], 
                           options=c("pngFiles", "credit", "canonicalName"))

brach_out %>% 
  filter(width == 64) %>% 
  filter(row_number()==1) %>% 
  pull(url) %>% 
  str_remove("/assets/images/submissions/") %>% 
  str_remove("\\.64\\.png")

image_data("a878800a-df2b-4b63-b286-965bc288f3a5",
           size = 64)

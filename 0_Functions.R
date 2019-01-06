library(tidyr)

#1 SCALE IMAGE ----
scale_outline <- function(image, img_size, outline_name){
  #Convert image to a data frame with RGB values
  img <- image %>% 
    as.data.frame() %>% 
    mutate(y=row_number())%>% 
    gather(x, value, -y) %>% 
    mutate(x = as.numeric(gsub("V", "", x)))
  
  img_size <- round(img_size, 0)
  
  #Wide or tall image? Shortest side should be `img_size` pixels
  if(max(img$x) > max(img$y)){
    img_scale_x <-  max(img$x) / max(img$y)
    img_scale_y <- 1
  } else {
    img_scale_x <- 1
    img_scale_y <-  max(img$y) / max(img$x)
  }
  
  #If only 1 img_size value, create a square image
  if(length(img_size) == 1){
    img_size2 <- c(img_size, img_size)
  } else {
    img_size2 <- img_size[1:2]
    img_scale_x <- 1
    img_scale_y <- 1
  }
  
  #Rescale the image
  img2 <- img %>% 
    mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
           x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>% 
    select(-x, -y) %>% 
    group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>% 
    #Get average R, G, B and convert it to hexcolor
    summarize_at(vars(value), funs(mean(.))) %>% 
    ungroup() %>% 
    #Center the image
    filter(x <= median(x) + img_size2[1]/2, x > median(x) - img_size2[1]/2,
           y <= median(y) + img_size2[2]/2, y > median(y) - img_size2[2]/2) %>%
    #Flip y
    mutate(y = (max(y) - y) + 1) %>% 
    #Center around 0
    mutate(x = x - round(median(x)),
           y = y - round(median(y))) %>% 
    filter(value > 0.15) %>% 
    mutate(Loc_map = outline_name)
  
  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  
  return(out_list)
  
}

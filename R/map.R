# create a simple map in R for PPT
# 2019-11-02

# load ---------

library(tidyverse)
library(urbnmapr)

place_data <- read_csv("data/places.csv")

ps_data <- read_csv("data/postsecondary.csv")

# clean -------

place_data_clean <- place_data %>%
  rename(state_name = state) %>%
  mutate(state_name = str_replace(state_name, "DC", "District of Columbia"))

place_map_data <- states %>%
  left_join(place_data_clean, by = "state_name")


# plot --------

place_ps_map <- ggplot() + 
# plot the state data
  geom_polygon(data = place_map_data,
               mapping = aes(x = long, y = lat, group = group, fill = status),
               color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
# plot points I want to show on top of the states  
  geom_point(data = ps_data, aes(x = long, y = lat, shape = program),
             color = "coral") +
# give the map nice colors/labels
  scale_fill_manual(values = c("gold", "grey76", "dodgerblue2")) +
  labs(fill = "Where I've lived/visited",
       shape = "Postsecondary Programs",
       title = "My Life, Travel, and Postsecondary Education in the US", 
       caption = "As of Fall 2019") + 
# get rid of the background so it will look good on a slide
  theme_void()+
  theme(legend.position = c(.9,.4),
        text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, face = "italic"))

ggsave(filename = "place_ps_map.png", plot = place_ps_map,
       height = 5, width = 8, units = "in")

# export to ppt -------

# these are the packages you'll need to export your ggplot graphic to ppt
library(officer)
library(rvg)

# run one line of code and you've got an editable plot on a .pptx slide
read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
# this is where you call your ggplot object
  ph_with_vg(code = print(place_ps_map), type = "body") %>% 
# this is where you provide the filename/location for your new ppt slide
  print(target = "place_ps_map.pptx")

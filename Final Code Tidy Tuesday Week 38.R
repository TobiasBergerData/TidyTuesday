library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(png)
library(stringr)
library(cowplot)
library(magick)
library(tidykids)


#Preparing the Data

df <- tidykids

Midwest_P_R <- df %>% filter(variable == "parkrec" & state == "Illinois" | 
                     variable == "parkrec" & state == "Indiana"|
                     variable == "parkrec" & state == "Iowa" |
                     variable == "parkrec" & state == "Kansas" |
                     variable == "parkrec" & state == "Michigan" |
                     variable == "parkrec" & state == "Minnesota" |
                     variable == "parkrec" & state == "Missouri" |
                     variable == "parkrec" & state == "Nebraska" |
                     variable == "parkrec" & state == "North Dakota" |
                     variable == "parkrec" & state == "Ohio" |
                     variable == "parkrec" & state == "South Dakota" |
                     variable == "parkrec" & state == "Wisconsin") 

Midwest_P_R$state <- tolower(Midwest_P_R$state) 


#Creating the mapping structure

states <- map_data("state")

midwest <- subset(states, region %in% c("illinois", "indiana", "iowa", "kansas", "michigan", 
                                         "minnesota", "missouri", "nebraska", "north dakota", 
                                         "ohio", "south dakota", "wisconsin"))

midwest <- midwest %>% rename(state = region)

Midwest_P_R_Spacial <- full_join(midwest, Midwest_P_R %>% filter(year == 2016), by = "state")

state_centroids_quick <- Midwest_P_R_Spacial %>% group_by(state) %>% summarize(x = mean(range(long)),y = mean(range(lat)))



#Creating the plot with images

image <- readPNG("PandR.png")
image1 <- readPNG("Ron.png")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



plot <- ggplot()+
  geom_polygon(data = Midwest_P_R_Spacial, aes(x = long, y = lat, fill = inf_adj_perchild, group = group), color = "gray80")+
  coord_fixed(1.3)+
  scale_fill_gradient2(low = "#559999", mid = "grey90", high = "#48B037", midpoint = median(Midwest_P_R$inf_adj_perchild))+
  geom_text(data = state_centroids_quick, aes(x = x, y = y, label = str_to_title(state)), size= 3.5, hjust = 0.5)+
  labs (fill = "in $1,000s \n per child", 
        title = "Public Spending on                                     in the Midwest in 2016 \n \n",
        subtitle = "in $1,000s per child, adjusted for inflation",
        caption = "Data Source: Urban Insitute \n Viz: @tberger_gtg",
        color = "green")+
  theme_minimal()+
  ditch_the_axes

ggdraw()+
    draw_image(image, x = -.13, y = 0.45, scale = 0.17)+
    draw_image(image1, x = 0.32, y = 0.3, scale = 0.4)+
    draw_plot(plot)+
    theme_minimal()

ggsave("Parks&Rec Plot 2016.png", width = 10.84, height = 5.93, units = "cm", dpi = "print")

#attach packages
library(sf)       #spatial data manipulation
library(eurostat) #access to Eurostat database
library(lubridate)#date class manipulation
library(dplyr)    #data wrangling
library(ggplot2)  #plotting
library(stringr)  #work with strings
library(cowplot)  #plot multiple gg objects
library(viridis)  #color palettes for plotting

#retrieve eurostat data on heating degree days
eur_data <- get_eurostat(id="nrg_chddr2_m")     #use dataset ID
eur_data <- eur_data %>% 
  group_by(geo,
           month=floor_date(time, "month")) %>% #group by region and month
  summarise(average = mean(values)) %>%         #get average monthly values
  filter(nchar(geo) == 5)                       #filter NUTS3 by string length

#retrieve spatial object from eurostat, in sf format and NUTS-3 level
eur_sf <- get_eurostat_geospatial(output_class = "sf", 
                                  resolution = "01",
                                  nuts_level = "3", 
                                  year="2016")
eur_sf <- eur_sf %>% 
  filter(!(geo %in% str_subset(eur_sf$geo, "FRY"))) #remove French DOM
eur_sf <- st_transform(eur_sf, crs=3035) #set CRS to Lambert Azimuthal Equal-Area Europe

#filter january data for heating degree days
january <- eur_data %>% 
  filter(month == "2019-01-01")

#join spatial data and heating degree data of January
euro_df <- left_join(eur_sf, january, by="geo")
euro_df$groups <- cut(euro_df$average, 
                      breaks=c(seq(0,550,50)),
                      labels = c("0-50", "50-100", "100-150", "150-200", 
                                 "200-250", "250-300", "300-350", 
                                 "350-400", "400-450", "450-500",
                                 "500+"),
                      include.lowest=TRUE)

#group by country and compute weighted averages
country <- euro_df %>% 
  group_by(CNTR_CODE) %>%
  summarise(mean = weighted.mean(average, as.numeric(st_area(geometry)),
                                 na.rm=T)) #weighted mean with area unit as weight
country_name <- c("Albania", "Austria", "Belgium", "Bulgaria", 
                  "Switzerland", "Cyprus", "Czechia", "Germany", 
                  "Denmark", "Estonia", "Greece", "Spain", 
                  "Finland", "France", "Croatia", "Hungary", 
                  "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania", 
                  "Luxembourg", "Latvia", "Montenegro", "Macedonia", 
                  "Malta", "Netherlands", "Norway", "Poland", 
                  "Portugal", "Romania", "Serbia", "Sweden", 
                  "Slovenia", "Slovakia", "Turkey", "UK") #vector of countries' name
country <- cbind(country, country_name) #bind vector to country dataframe
country$groups <- cut(country$mean, breaks = c(seq(0,550,50)),
                      labels = c("0-50", "50-100", "100-150", "150-200", 
                                 "200-250", "250-300", "300-350", 
                                 "350-400", "400-450", "450-500",
                                 "500+"),
                      include.lowest=TRUE) #cut values into specific intervals

#highest and lowest values
top <- euro_df %>%
  slice_max(average, n=10) %>% #highest values
  mutate(nuts = case_when(NUTS_NAME == "Norrbottens län" ~ paste(NUTS_NAME, "(SE)"),
                          NUTS_NAME == "Norrbottens län" ~ paste(NUTS_NAME, "(SE)"),
                          TRUE ~ paste(NUTS_NAME, "(FIN)"))) #add country ISO code
low <- euro_df %>% 
  slice_min(average, n=10) #lowest values
#vector of names to replace encoding problem for Greece
names_vector <- c(low$NUTS_NAME[1:7], "Dodekanesos", low$NUTS_NAME[9:10])
#cbind and add country ISO code
low <- cbind(low, names_vector) %>% 
  mutate(nuts = case_when(names_vector == "Dodekanesos" ~ paste(names_vector, "(EL)"),
                          names_vector == "Gozo and Comino/Ghawdex u Kemmuna" ~ paste(names_vector, "(MT)"),
         TRUE ~ paste(names_vector, "(ES)"))) %>% 
  select(-c(names_vector, NUTS_NAME)) %>% 
  mutate(nuts = str_squish(nuts))

#bar plot of high values
top_plot <- ggplot(data=top, aes(reorder(nuts, average), average, label=nuts))+
  geom_bar(stat="identity",
           fill="#120D32FF",
           color="white")+
  geom_text(position = position_stack(vjust=0.5), color="white", size=2)+
  ylim(0,520)+
  coord_flip()+
  theme_void()+
  ggtitle("Coldest")+
  theme(plot.title = element_text(hjust=0.5, color="white", face="bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "black", color = NA), 
        plot.background = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill = "black", color = NA))

#bar plot of low values
low_plot <- ggplot(data=low, aes(reorder(nuts, average), average, label=nuts))+
  geom_bar(stat="identity",
           fill="#FCFDBFFF",
           color="#120D32FF")+
  geom_text(position = position_dodge(width=1), hjust=-0.25, color="White", size=2)+
  ylim(0,520)+
  coord_flip()+
  theme_void()+
  ggtitle("Warmest")+
  ylab("Heating degree days")+
  theme(plot.title = element_text(hjust=0.5, color="white", face="bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "black", color = NA), 
        plot.background = element_rect(fill = "black", color = NA),
        legend.background = element_rect(fill = "black", color = NA))

#prepare additional data and color palette for mapping
total_eu <- st_union(euro_df) #unite geometry of Europe
my_palette <- rev(magma(12))  #manual palette for breaks

#plot map
eu_map <- ggplot()+
  geom_sf(data=na.omit(euro_df), aes(fill=groups), color=NA)+
  geom_sf(data=total_eu, color="white", fill=NA)+
  scale_fill_manual(values=my_palette,
                    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                         keywidth = unit(12, units = "mm"), 
                                         label.position = "bottom", 
                                         title.position = 'top', 
                                         nrow=1))+
  labs(fill="Average heating degree days in the EU (January 2019, NUTS-3 level)",
       title="How much heating do you need?",
       caption="Author: Augusto Dalla Ragione\nData: Eurostat\nProjection: Lambert azimuthal equal-area")+
  theme_void()+
  theme(plot.title=element_text(hjust = 0.5, face="bold", color="#C83E73FF", size=20),
        plot.background = element_rect(fill = "black", color = NA),
        panel.background = element_rect(fill = "black", color = NA), 
        legend.background = element_rect(fill = "black", color = NA),
        legend.position = c(0.6, 0.06),
        legend.direction = "horizontal",
        legend.title = element_text(color="white", face="bold"),
        legend.text = element_text(color="white"),
        plot.caption = element_text(color="white", face="italic"))

#polar plot
polar <- ggplot()+
  geom_bar(data=na.omit(country), 
           aes(reorder(country_name, mean), mean, fill=groups),
           color="white",
           stat="identity")+
  scale_fill_manual(values=my_palette,
                    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                         keywidth = unit(12, units = "mm"), 
                                         label.position = "bottom", 
                                         title.position = 'top', 
                                         nrow=1))+
  ggtitle("By country")+
  coord_polar()+
  theme_void()+
  theme(plot.title = element_text(hjust=0.5, face="bold", color="white"),
        legend.position = "none",
        axis.text.x = element_text(size=7, color="white"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        panel.background = element_rect(fill = "black", color = NA), 
        legend.background = element_rect(fill = "black", color = NA))

#plot final map using cowplot functions
heating_map <- ggdraw()+
  draw_plot(eu_map)+
  draw_plot(polar, x=0.09, y=0.5, width = 0.36, height = 0.36)+
  draw_plot(top_plot, x=0.7, y=0.70, width = 0.1, height = 0.25)+
  draw_plot(low_plot, x=0.7, y=0.45, width = 0.1, height = 0.25)+
  draw_text("Canary islands (Spain) are the\nwarmest area in the EU",
            x=0.25, y=0.08, color="white", size=8, fontface="italic")+
  draw_text("Lappi (Findland) is the\ncoldest area in the EU",
            x=0.64, y=0.88, color="white", size=8, fontface="italic")
heating_map



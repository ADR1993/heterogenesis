library(httr)
library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(ggplot2)
library(lubridate)
library(countrycode)
library(ggtext)
library(ggpubr)

#webpage link
link_head <- "https://worldathletics.org/records/all-time-toplists/sprints/100-metres/all/men/senior?regionType=world&timing=electronic&windReading=regular&page="
link_tail <- "&bestResultsOnly=false&firstDay=1900-01-01&lastDay=2024-08-04&maxResultsByCountry=all&eventId=10229630&ageCategory=senior"

#empty list
pages <- 271 #number of pages to scrape
ls <- vector(mode = "list", length = pages)

#loop
for(i in 1:pages){
  
  #extract data
  ls[i] <- read_html(paste0(link_head, i, link_tail)) %>% 
    html_elements(xpath = "//*[@class='records-table']") %>% 
    html_table()
  
  #status message
  print(paste("Iteration", i, "of", pages)) 
}

#bind together in one dataframe
df <- do.call(rbind, ls) %>% 
  select(-8) %>% 
  mutate(Mark = as.numeric(Mark)) %>% 
  relocate(Mark, Competitor, Date, WIND)

#writexl::write_xlsx(df, "olympic_scrape.xlsx")

#prepare dates
date_df <- do.call(rbind, strsplit(df$Date, " ")) %>% 
  data.frame() %>% 
  rename(D = 1, M = 2, Y = 3) %>% 
  mutate(M = str_to_title(M)) %>% 
  mutate(M = match(M, month.abb))

#create actual date variable in the main dataframe
df <- df %>% 
  mutate(date = paste0(date_df$D, "-", date_df$M, "-", date_df$Y)) %>% 
  mutate(date = dmy(date)) %>% 
  relocate(date, Mark, Competitor, WIND) %>% 
  select(-Date)

#top performances per year
top_df <- df %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  slice_max(order_by = -Mark, n = 60)

#title for the next plot
title <- "<span style='color:#FFB915'>Jamaica</span> 
          <span style='color:white'>(and</span> 
          <span style='color:#B22222'>Bolt</span><span style='color:white'>)</span> 
          <span style='color:white'>against</span> 
          <span style='color:#007848'>the</span> 
          <span style='color:#007848'>world</span>"
subtitle <- "<span style='color:white'>Jamaica accounts for 24% of all</span> 
          <span style='color:white'>sub-10</span>
          <span style='color:white'>races and 52% of all</span> 
          <span style='color:white'>sub-9.8</span> 
          <span style='color:white'>races in the 100-meters dash</span>"

#plot
fsize <- 2.5
ggplot()+
  geom_jitter(data = top_df[!(top_df$Nat == "JAM"),], 
             aes(x = date, y = Mark), color = "#007848", alpha = 0.8, size = fsize)+
  geom_jitter(data = top_df[top_df$Nat == "JAM" & top_df$Competitor != "Usain BOLT",], 
              aes(x = date, y = Mark), color = "#FFB915", alpha = 0.8, size = fsize)+
  geom_point(data = top_df[top_df$Competitor == "Usain BOLT",], 
              aes(x = date, y = Mark), color = "#B22222", alpha = 0.8, size = fsize)+
  
  geom_hline(yintercept = 10, color = "white", linetype = "dashed", linewidth = 0.8)+
  geom_hline(yintercept = 9.8, color = "white", linetype = "dashed", linewidth = 0.8)+
  
  #annotations
  annotate("text", x = ymd("1969-09-20"), y = 9.92, label = "Mexico City Olympics", 
           size = 5, color = "white", fontface = "bold")+ #annotate mexico city
  annotate("text", x = ymd("2020-7-20"), y = 10.18, label = "2020", 
           size = 5, color = "white", fontface = "bold")+ #annotate covid
  
  #rectangle annotations
  annotate("rect", xmin = c(ymd("1968-05-01")), xmax = c(ymd("1969-04-01")), 
           ymin = c(9.94) , ymax = c(10.32), 
           alpha = 0.1, color = "white", linewidth = 0.3)+
  annotate("rect", xmin = c(ymd("2019-12-01")), xmax = c(ymd("2021-02-01")), 
                   ymin = c(9.85) , ymax = c(10.16), 
           alpha = 0.1, color = "white", linewidth = 0.3)+
  
  #axis stuff
  scale_y_continuous(breaks = c(seq(9.55, 10.31, by = 0.1), 9.8, 10) )+
  scale_x_date(date_breaks = "4 years", date_labels = "%Y",
               limits = c(ymd("1967-01-01"), ymd("2024-07-01")))+
  
  #legend and stuff
  labs(x = "", y = "", 
       caption = "@AugustoRagione \n Data: Top sub-10.31 electronically-timed performances per year (up to 60) \n Source: World Athletics",
       #subtitle = "Jamaica accounts for 24% of all sub-10 races and 52% of all sub-9.8 races in the 100-meters dash"
       )+
  ggtitle(title, subtitle = subtitle)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        panel.grid.major =  element_line(color = "#000000"),
        panel.grid.minor =  element_line(color = "#000000"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.ticks = element_line(color = "white"),
        axis.text = element_text(color = "white", angle = 45),
        plot.title = element_markdown(size = 30, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(color = "white", hjust = 0.5, face = "bold", size = 16),
        #plot.subtitle = element_text(color = "white", hjust = 0.5, face = "bold", size = 16),
        plot.caption = element_text(color="white", face = "italic"))

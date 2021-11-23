rm(list=ls())

library(tidyverse)
library(curl)
library(paletteer)
library(sf)
library(ragg)
library(ggtext)
library(scales)
library(extrafont)
library(ggrepel)
library(gganimate)

# dates
plot_dates <- seq(from = as.Date("2021-08-01"), to = as.Date("2021-11-15"), length.out = 9)
areas_of_interest <- c("South West", "South East", "West Midlands")

# Cases by LTLA
cases_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingRate&format=csv"

df <- read.csv(cases_url)
df_casedata <- df %>% 
              mutate(date=as.Date(date)) %>%
              filter(date %in% plot_dates) %>%
              rename("Lacode" = "areaCode",
                     "Laname" = "areaName") %>% 
              select(-areaType) 

# Download map from Carl Baker in House of Commons GitHub library
ltla   <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/LocalAuthorities-lowertier.gpkg")
ltla   <- curl_download(url=source, destfile=ltla, quiet=FALSE, mode="wb")

Background <- st_read(ltla, layer = "7 Background") %>% 
  filter(Name == "England & Wales")

ltlacases  <- st_read(ltla, layer="4 LTLA-2019") %>% 
                  left_join(df_casedata, by="Lacode")

Groups <- st_read(ltla, layer="2 Groups") %>% filter(RegionNation %in% areas_of_interest)

Group_labels <- st_read(ltla, layer="1 Group labels") %>% 
                  mutate(just=if_else(LabelPosit=="Left", 0, 1)) %>% 
                  filter(RegionNation %in% areas_of_interest)


sites <- data.frame(longitude = c(25.18712), latitude = c(25.83519))

# Make plot in ggplot
plot1 <- ggplot()+
  geom_sf(data=Background %>% filter(Name == "England & Wales"), aes(geometry=geom), fill="White")+
  geom_sf(data=ltlacases %>% filter(grepl(pattern = "E", Lacode)), 
          aes(geometry=geom, fill=newCasesBySpecimenDateRollingRate, colour = RegionNation %in% areas_of_interest), 
          size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe, hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_paletteer_c("pals::ocean.haline", direction=-1, name="Cases per\n100,000")+
  geom_point(data = sites, 
             aes(x = longitude, y = latitude), 
             size = 4, 
             shape = 21, 
             fill = "darkred")+
  scale_colour_manual(values = c("True" = "red", "False" = "black"), guide = "none")+
  theme_void()+
  facet_wrap(~date, nrow = 3, ncol = 3)+
  theme(plot.title=element_text(face="bold", size=rel(1.2)),
        text=element_text(family="Lato"))+
  labs(title="COVID-19 cases are high across large parts of the UK",
       subtitle=paste0("Rolling 7-day average number of cases in the past week at Lower Tier Local Authority level\nData up to latest date"),
       caption="Data from PHE, Cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

plot1
ggsave(plot = plot1, 
       filename = "plots/LTLA_cartograms.png", 
       width = 16, 
       height = 8)

#=============#
# Code initially from Victim of Maths
#=============#

rm(list = ls())

library(dplyr)
library(ggplot2)
library(paletteer)
library(extrafont)
library(geofacet)
library(scales)

#=============#
# load data
#=============#

sanger_url <- "https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv"
sanger_data <- read.csv(sanger_url, sep = "\t")

arcgis_url <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
arcgis_data <- read.csv(arcgis_url)[,c(1,4)]
colnames(arcgis_data) <- c("LTLA", "Region")

#=============#
# clean data
#=============#

getLineages <- function(lineages,
                        variant_code,
                        exclude_code = NULL) {
  out <- c()
  
  for (x in 1:length(variant_code)) {
    out <- c(out, lineages[grep(pattern = variant_code[x], lineages)])
    
  }
  
  if (!is.null(exclude_code)) {
    out[which(out != exclude_code)]
  } else{
    out
  }
  
}

data <- merge(sanger_data, arcgis_data, all.x=TRUE) %>%
  mutate(Region=case_when(
    LTLA %in% c("E07000246", "E06000058", "E06000059") ~ "South West",
    LTLA %in% c("E07000245", "E07000244") ~ "East of England",
    TRUE ~ Region),
    WeekEndDate=as.Date(WeekEndDate),
    strain=case_when(
      Lineage %in% getLineages(lineages = unique(sanger_data$Lineage),
                               variant_code = "B.1.177") ~ "B.1.177",
      Lineage %in% getLineages(lineages = unique(sanger_data$Lineage),
                               variant_code = c("B.1.617.2", "AY.4", "AY.1", "AY.2", "AY.3",
                                                "AY.33", "AY.34"),
                               exclude_code = "AY.4.2") ~ "Delta non AY.4.2",
      Lineage %in% getLineages(lineages = unique(sanger_data$Lineage),
                               variant_code = c("B.1.351")) ~ "Beta",
      Lineage %in% getLineages(lineages = unique(sanger_data$Lineage),
                               variant_code = c("B.1.1.7")) ~ "Alpha",
      Lineage=="AY.4.2" ~ "Delta AY.4.2",
      TRUE ~ "Other variants")) %>%
  group_by(WeekEndDate, strain, Region) %>%
  summarise(Count=sum(Count)) %>%
  ungroup() %>%
  group_by(WeekEndDate, Region) %>%
  mutate(Total=sum(Count)) %>%
  ungroup() %>%
  mutate(prop=Count/Total)



#Compare regions
mygrid <- data.frame(name=c("North East", "North West", "Yorkshire and The Humber",
                            "West Midlands", "East Midlands", "East of England",
                            "South West", "London", "South East"),
                     row=c(1,1,1,2,2,2,3,3,3), 
                     col=c(2,1,3,1,2,3,1,2,3),
                     code=c(1:9))


#=============#
# Create plots
#=============#

plot1 <- ggplot(data, aes(x=WeekEndDate, y=Count, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced")+
  scale_fill_paletteer_d("beyonce::X127", name="Lineage")+
  facet_geo(~Region, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="The new 'AY4.2' COVID variant represents a small, but growing, proportion of new cases",
       subtitle=paste0("Number of total COVID-19 genomes sequenced by the Wellcome Sanger Institute identified as  selected major lineages.\nData up to ", 
                       max(data$WeekEndDate)),
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")

plot2 <- ggplot(data, aes(x=WeekEndDate, y=prop, fill=strain))+
  geom_col(position="stack")+
  scale_x_date(name="")+
  scale_y_continuous(name="Genomes sequenced", labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("beyonce::X127", name="Lineage")+
  facet_geo(~Region, grid=mygrid)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.4)))+
  labs(title="The new 'AY4.2' COVID variant represents a small, but growing, proportion of new cases",
       subtitle=paste0("Proportion of total COVID-19 genomes sequenced by the Wellcome Sanger Institute identified as selected major lineages.\nData up to ", max(data$WeekEndDate)),
       caption="Data from Wellcome Sanger Institute | Plot by @VictimOfMaths")

#=============#
# save plots
#=============#
ggsave(plot = plot1, 
       filename = "plots/COVIDGenomesCountxReg.png", 
       units="in", 
       width=10, 
       height=8)

ggsave(plot = plot2, 
       filename = "plots/COVIDGenomesStackedxReg.png", 
       units="in", 
       width=10, 
       height=8)

print(Sys.time())
print("Produced both Sanger plots")


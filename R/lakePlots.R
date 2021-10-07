# title
rm(list = ls())

# user specified inputs - urls and dates:
regional_hosp_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=covidOccupiedMVBeds&metric=hospitalCases&metric=newAdmissions&format=csv"
england_hosp_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=covidOccupiedMVBeds&metric=hospitalCases&metric=newAdmissions&format=csv"
case_url <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDateRollingSum&format=csv"

latestcol <- "DN"
plot_startdate <- "2020-08-01"

# read in cases from dashboard by region, combine midlands into a single region.
cases <- read.csv(case_url) %>% 
  mutate(Region=case_when(
    areaName %in% c("East Midlands", "West Midlands") ~ "Midlands",
    areaName %in% c("Yorkshire and The Humber", "North East") ~ 
      "North East and Yorkshire",
    TRUE ~ areaName),
    date=as.Date(date)) %>% 
  group_by(Region, date) %>% 
  summarise(cases=sum(newCasesBySpecimenDateRollingSum)/7) %>% 
  ungroup()

# Read in hospital Data, rowbind the data to combine datasets, then melt to long format by measure...
hospData <- 
    rbind(read.csv(regional_hosp_url), read.csv(england_hosp_url)) %>% 
    mutate(date = as.Date(x = date),
           covidOccupied_nonMVBeds = hospitalCases - covidOccupiedMVBeds)  %>% 
    melt(id.vars = c("areaCode", "areaName", "areaType", "date"), 
         variable.name = "measure" ) %>%
    mutate(measure = factor(x = measure, 
           levels = c("covidOccupied_nonMVBeds", "covidOccupiedMVBeds"))) %>%
    filter(date < max(cases$date))

setdiff(unique(hospData$areaName), unique(cases$Region))
intersect(unique(hospData$areaName), unique(cases$Region))


# CREATE FANCY REGIONAL PLOT:
custom_theme <- theme_classic() +
                theme(plot.title.position="plot", 
                      plot.caption.position="plot",
                      strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
                      plot.title=element_text(face="bold", size=rel(1.5), hjust=0, margin=margin(0,0,5.5,0)),
                      text=element_text(family="Lato"),
                      plot.subtitle=element_markdown())
                      
                      
                      
                      
engPlot <- ggplot()+

  custom_theme + 

  geom_col(data=cases %>% filter(date>as.Date(plot_startdate)) %>% 
                                   group_by(date) %>% 
                                   summarise(cases=sum(cases)) %>% 
                                   ungroup(), 
           aes(x=date, y=cases), fill="#47d4ae")+
  
  geom_col(data=hospData %>% filter(date > as.Date(plot_startdate) & areaName == "England" & measure != "newAdmissions"),
           aes(x=date, y=-value, fill = measure), 
           position="stack", show.legend=FALSE)+

  geom_hline(yintercept=0, colour="Black")+

  scale_x_date(name="", date_labels = "%b-%y", date_breaks = "month")+

  scale_y_continuous(name="", labels=abs, position = "right", breaks = seq(-4e+4, +5e+4, 1e+4))+

  scale_fill_manual(values=c("covidOccupiedMVBeds" = "#ff1437", "covidOccupied_nonMVBeds" = "#ff9f55"))+

  annotate(geom="text", x=as.Date("2020-09-01"), y=25000, 
           label="New cases in the population", hjust=0, family="Lato")+

  annotate(geom="text", x=as.Date("2020-09-01"), y=-20000, 
           label="Total patients in hospital", hjust=0, family="Lato")+

  labs(title="National plot showing hospital occupancy lags cases by around a fortnight",
       subtitle="Daily confirmed new COVID-19 cases and patients in hospital with COVID-19 in Mechanically Ventilated & all other beds in England",
       caption="Data from coronavirus.data.gov.uk")

regionPlot <- ggplot()+

  geom_col(data = cases %>% filter(date > as.Date(plot_startdate)), 
           aes(x = date, y=cases), 
           fill="#47d4ae")+

  geom_col(data=hospData %>% filter(date > as.Date(plot_startdate) & areaName != "England" & measure != "newAdmissions"),
           aes(x=date, y=-value, fill=measure), 
           position="stack", show.legend=FALSE)+

  geom_hline(yintercept = 0, colour = "Black")+

  scale_x_date(name="", date_labels = "%b-%y")+

  scale_y_continuous(name="", labels=abs, position = "right") +

  scale_fill_manual(values=c("covidOccupiedMVBeds" = "#ff1437", "covidOccupied_nonMVBeds" = "#ff9f55"))+

  facet_wrap(~ areaName)+

  custom_theme +

  labs(title = "Regional plots showing hospital occupancy lags cases by around a fortnight",
       subtitle="Daily confirmed <span style='color:#47d4ae;'>new COVID-19 cases</span> and patients in hospital with COVID-19 in <span style='color:#ff1437;'>Mechanically Ventilated</span> and<span style='color:#ff9f55;'> all other</span> beds",
       caption = "Data from coronavirus.data.gov.uk")
       
       
# save plots
ggsave(plot = engPlot, 
       filename = "plots/lakePlotOverall.png",
       width = 12, 
       height = 9)

ggsave(plot = regionPlot, 
       filename = "plots/lakePlotRegional.png",
       width = 12, 
       height = 9)

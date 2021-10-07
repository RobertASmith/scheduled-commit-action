rm(list=ls())

library(dplyr)
library(ggplot2)
library(RcppRoll)
library(lubridate)
library(ggtext)
library(extrafont)
library(paletteer)
library(tidyr)
library(officer)
library(readxl)
library(curl)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download data (have to do this separately for male and females because the dashboard is weird)
source.f <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=femaleCases&format=csv"

data.f <- read.csv(source.f) %>% 
  mutate(sex="Female")

source.m <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=maleCases&format=csv"

data.m <- read.csv(source.m) %>% 
  mutate(sex="Male")

#Combine and extract daily figures from cumulative ones
data <- bind_rows(data.f, data.m) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(age, sex) %>% 
  arrange(date) %>% 
  mutate(newcases=value-lag(value, 1),
         ratechange=rate-lag(rate, 1),
         cases_roll=roll_mean(newcases, 7, align="center", fill=NA),
         rates_roll=roll_mean(ratechange, 7, align="center", fill=NA)) %>% 
  ungroup() %>% 
  mutate(age=gsub("_", " ", age),
         age=factor(age, levels=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                                  "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                  "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                  "85 to 89", "90+")))


plot_COVIDCasesxSexRecent <- ggplot(data %>% filter(date>as.Date("2021-05-25") & date<max(date)-days(3)), 
       aes(x=date, y=rates_roll, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown(), 
        strip.text=element_blank())+
  geom_text(data=data %>% filter(date==as.Date("2021-06-16") & sex=="Male"),
            aes(x=date, y=140, label=age), colour="Black", family="Lato", fontface="bold")+
  labs(title="COVID case rates in 25-49 year old men and women have diverged in recent weeks",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in England, by age.",
       caption="Data from coronavirus.data.gov.uk | Plot by @VictimOfMaths")

plot_COVIDCasesxSexFull <- ggplot(data %>% filter(date<max(date)-days(3) & date> as.Date("2020-10-01")), 
       aes(x=date, y=rates_roll, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name = "", breaks = "3 months", date_minor_breaks = "3 months", date_labels = "%b-%y")+
  scale_y_continuous(name="Daily new cases per 100,000")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="The gender gap in COVID cases in younger adults in England is growing",
       subtitle="Rolling 7-day average of new COVID case rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> in England, by age.",
       caption="Data from coronavirus.data.gov.uk  ")

heatmapdata <- data %>% 
  select(sex, rates_roll, age, date) %>% 
  spread(sex, rates_roll) %>% 
  mutate(maleprop=Male/(Male+Female))

plot_COVIDCasesxSexHeatmap <- 
ggplot(heatmapdata %>% filter(date>as.Date("2020-10-01") & date<max(date)-days(3)))+
  geom_tile(aes(x=date, y=age, fill=maleprop))+
  theme_custom()+
  scale_fill_distiller(palette="PRGn", limits=c(0.33,0.67), name="", breaks=c(0.33,0.5,0.67),
                       labels=c("2 Female cases\nfor each\nmale case", "Equal male\nand female\ncases", 
                                "2 Male cases\nfor each\nfemale case"))+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_discrete(name="Age")+
  theme(legend.position = "top", plot.subtitle=element_markdown())+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases in 20-50 year olds are becoming increasingly female-dominated",
       subtitle="Ratio of <span style='color:#1b7837;'>female</span> to <span style='color:#762a83;'>male</span> cases in England, based on a 7-day rolling average",
       caption="Date from coronavirus.data.gov.uk")

plot_COVIDCasesxSexHeatmapU60s <- 
ggplot(temp<-heatmapdata %>% filter(date>as.Date("2020-10-01") & date<max(date)-days(3) &
         age %in% c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
                    "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                    "45 to 49", "50 to 54", "55 to 59")))+
  geom_tile(aes(x=date, y=age, fill=maleprop))+
  theme_custom()+
  scale_fill_distiller(palette="PRGn", limits=c(0.37,0.63), name="", breaks=c(0.37,0.5,0.63),
                       labels=c("17 Female cases\nfor every\n10 male cases", "Equal male\nand female\ncases", 
                                "17 Male cases\nfor every\n10 female cases"))+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_discrete(name="Age")+
  theme(legend.position = "top", plot.subtitle=element_markdown())+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases in 20-50 year olds are becoming increasingly female-dominated",
       subtitle="Ratio of <span style='color:#1b7837;'>female</span> to <span style='color:#762a83;'>male</span> cases in England, based on a 7-day rolling average",
       caption="Date from coronavirus.data.gov.uk")


#Calculate case rate ratios
caseratios <- data %>% 
  group_by(age, date) %>% 
  summarise(cases_roll=sum(cases_roll)) %>% 
  mutate(sex="Total") %>%
  ungroup() %>% 
  bind_rows(data) %>% 
  filter(!is.na(cases_roll)) %>% 
  select(age, sex, date, cases_roll) %>% 
  group_by(age, sex) %>% 
  mutate(caseratio=cases_roll/lag(cases_roll, 7)) %>% 
  ungroup()

#Whole population
popheatmap <- caseratios %>% 
  filter(sex=="Total" & date>as.Date("2020-04-01")) 


plot_COVIDCaseRatioHeatmap <- ggplot(popheatmap)+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk")

plot_COVIDCaseRatioHeatmapRecent <-
ggplot(popheatmap %>% filter(date>as.Date("2021-05-01")))+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="COVID cases are now falling across all age groups in England",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk")

#plot_COVIDCaseRatioHeatmapRecent
  
popheatmapxsex <- caseratios %>% 
  filter(sex!="Total" & date>as.Date("2020-04-01")) 


plot_COVIDCaseRatioHeatmapxSex <- 
ggplot(popheatmapxsex)+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  facet_wrap(~sex)+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk")

#plot_COVIDCaseRatioHeatmapxSex

plot_COVIDCaseRatioHeatmapxSexRecent <-
ggplot(popheatmapxsex %>% filter(date>as.Date("2021-05-01")))+
  geom_tile(aes(x=date, y=age, fill=caseratio))+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_discrete(name="Age")+
  scale_fill_paletteer_c("pals::warmcool", limit=c(0.249,4), direction=-1 ,
                         trans="log", breaks=c(0.25, 0.5, 1, 2, 4), 
                         labels=c("-75%", "-50%", "No change", "+100%", "+300%"),
                         name="Change in cases in the past week")+
  facet_wrap(~sex)+
  theme_custom()+
  theme(legend.position="top")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Generally COVID case numbers have risen or fallen across all age groups at once",
       subtitle="Weekly change in the rolling 7-day average number of new COVID cases in England, by age group",
       caption="Data from coronavirus.data.gov.uk")

#plot_COVIDCaseRatioHeatmapxSexRecent

PHE_excel_file_path <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1020037/Weekly_Influenza_and_COVID19_report_data_W38_v2.xlsx"
temp <- tempfile()
temp <- curl_download(url=PHE_excel_file_path, 
                      destfile=temp, 
                      quiet=FALSE, 
                      mode="wb")

data_m <- read_excel(temp, sheet="Figure 7. Positivity by age", range="C121:L174") %>% 
  mutate(date=seq.Date(from=as.Date("2020-09-14"), by="weeks", length.out=nrow(.))) %>% 
  gather(age, positivity, c(1:ncol(.)-1)) %>% 
  mutate(age=gsub("_", " to ", age), sex="Male")

data_f <- read_excel(temp, sheet="Figure 7. Positivity by age", range="C177:L230") %>% 
  mutate(date=seq.Date(from=as.Date("2020-09-14"), by="weeks", length.out=nrow(.))) %>% 
  gather(age, positivity, c(1:ncol(.)-1)) %>% 
  mutate(age=gsub("_", " to ", age), sex="Female")

data <- bind_rows(data_m, data_f) %>% 
  mutate(age=factor(age, levels=c("0 to 4", "5 to 9", "10 to 19", "20 to 29", 
                                  "30 to 39", "40 to 49", "50 to 59",
                                  "60 to 69", "70 to 79", "80+")),
         positivity=positivity/100)

PHE_plot_positivity <- 
ggplot(data %>% filter(date>as.Date("2021-05-25")), aes(x=date, y=positivity, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_continuous(name="Positivity")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Positivity rates are consistently higher among men",
       subtitle="Rolling 7-day average of COVID test positivity rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> for pillar 2 (community) testing in England, by age.",
       caption="Data from PHE")

PHE_plot_positivity_FULL <- 
ggplot(data %>% filter(date>as.Date("2020-10-01")), aes(x=date, y=positivity, colour=sex))+
  geom_line(show.legend=FALSE)+
  scale_x_date(name="", date_labels = "%b-%y")+
  scale_y_continuous(name="Positivity")+
  scale_colour_manual(values=c("#00cc99", "#6600cc"))+
  facet_wrap(~age)+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Positivity rates are consistently higher among men",
       subtitle="Rolling 7-day average of COVID test positivity rates in <span style='color:#6600cc;'>men</span> and <span style='color:#00cc99;'>women</span> for pillar 2 (community) testing in England, by age.",
       caption="Data from PHE")

myPPT <- 
    # read in template
     read_pptx() %>%

    # Male/Female line charts

      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

      ph_with(value = plot_COVIDCasesxSexFull, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%

      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

      ph_with(value = plot_COVIDCasesxSexRecent, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%

    # Positivity by Sex
      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

      ph_with(value = PHE_plot_positivity, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%

    # Positivity by Sex
      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

      ph_with(value = PHE_plot_positivity_FULL, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%

     # Male/Female heatmap
      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

      ph_with(value = plot_COVIDCasesxSexHeatmap, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%
  
      add_slide(layout = "Title and Content", 
              master = "Office Theme") %>%

      ph_with(value = plot_COVIDCasesxSexHeatmapU60s, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%

      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

     # Male/Female % change

      ph_with(value = plot_COVIDCaseRatioHeatmapxSex, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7)) %>%

      add_slide(layout = "Title and Content", 
                master = "Office Theme") %>%

      ph_with(value = plot_COVIDCaseRatioHeatmapxSexRecent, 
              location = ph_location("body", left = 0.5, top = 0.5, width = 12, height = 7))


# output to file
print(myPPT, target = "plots/ageSexCases.pptx")





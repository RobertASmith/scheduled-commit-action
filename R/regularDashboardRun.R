# print time & date
print(Sys.time())

# library for ggplot functions
library(ggplot2)

# read in data
df_plot <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newCasesBySpecimenDate&format=csv")

# create plot
p_CasesAutomationTest <- 
  ggplot(data = df_plot)+
  theme_classic()+
  geom_point(aes(x = as.Date(date),
                 y = newCasesBySpecimenDate))+
  scale_x_date(name = "Date")+
  scale_y_continuous(name = "Cases by Specimen Date")+
  ggtitle("Number of Daily Cases by Specimen Date",
          subtitle = paste(Sys.time()))
  
# save plot
ggsave(plot = p_CasesAutomationTest,
       filename = "plots/CasesAutomationTest.png")



library(RColorBrewer)
brewer.pal(11,"BrBG")
brewer.pal(11,"PiYG")


brewer.pal(9,"GnBu")
brewer.pal(11,"Spectral")
brewer.pal(12,"Set3")
brewer.pal(11,"RdYlBu")


### scratchpad

library(DiagrammeR)
library(ggplot2)
library(scales)

dataForecast <- makeForecast(busA,"2021",lengthOut,50,busASupplySlope)


#demandLineSlope<-

maxDemand<-max(dataForecast$forecast, na.rm=TRUE)  
minDemand<-min(dataForecast$forecast, na.rm=TRUE)  

#minDate<-dataForecast$date[dataForecast$forecast==minDemand, na.rm=TRUE)]

 
demandLabelCoordinate<-min(dataForecast$forecast, na.rm=TRUE)+(max(dataForecast$forecast, na.rm=TRUE)-min(dataForecast$forecast, na.rm=TRUE))/2
supplyLabelCoordinate<-min(dataForecast$forecastSupply, na.rm=TRUE)+(max(dataForecast$forecastSupply, na.rm=TRUE)-min(dataForecast$forecastSupply, na.rm=TRUE))/2

dataForecast %>%
  ggplot(aes(date, headcount)) +
  geom_line(aes(x = date, y = headcount),na.rm=TRUE,color="#878787", size=1) +
  geom_point(aes(x = date, y = headcount),na.rm=TRUE,color="#878787",size=1) +
  geom_line(aes(x = date, y = forecast),na.rm=TRUE,color="#0868AC", size=1) +
  geom_line(aes(x = date, y = forecastSupply),na.rm=TRUE,color="#FB8072", size=1) +
  theme_fivethirtyeight() +
  ylab("Headcount") +
  #labs(title="Projected Supply and Demand") +
  theme(axis.title.y = element_text()) +
  annotate("text", x = median(dataForecast$date)+265, y = demandLabelCoordinate, label = "Demand", color="#0868AC") +
  annotate("text", x = median(dataForecast$date)+265, y = supplyLabelCoordinate, label = "Supply", color="#FB8072")




dataForecast %>%
  ggplot(aes(date, headcount)) +
  labs(
    title = "<span style='font-size:11pt'><span style='color:#0868AC;'>Demand</span> vs. <span style='color:#FB8072;'>Supply</span> </span>
    ") +
  geom_line(aes(x = date, y = headcount),na.rm=TRUE,color="#878787", size=1) +
  geom_point(aes(x = date, y = headcount),na.rm=TRUE,color="#878787",size=1) +
  geom_line(aes(x = date, y = forecast),na.rm=TRUE,color="#0868AC", size=1) +
  geom_line(aes(x = date, y = forecastSupply),na.rm=TRUE,color="#FB8072", size=1) +
  theme_fivethirtyeight() +
  ylab("Headcount") +
  #labs(title="Projected Supply and Demand") +
  theme(axis.title.y = element_text()) +
  theme(
    plot.title = element_markdown(lineheight = 1.1)
  )









min(dataForecast$forecast, na.rm=TRUE)

max(dataForecast$forecast, na.rm=TRUE)


hiring<-data.frame(Business=c('Business A','Business B','Business C'),
                   pectFemale=c(.35,.48,.46),
                   pectFemaleHires=c(.45,.38,.55))

promotions<-data.frame(Business=c('Business A','Business B','Business C'),
                   pectFemaleEligible=c(.25,.20,.35),
                   pectFemalePromotions=c(.15,.25,.32))


ggplot(hiring, aes(x=pectFemale, y=pectFemaleHires)) + 
  geom_point(color="#FDAE6B",size=25) +
  #scale_size_continuous(range=c(2,12)) +
  geom_label(aes(label = Business)) +
  scale_x_continuous('Female Workforce as % of Total Headcount',labels=percent, limits=c(.25,.75)) +
  scale_y_continuous('Female Promotions as % of Total Promotions',labels = percent, limits=c(.25,.75)) +
  labs(title="Female Headcount Percentage vs. Female Promotion Percentage", subtitle="Previous Year") +
  geom_abline(slope=1)



ggplot(promotions, aes(x=pectFemaleEligible, y=pectFemalePromotions)) + 
  geom_point(color="#FDAE6B",size=25) +
  #scale_size_continuous(range=c(2,12)) +
  geom_label(aes(label = Business)) +
  scale_x_continuous('Female Workforce as % of Total Headcount',labels=percent, limits=c(0,.50)) +
  scale_y_continuous('Female Promotions as % of Total Promotions',labels = percent, limits=c(0,.50)) +
  labs(title="Female Headcount Percentage vs. Female Promotion Percentage", subtitle="Previous Year") +
  geom_abline(slope=1)

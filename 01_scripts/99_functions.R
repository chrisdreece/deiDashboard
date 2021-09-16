
### experiment and create functions


### make Hist
makeHist <- function(year,length,startHeadcount,endHeadcount) {
  tibble(
    date = tk_make_timeseries(year, by = "quarter", length_out=length),
    headcount=round(seq(startHeadcount,endHeadcount,length=length),0),
    noise=round(runif(length, min=-50, max=50),0)
  ) %>%
    mutate(headcount=headcount+noise) %>%
    select(-noise)
}

### make Forecast
makeForecast <- function(dataHist,yearForecastStart,length,growthSlope,supplySlope) {
  current<-dataHist$headcount[dataHist$date==max(dataHist$date)]
  ending<-current+current*growthSlope/100
  start<-current+(ending-current)/length
  endingSupply<-current-current*supplySlope/100
  startSupply<-current-(current-endingSupply)/length
  
  
  tibble(
    date = tk_make_timeseries(yearForecastStart, by = "quarter", length_out=length),
    forecast=round(seq(start,ending,length=length),0),
    forecastSupply=round(seq(startSupply,endingSupply,length=length),0),
  ) %>%
    bind_rows(dataHist,.) %>%
    mutate(forecast=ifelse(date==max(dataHist$date),headcount,forecast),
           forecastSupply=ifelse(date==max(dataHist$date),headcount,forecastSupply))
}

#busAForecast<-makeForecast(busA,"2021",lengthOut,50,30)



makePlot <- function(dataForecast) {

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
}

#makePlot(busAForecast,"Business A")


### develop plotly chart


diversity<- data.frame(
  period=c(rep('prevYear',4),rep('forecast',4)),
  gender=rep(c('Female','Female','Male','Male'),2),
  mgr=rep(c('Manager','Individual Contributor'),4),
  count=c(91,1097,518,1341,227,874,719,1092)) %>%
  mutate(gender=as.factor(gender)) %>%
  mutate(gender=relevel(gender, "Male")) %>%
  group_by(period,mgr) %>%
  mutate(total=sum(count)) %>%
  ungroup() %>%
  mutate(countPect=round(count/total,3)) %>%
  group_by(mgr,period) %>%
  mutate(label_y = cumsum(countPect) - 0.5 * countPect)

str(diversity)

colors1<-c("#9970AB","#5AAE61")
myColors<-colors1
names(myColors) <- c('Male','Female')




percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#ggplot(diversity,aes(x = period, y = countPect, fill=forcats::fct_rev(gender))) +
ggplot(diversity,aes(x = period, y = countPect, fill=gender)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=myColors) +
  coord_flip() +
  facet_grid(. ~ mgr) +
  scale_y_continuous("Percent of Total Headcount",labels = scales::percent_format()) +
  scale_x_discrete('',labels=c('Projected \n (2023 Year End)','Current \n (2020 Year End)')) +
  labs(title = 'Gender Representation', subtitle = 'Current and Projected') +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(.85, 0.9), legend.title = element_blank()) +
  theme(plot.background = element_rect(colour = "black",size = 1)) +
  geom_text(aes(y = label_y, label = percent(countPect)), colour = "white") +
  geom_hline(yintercept=.25)



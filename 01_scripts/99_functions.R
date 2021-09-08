
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

makePlot <- function(dataForecast,plotTitle) {
  gap<-as.character(dataForecast$forecast[dataForecast$date==max(dataForecast$date)]-dataForecast$forecastSupply[dataForecast$date==max(dataForecast$date)])
  gapText<-grobTree(textGrob(paste("Anticipated Gap: ",gap,sep=''), x=0.15,  y=0.85, hjust=0,
                             gp=gpar(col="purple", fontsize=13)))
  dataForecast %>%
    ggplot(aes(date, headcount)) +
    geom_line(aes(x = date, y = headcount),na.rm=TRUE,color="#1B7837", size=1) +
    geom_point(aes(x = date, y = headcount),na.rm=TRUE,color="#1B7837",size=3) +
    geom_line(aes(x = date, y = forecast),na.rm=TRUE,color="blue", size=1) +
    geom_line(aes(x = date, y = forecastSupply),na.rm=TRUE,color="red", size=1) +
    theme_fivethirtyeight() +
    ylab("Headcount") +
    theme(axis.title.y = element_text()) +
    labs(title = plotTitle) +
    annotation_custom(gapText)
}

#makePlot(busAForecast,"Business A")


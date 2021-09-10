
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

  dataForecast %>%
    ggplot(aes(date, headcount)) +
    geom_line(aes(x = date, y = headcount),na.rm=TRUE,color="#1B7837", size=1) +
    geom_point(aes(x = date, y = headcount),na.rm=TRUE,color="#1B7837",size=3) +
    geom_line(aes(x = date, y = forecast),na.rm=TRUE,color="blue", size=1) +
    geom_line(aes(x = date, y = forecastSupply),na.rm=TRUE,color="red", size=1) +
    theme_fivethirtyeight() +
    ylab("Headcount") +
    theme(axis.title.y = element_text())
}

#makePlot(busAForecast,"Business A")


### develop plotly chart


diversity<- data.frame(
  period=c(rep('prevYear',4),rep('forecast',4)),
  gender=rep(c('Female','Female','Male','Male'),2),
  mgr=rep(c('Manager','Staff'),4),
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
names(myColors) <- levels(diversity$gender)


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
  geom_text(aes(y = label_y, label = percent(countPect)), colour = "white")


ggplot(diversitySub) +
  geom_bar( aes(x=mgr, y=count, fill=gender), position='dodge', stat="identity", alpha=0.5) +
  geom_errorbar( aes(x=Species, ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.5) +
  ggtitle("using confidence interval")




ce <- cabbage_exp %>%
  arrange(Date, rev(Cultivar))

data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart



diversitySub<-filter(diversity,period=='prevYear')
fig <- diversitySub %>% plot_ly(labels = ~gender+~mgr, values = ~count)
fig <- fig %>% add_pie(hole = 0.6)
fig <- fig %>% layout(title = "Donut charts using Plotly",  showlegend = F,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


# Get Manufacturer
mtcars$manuf <- sapply(strsplit(rownames(mtcars), " "), "[[", 1)

df <- mtcars
df <- df %>% group_by(manuf)
df <- df %>% summarize(count = n())
fig <- df %>% plot_ly(labels = ~manuf, values = ~count)
fig <- fig %>% add_pie(hole = 0.6)
fig <- fig %>% layout(title = "Donut charts using Plotly",  showlegend = F,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig





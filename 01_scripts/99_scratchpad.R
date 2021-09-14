

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

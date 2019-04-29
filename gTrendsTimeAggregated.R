# Fredrik Cederlöf, Head of CX & Analytics Collector Bank
# https://www.linkedin.com/in/fredrikcederlof/

library(gtrendsR)
library(reshape2)
library(ggplot2)

# search keywords
keywords=c("mathem","linas matkasse","mat.se")
#keywords=c("billån","mc lån","bostadsklån", "samla lån")

# set the geographic area: SE = Sweden
country=c('SE')

# set the time window
time = "today+5-y"

# set channels
channel='web'

# ggplot2 theme function
my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}

# Run query 
trends = gtrends(keywords, gprop = channel, geo = country, time = time)
time_trend=trends$interest_over_time

df2 <- trends$interest_over_time

ggplot(df2, aes(date, hits, colour = keyword)) + 
  geom_line()+
  geom_path(size = 1)
   
# Convert date to month & rename column
time_trend$date <- format(time_trend$date,"%m")
names(time_trend)[1]<-"month"

# Convert month to character and switch to text
time_trend$month <- as.character(time_trend$month)
time_trend$month[time_trend$month == "01"] <- "JAN"
time_trend$month[time_trend$month == "02"] <- "FEB"
time_trend$month[time_trend$month == "03"] <- "MAR"
time_trend$month[time_trend$month == "04"] <- "APR"
time_trend$month[time_trend$month == "05"] <- "MAJ"
time_trend$month[time_trend$month == "06"] <- "JUN"
time_trend$month[time_trend$month == "07"] <- "JUL"
time_trend$month[time_trend$month == "08"] <- "AUG"
time_trend$month[time_trend$month == "09"] <- "SEP"
time_trend$month[time_trend$month == "10"] <- "OKT"
time_trend$month[time_trend$month == "11"] <- "NOV"
time_trend$month[time_trend$month == "12"] <- "DEC"

# plot time_trend with geom_bar
ggplot(data=time_trend, aes(x=month, weight=hits))+
  geom_bar(fill = '#5DCAAF')+xlab('Month')+ylab('Relative Interest')+ theme_bw()+
  facet_wrap(~ keyword, scales = "free") +
 my_theme() +
  ggtitle("Söktrender i kategorin privatlån")+
  labs(subtitle='Fördelningar av söktermer per månad baserat på 5 års sökdata.', 
       caption = 'Source: Google Trends')+
  scale_x_discrete(limits=c("JAN","FEB","MAR","APR","MAJ","JUN","JUL","AUG","SEP","OKT","NOV","DEC"))

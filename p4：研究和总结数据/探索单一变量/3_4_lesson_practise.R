library(ggplot2)
data(diamonds)
summary(diamonds)
?diamonds

ggplot(aes(x = price),data = diamonds)+
  geom_histogram()+
  scale_x_continuous(limits = c(0,10000))

sum(diamonds$price < 500)
sum(diamonds$price < 250)
sum(diamonds$price >= 15000)

ggplot(aes(x = price), data = diamonds) +
  geom_histogram()+
  scale_x_continuous(limits = c(0,10000), breaks = seq(0,10000,500))+
  facet_wrap(~cut)

by(diamonds$price,diamonds$cut,summary)


qplot(x = price, data = diamonds) + facet_wrap(~cut)
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

ggplot(diamonds, aes(x=price/carat)) + geom_histogram() + facet_wrap(~cut, scales="free") + scale_x_log10()

ggplot(diamonds, aes(x = price/carat)) +
  geom_histogram(color="black", fill="DarkOrange",binwidth=0.05)+
  theme(axis.text.x=element_text(angle=0))+
  scale_x_log10(expression(paste(Log[10], "of Price")),
                breaks = trans_breaks("log10",function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  facet_grid(cut~., scale="free") +ylab("Count")


ggplot(diamonds, aes(x=clarity, y=price)) + geom_boxplot() + xlab('Clarity') + ylab('Price')
by(diamonds$price, diamonds$color, summary)


IQR(diamonds$price[diamonds$color=='D'])
IQR(diamonds$price[diamonds$color=='J'])

ggplot(diamonds, aes(x=color, y=price/carat)) + geom_boxplot() + xlab('Color') + ylab('Price')

qplot(data=diamonds, x=carat, xlab="Carat",ylab="Frequency",binwidth=0.1, geom='freqpoly')+
  scale_x_continuous(breaks=seq(0,5,0.2)) +
  scale_y_continuous(breaks=seq(0,12000,2000))

getwd()
setwd("C:/Users/10415/Documents/R/3")

library('xlsx', quietly=TRUE)
library(reshape2)


library(lubridate)
library(scales)
bdays<-read.csv('birthdaysExample.csv')
bdays$dates <- as.Date(bdays$dates, '%m/%d/%y')


ggplot(bdays, aes(x=dates))+
  geom_histogram(binwidth=1, color="gray", fill ="blue")+
  scale_x_date(labels = date_format('%b'), breaks = date_breaks("months"), limits=c(as.Date("2016-01-01"), as.Date("2016-12-31")))+
  xlab('Birthday') +ylab('Count') +ggtitle('Histogram of Brithdays')

ggplot(bdays, aes(x=month(bdays$date))) +
  geom_bar()+
  scale_x_continuous(breaks=seq(1,12),labels=month.abb)+
  xlab('Month') +ylab('Number of Birthdays')+
  ggtitle('Birthdays by Month')
# 8 习题集：探索多个变量

#8.1  带有分面和颜色的价格直方图
library(ggplot2)
data(diamonds)


ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_histogram(bins = 50) +
  scale_fill_brewer(type = 'qual') +
  facet_wrap(~color)

#8.2  价格与按切工填色的表格
ggplot(diamonds, aes(x = table, y = price, color = cut)) +
  geom_point(position = position_jitter(), alpha = 0.25) +
  scale_color_brewer(type = 'qual')

#8.4  价格与体积和钻石净度
diamonds$volume <- with(diamonds, x*y*z)
ggplot(subset(diamonds, volume < quantile(volume, 0.99)),aes(volume, y = price, color = clarity)) +
  geom_point(alpha  = 0.25) +
  scale_color_brewer(type = 'div') +
  scale_y_log10()

#8.5 新建友谊的比例
pf <- read.delim('pseudo_facebook.tsv')

pf$prop_initiated <- pf$friendships_initiated / ifelse(pf$friend_count > 0, pf$friend_count, 1)

#8.6  prop_initiated 与使用时长
pf$year_joined <- 2014 - ceiling(pf$tenure/365)
pf$year_joined.bucket <-  cut(pf$year_joined, breaks = c(2004,2009,2011,2012,2014), right= TRUE)

ggplot(pf, aes(x=tenure, y=prop_initiated, color = year_joined.bucket)) +
  geom_line(stat='summary', fun.y=median, na.rm=TRUE)

#8.7  平滑化 prop_initiated 与使用时长
ggplot(pf, aes(x = 25 * round(tenure / 25), y = prop_initiated, color = year_joined.bucket)) +
  geom_line(stat = 'summary', fun.y = median, na.rm = TRUE)


ggplot(pf, aes(x = tenure, y = prop_initiated, color = year_joined.bucket)) +
  geom_smooth(na.rm = TRUE)

#8.9  最大的组均值 prop_initiated
mean(pf$prop_initiated[pf$year_joined.bucket == '(2012,2014]'],na.rm = TRUE)

#8.10  经过分组、分面和填色的价格/克拉
ggplot(diamonds, aes(x = cut, y = price/carat, color = color)) +
  geom_point(position = position_jitter(), alpha = 0.33) +
  facet_wrap(~clarity) +
  scale_color_brewer(type = 'div')

#8.11  Gapminder 多变量分析
library('xlsx', quietly = TRUE)
library(reshape2)
library(dplyr, quietly = TRUE)


sugar<- read.xlsx("indicator_sugar_consumption.xlsx", sheetIndex=1)
bmi <- read.xlsx('Indicator_BMI_male_ASM.xlsx', sheetIndex = 1)
bp <- read.xlsx('Indicator_SBP_male_ASM.xlsx', sheetIndex = 1)


df_countries <- function(df){
  colnames(df)[1] <- 'country'
  df$country <- gsub('','_',df$country)
  df$country <- gsub(',','',df$country)
  return(df)
}

countries <- function(df){
  countries_list <- as.character(df$country)
}

years <- function(df){
  years_list <- as.numeric(gsub('X','',colnames(df)[-1]))
}




#get rid of NA rows and columns
sugar[ncol(sugar)] <- NULL
sugar <- sugar[!is.na(sugar[[1]]),]


sugar <- df_countries(sugar)
sugar_countries <- countries(sugar)
sugar_years <- years(sugar)


bmi <- df_countries(bmi)
bmi_countries <- countries(bmi)
bim_years <- years(bmi)

bp <- df_countries(bp)
bp_countries <- countries(bp)
bp_years <- years(bp)


# scatter plot of bp vs. sugar for 2004, colored via bmi (cut into groups)
sugar2004 <- sugar[,c('country','X2004')]
colnames(sugar2004)[2] <- 'sugar'

bp2004 <- bp[, c('country','X2004')]
colnames(bp2004)[2] <- 'bp'

bmi2004 <- bmi[,c('country', '2004')]
colnames(bmi2004)[2] <-'bmi'

df2004 <- merge(sugar2004, bp2004, by = 'country', all=FALSE) %>% merge(bmi2004, by='country',all=FALSE)
df2004$bmi_groups <- cut(df2004$bmi, breaks=5)

ggplot(df2004, aes(x = sugar, y = bp, color = bmi_groups, size = bmi)) +
  geom_point() +
  xlab('Sugar (g per Person per Day') +
  ylab('Blood Pressure') +
  ggtitle('The Relationship Between Sugar Consumption, Blood Pressure, and BMI in 2004')



# line plot of the median bmi(over all countries) vs. time
bmi2 <- melt(bmi, id.vars = 'country', na.rm = TRUE, variable.name = 'year')
bmi2$year <- as.numeric(gsub('X', '', bmi2$year))

ggplot(bmi2 aes(x=year, y = value)) +
  geom_line(stat='summary', fun.y=median) +
  xlab('Year') +
  ylab('Media BMI') +
  ggtitle('Worldwide  Median BMI from 1980-2008')


# line plots of bp vs. year for a random sample of countries
bp2 <- melt(bp, id.vars= 'country', na.rm = TRUE, variable.name = 'year')
bp2$year <- as.numeric(gsub('X','',bp2$year))
bp2$country <- factor(bp2$country)

set.seed(1)
random_countries <- sample(levels(bp2$country), 16)

ggplot(subset(bp2, country %in% random_countries), aes(x=year, y=value)) +
  facet_wrap(~country) +
  geom_line()+
  geom_point()+
  xlab('Year')+
  ylab('Blood Pressure')+
  ggtitle('Blood Pressure from 1980-2008')







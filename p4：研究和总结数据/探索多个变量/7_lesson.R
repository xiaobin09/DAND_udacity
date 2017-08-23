getwd()
setwd('C:/Users/10415/Documents/R/7_8')
getwd()
pf <- read.csv('pseudo_facebook.tsv', sep='\t')

#7.3
library(dplyr)
ggplot(aes(x = gender, y = age),
       data = subset(pf, !is.na(gender))) + geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shap = 4)

ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)


#7.4  绘制条件小结
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n =n()) %>%
  ungroup() %>%
  arrange(age)

head(pf.fc_by_age_gender)


library('ggplot2')
ggplot(aes(x =age, y =median_friend_count),
       data =pf.fc_by_age_gender) +
  geom_line(aes(color = gender))


#(2)
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +geom_line(aes(color = gender), stat = 'summary',
                                                     fun.y = median)
#7.7  重塑数据
library(reshape2)
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)

#7.8 比旅图
ggplot(aes(x = age, y = female / male),
       data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)

# 7.9 第三个定量变量
pf$year_joined <- floor(2014 - pf$tenure/365)

#7.10  切割一个变量
summary(pf$year_joined)
table(pf$year_joined)

pf$year_joined.bucket <- cut(pf$year_joined,
                             c(2004, 2009, 2011, 2012, 2014))

#7.11  绘制在一起
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = median)

#7.12绘制总均值
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean) +
  geom_line(stat = 'summary', fun.y = mean, linetype =2)

# 7.13 好友率
with(subset(pf, tenure >= 1), summary(friend_count / tenure))


#7.14 申请好友数
ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket),)

#7.15 偏差方差折衷
ggplot(aes( x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_smooth(aes(color = year_joined.bucket))
  
# 7.19  重访直方图
yo <- read.csv('yogurt.csv')
head(yo)
qplot(data = yo, x = price, fill = I('#F79420'),binwidth = 10)

# 7.20 购买数量
yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

summary(yo$all.purchases)
#7.21   随时间变化的价格
ggplot(aes(x = time, y = price), data = yo) +
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420'))


#7.23  查看家庭样本
set.seed(4230)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price),
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap( ~id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)

#7.27  更多变量
nci <- read.table('nci.tsv')

colnames(nci) <- c(1:64)

#7.28  热图
#melt the data to long format
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c('gene','case','value')
head(nci.long.samp)

# make the heat map
ggplot(aes(y = gene, x = case, fill = value),
       data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c('blue','red'))(100))



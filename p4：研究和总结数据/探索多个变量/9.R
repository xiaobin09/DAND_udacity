setwd("C:/Users/10415/Documents/R/9_p")
getwd()

#9.3.散点图回顾
library("ggplot2")
data(diamonds)

ggplot(diamonds, aes(x = carat, y = price)) +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99))) +
  geom_point(fill = I('#F79420'), color = I('black'), shape = 21)


#9.4 价格与克拉的关系
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(color = '#F79420', alpha = 1/4 )+
  stat_smooth(method = 'lm')+
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)))
  

#9.7  ggpairs 函数

library(ggplot2)
library(GGally)
library(scales)
library(memisc)



# 9.8  对钻石的需求
library(ggplot2)
plot1 <- qplot(data = diamonds, x = price, binwidth = 100, fill = I('#099DD9'))+
  ggtitle('Price')

plot2 <- qplot(data = diamonds, x = price, binwidth = 0.01, fill = I('#F79420')) +
  ggtitle('Price (log10)') +
  scale_x_log10()


library(gridExtra)
library(grid)
grid.arrange(plot1, plot2, ncol=2)


# 9.10  散点图转换
library(ggplot2)
library(scales)
cuberoot_trans = function()trans_new('cuberoot',
                                     transform = function(x)x^(1/3),
                                     inverse = function(x)x^3)

ggplot(aes(carat, price), data = diamonds) +
  geom_point() +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2,3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

#9.11  复习过度绘制
head(sort(table(diamonds$carat), decreasing = T))
head(sort(table(diamonds$price), decreasing = T))

library(RColorBrewer)

ggplot(aes(carat, price), data = diamonds) +
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') +
  scale_color_brewer(type ='div',
                     guide = guide_legend(title = 'Clarity', reverse=TRUE,
                                          override.aes = list(alpha = 1, size = 2)))+
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2,3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) +
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')


#9.15 价格与克拉和切工
library(RColorBrewer)
library(ggplot2)
ggplot(aes(x = carat, y =price, color = cut), data = diamonds) +
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guid = guide_legend(title = 'Cut',
                                        reverse = TRUE,
                                        override.aes = list(alpha = 1, size=2))) +
  scale_x_continuous(trans = cuberoot_trans(),limits = c(0.2, 3),
                     breaks = c(0.2,0.5,1,2,3)) +
  scale_y_continuous(trans = log10_trans(),limits =c(350,15000),
                     breaks =c(350,1000,5000,10000,15000)) +
  ggtitle('Price (log10) vs Cube Root of Carat and Cut')


# 9.17 价格与克拉和颜色
ggplot(aes(x = carat, y =price, color = color), data = diamonds) +
  geom_point(alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guid = guide_legend(title = 'Color',
                                         reverse = FALSE,
                                         override.aes = list(alpha = 1, size=2))) +
  scale_x_continuous(trans = cuberoot_trans(),limits = c(0.2, 3),
                     breaks = c(0.2,0.5,1,2,3)) +
  scale_y_continuous(trans = log10_trans(),limits =c(350,15000),
                     breaks =c(350,1000,5000,10000,15000)) +
  ggtitle('Price (log10) vs Cube Root of Carat and Color')

#9.20 构建线性模型
library(lattice)
library(MASS)
library(memisc)
m1 <- lm(I(log(price))~I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~. +carat)
m3 <- update(m2, ~. +cut)
m4 <- update(m3, ~. +color)
m5 <- update(m4, ~. +clarity)
mtable(m1, m2, m3, m4, m5)











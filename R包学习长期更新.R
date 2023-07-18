# -------------------------------------------------------------------------#
#                                Introduction 
# 学习常见的R包，包括：
#               readr dplyr tidyr lubridate stringr forcats ggplot2
# 
#               分包来学！长期更新！先掌握常用函数再慢慢探索
#                       
#               push自己尽快掌握R
#
#                                                               by Congyuan He
#                                                                  2023.06.29                                                                   
# -------------------------------------------------------------------------#

# 1各类R包学习 ------------------------------------------------------------------
## 1.2 readr read_excel-------------------------------------------------------------------
library('readr')
library('readxl')

#设置环境路径
setwd("/Users/hecongyuan/Documents/Study/R/R_github/Rpackage_learn/") 

#设置文件路径
PathData <- c("/Users/hecongyuan/Documents/Study/R/R_github/Rpackage_learn/Data/")

#数据读取
TestData <- read_csv(paste(PathData,"test.csv", sep =""))
TestData <- read_excel(paste(PathData,"test.xlsx", sep =""))
load(file = paste(PathData,"test.Rdata", sep = ""))

#数据保存 尽量存成Rdata XlSX MAC系统似乎会保存成乱码只能有txt打开 但是也可以用R正常读
write_csv(TestData, file = paste(PathData,"test.csv",sep = ""))#用txt格式打开正常，也可以正常读取
write.table(TestData, file = paste(PathData,"test.csv",sep = ""))#用txt格式打开正常，也可以正常读取
write.table(TestData, file = paste(PathData,"test.xlsx",sep = ""))
save(test, file = paste(PathData, "test.Rdata", sep = ""))



## 1.2 dplyr -------------------------------------------------------------------


## 1.3 tidyr -------------------------------------------------------------------


## 1.4 lubridate ---------------------------------------------------------------


## 1.5 stringr -----------------------------------------------------------------


## 1.6 forcats -----------------------------------------------------------------


## 1.7 ggplot2 -----------------------------------------------------------------


# 2 实用代码实践笔记 ----------------------------------------------------------------

## 2.1实用快捷操作####
print('command shift D 行末快速复制')
print ('shift N 新建脚本')
print ('command shift R 新建代码层次结构 加#确定层级')
print('环境窗口的 import Dataset直接预览导入数据 csv格式用readr打开')
print('Ctrl L 清除命令窗口')

## 2.2路径管理####
print('path_dat <- c("D:/代码和外源数据/郭冰/zcm/SES/prepare/")')#设置数据路径
print('path_func <- c("D:/代码和外源数据/郭冰/zcm/SES/function_code/")')#设置所需函数路径

print('source(paste(path_func,"summarymm.R",sep=""))')#加载函数
print('load(paste(path_dat,"DATA_ana.Rdata",sep = "")')#加载数据

## 2.3泊松回归实践####
library(datasets)#需要用的数据包
library(arm)#se.coef提取模型系数
library(jtools)#提供plot_sum()画图
#创建6种颜色向量用于分组
colors <- c("Red","Blue","Gold","Black","Pink","Green")
#创建一个list放入不同U对应的分布数据
poisson.dist <- list()
u <- c(1:6)
for (i in 1:6) {
  poisson.dist[[i]] <- c(dpois(0:20,i))#每个U对应的possion分布数据
}
#作图 看一下possion分布啥样子
plot(unlist(poisson.dist[1]),type = "o", xlab = "y", ylab = "P(y)",col = colors[i])
for (i in 1:6) {
  lines(unlist(poisson.dist[i]),type = "o", xlab = "y", ylab = "P(y)",col = colors[i])
}

#案例1——对于计数数据
?warpbreaks#该数据集查看每根固定长度的织机上不同类型的织机发生了多少经纱断裂
data <- warpbreaks#数据包中的纱线数据
ls.str(data)
hist(data$breaks)
mean(data$breaks)
#28.15
var(data$breaks)
#174.20
#方差远远大于均值，表明模型中出现过分散现象
#拟合poisson model
poisson.model <- glm(breaks ~ wool + tension, data, family = poisson(link = "log"))
summary(poisson.model)

#qcc包自带函数检验是否存在过度离势
deviance(poisson.model)/df.residual(poisson.model)
qcc.overdispersion.test(data$breaks,type = "poisson")

#为获得更准确的标准误，用拟泊松模型
poisson.model2 <- glm(breaks ~ wool + tension, data, family = quasipoisson(link = "log"))
summary(poisson.model2)
#P小于0.05 存在过度离势
#提取模型系数
coef1 <- coef(poisson.model)
coef2 <- coef(poisson.model2)
se.coef1 <- se.coef(poisson.model)
se.coef2 <- se.coef(poisson.model2)
models.both <- cbind(coef1,coef2,se.coef1,se.coef2, exponent = exp(coef1))
models.both
#如果我们将羊毛类型从A更改为B，则假设所有其他变量都相同，则折断次数将减少18.6%。
plot_summs(poisson.model2, scale = TRUE, exp = TRUE)
plot_summs(poisson.model, poisson.model2, scale = TRUE, exp = TRUE)

#分组算出均值极其标准误

predict(poisson.model2,data.frame(tension = "L", wool = "A"),type = "response", se.fit = TRUE)


#案例2——对于速率数据
library(ISwR)#用这个案例数据

??eba1977
data(eba1977)
cancer.data <- eba1977
ls.str(eba1977)

#要建立一个offset项目
logpop <- log(cancer.data[,3])
new.cancer.data <- cbind(cancer.data, logpop)
new.cancer.data 
#建模
poisson.model.rate <- glm(cases ~ city + age +offset(logpop), family = poisson(link = "log"), 
                          data = new.cancer.data)
summary(poisson.model.rate)

#后续参考<https://blog.csdn.net/dege857/article/details/119593047>


#













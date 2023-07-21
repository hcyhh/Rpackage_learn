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


##SES的LCA分类分析####
#使用3%样例数据测试
library('readr')
library('readxl')
library('tidyverse')
library("poLCA")
data <- read_excel("/Users/hecongyuan/Documents/Temporary/西南队列/样例数据及变量对照表/3%基础样例数据20200611.xlsx")
#变量生成
data <- data %>% 
  mutate(#基础变量生成
    Occupation=case_when(A7=="1"~"Agricultural_forestry_and_fishery_worker",
                         A7=="2"~"Retiree",
                         A7=="3"~"Factory_worker",
                         A7=="4"~"Homemaker",
                         A7=="5"~"Administrator",
                         A7=="6"~"Private_business_owner",
                         A7=="7"~"Professionals",
                         A7=="8"~"Laid_off_workers",
                         A7=="9"~"Sales_and_service_staff",
                         A7=="10"~"Un-classified"),
    Marital=case_when(A5==1 ~ 'Married/cohabited',
                      A5%in%c(2,3,4)~'others',
                      TRUE ~ 'unknown'),
    Education=case_when(A6%in%c(1,2) ~ '<6',
                        A6==3 ~ '6~9',
                        A6==4 ~ '9~12',
                        A6%in%c(5,6) ~ '>12',
                        TRUE ~ 'unknown') ,
    Household_Income=case_when(A13==1 ~ '<12000',
                               A13==2 ~ '12000-19999',
                               A13==3 ~ '20000-59999',
                               A13==4 ~ '60000-99999',
                               A13==5 ~ '100000-199999',
                               A13==6 ~ '>200000',
                               TRUE ~ 'unknow'),
    poor_family=case_when(A15==1 ~ '是',
                         A15==2 ~ '否',
                         TRUE ~ 'unknow')

  ) %>% 
  dplyr::select(Occupation,Marital,Education,Household_Income,poor_family) 

#unknow处理NA
data[data == 'unknow'] <- NA

#查看缺失数据函数
QS <- function(data){
  columns <- c(colnames(data))
  show <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(data) = columns
  colnames(show) <- names(data)
  for (i in 1:ncol(data)) {
    show[1,i] <-sum(is.na(data[colnames(data)[i]]))
  }
  show
}
QS(data)

#删除NA
data <- na.omit(data)

#初步的统计描述
stat_desc_2lists<- function(dat) {
  # Initialize empty lists to store results
  num_results <- list()
  char_results <- list()
  
  # Get the names of variables in the dataset
  var_names <- names(dat)
  
  # Loop over each variable
  for (var in var_names) {
    # Get the type of the variable
    var_type <- class(dat[[var]])
    
    # Perform statistical description based on variable type
    if (var_type == "numeric") {
      num_results[[var]] <- summary(dat[[var]])
    } else if (var_type %in% c("factor", "character")) {
      if (length(table(dat[[var]])) > 10) {
        char_results[[var]] <- "Abnormal Variable"
      } else {
        char_results[[var]] <- table(dat[[var]])
      }
    }
  }
  
  # Return the results lists
  return(list(Numeric = num_results, Character = char_results))
}
results <- stat_desc_2lists(data)
results$Character
results$Numeric

ls.str(data)
#统一修改变变量类型
cov_factor <- colnames(data)
data[,cov_factor] <- data.frame(lapply(data[,cov_factor], function(x) as.factor(x)))

#查看所有变量的因子水平
columns <- colnames(data)
# 循环遍历所有列
for (col in columns) {
  # 判断当前列是否为因子变量
  if (is.factor(data[[col]])) {
    # 获取因子变量的分类水平
    factor_levels <- levels(data[[col]])
    # 打印输出
    cat(paste("因子变量", col, "的分类水平:"))
    print(factor_levels)
  }
}

#更改因子水平
data$Education <- factor(data$Education, levels = c("<6","6~9","9~12",">12"))
data$Household_Income <- factor(data$Household_Income, levels = c("<12000","12000-19999","20000-59999","60000-99999","100000-199999",">200000"))

#LCA
#统一修改变变量类型
cov_factor <- colnames(data)
data[,cov_factor] <- data.frame(lapply(data[,cov_factor], function(x) as.numeric(x)))

f1 <- cbind(Occupation, Marital, Education, Household_Income, poor_family)~1
LCA2 <- poLCA(f1, data=data, nclass=3, graphs=TRUE)


max_II <- -100
min_bic <- 100
for(i in 2:5){
  lc <- poLCA(f1, data, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model

lcmodel <- reshape2::melt(LCA_best_model$probs, level=2)
zp1 <- ggplot(lcmodel,aes(x = L2, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(Var1 ~ .) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
zp1 <- zp1 + labs(x = "Fragebogenitems",y="Anteil der Item-\nAntwortkategorien", fill ="Antwortkategorien")
zp1 <- zp1 + theme( axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank())
zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
print(zp1)


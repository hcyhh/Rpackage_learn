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
path_data <- c("/Users/hecongyuan/Documents/Study/R/R_github/Rpackage_learn/Data/")
path_function <- c("/Users/hecongyuan/Documents/Study/R/R_github/Rpackage_learn/Function/")
#加载函数
source(paste0(path_function,"function.R"))
#数据读取
TestData <- read_csv(paste(path_data,"test.csv", sep =""))
TestData <- read_excel(paste(path_data,"test.xlsx", sep =""))
load(file = paste(path_data,"test.Rdata", sep = ""))

#数据保存 尽量存成Rdata XlSX MAC系统似乎会保存成乱码只能有txt打开 但是也可以用R正常读
write_csv(TestData, file = paste(path_data,"test.csv",sep = ""))#用txt格式打开正常，也可以正常读取
write.table(TestData, file = paste(path_data,"test.csv",sep = ""))#用txt格式打开正常，也可以正常读取
write.table(TestData, file = paste(path_data,"test.xlsx",sep = ""))
save(test, file = paste(path_data, "test.Rdata", sep = ""))



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

tab
## 2.4 SES的LCA分类分析####
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

# #查看缺失数据函数
# QS <- function(data){
#   columns <- c(colnames(data))
#   show <- data.frame(matrix(nrow = 0, ncol = length(columns)))
#   colnames(data) = columns
#   colnames(show) <- names(data)
#   for (i in 1:ncol(data)) {
#     show[1,i] <-sum(is.na(data[colnames(data)[i]]))
#   }
#   show
# }
QS(data)

#删除NA
data <- na.omit(data)

#初步的统计描述
# stat_desc_2lists<- function(dat) {
#   # Initialize empty lists to store results
#   num_results <- list()
#   char_results <- list()
#   
#   # Get the names of variables in the dataset
#   var_names <- names(dat)
#   
#   # Loop over each variable
#   for (var in var_names) {
#     # Get the type of the variable
#     var_type <- class(dat[[var]])
#     
#     # Perform statistical description based on variable type
#     if (var_type == "numeric") {
#       num_results[[var]] <- summary(dat[[var]])
#     } else if (var_type %in% c("factor", "character")) {
#       if (length(table(dat[[var]])) > 10) {
#         char_results[[var]] <- "Abnormal Variable"
#       } else {
#         char_results[[var]] <- table(dat[[var]])
#       }
#     }
#   }
#   
#   # Return the results lists
#   return(list(Numeric = num_results, Character = char_results))
# }
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

# 3 毕业设计代码 ----------------------------------------------------------------
## 3.1 数据变量整理####
#处理数据函数
library(tidyverse)
library(Hmisc)
library(lubridate)
#设置文件路径
path_data <- c("/Users/hecongyuan/Documents/Study/R/R_github/Rpackage_learn/Data/")
path_function <- c("/Users/hecongyuan/Documents/Study/R/R_github/Rpackage_learn/Function/")
load(file = paste0(path_data,"何从源3%样例数据及其代码/test.rdata")) 

#处理重复值
colnames(test)[str_which(colnames(test), "\\.x")]
test <- test %>% 
  dplyr::rename(A2 = A2.x)


#处理队列问卷变量（不包括自报疾病）
preprocess_survey <- function(dat_input) {
  thead <- Sys.time()
  ##start ####
  ## Part 1: 指定并提取分析变量 ----------
  ## 指定问卷变量 ####
  var_info <- c("调查日期", "调查开始时间", "个人编码", 
                "age", "A1", "A2", "A4", "A5", "A6", "A7", "A8A", "A13", "A15")
  
  var_inexposure <- c("B1", "B2", "B3", "B6", paste0("B6", LETTERS[1:7]),
                      "B10A", "B8", "B8A", "B8B", "B11", "B12", "B13")
  
  var_drinking <- c("C2", "C4", "D1", "D3", "D11")
  
  var_disease <- c("E2",      
                   "E3A", "E3A_AGE", "E3A_A",
                   "E3B", "E3B_AGE", "E3B_A", 
                   "E6", "E7", "E10A", "E10B", "E10C", "E10D")
  
  var_activity <- c("F1", "F13", "F15", "F16", "FF")
  
  var_women <- c("G1", "G2", "G2A", "G4A", "G5A1", "G6")
  
  var_food <- c("H1a",  "H1b",  "H1c",
                "H2_a", "H2_b", "H2_c","H2_d","H2_e", "H2_f", "H3", 
                paste0(rep(paste0("H", c(4:16)), each = 5), c("", paste0("B", 1:4))), 
                paste0(rep(paste0("H", c(4:16)), each = 2), c("_NUM", "_NUM1")), 
                "H23", "H27")
  # "薯类", "红肉类及制品", "家禽及制品", "水产_海鲜产品", #"pure_alchol", 
  # "蛋类及制品", "新鲜蔬菜", "豆制品", "腌制蔬菜", "新鲜水果", "乳类及制品",
  # "大米added", "面食added", "杂粮added","oil_plant", "oil_animal", "salt", "tea_weight",
  # "energy", "protein", "fat", "carbohydrate", "RC1","RC2", "RC3",
  # "MED_杂粮", "MED_鱼", "MED_蔬菜", "MED_豆类", "MED_水果", "MED_USratio",
  # "MED_红肉", "MED_alchol", "ratio", "MED_score_x",
  # "DASH_杂粮addscore", "DASH_新鲜水果score", "DASH_新鲜蔬菜score", "DASH_豆制品score",
  # "DASH_红肉类及制品score", "DASH_乳类及制品score", "DASH_saltaddedscore", 
  # "DASH_score")
  
  var_psychology <- c("I1", "I3A", "I3b", "I3c", "I3d",  
                      "I4", "I8A", "I8b", "I9A", "I9b")
  
  var_plus <- c("E1A4")
  
  var_survey_all <- c(var_info, var_inexposure, var_drinking, var_disease, 
                      var_activity, var_women, var_food, var_psychology,var_plus)
  
  ## 提取分析变量 ####
  dat_tmp <- dat_input %>% 
    dplyr::select(all_of(var_survey_all))
  
  ## Part 2: 调查问卷变量处理 ----------
  ## A.一般情况 ####
  t1head <- Sys.time()
  
  dat_tmp <- dat_tmp %>% 
    mutate(个人编码 = as.character(个人编码),
           调查日期 = as.character(调查日期)) %>% 
    
    # 1.性别
    #mutate(male = as_factor(3 - as.numeric(A1))) %>% 
     mutate(gender = factor(A1, levels = c(1, 2),
                                   labels = c("male", "female"))) %>% 
    #                  fct_explicit_na(., na_level = NA_character_)) %>% 
    #male(男2，女1)
    
    # 2.年龄，6级，10岁一个年龄段
    mutate(age_level5 = case_when(age >= 30 & age < 40 ~ 1,
                                  age >= 40 & age < 50 ~ 2,
                                  age >= 50 & age < 60 ~ 3,
                                  age >= 60 & age < 70 ~ 4,
                                  age >= 70            ~ 5,
                                  TRUE ~ NA_real_) %>% 
             factor(levels = c(1:5))) %>% 
    
    # 3.是否少数民族，两级
    mutate(ethnicity2 = case_when(A2 %in% c(2:7) ~ 1,
                                  A2 == 1 ~ 2) %>%
             factor(levels = c(1,2)), # 1:non-Han 2:Han 
           ethnicity7 = as.factor(A2)) %>%  # 1-Han, 2-Dong, 3-Buyi, 4-Yi, 5-Miao, 6-Bai, 7-Zang
    
    # 4.教育程度,三级
    mutate(education3 = case_when(A6 %in% c(1, 2, 3) ~ 1,
                                  A6 %in% c(4) ~ 2,
                                  A6 %in% c(5, 6) ~ 3) %>% as.factor(),
           education6 = as.factor(A6)) %>% 
    mutate(education2 = case_when(A6 %in% c(1, 2, 3) ~ 1,
                                  A6 %in% c(4,5, 6) ~ 2) %>% as.factor()) %>%
    
    # c("未正规上过学", "小学", "初中", "高中", "大专", "本科及以上")
    #         1           2       3       4      5          6
    # 5.有无医保
    mutate(medicare = case_when(A8A == 5 ~ 1,
                                A8A != 5 ~ 2,
                                TRUE     ~ NA_real_) %>% 
             factor(., levels = c(1, 2))) %>% 
    # labels = c("无医保", "有医保")
    
    # 6.家庭年收入 ，5分类，三分类
    mutate(income5 = case_when(A13 == 1 ~ 1,
                               A13 == 2 ~ 2,
                               A13 == 3 ~ 3,
                               A13 == 4 ~ 4,
                               A13 %in% c(5,6) ~ 5,
                               TRUE ~ NA_real_) %>% 
             factor(levels = c(1:5),labels = c("<12k", "12k-", "20k-", "60k-", "100K-"))) %>% 
    mutate(income3 = case_when(A13 %in% c(1, 2) ~ 1,
                               A13 %in% c(3, 4) ~ 2,
                               A13 %in% c(5, 6) ~ 3) %>% 
             factor(levels = c(1:3),labels = c("<20k-", "20k-", "100K-"))) %>%
    mutate(income2 = case_when(A13 %in% c(1, 2) ~ 1,
                               A13 %in% c(3,4,5, 6) ~ 2) %>% 
             factor(levels = c(1:2),labels = c("<20k-", "20k-")))%>%
    # 7.婚姻状况2分类
    mutate(Marital_status2 = case_when(A5 %in% c(2,3,4) ~ "1",
                                       A5 ==        1   ~ "2",
                                       TRUE             ~ NA_character_) %>%
             factor(.,levels = c("1","2"),labels = c("其他","已婚/同居")),
           # labels = c("其他","已婚/同居")
           Marital_status4 = as.factor(A5))  %>%
    
    # 8.职业变量
    mutate(occupation3 = case_when( A7 %in% c(2,4,8)          ~ 1,
                                    A7 == 1                   ~ 2,
                                    A7 %in% c(3,5,6,7,9,10) ~ 3) %>%
             factor(., levels=c(1:3), labels=c("无业，离/退休","农林牧渔劳动者","有业"))) %>%
    # labels=c("无业，离/退休","农林牧渔劳动者","有业")
    # mutate(occupation2 = case_when( A7  == 1                   ~ 1,
    #                                 A7  != 1                   ~ 2) %>%
    #          factor(., levels=c(1:2), labels=c("农林牧渔劳动者","有业或离退休"))) %>%
    mutate(occupation9 = case_when(A7 %in% c(4,8) ~ 1, #"无业"(家务,待业/下岗)
                                   A7 == 1 ~ 2,        #"农林牧渔劳动者"
                                   A7 == 2 ~ 3,        #"离/退休"
                                   A7 == 3 ~ 4,        #"工人"
                                   A7 == 9 ~ 5,        #销售及服务工作人员"
                                   A7 == 6 ~ 6,        #"私营业主"
                                   A7 == 7 ~ 7,        #"专业技术人员"
                                   A7 == 5 ~ 8,        #"行政及管理人员"
                                   A7 == 10 ~ 9,       #"其他"
                                   TRUE ~ NA_real_) %>%
             factor(., levels = c(1:9), labels = c("无业","农林牧渔劳动者","离/退休","工人",
                                                   "销售及服务工作人员","私营业主","专业技术人员",
                                                   "行政及管理人员","其他"))) %>%
    #         labels = c("无业","农林牧渔劳动者","离/退休","工人",销售及服务工作人员",
    #                    "私营业主","专业技术人员","行政及管理人员","其他")
    mutate(unemploy = case_when(A7 %in% c(2,4,8) ~ "1",
                                A7 %in% c(1,3,4,5,6,7,9,10) ~ "2") %>%
             factor(., levels=c("1","2"), labels = c("无业","有业"))) %>%
    # labels = c("无业","有业")

    # 9.农村城市
    mutate(urban = case_when(A4 == 1 ~ 1,
                             A4 %in% c(2,3) ~ 2,
                             TRUE ~ NA_real_) %>%
             factor(.,levels=c(1,2), labels = c("农村居民","城镇居民"))) %>%
    # labels = c("农村居民","城镇居民")
    #1 农业户口2 非农业户口3 统一居民户口4 没有户口
    # 11.平困户认定
    mutate(ppoor = factor(as.numeric(test$A15),labels = c("是","否"))) # 1是2否
  t1end <- Sys.time()
  t1diff <- round(t1end - t1head, 2)
  cat("已完成：一般情况变量", " 耗时：", t1diff, "秒", "\n")
  
  ## B.吸烟情况及个人环境因素暴露 ####
  t2head <- Sys.time()
  
  dat_tmp <- dat_tmp %>% 
    # 吸烟状态分类，二分类，三分类
    mutate(smoke3 = factor(B1, 
                           levels = c(1:3),
                           labels = c("从不吸烟", "吸烟", "戒烟")),
    
    # 被动吸烟分类，二分类
    sec_smoke = factor(B8,levels = c(2, 1),labels = c("否", "是"))) 
  # mutate(sec_smoke = factor(2 - as.numeric(B8), levels = c(0, 1))) 
  # sec_smoke = factor(B8,
  #                         levels = c(2, 1),
  #                         labels = c("no", "yes")) %>% 
  #        fct_explicit_na(., na_level = NA_character_)) %>% 
  
  # # 开始吸烟年龄
  # mutate(smoke_start_age = case_when(B3 < 20           ~ 1,
  #                                    B3 >= 20 & B3 <25 ~ 2,
  #                                    B3 >= 25          ~ 3) %>% 
  #          factor(levels = 1:3,
  #                 labels = c("<20", "20-", "25-"))) %>% 
  # 
  # # 吸烟持续时间
  # mutate(smoke_time = case_when(B1 == 1 ~ 0,                   # “从不吸烟”：0 年
  #                               B1 == 2 ~ age - B3,            # “吸烟”：年龄-开始吸烟年龄
  #                               B1 == 3 ~ age - B3 - B2)) %>%  # “戒烟”：年龄-开始吸烟年龄-戒烟年龄
  # # 被动吸烟时间
  # mutate(sec_smoke_time = case_when(B8 == 2 ~ 0,                 # “无被动吸烟”：0年
  #                                   B8 == 1 ~ B8A * 52 * B8B))   # “有被动吸烟”
  
  ## 室内空气污染
  dat_tmp <- dat_tmp %>%
    mutate(solid_fuel = ifelse(str_detect(.$B12, '2') | str_detect(.$B12, '3'), 1, 0))
  
  dat_tmp <- dat_tmp %>%
    mutate(indoor_pollution_5 = case_when(
      B11 %in% c(1, 2, 3) & solid_fuel == 1 & B13 == 3          ~ "very heavy",
      B11 %in% c(1, 2, 3) & solid_fuel == 0 & B13 == 3          ~ "heavy",
      B11 %in% c(1, 2, 3) & solid_fuel == 1 & B13 %in% c(1, 2)  ~ "moderate",
      B11 %in% c(1, 2, 3) & solid_fuel == 0 & B13 %in% c(1, 2)  ~ "light",
      B11 %in% c(4, 5)                                          ~ "very light",
      TRUE ~ NA_character_) %>% 
        factor(levels = c("very light", "light", "moderate", "heavy", "very heavy"))) %>% 
    mutate(indoor_pollution_3 = case_when(
      indoor_pollution_5 == "very heavy"                      ~ "heavy",
      indoor_pollution_5 %in% c("heavy", "moderate", "light") ~ "moderate",
      indoor_pollution_5 == "very light"                      ~ "light",
      TRUE ~ NA_character_) %>% 
        factor(levels = c("light", "moderate", "heavy")))
  
  ## C.饮酒情况 ####
  dat_tmp <- dat_tmp %>% 
    mutate(alcohol3 = case_when(C2 == 1                    ~ 1,
                                C2 %in% c(2,3,4) | C4 == 1 ~ 2,
                                C2 == 5 | C4 %in% c(2,3)   ~ 3,
                                TRUE ~ NA_real_) %>% 
             factor(levels = c(1:3), 
                    labels = c("从不饮酒", "偶尔饮酒", "经常饮酒")))
  
  ## D.饮茶情况 ####
  dat_tmp <- dat_tmp %>%
    mutate(tea = case_when(D1 == 2        ~ 1,
                           D3 %in% c(1,2) ~ 2,
                           D3 %in% c(3,4) ~ 3,
                           TRUE ~ NA_real_) %>% 
             factor(levels = c(1:3),
                    labels = c("从不饮茶", "偶尔饮茶", "经常饮茶")))
  
  t2end <- Sys.time()
  t2diff <- round(t2end- t2head, 2)
  cat("已完成：吸烟、饮酒、饮茶", " 耗时：", t2diff, "秒", "\n")
  
  ## H.饮食情况 ####
  t3head <- Sys.time()
  
  # 函数：计算食物的食用频率
  # cal_foodfreq <- function(dat, var_index){
  #   
  #   SYS <- paste0(rep(var_index, times = 5), c("", "B1", "B2", "B3", "B4"))
  #   
  #   index_notNA <- !is.na(dat[, SYS[1]])
  #   
  #   ### 有些是因为没有填写问卷导致的NA，这部分不能替换为0
  #   dat[index_notNA, SYS][is.na(dat[index_notNA, SYS])] <- 0
  #   
  #   # 食用频率统一换算为(次数/周)
  #   var_new <- as_vector(dat[, SYS[2]]*7 +   
  #                        dat[, SYS[3]] + 
  #                        ((dat[, SYS[3]]*12)/365)*7 + 
  #                        dat[, SYS[4]]/52)
  #   
  #   return(var_new)
  # }
  
  # 函数：将食用频率划分为若干等级
  # rate_foodfreq <- function(var){
  #   eatlevel = c("never or little", "1-3d/w", "4-6d/w", "everyday") #, "unclassified")
  #   
  #   var_new <- case_when(var > 6           ~ 3,
  #                        var >=4 & var <=6 ~ 2,
  #                        var >=1 & var <4  ~ 1,
  #                        var <1            ~ 0,
  #                        TRUE              ~ NA_real_) %>% 
  #     factor(levels = c(0,1,2,3),
  #            labels = eatlevel)
  #   return(var_new)
  # }
  
  # 计算各类食物的食用频率(次/周)
  # dat_tmp <- dat_tmp %>% 
  #   mutate(n_vegetable         = cal_foodfreq(dat_tmp, 'H12'),  # 蔬菜
  #          n_fruit             = cal_foodfreq(dat_tmp, 'H15'),  # 水果
  #          n_milk              = cal_foodfreq(dat_tmp, 'H16'),  # 奶制品
  #          n_redmeat           = cal_foodfreq(dat_tmp, 'H8'),   # 红肉
  #          n_poultrymeat       = cal_foodfreq(dat_tmp, 'H9'),   # 家禽
  #          n_pickled_vegetable = cal_foodfreq(dat_tmp, 'H14'))  # 腌制蔬菜
  
  # 将食用频率划分为若干等级
  # dat_tmp <- dat_tmp %>%
  #   mutate(Vegetable_C         = rate_foodfreq(Vegetable),
  #          Fruit_C             = rate_foodfreq(Fruit),
  #          Milk_C              = rate_foodfreq(Milk),
  #          Meet_red_C          = rate_foodfreq(Meet_red),
  #          Meet_poultry_C      = rate_foodfreq(Meet_poultry),
  #          Pickled_vegetable_C = rate_foodfreq(Pickled_vegetable))
  
  # 计算每类食物摄入量/周
  # dat_tmp <- dat_tmp %>% 
  #   mutate(in_vegetable         = n_vegetable * H12_NUM, 
  #          in_fruit             = n_fruit * H15_NUM,
  #          in_milk              = n_milk * H16_NUM,
  #          in_redmeat           = n_redmeat * H8_NUM,
  #          in_poultrymeat       = n_poultrymeat * H9_NUM,
  #          in_pickled_vegetable = n_pickled_vegetable * H14_NUM)
  # 
  # dat_tmp <- dat_tmp %>% 
  #   mutate(intake_vegetable = 新鲜蔬菜,
  #          intake_fruit     = 新鲜水果,
  #          intake_redmeat   = 红肉类及制品,
  #          intake_whitemeat = 家禽及制品)
  # 
  # # 专门判定蔬菜水果、红肉的摄入量二分类
  # dat_tmp <- dat_tmp %>% 
  #   mutate(intake_fat_level     = fat/7 >= 75,
  #          intake_redmeat_level = intake_redmeat/7 >= 75,
  #          intake_fruitveg_level = (intake_fruit + intake_vegetable)/7 >= 500)
  # 
  t3end <- Sys.time()
  t3diff <- round(t3end- t3head, 2)
  cat("已完成：饮食情况相关变量。", " 耗时：", t3diff, "秒", "\n")
  
  ## F.体力活动情况 ####
  t4head <- Sys.time()
  
  dat_tmp <- dat_tmp %>%
    # 劳动强度 
    mutate(Labour_str = case_when(F1 %in% c(1,2,5) ~ "Lightly",
                                  F1 == 3 ~ "Generally",
                                  F1 == 4 ~ "Heavily",
                                  FF == 2 ~ "Farming")) %>% 
    # 体育锻炼 
    mutate(PA_F13 = case_when(F13 %in% c(1,2) ~ "low",
                              F13 %in% c(3,4) ~ "moderate",
                              F13 == 5        ~ "high")) %>% 
    # 每周参加体育锻炼的时长 
    mutate(PA_F15 = case_when(F15 < 2               ~ "low",
                              F15 >= 2 & F15 <= 3   ~ "moderate",
                              F15 > 3               ~ "high",
                              TRUE                  ~ "low")) %>% 
    # 出汗或心跳加快 
    mutate(PA_F16 = case_when(F16 %in% c(1,2,3) ~ "low",
                              F16 == 4          ~ "moderate",
                              F16 == 5          ~ "high")) %>% 
    mutate_at(vars(c("PA_F13", "PA_F15", "PA_F16")), ~factor(., levels = c("low", "moderate", "high")))
  # MET(Metabolic Equivalent)
  # mutate(MET = non_sedentary) %>% 
  # mutate(MET_level = Hmisc::cut2(.$MET, g = 4))
  
  t4end <- Sys.time()
  t4diff <- round(t4end- t4head, 2)
  cat("已完成：体力活动相关变量。", " 耗时：", t4diff, "秒", "\n")
  
  ## G.女性生育史 ####
  t5head <- Sys.time()
  
  dat_tmp <- dat_tmp %>% 
    # 绝经
    mutate(menopausal = case_when(
      A1 == 2 & G2 == 4 ~ 1,  # 女性绝经
      A1 == 2 & G2 %in% c(1,2,3) ~ 0, # 女性未绝经
      A1 == 1 ~ 2)) # 男性
  
  
  ## E3.药物使用转码 ####
  dat_tmp <- dat_tmp %>% 
    # 高血压药物使用
    mutate(HPT_drug = case_when(E3A == 1 & E3A_A == 1 ~ 2,
                                E3A == 1 & E3A_A == 2 ~ 1,
                                E3A == 1 & E3A_A == 3 ~ 0,
                                TRUE ~ NA_real_) %>% 
             factor(levels = c(0, 1, 2),
                    labels = c("Never", "Intermittent", "Regular"))) %>% 
    
    # 糖尿病药物使用
    mutate(DB_drug = case_when(E3B == 1 & E3B_A == 1 ~ 2,
                               E3B == 1 & E3B_A == 2 ~ 1,
                               E3B == 1 & E3B_A == 3 ~ 0,
                               TRUE ~ NA_real_) %>% 
             factor(levels = c(0, 1, 2),
                    labels = c("Never", "Intermittent", "Regular")))
  
  ## E10.家族史转码 ####
  
  # 家族史(Family Health History) 变量说明：  
  # E6 有几个兄弟姐妹
  # E7 有几个子女
  # E10A	生母患有下列哪种疾病
  # E10B	生父患有下列哪种疾病
  # E10C	亲兄弟姐妹患有下列哪种疾病
  # E10D	亲生子女患有下列哪种疾病
  
  dat_tmp <- dat_tmp %>% 
    # 若兄弟姐妹(E6)或子女(E7)人数为0，则将E10C或E10D的值由NA改为0
    mutate_at(c("E10A", "E10B", "E10C", "E10D"), as.character) %>% 
    mutate(E10C = case_when(E6 == 0 ~ "0",
                            TRUE    ~ E10C),
           E10D = case_when(E7 == 0 ~ "0",
                            TRUE    ~ E10D)) %>% 
    # 将4个题目的答案合并在一起
    mutate(familyHH = paste0(E10A, E10B, E10C, E10D)) %>% 
    # 高血压家族史
    mutate(FH_HPT = case_when(str_detect(familyHH, "1") ~ 1,
                              str_detect(familyHH, "NA") ~ NA_real_,
                              str_detect(familyHH, "[^1]") ~ 0) %>% 
             factor(levels = c(0, 1),
                    labels = c("no", "yes"))) %>% 
    # 中风家族史
    mutate(FH_stroke = case_when(str_detect(familyHH, "2") ~ 1,
                                 str_detect(familyHH, "NA") ~ NA_real_,
                                 str_detect(familyHH, "[^2]") ~ 0) %>% 
             factor(levels = c(0, 1),
                    labels = c("no", "yes"))) %>%  
    # 急性心梗家族史
    mutate(FH_AMI = case_when(str_detect(familyHH, "3") ~ 1,
                              str_detect(familyHH, "NA") ~ NA_real_,
                              str_detect(familyHH, "[^3]") ~ 0) %>% 
             factor(levels = c(0, 1),
                    labels = c("no", "yes"))) %>% 
    # 糖尿病家族史
    mutate(FH_DB = case_when(str_detect(familyHH, "4") ~ 1,
                             str_detect(familyHH, "NA") ~ NA_real_,
                             str_detect(familyHH, "[^4]") ~ 0) %>% 
             factor(levels = c(0, 1),
                    labels = c("no", "yes"))) %>% 
    # 恶性肿瘤家族史
    mutate(FH_cancer = case_when(str_detect(familyHH, "5") ~ 1,
                                 str_detect(familyHH, "NA") ~ NA_real_,
                                 str_detect(familyHH, "[^5]") ~ 0) %>% 
             factor(levels = c(0, 1),
                    labels = c("no", "yes"))) %>% 
    # 精神疾患家族史
    mutate(FH_mental = case_when(str_detect(familyHH, "6") ~ 1,
                                 str_detect(familyHH, "NA") ~ NA_real_,
                                 str_detect(familyHH, "[^6]") ~ 0) %>% 
             factor(levels = c(0, 1),
                    labels = c("no", "yes")))  
  
  t5end <- Sys.time()
  t5diff <- round(t5end- t5head, 2)
  cat("已完成：女性生育史、服药史、家族史。", " 耗时：", t5diff, "秒", "\n")
  ## plus####
  dat_tmp$E1A4 <- as.numeric(dat_tmp$E1A4)
  dat_tmp <- dat_tmp %>% 
    mutate(pain_score = case_when(E1A4 == 1 ~ 1,
                                  E1A4 == 2 ~ 2,
                                  E1A4 == 3 ~ 3,
                                  E1A4 == 4 ~ 4,
                                  E1A4 == 5 ~ 5,
                                  TRUE ~ NA_real_) %>%
             factor(levels = c("1", "2", "3","4","5"))) %>%
    mutate(self_health = case_when(E2 %in% c(1,2) ~ "1",
                                   E2 == 3        ~ "2",
                                   E2 %in% c(4,5) ~ "3",
                                   TRUE ~ NA_character_) %>%
             factor(levels = c("1", "2", "3")))
  
  ## 函数结束 ####
  dat_output <- dat_tmp %>% 
    dplyr::select(all_of(var_survey_all),
           gender, age, age_level5, ethnicity2, ethnicity7,
           education2,education3, education6, medicare, income2, income3, income5, Marital_status2,
           occupation3,occupation9,unemploy,urban,ppoor,
           smoke3, 
           sec_smoke, 
           indoor_pollution_3, alcohol3, tea,
           # intake_vegetable, intake_fruit, intake_redmeat, intake_whitemeat, 
           # intake_fat_level, intake_redmeat_level, intake_fruitveg_level,
           # energy, protein, fat, carbohydrate,
           # MED_score_x, MED_alchol, DASH_score, 
           PA_F13, PA_F15, PA_F16, 
           HPT_drug, DB_drug,E3B_A,
           FH_HPT, FH_stroke, FH_AMI, FH_DB, FH_cancer,
           menopausal,
           pain_score, self_health)
  
  
  nr <- nrow(dat_output)
  nc <- ncol(dat_output)
  
  tend <- Sys.time()
  tdiff <- round(tend- thead, 2)
  cat("提取并处理问卷变量：", nr, "行，", nc, "列", "\n", 
      "\n",
      "共耗时：", tdiff, "秒",     "\n")
  
  return(dat_output)
  
}
# 处理队列体检、血生化变量（包括自报疾病
preprocess_measure <- function(dat_input) {
  thead <- Sys.time()
  
  ## Part 1: 指定并提取分析变量 ####
  t1head <- Sys.time()
  ## 指定体检变量 ####
  var_phyexam_all <- c("Q2_2_1A", "Q2_2_2A", "Q2_2_3A", "Q2_2_1B", "Q2_2_2B",
                       "Q2_2_3B", "Q2_2_1C", "Q2_2_2C", "Q2_2_3C", 
                       "Q4_1_1",  "Q4_1_1_Other", "Q4_2_1",  "Q4_2_1_Other",
                       "Q5_1",    "Q5_2",    "Q5_3",    "Q5_4",
                       "Q6_1",    "Q6_2",    "Q7_1",    "Q9_1","Q10_1",   "Q11_1")
  
  ## 指定生化变量 ####
  bio_RT <- c('WBC',  'RBC',   'HGB',  'HCT',  'MCV',  'MCH',  'MCHC',  'RDW',  'RDW_SD',
              'PLT',  'P_LCR', 'MPV',  'PCT',  'PDW',  'LYM1', 'LYM2',  'MON1', 'MON2',
              'NEU1', 'NEU2',  'EOS1', 'EOS2', 'BAS1', 'BAS2', 'NRBC2', 'NRBC1', "PLT")
  bio_gluco  <- c('GLU', 'HBA1C')
  bio_heart  <- c('CK_MB')
  bio_lipids <- c('CHOL', 'LDL_CH', 'TG', 'HDL_CH')
  bio_liver  <- c('TP',  'ALB', 'GLOB', 'A_G', 'ALT', 'AST', 'AST_ALT', 
                  'GGT', 'ALP', 'TBIL', 'IBIL', 'DBIL')
  bio_kidney <- c('Cr', 'UREA', 'UA')
  
  bio_metabolic  <- c('ASO', 'RF')
  
  var_bio_all <- c("Q3_1", "Q8_2", 
                   bio_gluco, bio_lipids, bio_liver, bio_RT, 
                   bio_heart, bio_kidney, bio_metabolic)
  
  ## 指定问卷中需要用的变量 ####
  var_survey <- c("个人编码", "A1", "age", "B1", "C2",
                  "I3A", "I3b", "I3c", "I3d", "I4", 
                  "I8A", "I8b", "I9A", "I9b")
  
  var_selfreport <- c("E3A",     "E3B",  
                      
                      "E3CA_A",  "E3C4_B",
                      "E3C_A",   "E3C2_A",  "E3C3_A",  
                      "E3CB_A",  "E3C4_A",  "E3C5_A",  "E3C6_A",  
                      "E3C7_A",  "E3C7_AGE", "E3C8_A",  "E3C9_A",  "E3C10_A", 
                      "E3C11_A", "E3C12_A", "E3CC_A",  "E3C13_A", 
                      "E3C14_A", "E3C15_A", "E3C16_A", 
                      "E3C17_A", "E3C17_AGE", "E3C17_OTHER")
  var_gout <- c("E3C18_OTHER","E3C19_OTHER","E3C20_OTHER")
  var_treatment <- c("E3A_A", "E3B_A", "E3CA_B")
  
  # 变量说明：
  # E3A	    是否患高血压      E3A_A  降压药服用情况
  # E3B	    是否患糖尿病      E3B_A  降糖药服用情况
  # E3CA_A	是否患高脂血症    E3CA_B 目前是否接受高脂血症治疗
  # E3C_A 	是否患冠心病
  # E3C2_A	是否患中风
  # E3C3_A	是否患风心病
  # E3CB_A	是否患肺源性心脏病
  # E3C4_A	是否患肺结核      E3C4_B	是否正在接受抗肺结核治疗
  # E3C5_A	是否患慢性支气管炎/肺气肿
  # E3C6_A	是否患哮喘
  # E3C7_A	是否患慢性肝炎/肝硬化
  # E3C8_A	是否患消化道溃疡
  # E3C9_A	是否患胃肠炎
  # E3C10_A	是否患胆结石/胆囊炎
  # E3C11_A	是否患骨折
  # E3C12_A	是否患风湿性关节炎
  # E3CC_A	是否患类风湿性关节炎
  # E3C13_A	是否患椎间盘疾病
  # E3C14_A	是否患精神心理疾患
  # E3C15_A	是否患神经衰弱
  # E3C16_A	是否患脑外伤
  # E3C17_A	是否患恶性肿瘤
  
  # EH1_A	  是否患高原红细胞增多症
  # EH2_A	  是否患高原心脏病
  # EH3_A	  是否患混合性慢性高原病
  # EH4_A	  是否患睡眠呼吸暂停低通气综合征
  
  ## 提取分析变量 ####
  dat_tmp <- dat_input %>% 
    dplyr::select(all_of(var_survey), all_of(var_selfreport), all_of(var_treatment), 
           all_of(var_phyexam_all), all_of(var_bio_all),all_of(var_gout),
           体检日期 = 日期) %>% 
    mutate_at(vars("个人编码"), ~ as.character(.)) 
  
  unify_percent_value <- function(v_input){
    #   # 该函数用于转换带%号的变量
    #   # 转换逻辑：小于1的变量值均 × 100
    #   # 这个逻辑有问题，不能一刀切。
    #   # 因该类变量主要是血常规指标，后面用不到，因此不再进行这一步转换。
    #   # 相关变量查看方式：
    #   # a3 <- dat_tmp[, percent_ID]
    #   # res <- apply(a3, 2, function(x) sum(x < 1, na.rm = T))
    #   # percent_ID[res > 0]
    #   for(i in 1:length(v_input)){
    #     if(!is.na(v_input[i])){
    #       if(v_input[i] < 1){
    #         v_input[i] <- v_input[i]*100   
    #         ###部分百分数（percent）类型变量内部格式不一，一般为百分数乘100
    #         ###故取值不应小于1，所有小于1的个体值都乘以100
    #       }                             
    #     }  
    #   } 
    #   return(v_input)
  }
  # rm_units <- function(v_input) {
  #   v_output <- str_extract(v_input, "(\\d)+(\\.\\d*)?|\\d+") %>%
  #     as.numeric()
  #   return(v_output)
  # }
  # 
  # dat_ckd <- dat_ckd %>% 
  #   mutate_at(vars(var_media), ~rm_units(.))   
  
  ## 具有百分号的变量，里面有些数值小于1 ##
  # percent_ID  <- c("HCT",  "PCT",  "RDW",  "PDW",  "HBA1C", "P_LCR",
  #                  "LYM1", "MON1", "NEU1", "EOS1", "BAS1",  "NRBC1")
  
  # dat_tmp <- dat_tmp %>%
  #   ## 转为数值型
  #   mutate_at(vars(var_selfreport, var_phyexam_all, var_exposure), funs(as.numeric(.)))
  # 
  t1end <- Sys.time()
  t1diff <- round(t1end - t1head, 2)
  cat("已完成任务1：提取分析变量。", " 耗时：", t1diff, "秒", "\n")
  
  ## Part 2: 体检变量处理 ----------
  t4head <- Sys.time()
  
  dat_tmp <- dat_tmp %>% 
    mutate_at(vars(all_of(var_phyexam_all[!(var_phyexam_all %in% c("Q4_1_1_Other" ,"Q4_2_1_Other"))])), as.numeric) %>% 
    # 将体检变量999变成NA
    mutate_at(vars(all_of(var_phyexam_all[!(var_phyexam_all %in% c("Q4_1_1_Other" ,"Q4_2_1_Other"))])), list(~ na_if(., 999))) %>% 
    # 血压
    mutate(SBP = (as.numeric(Q2_2_1A) + as.numeric(Q2_2_2A) + as.numeric(Q2_2_3A))/3,
           DBP = (as.numeric(Q2_2_1B) + as.numeric(Q2_2_2B) + as.numeric(Q2_2_3B))/3) %>%
    # 心率
    mutate(HeartRate = (as.numeric(Q2_2_1C) + as.numeric(Q2_2_2C) + as.numeric(Q2_2_3C))/3) %>% 
    # BMI
    mutate(BMI = as.numeric(Q5_2)/(as.numeric(Q5_1)*0.01)^2,
           BMI_China = cut(BMI, breaks = c(0, 24, 28, Inf), right = F),
           BMI_WHO   = cut(BMI, breaks = c(0, 25, 30, Inf), right = F))
  
  # 计算血压测量NA的次数
  # dat_tmp$SBP_1 <- ifelse(is.na(dat_tmp$Q2_2_1A ),1,0) 
  # dat_tmp$SBP_2 <- ifelse(is.na(dat_tmp$Q2_2_2A ),1,0) 
  # dat_tmp$SBP_3 <- ifelse(is.na(dat_tmp$Q2_2_3A ),1,0) 
  # dat_tmp$SBP_NA <- dat_tmp$SBP_1+dat_tmp$SBP_2+dat_tmp$SBP_3
  # 
  # dat_tmp$DBP_1 <- ifelse(is.na(dat_tmp$Q2_2_1B ),1,0) 
  # dat_tmp$DBP_2 <- ifelse(is.na(dat_tmp$Q2_2_2B ),1,0) 
  # dat_tmp$DBP_3 <- ifelse(is.na(dat_tmp$Q2_2_3B ),1,0) 
  # dat_tmp$DBP_NA <- dat_tmp$DBP_1+dat_tmp$DBP_2+dat_tmp$DBP_3
  
  t4end <- Sys.time()
  t4diff <- round(t4end - t4head, 2)
  cat("已完成任务2：体检变量处理。", " 耗时：", t4diff, "秒", "\n")
  
  ## Part 3: 血生化变量处理 ----------
  t5head <- Sys.time()
  
  ## 函数：去掉血生化变量的单位
  rm_units <- function(v_input) {
    # 该函数用于去掉变量中的单位、大于/小于符号等。
    # 注意：大于/小于符号被直接去掉了，可能有问题，要考虑。
    # 对大于/小于符号的查看：
    # a <- dat_tmp[, var_bio_all]
    # res <- apply(a, 2, function(x) sum(str_detect(x, "<|>"), na.rm = T))
    # var_bio_all[res > 0]
    
    v_output <- str_extract(v_input, "(\\d)+(\\.\\d*)?|\\d+") %>% 
      as.numeric()
    return(v_output)
  }
  
  dat_tmp <- dat_tmp %>% 
    ## 去掉生化变量的单位，只保留数值
    mutate_at(vars(all_of(var_bio_all)), ~rm_units(.))
  
  ## 函数：将血生化变量的连续型值转为分类值
  # define_biomarker <- function(df, var, lowcut, highcut, highercut){
  #   varnm <- paste0(enquo(var), "_level")[2]
  #   var       <- enquo(var)
  #   lowcut    <- enquo(lowcut)
  #   highcut   <- enquo(highcut)
  #   highercut <- enquo(highercut)
  #   # print(varnm)
  #   
  #   if (varnm == "GLU_level") {
  #     df %>%
  #       mutate(!!varnm := case_when(!!var <  !!lowcut                        ~ "hypoglycemia",
  #                                   !!var >= !!lowcut  & !!var < !!highcut   ~ "normal",
  #                                   !!var >= !!highcut & !!var < !!highercut ~ "IFG",
  #                                   !!var >= !!highercut                     ~ "DM",
  #                                   is.na(!!var)                             ~ "missing")) %>% 
  #       mutate_at(.vars = varnm, ~fct_relevel(., c("hypoglycemia", "normal", "IFG", "DM", "missing")))
  #     
  #   } else if (varnm %in% paste0(c("CHOL", "LDL_CH", "TG"), "_level")) {
  #     df %>%
  #       mutate(!!varnm := case_when(!!var <  !!lowcut                      ~ "desirable",
  #                                   !!var >= !!lowcut & !!var < !!highcut  ~ "borderline high",
  #                                   !!var >  !!highcut                     ~ "high",
  #                                   TRUE                                   ~ "missing")) %>% 
  #       mutate_at(.vars = varnm, ~fct_relevel(., c("desirable", "borderline high", "high", "missing")))
  #   } else {
  #     df %>%
  #       mutate(!!varnm := case_when(!!var <  !!lowcut                      ~ "low",
  #                                   !!var >= !!lowcut & !!var < !!highcut  ~ "normal",
  #                                   !!var >  !!highcut                     ~ "high",
  #                                   TRUE                                   ~ "missing")) %>% 
  #       mutate_at(.vars = varnm, ~fct_relevel(., c("low", "normal", "high", "missing")))
  #   }
  #   
  # }
  # 
  # standard <- lookup_cat_bio %>%
  #   pivot_wider(names_from = name,
  #               values_from = c(lowcut, highcut, highercut))
  # 
  # dat_tmp <- dat_tmp %>% 
  #   ## 生成 gender_tmp, age_tmp
  #   mutate(gender_tmp = A1,
  #          age_tmp = ifelse(age < 60, 1, 2)) %>%
  #   ## 合并血生化原始数据和界值表
  #   left_join(standard, by = c("gender_tmp", "age_tmp")) %>%
  #   ## 将血生化变量的连续型值转为分类值
  #   define_biomarker(., GLU, lowcut_GLU, highcut_GLU, highercut_GLU) %>% 
  #   define_biomarker(., Cr, lowcut_Cr, highcut_Cr, highercut_Cr) %>% 
  #   define_biomarker(., TG, lowcut_TG, highcut_TG, highercut_TG) %>% 
  #   define_biomarker(., CHOL, lowcut_CHOL, highcut_CHOL, highercut_CHOL) %>% 
  #   select(-str_which(colnames(.), "lowcut|highcut|highercut"))
  
  t5end <- Sys.time()
  t5diff <- round(t5end - t5head, 2)
  cat("已完成任务3：血生化变量处理。", " 耗时：", t5diff, "秒", "\n")
  
  ## Part 4: 综合诊断疾病 ----------
  ## 自报疾病转码(E3) ####
  t_sf_head <- Sys.time()
  
  # 转换自报疾病的编码: (1=患病, 2=未患病) --> (1=患病, 0=未患病)
  trans_12_10 <- function(x) {
    newx <- case_when(x == 1 ~ "1",
                      x == 2 ~ "0",
                      TRUE   ~ NA_character_) %>% 
      as.factor()
    return(newx)
  }
  
  dat_tmp <- dat_tmp %>% 
    mutate_at(vars(all_of(var_selfreport)), ~ trans_12_10(.))
  
  t_sf_end <- Sys.time()
  t_sf_diff <- round(t_sf_end - t_sf_head, 2)
  cat("已完成任务4-1：自报疾病转码。", " 耗时：", t_sf_diff, "秒", "\n")
  
  ## 自报+体检诊断：高血压 HPT  ####
  t_hyp_head <- Sys.time()
  
  dat_tmp <- dat_tmp %>% 
    # 高血压(自报)
    mutate(HPT_report = E3A) %>% 
    # 高血压(自报患病及服药情况)
    mutate(HPT_report_med = case_when(
      E3A == 0                      ~ "0",  # 自报不患病
      E3A == 1 & E3A_A == 1         ~ "1",  # 自报患病且服药
      E3A == 1 & E3A_A %in% c(2,3)  ~ "2",  # 自报患病不服药
      TRUE                          ~ NA_character_)) %>%
    # 高血压(体检)
    mutate(HPT_test = case_when(SBP >= 140 | DBP >= 90 ~ "1",
                                SBP <  140 & DBP <  90 ~ "0",
                                TRUE                   ~ NA_character_)) %>%
    # 高血压(自报或体检诊断)
    mutate(HPT_total = case_when(HPT_report_med == 1 | HPT_test == 1 ~ "1",
                                 HPT_report_med == 0 & HPT_test == 0 ~ "0",
                                 TRUE                                ~ NA_character_)) %>%
    # 高血压诊断分类
    mutate(HPT_index = case_when(
      HPT_report_med == 1 & HPT_test == 1 ~ "both",
      HPT_report_med == 1 & (HPT_test == 0 | is.na(HPT_test)) ~ "report_only",
      HPT_test == 1  &  (HPT_report_med == 0| is.na(HPT_report_med)) ~ "test_only"))
  
  #高血压患病时间-现住所居住时间:1居住期间患病
  # dat_tmp$Hypertension_time <- 'before'
  # dat_tmp$Hypertension_time[which((dat_tmp$age - dat_tmp$B10A - dat_tmp$E3A_AGE)<= 0)] <- 'after'
  
  ##高血压诊断年限
  # dat_tmp$Hypertension_diagtime <- dat_tmp$age  - dat_tmp$E3A_AGE
  
  t_hyp_end <- Sys.time()
  t_hyp_diff <- round(t_hyp_end - t_hyp_head, 2)
  cat("已完成任务4-2：高血压自报+体检诊断。", " 耗时：", t_hyp_diff, "秒", "\n")
  
  ## 自报+体检诊断：糖尿病 DB ####
  t_db_head <- Sys.time()
  
  dat_tmp <- dat_tmp %>% 
    # 糖尿病(自报)
    mutate(DB_report = E3B) %>% 
    # 糖尿病(自报患病及服药情况)
    mutate(DB_report_med = case_when(
      E3B == 0                      ~ "0",  # 不患病
      E3B == 1 & E3B_A == 1         ~ "1",  # 自报患病且服药
      E3B == 1 & E3B_A %in% c(2,3)  ~ "2",  # 自报患病不服药
      TRUE                          ~ NA_character_)) %>% 
    # 糖尿病(体检)
    mutate(DB_test = case_when(GLU >= 7.0 | HBA1C >= 6.5 ~ "1",
                               GLU  < 7.0 & HBA1C  < 6.5 ~ "0",
                               TRUE                      ~ NA_character_)) %>% 
    # 糖尿病(自报或体检诊断) 
    mutate(DB_total = case_when(DB_report_med == 1 | DB_test == 1 ~ "1",
                                DB_report_med == 0 & DB_test == 0 ~ "0",
                                TRUE                              ~ NA_character_)) %>% 
    # 糖尿病诊断分类
    mutate(DB_index = case_when(
      DB_report_med == 1 & DB_test == 1 ~ "both",
      DB_report_med == 1 & (DB_test == 0 | is.na(DB_test)) ~ "report_only",
      DB_test == 1  &  (DB_report_med == 0 | is.na(DB_report_med)) ~ "test_only"))
  
  #糖尿病患病时间-现住所居住时间：after为居住期间患病
  # dat_tmp$DB_time <- 'before'
  # dat_tmp$DB_time[which((dat_tmp$age - dat_tmp$B10A - dat_tmp$E3B_AGE)<= 0)] <- 'after'
  
  ##糖尿病诊断年限
  # dat_tmp$DB_diagtime <-  dat_tmp$age  - dat_tmp$E3B_AGE                                     
  
  ## 糖尿病前期 preDB ####
  dat_tmp <- dat_tmp %>%
    mutate(preDB = case_when(
      (GLU >= 5.6 & GLU <= 6.9) | (HBA1C >= 5.7 & HBA1C <= 6.4) ~ "1",
      TRUE ~ "others"))
  
  t_db_end <- Sys.time()
  t_db_diff <- round(t_db_head - t_db_head, 2)
  cat("已完成任务4-3：糖尿病自报+体检诊断；糖尿病前期诊断。", " 耗时：", t_db_diff, "秒", "\n")
  
  # 心血管代谢异常征 MD####
  t_cm_head <- Sys.time()
  
  dat_tmp <- dat_tmp %>%
    # condition 1. waist circumference，选取界值85，90@
    mutate(cm_cond1 = case_when((A1==2 & Q5_3>=85) | (A1==1 & Q5_3>=90) ~ 1,
                                (A1==2 & Q5_3< 85) | (A1==1 & Q5_3< 90) ~ 0,
                                TRUE ~ NA_real_)) %>%
    # condition 2. elevated TAG
    mutate(cm_cond2 = case_when(TG >= 1.7 | E3CA_B == 1 ~ 1,
                                TG <  1.7               ~ 0,
                                TRUE ~ NA_real_)) %>%
    # condition 3. reduced HDL-cholesterol
    mutate(cm_cond3 = case_when((A1==1 & HDL_CH <1.03) | (A1==2 & HDL_CH <1.29) ~ 1,
                                (A1==1 & HDL_CH>=1.03) | (A1==2 & HDL_CH>=1.29) ~ 0,
                                TRUE ~ NA_real_)) %>%
    # condition 4. high blood pressure and/or anti-hypertensive treatment
    mutate(cm_cond4 = case_when(SBP >= 130 | DBP >= 85 | E3A_A == 1 ~ 1,
                                SBP <  130 & DBP <  85              ~ 0,
                                TRUE ~ NA_real_)) %>%
    # condition 5. raised fasting glucose (FG) and/or use of antidiabetic medication
    mutate(cm_cond5 = case_when(GLU >= 5.6 | E3B_A == 1 ~ 1,
                                GLU <  5.6              ~ 0,
                                TRUE ~ NA_real_)) %>%
    # 心脏代谢异常综合征(MD)
    mutate(MD = case_when(
      cm_cond1 + cm_cond2 + cm_cond3 + cm_cond4 + cm_cond5 >= 3 ~ 1,
      cm_cond1 + cm_cond2 + cm_cond3 + cm_cond4 + cm_cond5 <  3 ~ 0,
      TRUE ~ NA_real_)) %>%
    mutate(MD = factor(MD, levels = c(0, 1)))
  # select(-c(cm_cond1, cm_cond2, cm_cond3, cm_cond4, cm_cond5))
  
  t_cm_end <- Sys.time()
  t_cm_diff <- round(t_cm_head - t_cm_head, 2)
  cat("已完成任务4-4：代谢异常征诊断。", " 耗时：", t_cm_diff, "秒", "\n")
  
  # ## 代谢相关脂肪肝 MAFLD ####
  # t_MAFLD_head <- Sys.time()
  # 
  # dat_tmp <- dat_tmp %>% 
  #   # hepatic steatosis (detected by imaging techniques)
  #   mutate(FLD_img = case_when(
  #     is.na(Q4_1_1)                      ~ NA_real_,
  #     Q4_1_1 == 1                        ~ 0,
  #     str_detect(Q4_1_1_Other, "脂肪肝") ~ 1,
  #     str_detect(Q4_1_1_Other, "肝实质回声") ~ 2,
  #     str_detect(Q4_1_1_Other, "肝硬化") ~ 3,
  #     TRUE ~ 4)) # 2、4均并到0; 3排除
  # 
  # cat("计算MAFLD危险因素-开始", "\n")
  # t_MAFLD_risk_head <- Sys.time()
  # dat_tmp <- dat_tmp %>% 
  #   # criterion 1: overweight or obesity
  #   mutate(MAFLD_OO = BMI >= 23) %>% 
  #   # criterion 2: Type 2 diabetes mellitus
  #   mutate(MAFLD_T2DB = DB_total) %>% 
  #   # criterion 3: evidence of metabolic dysregulation
  #   mutate(
  #     # waist circumference 
  #     MAFLD_risk1 = case_when(
  #       (A1==1 & Q5_3>=90) | (A1==2 & Q5_3>=80) ~ 1,
  #       (A1==1 & Q5_3< 90) | (A1==2 & Q5_3< 80) ~ 0,
  #       TRUE ~ NA_real_),
  #     # high blood pressure
  #     MAFLD_risk2 = case_when(
  #       SBP >= 130 | DBP >= 85 | E3A_A == 1  ~ 1,
  #       SBP <  130 & DBP <  85               ~ 0,
  #       TRUE ~ NA_real_),
  #     # elevated plasma triglycerides
  #     MAFLD_risk3 = case_when(
  #       TG >= 1.7 | E3CA_B == 1 ~ 1,
  #       TG <  1.7               ~ 0,
  #       TRUE ~ NA_real_),
  #     # reduced HDL-cholesterol
  #     MAFLD_risk4 = case_when(
  #       (A1==1 & HDL_CH <1) | (A1==2 & HDL_CH <1.3)  ~ 1,
  #       (A1==1 & HDL_CH>=1) | (A1==2 & HDL_CH>=1.3) ~ 0,
  #       TRUE ~ NA_real_),
  #     # prediabetes
  #     MAFLD_risk5 = case_when(
  #       (GLU >= 5.6 & GLU <= 6.9) | (HBA1C >= 5.7 & HBA1C <= 6.4) ~ 1,
  #       (GLU < 5.6 & HBA1C < 5.7) ~ 0,
  #       TRUE ~ NA_real_)) 
  # # metabolic dysregulation
  # t_MAFLD_risk_end <- Sys.time()
  # t_MAFLD_risk_diff <- round(t_MAFLD_risk_end - t_MAFLD_risk_head, 2)
  # cat("计算MAFLD危险因素-结束", " 耗时：", t_MAFLD_risk_diff, "秒", "\n")
  # 
  # cat("判断MAFLD-开始", "\n")
  # dat_tmp <- dat_tmp %>% 
  #   rowwise() %>% 
  #   mutate(MAFLD_rf = sum(MAFLD_risk1, MAFLD_risk2, MAFLD_risk3, MAFLD_risk4, MAFLD_risk5, na.rm = TRUE)) %>% 
  #   mutate(MAFLD_rf_na = 
  #            MAFLD_risk1 + MAFLD_risk2 + MAFLD_risk3 + MAFLD_risk4 + MAFLD_risk5) 
  # 
  # # diagnose MAFLD
  # dat_tmp <- dat_tmp %>% 
  #   mutate(MAFLD = case_when(
  #     (FLD_img == 1 & MAFLD_OO == 1)   | 
  #       (FLD_img == 1 & MAFLD_T2DB == 1) | 
  #       (FLD_img == 1 & MAFLD_rf >=2)    ~ 1, # & MAFLD_OO == 0
  #     (FLD_img == 1 & MAFLD_rf_na < 2) ~ 0, # & MAFLD_OO == 0 
  #     FLD_img == 0                    ~ 0,
  #     FLD_img == 2                    ~ 2, # B超提示肝实质回声增强/增密
  #     FLD_img == 3                    ~ 3, # B超提示肝硬化
  #     FLD_img == 4                    ~ 4, # B超提示脂肪肝、肝实质回声或肝硬化以外的其他结果
  #     is.na(FLD_img)                   ~ NA_real_, # B超缺失
  #     TRUE ~ NA_real_)) %>% 
  #   mutate(MAFLD = factor(MAFLD, levels = c(0, 1, 2, 3, 4)))
  # 
  # t_MAFLD_diagnose_end <- Sys.time()
  # t_MAFLD_diagnose_diff <- round(t_MAFLD_diagnose_end - t_MAFLD_risk_end, 2)
  # cat("判断MAFLD-结束", " 耗时：", t_MAFLD_diagnose_diff, "秒", "\n")
  # 
  # ## MAFLD scoring system ####
  # cat("计算MAFLD评分-开始", "\n")
  # 
  # dat_tmp <- dat_tmp %>% 
  #   mutate(IFG = GLU > 5.6 & GLU < 6.9) %>% 
  #   mutate(APRI = ((AST/40) / PLT) * 100,
  #          BARD = (BMI >= 28) + (AST_ALT >= 0.8) * 2 + (DB_total == 1),
  #          FIB4 = (round(age, 0) * AST) / (PLT * sqrt(ALT)),
  #          NFS  =  -1.675 + 0.037*round(age, 0) + 0.094 * BMI + 
  #            1.13 * (IFG == 1 | DB_total == 1) + 0.99 * AST_ALT - 
  #            0.013 * PLT - 0.66 * ALB / 1000)
  # t_MAFLD_score_end <- Sys.time()
  # t_MAFLD_score_diff <- round(t_MAFLD_score_end - t_MAFLD_diagnose_end, 2)
  # cat("计算MAFLD评分-结束", " 耗时：", t_MAFLD_score_diff, "秒", "\n")
  # 
  # t_MAFLD_end <- Sys.time()
  # t_MAFLD_diff <- round(t_MAFLD_end - t_MAFLD_head, 2)
  # cat("已完成任务4-5：代谢相关脂肪肝诊断。", " 耗时：", t_MAFLD_diff, "秒", "\n")
  # 
  # # ## 非酒精性脂肪肝 NAFLD ####
  # # t_NAFLD_head <- Sys.time()
  # # cat("计算饮酒量-开始", "\n")
  # # dat_tmp <- dat_tmp %>%
  # #   # hepatic steatosis (detected by imaging techniques)
  # #   # same as MAFLD
  # #   
  # #   # exclusion of excessive alcohol consumption
  # #   # mutate_all(~ if_else(is.na(.x), 0, .x)) %>%  # 将NA替换为0
  # #   mutate_at(vars("C6_1_A", "C6_1_B", "C6_1_C", "C6_1_D", 
  # #                  "C6_1_E", "C6_1_F", "C6_1_G"), 
  # #             replace_na, 0) %>%  # 将NA替换为0
  # #   mutate(alcohol_intake_day = C6_1_A*500*0.04/7 +   # beer 1瓶啤酒换算为500ml
  # #            C6_1_B*50*0.08/7  +   # rice wine            
  # #            C6_1_C*50*0.40/7 +    # highland barley wine 膳食组：0.05
  # #            C6_1_D*50*0.12/7 +    # grape wine
  # #            C6_1_E*50*0.53/7 +    # strong spirits
  # #            C6_1_F*50*0.38/7 +    # weak spirits
  # #            C6_1_G*50*0.35/7)     # mile spirits
  # # t_alcohol_end <- Sys.time()
  # # t_alcohol_diff <- round(t_alcohol_end - t_NAFLD_head, 2)
  # # cat("计算饮酒量-结束", " 耗时：", t_alcohol_diff, "秒", "\n")
  # # 
  # # cat("判断NAFLD-开始", "\n")
  # # dat_tmp <- dat_tmp %>% 
  # #   mutate(NAFLD_exclusion_1 = case_when(
  # #     (C2 == 1 | C2 == 2 | C2 == 3) ~ 0,
  # #     (A1==1 & alcohol_intake_day  > 30) | (A1==2 & alcohol_intake_day  > 20) ~ 1, # 酒精摄入过量，判断NAFLD时应排除
  # #     (A1==1 & alcohol_intake_day <= 30) | (A1==2 & alcohol_intake_day <= 20) ~ 0, # 酒精摄入未过量
  # #     TRUE ~ NA_real_)) %>% 
  # #   mutate(alcohol2 = NAFLD_exclusion_1 %>% 
  # #            factor(levels = c(0,1), 
  # #                   labels = c("饮酒未过量", "饮酒过量"))) %>% # 该行仅创建alcohol的2分类变量，为了后面建模用，对此处判断NAFLD不起作用
  # #   mutate(NAFLD_exclusion_2 = as.factor(E3C7_A == 1)) %>%     # 自报慢性肝炎/肝硬化
  # #   mutate(NAFLD = case_when(
  # #     FLD_img == 0                          ~ 0, # 正常
  # #     FLD_img == 1 & NAFLD_exclusion_1 == 0 ~ 1, # 非酒精性脂肪肝 NAFLD
  # #     FLD_img == 1 & NAFLD_exclusion_1 == 1 ~ 9, # 酒精性脂肪肝
  # #     FLD_img == 2                          ~ 2, # B超提示肝实质回声增强/增密
  # #     FLD_img == 3                          ~ 3, # B超提示肝硬化
  # #     FLD_img == 4                          ~ 4, # B超提示脂肪肝、肝实质回声或肝硬化以外的其他结果
  # #     TRUE ~ NA_real_)) %>%                      # B超缺失 或其他情况??
  # #   
  # #   mutate(NAFLD = factor(NAFLD, levels = c(0, 1, 2, 3, 4, 9)))
  # # t_NAFLD_diagnose_end <- Sys.time()
  # # t_NAFLD_diagnose_diff <- round(t_NAFLD_diagnose_end - t_alcohol_end, 2)
  # # cat("判断NAFLD-结束", " 耗时：", t_NAFLD_diagnose_diff, "秒", "\n")
  # # 
  # # # exclusion of viral hepatitis
  # # # mutate(NAFLD_exclusion_2 = case_when
  # # 
  # # t_NAFLD_end <- Sys.time()
  # # t_NAFLD_diff <- round(t_NAFLD_end - t_NAFLD_head, 2)
  # # cat("已完成任务4-5：非酒精性脂肪肝诊断。", " 耗时：", t_NAFLD_diff, "秒", "\n", "\n")
  # # 
  # 
  ## ASCVD一级预防血脂分层：升高，边缘升高，降低 ####
  # t_prevent_head <- Sys.time()
  # 
  # dat_tmp <- dat_tmp %>% 
  #   # cond1. 根据CHOL判断
  #   mutate(ASCVD_CHOL = case_when(CHOL >= 6.2              ~ "升高",
  #                                 CHOL >= 5.2 & CHOL < 6.2 ~ "边缘升高",
  #                                 CHOL  < 5.2              ~ "合适水平",
  #                                 TRUE                     ~ "missing")) %>% 
  #   # cond2. 根据LDL_CH判断
  #   mutate(ASCVD_LDL = case_when(LDL_CH >= 4.1                ~ "升高",
  #                                LDL_CH >= 3.4 & LDL_CH < 4.1 ~ "边缘升高",
  #                                LDL_CH  < 3.4                ~ "合适水平",
  #                                TRUE                         ~ "missing")) %>% 
  #   # cond3. 根据TG
  #   mutate(ASCVD_TG = case_when(TG >= 2.3            ~ "升高",
  #                               TG >= 1.7 & TG < 2.3 ~ "边缘升高",
  #                               TG  < 1.7            ~ "合适水平",
  #                               TRUE                 ~ "missing")) %>% 
  #   # 取并集
  #   mutate(ASCVD_bloodlipid = case_when(
  #     (ASCVD_CHOL == "升高") | (ASCVD_LDL == "升高") | (ASCVD_TG == "升高") ~ "升高",
  #     (ASCVD_CHOL == "边缘升高") | (ASCVD_LDL == "边缘升高") | (ASCVD_TG == "边缘升高") ~ "边缘升高",
  #     (ASCVD_CHOL == "合适水平") | (ASCVD_LDL == "合适水平") | (ASCVD_TG == "合适水平") ~ "合适水平",
  #     HDL_CH < 1.0 ~ "降低",
  #     TRUE ~ "missing")) %>% 
  #   mutate_at(.vars = "ASCVD_bloodlipid", 
  #             ~ fct_relevel(., c("missing", "合适水平", "边缘升高", "升高"))) %>% 
  #   select(-c(ASCVD_CHOL, ASCVD_LDL, ASCVD_TG))
  # 
  # t_prevent_end <- Sys.time()
  # t_prevent_diff <- round(t_prevent_head - t_prevent_head, 2)
  # cat("已完成任务4-5：ASCVD一级预防血脂分层。", " 耗时：", t_prevent_diff, "秒", "\n")
  
  ## ASCVD血脂异常风险：极高危，高危，中危 ####
  # t_chol_head <- Sys.time()
  # 
  # # 判定两种情况下的危险因素个数
  # dat_tmp <- dat_tmp %>% 
  #   # 判定10年ASCVD发病风险的危险因素
  #   mutate(ASCVD_10y_HR1 = case_when(B1 == 2 ~ 1,
  #                                    TRUE    ~ 0),
  #          ASCVD_10y_HR2 = case_when((A1 == 1 & age >= 45) | (A1 == 2 & age >= 55) ~ 1,
  #                                    TRUE ~ 0),
  #          ASCVD_10y_HR3 = case_when((A1 == 1 & HDL_CH < 1.30) | (A1 == 2 & HDL_CH < 1.29) ~ 1,
  #                                    TRUE ~ 0)) %>% 
  #   mutate(ASCVD_10y_rf = ASCVD_10y_HR1 + ASCVD_10y_HR2 + ASCVD_10y_HR3) %>% 
  #   select(-c(ASCVD_10y_HR1, ASCVD_10y_HR2, ASCVD_10y_HR3)) %>% 
  #   # 判定余生ASCVD发病风险（高危）的危险因素 
  #   mutate(ASCVD_lf_HR1 = case_when(SBP >= 160 | DBP >=100 ~ 1, 
  #                                   TRUE                   ~ 0),
  #          ASCVD_lf_HR2 = case_when(CHOL - HDL_CH >= 5.2 ~ 1,
  #                                   TRUE                 ~ 0),
  #          ASCVD_lf_HR3 = case_when(HDL_CH < 1.0 ~ 1, 
  #                                   TRUE         ~ 0),
  #          ASCVD_lf_HR4 = case_when(BMI >= 28 ~ 1,
  #                                   TRUE      ~ 0),
  #          ASCVD_lf_HR5 = case_when(B1=="吸烟" ~ 1, 
  #                                   TRUE       ~ 0)) %>% 
  #   mutate(ASCVD_lifetime_rf = ASCVD_lf_HR1 + ASCVD_lf_HR2 + ASCVD_lf_HR3 + ASCVD_lf_HR4 + ASCVD_lf_HR5) %>% 
  #   select(-c(ASCVD_lf_HR1, ASCVD_lf_HR2, ASCVD_lf_HR3, ASCVD_lf_HR4, ASCVD_lf_HR5))
  # 
  # # 综合判定ASCVD发病风险等级（现在、10年、余生）
  # dat_tmp <- dat_tmp %>% 
  #   # current risk
  #   mutate(ASCVD_risk_current = case_when(
  #     ((LDL_CH >= 4.9 | CHOL >= 7.2) |
  #        (DB_both == 1 & LDL_CH >= 1.8 & LDL_CH < 4.9)|(DB_both == 1 & age >= 40 & CHOL >= 3.1 & CHOL < 7.2)) ~ "高危",
  #     TRUE ~ "其他")) %>% 
  #   # 10-year risk
  #   mutate(ASCVD_risk_10y = case_when(
  #     ASCVD_risk_current == "其他" & 
  #       ((HPT_both == 1 & ASCVD_10y_rf == 3 & ((LDL_CH >= 1.8 & LDL_CH < 4.9) | (CHOL >= 3.1 & CHOL < 7.2))) |
  #          (HPT_both == 1 & ASCVD_10y_rf == 2 & ((LDL_CH >= 2.6 & LDL_CH < 4.9) | (CHOL >= 4.1 & CHOL < 7.2)))) ~ "高危",
  #     ASCVD_risk_current == "其他" & 
  #       ((HPT_both == 1 & ASCVD_10y_rf == 2 & ((LDL_CH >= 1.8 & LDL_CH < 2.6) | (CHOL >= 3.1 & CHOL < 4.1))) |
  #          (HPT_both == 1 & ASCVD_10y_rf == 1 & ((LDL_CH >= 2.6 & LDL_CH < 4.9) | (CHOL >= 4.1 & CHOL < 7.2))) |
  #          (HPT_both == 0 & ASCVD_10y_rf == 3 & ((LDL_CH >= 2.6 & LDL_CH < 4.9) | (CHOL >= 4.1 & CHOL < 7.2))) |
  #          (HPT_both == 0 & ASCVD_10y_rf == 2 & ((LDL_CH >= 3.4 & LDL_CH < 4.9) | (CHOL >= 5.2 & CHOL < 7.2)))) ~ "中危",
  #     TRUE ~ "其他")) %>% 
  #   # lifetime risk
  #   mutate(ASCVD_risk_liftime = case_when(
  #     (ASCVD_risk_10y == "中危") & (age < 55) & (ASCVD_lifetime_rf >= 2) ~ "高危",
  #     TRUE ~ "其他")) %>% 
  #   select(-c(ASCVD_10y_rf, ASCVD_lifetime_rf))
  # 
  # t_chol_end <- Sys.time()
  # t_chol_diff <- round(t_chol_head - t_chol_head, 2)
  # cat("已完成任务4-6：ASCVD血脂异常风险筛查。", " 耗时：", t_chol_diff, "秒", "\n")
  
  ## 量表诊断：睡眠、焦虑、抑郁 ####
  t_mental_head <- Sys.time()
  
  dat_tmp <- dat_tmp %>%
    # 失眠症
    mutate(sleep_insomnia = case_when(
      I3A == 1 | I3b == 1 | I3c == 1 ~ 1,
      I3A == 2 & I3b == 2 & I3c == 2 ~ 0,
      TRUE                           ~ NA_real_)) %>%
    mutate(sleep_insomnia = factor(sleep_insomnia, levels = c(0, 1))) %>%
    # # 睡眠障碍
    # mutate(sleep_disorder = case_when(
    #   I3A == 1 | I3b == 1 | I3d == 1 ~ "1",
    #   I3A == 2 & I3b == 2 & I3d == 2 ~ "0",
    #   TRUE                           ~ "missing")) %>%
    # # 睡眠时长
    # mutate(sleep_duration = case_when(
    #   (I6A <= 6) | (I5A + I6A <= 6)  ~ "short sleeper",
    #   (I6A >= 9) | (I5A + I6A >= 9)  ~ "long sleeper",
    #   (I4 == 2 & I6A > 6 & I6A < 9) |
    #     (I5A + I6A > 6 & I5A + I6A) < 9  ~ "normal",
  #   TRUE                               ~ "missing")) %>%
  # # 午睡时间
  # # mutate(sleep_naptime = )
  # # 打呼噜习惯
  # mutate(sleep_snoring = case_when(I7 == 1 ~ "经常有",
  #                                  I7 == 2 ~ "有时有",
  #                                  I7 == 3 ~ "否/不知道") %>%
  #     factor(levels = c("经常有", "有时有", "否/不知道"))) %>%
  # 抑郁
  mutate(
    I8A_new = as.numeric(I8A) - 1,
    I8B_new = as.numeric(I8b) - 1,
    depressive_disorder = case_when(
      I8A_new + I8B_new >= 3 | I8A_new >= 3 | I8B_new >= 3 ~ "1",
      I8A_new + I8B_new  < 3                       ~ "0",
      TRUE                                 ~ NA_character_)) %>%
    # 焦虑
    mutate(
      I9A_new = as.numeric(I9A) - 1,
      I9B_new = as.numeric(I9b) - 1,
      anxiety_disorder = case_when(
        I9A_new + I9B_new >= 3 | I9A_new >= 3 | I9B_new >= 3 ~ "1",
        I9A_new + I9B_new  < 3                       ~ "0",
        TRUE                                 ~ NA_character_)) %>%
    # 焦虑和抑郁
    mutate(
      depressive_anxiety = case_when(anxiety_disorder==0 & depressive_disorder==0~"0",
                                     (anxiety_disorder==0 & depressive_disorder==1)|(anxiety_disorder==1 & depressive_disorder==0)~"1",
                                     anxiety_disorder==1 & depressive_disorder==1~"2") %>%
        factor(levels = c("0","1","2"))) %>%
    
    dplyr::select(-c("I8A", "I8b", "I8A_new", "I8B_new",
                     # "I3A", "I3b", "I3c", "I4", "I5A", "I6A", "I7",
                     "I9A", "I9b", "I9A_new", "I9B_new"))
  
  t_mental_end <- Sys.time()
  t_mental_diff <- round(t_mental_head - t_mental_head, 2)
  cat("已完成任务4-7：睡眠、心理判断。", " 耗时：", t_mental_diff, "秒", "\n")
  ## 高尿酸血症 ####
  t_hyperuricemia_head <- Sys.time()
  
  dat_tmp <- dat_tmp %>%
    # 高尿酸血症
    mutate(HSUA = case_when(
      (UA >417 & A1 == 1)|(UA >357 & A1 == 2) ~ "1",
      (UA <=417 & A1 == 1)|(UA <=357 & A1 == 2) ~ "0",
      TRUE                           ~ NA_character_) %>%
        factor(levels = c("0","1"))) 
  
  t_hyperuricemia_end <- Sys.time()
  t_hyperuricemia_diff <- round(t_hyperuricemia_head - t_hyperuricemia_head, 2)
  cat("已完成任务4-8：高尿酸血症判断。", " 耗时：", t_hyperuricemia_diff, "秒", "\n")
  ## 痛风 #####
  t_gout_head <- Sys.time()
  dat_tmp <- dat_tmp %>%
    # 痛风
    mutate(gout = case_when(
      E3C18_OTHER == "痛风" | E3C19_OTHER == "痛风" | E3C20_OTHER == "痛风" |
        E3C18_OTHER == "脚痛风" | E3C19_OTHER == "脚痛风" | E3C20_OTHER == "脚痛风" |
        E3C18_OTHER == "关节痛风" | E3C19_OTHER == "关节痛风" | E3C20_OTHER == "关节痛风" |
        E3C18_OTHER == "痛风结石" | E3C19_OTHER == "痛风结石" | E3C20_OTHER == "痛风结石" |
        E3C18_OTHER == "痛风性肾病" | E3C19_OTHER == "痛风性肾病" | E3C20_OTHER == "痛风性肾病" |
        E3C18_OTHER == "痛风性关节炎" | E3C19_OTHER == "痛风性关节炎" | E3C20_OTHER == "痛风性关节炎"  ~ "1",
      TRUE                           ~ "0") %>%
        factor(levels = c("0","1"))) 
  t_gout_end <- Sys.time()
  t_gout_diff <- round(t_gout_head - t_gout_head, 2)
  cat("已完成任务4-8：高尿酸血症判断。", " 耗时：", t_gout_diff, "秒", "\n")
  ## 函数结束 ####
  tend <- Sys.time()
  tdiff <- round(tend - thead, 2)
  cat("已完成全部处理。", " 共耗时：", tdiff, "秒", "\n")
  
  return(dat_tmp) 
}

test1 <- preprocess_survey(test) %>% 
  select(3,175:209)
test2 <- preprocess_measure(test) %>% 
  select(1,119:147)
test3 <- left_join(test1,test2,by = "个人编码")
save(test3, file = paste(path_data, "test3.Rdata", sep = ""))
## 3.2 数据初步情况探索####
#用初步清洗的数据做描述性分析，套用初步数据探索1.rmd即可


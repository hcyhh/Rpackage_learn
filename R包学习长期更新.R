# -------------------------------------------------------------------------#
#                                Introduction 
# 学习常见的R包，包括：
#               readr dplyr tidyr lubridate stringr forcats ggplot2
# 
#。             分包来学！长期更新！push自己尽快掌握R
#
#                                                               by Congyuan He
#                                                                  2023.06.29                                                                   
# -------------------------------------------------------------------------#

# 1各类R包学习 ------------------------------------------------------------------
## 1.2 readr read_excel-------------------------------------------------------------------
library('readr')
library('readxl')
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




















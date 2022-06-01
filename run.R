library(reshape2)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyverse)

bac <- read.table(file = "inst/extdata/data.txt",header = TRUE,row.names = 1,
                  sep = "\t",quote = "")
group <- read.table(file = "inst/extdata/group.txt",header = TRUE,sep = "\t")

##定义group的列名和分子，后续我会整合在运行函数里
colnames(group) <- c("variable","Group")
group$Group <- factor(group$Group)

##定义出图的配色
cbbPalette <- c("#B2182B","#56B4E9","#E69F00","#009E73","#F0E442","#0072B2",
                "#D55E00","#CC79A7","#CC6666","#9999CC","#66CC99","#99999",
                "#ADD1E5")

source("R/tax.summary.R") ##将OTU表格转化为门至种水平的丰度表
result <- tax.summary(bac) ##返回一个list，从1-6分别对应门至属水平丰度表

otu <- result[[1]] ##使用门水平数据运行分析
source("R/wilcox.R")
source("R/wilcox.abun.R")
source("R/wilcox.plot.R")
wilcox.result <- wilcox.biomarker(otu,group) ##返回一个list，包含两个列表，一个是所有输入条目的检验结果，另一个是只有p<0.05的结果

Phylum <- wilcox.abun(otu,wilcox.result[[2]],group) ##返回一个list，包含两个列表，一个对应左半边图像的绘图数据，另一个我对应半边图像的绘图数据
abun.bar <- Phylum[[1]]
diff.mean <- Phylum[[2]]

p <- wilcox.plot(Phylum[[1]],Phylum[[2]]) ## 生成结果图
p

rm(list = ls())




# 1.找到每一种与FI关系最密切的food --------------------------------------------------------------------------
library(dietaryindex)

## 读取2005-2006、2007-2008、2009-2010三个周期数据
setwd("F:/我的研究/DII+FI/dietaryindex/NHANES_combined")
load("NHANES_20052006.rda")
load("NHANES_20072008.rda")
load("NHANES_20092010.rda")

## 20052006两天的数据
FPED_PATH <- NHANES_20052006$FPED # FPED食物模式等效物膳食摄入量 first-day
NUTRIENT_PATH <- NHANES_20052006$NUTRIENT # nutrient data first-day
DEMO_PATH <- NHANES_20052006$DEMO # demo data
FPED_PATH2 <- NHANES_20052006$FPED2 # FPED食物模式等效物膳食摄入量 second-day
NUTRIENT_PATH2 <- NHANES_20052006$NUTRIENT2 # nutrient data second-day

DII_d <- DII_NHANES_FPED(
  FPED_PATH,
  NUTRIENT_PATH,
  DEMO_PATH,
  FPED_PATH2,
  NUTRIENT_PATH2
)

## 20072008两天的数据
FPED_PATH <- NHANES_20072008$FPED # FPED食物模式等效物膳食摄入量 first-day
NUTRIENT_PATH <- NHANES_20072008$NUTRIENT # nutrient data first-day
DEMO_PATH <- NHANES_20072008$DEMO # demo data
FPED_PATH2 <- NHANES_20072008$FPED2 # FPED食物模式等效物膳食摄入量 second-day
NUTRIENT_PATH2 <- NHANES_20072008$NUTRIENT2 # nutrient data second-day

DII_e <- DII_NHANES_FPED(
  FPED_PATH,
  NUTRIENT_PATH,
  DEMO_PATH,
  FPED_PATH2,
  NUTRIENT_PATH2
)

## 20092010两天的数据
FPED_PATH <- NHANES_20092010$FPED # FPED食物模式等效物膳食摄入量 first-day
NUTRIENT_PATH <- NHANES_20092010$NUTRIENT # nutrient data first-day
DEMO_PATH <- NHANES_20092010$DEMO # demo data
FPED_PATH2 <- NHANES_20092010$FPED2 # FPED食物模式等效物膳食摄入量 second-day
NUTRIENT_PATH2 <- NHANES_20092010$NUTRIENT2 # nutrient data second-day

DII_f <- DII_NHANES_FPED(
  FPED_PATH,
  NUTRIENT_PATH,
  DEMO_PATH,
  FPED_PATH2,
  NUTRIENT_PATH2
)


# 2005-2006周期没有维生素D
DII_d$VITD <- NA


# 合并
DII <- rbind(DII_d, DII_e, DII_f)

# 保存数据
setwd("F:/我的研究/DII+FI/1、data")
save(DII, file = "DII.RData")




# 2.multiple testing ------------------------------------------------------
# Multiple testing，也称为多重检验或多重比较，是指在统计分析中同时进行多个假设检验的过程。在研究中，当我们对多个变量、因素或总体参数进行检验时，会遇到一个问题：如果只关注单一检验的显著性水平（例如p值），那么由于偶然性，即使所有零假设均为真，也会有一定概率错误地拒绝至少一个零假设，这种现象称为第一类错误（假阳性错误）。
#
# 为了控制这种错误累积的风险，研究人员使用多种校正方法来调整p值或显著性水平，确保整体错误率保持在一个可以接受的范围内。常用的多重检验校正方法包括：
#
# 1. **Bonferroni校正**：这是最保守的方法之一，通过将每个检验的显著性水平α除以总的检验次数m（即α/m），来得到新的阈值。只有当任何一项检验的p值小于这个调整后的阈值时，才认为该检验结果显著。
#
# 2. **Benjamini-Hochberg校正**（FDR控制）：这种方法控制的是错误发现率(False Discovery Rate)，即期望错误发现的比例。按照p值从最小到最大排序，找到最大的i，使得第i个p值小于i/m×α，然后拒绝所有p值小于或等于这个p值的假设。
#
# 3. **Holm-Bonferroni方法**：这是一种逐步校正的方法，比Bonferroni校正稍微不那么保守。首先将所有的p值从小到大排序，然后对每个检验应用逐步减小的校正（α/(m+1-i)），其中i是检验的位置。
#
# 进行多重检验校正有助于减少错误的发现，但同时也可能增加第二类错误（假阴性错误）的概率，即未能拒绝实际上是错误的零假设。因此，在选择合适的校正方法时，需要权衡控制错误类型之间的平衡，以及考虑具体研究的背景和目的。

# 提取基线表当中的P值
p_value <- c(
    '0.016',
    '0.001',
    '0.009',
    '0.057',
    '0.106',
    '0.001',
    '0.046',
    '0.003',
    '0.084',
    '0.081',
    '0.003',
    '0.001',
    '0.001',
    '0.001',
    '0.009',
    
    
    '0.007',
    '0.734',
    '0.543',
    '0.005',
    '0.011',
    
    '0.010',
    '0.557',
    '0.426',
    '0.006',
    '0.014',
    
    '0.032', 
    '0.603',
    '0.318',
    '0.032',
    '0.039'
    
)




bonferroni_adjusted_p <- p.adjust(p_value, method = "bonferroni")

bh_adjusted_p <- p.adjust(p_value, method = "BH")

holm_adjusted_p <- p.adjust(p_value, method = "holm")

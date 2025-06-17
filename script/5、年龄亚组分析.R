rm(list = ls())

# 加载包
library(nhanesR)
library(survey)

# 加载数据
setwd("F:/我的研究/DII+FI/1、data")
load(file = "clean_data.RData")

# 查看亚组
table(d$Age_group)
subgroup = c("20-44","45-64","65-85")

for (i in 1:length(subgroup)) {
    
    # 加载数据
    setwd("F:/我的研究/DII+FI/1、data")
    load(file = "clean_data.RData")
    
    # 四分DII
    d$DIIQ <- quant(d$DII, n = 4,Q = TRUE,round=2)
    d$DIIQ.median <- quant.median(d$DII, n = 4,round=2)
    
    # 1.取子集
    subgroup = c("20-44","45-64","65-85")
    d <- subset(d,d$Age_group == subgroup[i])
    
    # 2.加权
    nhs.design <- svy_design(d)
    
    # 3.多模型————fit2、fit3删掉亚组变量！！！
    fit1 <- svyglm(FI ~ DIIQ, design = nhs.design, family = quasibinomial) |>reg_table()
    fit2 <- svyglm(FI ~ DIIQ + Sex + Race + Educational_level + PIR, 
                   design = nhs.design, family = quasibinomial) |>reg_table()
    fit3 <- svyglm(FI ~ DIIQ + Sex + Race + Educational_level + PIR + 
                       BMI + CRP + Energy + Smoking_status + Alcohol_consumption +
                       Dyslipidemia + Diabetes + Hypertension + Physical_activity, 
                   design = nhs.design, family = quasibinomial) |>reg_table()
    
    # 4.导出数据 
    setwd("F:/我的研究/DII+FI/3、output")
    crude.Model.n(fit1,fit2,fit3,round = 2,xlsx = paste0("亚组分析",subgroup[i],"(2位小数)", ".xlsx"))
    crude.Model.n(fit1,fit2,fit3,round = 3,xlsx = paste0("亚组分析",subgroup[i],"(3位小数)", ".xlsx"))
}

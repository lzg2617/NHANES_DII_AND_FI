rm(list = ls())

# 加载包
library(gtsummary)
library(nhanesR)
library(survey)

# 加载数据
setwd("F:/我的研究/DII+FI/1、data")
load(file = "clean_data.RData")

# 四分DII -------------------------------------------------------------------
# quant(d$DII)
d$DIIQ <- quant(d$DII, n = 4,Q = TRUE,round=2)
d$DIIQ.median <- quant.median(d$DII, n = 4,round=2)

# 加权
nhs.design <- svy_design(d)


# 多模型 ---------------------------------------------------------------------

# 单因素
svy_uv.logit(nhs.design,y = "FI",x = "DII")
svy_uv.logit(nhs.design,y = "FI",x = "DIIQ")

# 批量单因素逻辑回归
svy_uv.logit(design = nhs.design,
             y = 'FI',
             x = c("Sex", "Age", "Age_group", 
                   "Race", "Educational_level", "PIR", "BMI", "CRP", "Energy", "BMI_group", 
                   "Smoking_status", "Alcohol_consumption", "Dyslipidemia", "Diabetes", 
                   "Hypertension", "Physical_activity", "DIIQ"),
             round = 3)

# DIIQ####
fit1 <- svyglm(FI ~ DIIQ, design = nhs.design, family = quasibinomial) |>reg_table()
fit2 <- svyglm(FI ~ DIIQ + Sex + Age + Race + Educational_level + PIR, 
               design = nhs.design, family = quasibinomial) |>reg_table()

fit3 <- svyglm(FI ~ DIIQ + Sex + Age + Race + Educational_level + PIR + 
                   BMI + CRP + Energy + Smoking_status + Alcohol_consumption +
                   Dyslipidemia + Diabetes + Hypertension + Physical_activity, 
               design = nhs.design, family = quasibinomial) |>reg_table()

# DII####
fit4 <- svyglm(FI ~ DII, design = nhs.design, family = quasibinomial) |>reg_table(round = 2)
fit5 <- svyglm(FI ~ DII + Sex + Age + Race + Educational_level + PIR, 
               design = nhs.design, family = quasibinomial) |>reg_table(round = 2)
fit6 <- svyglm(FI ~ DII + Sex + Age + Race + Educational_level + PIR + 
                   BMI + CRP + Energy + Smoking_status + Alcohol_consumption +
                   Dyslipidemia + Diabetes + Hypertension + Physical_activity, 
               design = nhs.design, family = quasibinomial) |>reg_table(round = 2)

# 注：模型相比之前——没有调整CRP！



# 导出数据 --------------------------------------------------------------------
setwd("F:/我的研究/DII+FI/3、output")
crude.Model.n(fit1,fit2,fit3)
crude.Model.n(fit1,fit2,fit3,round = 3,xlsx = "多模型3.xlsx")
crude.Model.n(fit1,fit2,fit3,round = 2,xlsx = "多模型2.xlsx")


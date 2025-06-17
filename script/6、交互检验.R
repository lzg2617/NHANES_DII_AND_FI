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


# 保存数据
setwd("F:/我的研究/DII+FI/3、output")
# 模型1
stratum_model(object = nhs.design,y = 'FI',x = 'DIIQ',
              stratum = c("Age_group","Sex","Race","BMI_group"),
              adjust = c("Sex", "Age", "Race", "Educational_level", "PIR", "BMI", 
                         "CRP", "Energy", "Smoking_status", "Alcohol_consumption", 
                         "Dyslipidemia", "Diabetes", "Hypertension", 
                         "Physical_activity"),
              round = 3)
# 年龄亚组
stratum_model(object = nhs.design,y = 'FI',x = 'DIIQ',
              stratum = c("Age_group"),
              adjust = c("Sex", "Race", "Educational_level", "PIR", "BMI", 
                         "CRP", "Energy", "Smoking_status", "Alcohol_consumption", 
                         "Dyslipidemia", "Diabetes", "Hypertension", 
                         "Physical_activity"),
              round = 3)

# 性别亚组
stratum_model(object = nhs.design,y = 'FI',x = 'DIIQ',
              stratum = c("Sex"),
              adjust = c("Age", "Race", "Educational_level", "PIR", "BMI", 
                         "CRP", "Energy", "Smoking_status", "Alcohol_consumption", 
                         "Dyslipidemia", "Diabetes", "Hypertension", 
                         "Physical_activity"),
              round = 3)
# 种族亚组
stratum_model(object = nhs.design,y = 'FI',x = 'DIIQ',
              stratum = c("Race"),
              adjust = c("Sex", "Age", "Educational_level", "PIR", "BMI", 
                         "CRP", "Energy", "Smoking_status", "Alcohol_consumption", 
                         "Dyslipidemia", "Diabetes", "Hypertension", 
                         "Physical_activity"),
              round = 3)
# BMI亚组
stratum_model(object = nhs.design,y = 'FI',x = 'DIIQ',
              stratum = c("BMI_group"),
              adjust = c("Sex", "Age", "Race", "Educational_level", "PIR", 
                         "CRP", "Energy", "Smoking_status", "Alcohol_consumption", 
                         "Dyslipidemia", "Diabetes", "Hypertension", 
                         "Physical_activity"),
              round = 3)

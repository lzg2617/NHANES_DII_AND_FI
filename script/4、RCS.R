rm(list = ls())

# 加载包
library(gtsummary)
library(nhanesR)
library(survey)
library(ggplot2)

# 加载数据
setwd("F:/我的研究/DII+FI/1、data")
load(file = "clean_data.RData")

# 加载包
library(rms)

# 加权
nhs.design <- svy_design(d)


# RCS ---------------------------------------------------------------------

f1 <- svyglm(FI ~ rcs(DII), design = nhs.design, family = quasibinomial)
r1 <- RCS(f1,log = F,nknots = 4)
optimal_nKnots(f1)
ggplot(r1)

f2 <- svyglm(FI ~ rcs(DII) + Sex + Age + Race + Educational_level + PIR, 
               design = nhs.design, family = quasibinomial)
r2 <- RCS(f1,log = F,nknots = 4)
optimal_nKnots(f2)
ggplot(r2)

f3 <- svyglm(FI ~ rcs(DII) + Sex + Age + Race + Educational_level + PIR + 
                 BMI + CRP + Energy + Smoking_status + Alcohol_consumption +
                 Dyslipidemia + Diabetes + Hypertension + Physical_activity, 
             design = nhs.design, family = quasibinomial)
r3 <- RCS(f3,log = F,nknots = 3)
optimal_nKnots(f3)
ggplot(r3)+
    xlab("dietary inflammatory index")+
    annotate("text", x = -1, y = 2, 
             label = "p-nonlinear<0.001", fontface = "bold",
             size = 4)+
    annotate("text", x = -1, y = 1.8, 
             label = "P-overall=0.234", fontface = "bold",
             size = 4)



# 导出 ----------------------------------------------------------------------
setwd("F:/我的研究/DII+FI/3、output")
export::graph2tif(p, file = "RCS", width = 5.5, height = 4.5)



# RCSSCI ---------------------------------------------------------------------


library(rcssci)

rcssci_logistic(data = d,knot = 3,y = "FI",x = "DII",
                covs = c("Sex", "Age","Race", "Educational_level", "PIR", "BMI",
                         "CRP", "Energy", "Smoking_status", "Alcohol_consumption", 
                         "Dyslipidemia", "Diabetes","Hypertension", 
                         "Physical_activity"),
                prob = 0.5,
                filepath = "F:/我的研究/DII+FI/3、output")
rm(list = ls())
library(nhanesR)
library(tidyverse)


# *读取之前清洗好的数据 --------------------------------------------------------
setwd("F:/我的投稿/2、DII与FI的关系/DII and FI/data")
d <- read.csv("Age_WHO.csv")


# *数据的还原 ------------------------------------------------------------------
str(d)

# 数值变量的因子化
d <- d |>
  mutate(across(
    c(
      "Sex", "FI", "Age_new", "Race_new", "Educational_new",
      "PIR_new", "BMI_new", "Dyslipidemia_new", "smoking",
      "drink", "Diabetes_new", "hypertension_new", "activity",
      "Self_heath_evaluation", "Age_WHO"
    ),
    as.factor
  ))

# 还原成字符串
# Recode(d)
d$Sex <- Recode(d$Sex,
  "1::male",
  "2::female",
  to.numeric = FALSE
)
table(d$Sex)

# d$FI <- Recode(d$FI,
# 	"0::",
# 	"1::",
# 	to.numeric = FALSE)
table(d$FI)

d$Age_WHO <- Recode(d$Age_WHO,
  "1::20-44",
  "2::45-64",
  "3::65-85",
  to.numeric = FALSE
)
table(d$Age_WHO)

d$Race_new <- Recode(d$Race_new,
  "1::non-Hispanic white",
  "2::non-Hispanic black",
  "3::Mexican American",
  "4::Other Race",
  to.numeric = FALSE
)
table(d$Race_new)

d$Educational_new <- Recode(d$Educational_new,
  "1::less than high school",
  "2::high school",
  "3::college and above",
  to.numeric = FALSE
)
table(d$Educational_new)

d$PIR_new <- Recode(d$PIR_new,
  "1::<2",
  "2::≥2",
  "3::unknow",
  to.numeric = FALSE
)
table(d$PIR_new)

d$BMI_new <- Recode(d$BMI_new,
  "1::<25",
  "2::25-30",
  "3::≥30",
  to.numeric = FALSE
)
table(d$BMI_new)

d$Dyslipidemia_new <- Recode(d$Dyslipidemia_new,
  "1::no",
  "2::yes",
  to.numeric = FALSE
)
table(d$Dyslipidemia_new)

d$smoking <- Recode(d$smoking,
  "1::no smoking",
  "2::past smoking",
  "3::current smoking",
  to.numeric = FALSE
)
table(d$smoking)

d$drink <- Recode(d$drink,
  "1::no drinking or Light drinking",
  "2::moderate drinking",
  "3::heavy drinking",
  "4::unknow",
  to.numeric = FALSE
)
table(d$drink)

d$Diabetes_new <- Recode(d$Diabetes_new,
  "1::no",
  "2::yes",
  to.numeric = FALSE
)
table(d$Diabetes_new)

d$hypertension_new <- Recode(d$hypertension_new,
  "1::no",
  "2::yes",
  to.numeric = FALSE
)
table(d$hypertension_new)

d$activity <- Recode(d$activity,
  "1::inactive",
  "2::moderate",
  "3::vigorous",
  to.numeric = FALSE
)
table(d$activity)

# 提取所需要的列并进行重命名
dput(names(d))

d <- d[, c(
  "SEQN", "SDMVSTRA", "SDMVPSU", "Weight_new",
  "Sex", "Age", "Age_WHO", "Race_new", "Educational_new", "PIR_new", "BMI",
  "CRP", "energy", "BMI_new",
  "smoking", "drink",
  "Dyslipidemia_new", "Diabetes_new", "hypertension_new",
  "activity",
  "DII", "FI"
)]

d <- rename(
  .data = d,
  nhs_wt = Weight_new,
  Age_group = Age_WHO,
  Race = Race_new,
  Educational_level = Educational_new,
  PIR = PIR_new,
  BMI_group = BMI_new,
  Smoking_status = smoking,
  Alcohol_consumption = drink,
  Dyslipidemia = Dyslipidemia_new,
  Diabetes = Diabetes_new,
  Hypertension = hypertension_new,
  Physical_activity = activity,
  Energy = energy
)


# *纳入审稿人需要删除的患者seqn --------------------------------------------------------

# 1.排除泻药（laxative）####

FI.tsv <- nhs_tsv("BHQ", years = 2005:2010)
d.la <- nhs_read(FI.tsv,
  varLabel = T,
  refuse_dontknow_toNA = T,
  psu_strat = F,
  lower_cd = T
)

# 重命名列
d.la <- col_rename(d.la, c(
  "bhq010:gas",
  "bhq020:mucus",
  "bhq030:liquid",
  "bhq040:solid",
  "bhq100:laxative",
  "bhq110:laxative.times"
))

# 重新赋值变量
d.la$mucus <- Recode(d.la$mucus,
  "never?::no",
  "2 or more times a week::yes",
  "1-3 times a month, or::yes",
  "once a day::yes",
  "once a week::yes",
  "2 or more times a day::yes",
  "never::no",
  "1-3 times a month::yes",
  "NA::no",
  to.numeric = FALSE
)

d.la$liquid <- Recode(d.la$liquid,
  "never?::no",
  "once a day::yes",
  "2 or more times a week::yes",
  "1-3 times a month, or::yes",
  "once a week::yes",
  "2 or more times a day::yes",
  "never::no",
  "1-3 times a month::yes",
  "NA::no",
  to.numeric = FALSE
)

d.la$solid <- Recode(d.la$solid,
  "never?::no",
  "1-3 times a month, or::yes",
  "2 or more times a week::yes",
  "2 or more times a day::yes",
  "once a week::yes",
  "once a day::yes",
  "never::no",
  "1-3 times a month::yes",
  "NA::no",
  to.numeric = FALSE
)

d.la$laxative <- Recode(d.la$laxative,
  "no::",
  "yes::",
  "NA::no",
  to.numeric = FALSE
)

d.la$laxative.times <- Recode(d.la$laxative.times,
  "1-3 times a week::yes",
  "2-3 times a month::yes",
  "most days::yes",
  "once a month::yes",
  "NA::no",
  to.numeric = FALSE
)

d.la <- d.la[, c("seqn", "laxative")]
table(d.la$laxative, useNA = "i")



# 2.排除孕妇####
d.preganant <- diag_Pregnant(years = 2005:2010)
table(d.preganant$Pregnant, useNA = "i")


# 3.癌症####
tsv <- nhs_tsv("MCQ", years = 2005:2010)
d.cancer <- nhs_read(tsv)
d.cancer <- d.cancer[, c("seqn", "mcq220")]
colnames(d.cancer)[2] <- "cancer"
table(d.cancer$cancer, useNA = "i")



# 4.药物(antibiotics)####
# drug_search("antibiotics")

d.antibiotics <- drug_anti.infectives(
  years = 2005:2010,
  take_drug = T,
  DrugNumber = T,
  fdaNDC = T,
  drugname = T,
  dcn = T,
  icn = T, icd10 = T
)
# 提取服用抗生素药物患者43人
ck <- stringr::str_detect(d.antibiotics$icn, "antibiotics")

d.antibiotics$antibiotic <- ifelse(ck, "yes", "no")
table(d.antibiotics$antibiotic)

d.antibiotics <- d.antibiotics[, c("seqn", "antibiotic")]


# *删除上述患者 -----------------------------------------------------------------
# 合并数据集
colnames(d)[1:3] <- c("seqn", "sdmvstra", "sdmvpsu")

d <- Left_Join(d,
  d.la,
  d.preganant,
  d.cancer,
  d.antibiotics,
  by = "seqn"
)

# 删除患者
# 1.泻药  drop:498(4%)
table(d$laxative, useNA = "i")
d <- drop_row(d, d$laxative == "yes")

# # 2.怀孕 drop:336(3%)
# table(d$Pregnant,useNA = 'i')
# d <- drop_row(d, d$Pregnant == 'yes')

# 3.癌症 drop:1105(10%)
# table(d$cancer,useNA = 'i')
# d <- drop_row(d, d$cancer == 'Yes')

# 4.抗生素
table(d$antibiotic)
d <- drop_row(d, d$antibiotic == "yes")

# *保存数据 -------------------------------------------------------------------
setwd("F:/我的研究/DII+FI/1、data")
save(d, file = "clean_data.RData")

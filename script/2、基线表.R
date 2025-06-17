rm(list = ls())

# 加载包
library(gtsummary)
library(nhanesR)
library(survey)

# 加载数据
setwd("F:/我的研究/DII+FI/1、data")
load(file = "clean_data.RData")


# 加权
nhs.design <- svy_design(d)

svy_population(nhs.design)


# svyttest(Age~FI,design = nhs.design)
# svyttest(Energy~FI,design = nhs.design)
# svyttest(BMI~FI,design = nhs.design)
# svyttest(DII~FI,design = nhs.design)


# 基线表 ---------------------------------------------------------------------
tb1 <-
  tbl_svysummary(nhs.design,
    by = "FI",
    include = -c(
      "seqn", "sdmvpsu", "sdmvstra", "nhs_wt", "Age",
      "laxative", "Pregnant", "cancer", "antibiotic",
      "BMI"
    ), # 删除不展示的变量
    type = list(where(is.factor) ~ "categorical"), # 将因子类型转换层字符串，防止二分类变量不显示
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)", # 设置分类变量展示
      all_continuous() ~ "{mean} (±{mean.std.error})"
    ), # 设置连续变量展示
    digits = list(
      all_categorical() ~ c(0, 1), # 设置小数
      all_continuous() ~ 1,
      "DII" ~ 2
    ),
    label = list(
      Age_group ~ "Age, year", # 设置标签
      BMI_group ~ "BMI, kg/m2",
      Energy ~ "Energy intake, Kcal/day"
    ),
  ) |>
  add_overall() |> # 添加Total列
  add_p(pvalue_fun = function(x) scales::pvalue(x, accuracy = 0.001)) |> # p值保留3位有效数值
  # 加粗p<0.05
  bold_p(t = 0.05) |>
  modify_header(all_stat_cols() ~
    "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") |>
  modify_footnote(update = all_stat_cols() ~
    "mean(±sd) for continuous; n(%) for categorical")
tb1

# 导出数据
tb1 |>
  as_flex_table() |>
  flextable::save_as_docx(path = "F:/我的研究/DII+FI/3、output/基线表—多重调整前.docx")

# multiple testing --------------------------------------------------------

# 自定义函数进行 Benjamini-Hochberg 调整
adjust_p_bh <- function(p) {
  p.adjust(p, method = "BH")
}

tb2 <-
  tbl_svysummary(
    nhs.design,
    by = "FI",
    include = -c(
      "seqn", "sdmvpsu", "sdmvstra", "nhs_wt", "Age", "laxative",
      "Pregnant", "cancer", "antibiotic", "BMI"
    ), # 删除不展示的变量
    type = list(where(is.factor) ~ "categorical"), # 将因子类型转换层字符串，防止二分类变量不显示
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)", # 设置分类变量展示
      all_continuous() ~ "{mean} (±{mean.std.error})"
    ), # 设置连续变量展示
    digits = list(
      all_categorical() ~ c(0, 1), # 设置小数
      all_continuous() ~ 1,
      "DII" ~ 2
    ),
    label = list(
      Age_group ~ "Age, year", # 设置标签
      BMI_group ~ "BMI, kg/m2",
      Energy ~ "Energy intake, Kcal/day"
    )
  ) |>
  add_overall() |> # 添加 Total 列
  add_p(pvalue_fun = function(x) scales::pvalue(x, accuracy = 0.001)) |> # p 值保留 3 位有效数值
  # 进行 Benjamini-Hochberg 校正
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(p.value.adjusted = adjust_p_bh(p.value)) %>%
      dplyr::mutate(p.value = p.value.adjusted)
  ) |>
  # 加粗 p < 0.05
  bold_p(t = 0.05) |>
  modify_header(all_stat_cols() ~
    "**{level}**, N = {n_unweighted} ({style_percent(p)}%)") |>
  modify_footnote(update = all_stat_cols() ~
    "mean(±sd) for continuous; n(%) for categorical")

tb2

# 导出数据
tb2 |>
  as_flex_table() |>
  flextable::save_as_docx(path = "F:/我的研究/DII+FI/3、output/基线表—多重调整后.docx")



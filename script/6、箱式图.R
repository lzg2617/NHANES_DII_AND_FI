rm(list = ls())

# 加载包
library(ggpubr)
library(ggplot2)

# 加载数据
setwd("F:/我的研究/DII+FI/1、data")
load(file = "clean_data.RData")


# 绘制箱式图 -------------------------------------------------------------------
my_comparisons <- list(c("1", "0"))

d |> ggplot(aes(x = FI, y = DII, fill = FI)) +
  geom_boxplot(outliers = F, fill = c("#00bdc3", "#f08705")) +
  stat_compare_means(
    comparisons = my_comparisons, method = "t.test",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
      symbols = c("***", "**", "*", "-*", "ns")
    )
  ) +
  guides(fill = F) +
  stat_summary(fun.y = "mean", geom = "point", shape = 20, size = 3, colour = "black") +
  annotate("text", x = 1.1, y = 1.2, label = "mean=1.33", colour = "black", size = 3) +
  annotate("text", x = 2.1, y = 1.2, label = "mean=1.53", colour = "black", size = 3) +
  theme_classic2()


# 导出图片 --------------------------------------------------------------------
setwd("F:/我的研究/DII+FI/3、output")
export::graph2tif(file = "Figure 2", width = 5, height = 5)

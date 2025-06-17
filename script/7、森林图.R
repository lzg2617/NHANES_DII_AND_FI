
# 森林图 ---------------------------------------------------------------------
rm(list = ls())


# 加载包
library(forestploter)
library(openxlsx)

# 读取数据
setwd("F:/我的研究/DII+FI/3、output")
d <- read.xlsx("forsest plot.xlsx")

d <- d[33:64,]

# -------------------------------------------------------------------------

d$P.for.interaction[is.na(d$P.for.interaction)] <- ""
d$`OR.(95%.CI)`[is.na(d$`OR.(95%.CI)`)] <- ""

# 添加一列空白
d$` ` <- paste(rep(" ", 32), collapse = "")


# 定义主题
tm <- forest_theme(base_size = 10,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#5b9bd5",
                   ci_fill = "black",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20")

plot <- forest(d[, c(1,8,6,7)],#提出一行空的到前面来给森林图
               est = d$OR,#OR(黑点的位置)
               lower = d$conf.low,#上限
               upper = d$conf.high,#下限
               ci_column = 2,#把森林图放到第二行http://127.0.0.1:30823/graphics/plot_zoom_png?width=359&height=913
               ref_line = 1,#参考线设置为1
               theme = tm,#用上面设置好的主题
               xlim = c(0, 3),#x轴长度
               ticks_at = seq(0,3),#刻度
               arrow_lab = c("lower risk", "higher risk")#添加文本
)
plot

export::graph2pdf(file = "forest2",width = 12, height = 14)

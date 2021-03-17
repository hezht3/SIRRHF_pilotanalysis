############################################# SIRRHF NSFC linechart #############################################
# programmer: Zhengting He
# March 12th, 2020

setwd("F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/data/dataset/line chart")

require(dplyr)
require(ggplot2)

############################################ line chart of normal BP ############################################
data_nor <- read.csv("normal BP.csv")

data_nor$period <- factor(data_nor$period, levels = c("screen", "baseline", "week4", "week8", "week12"))

# BP & HR, stratify by sex
data_nor$var <- factor(data_nor$var, levels = c("sbp", "dbp", "hr"))
data_nor$sex <- factor(data_nor$sex, levels = c("all", "male", "female"), labels = c("总体", "男性", "女性"))
tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/normal_BP & HR, stratify by sex.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_nor[1:45,], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_nor[1:45,], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_nor[1:45,], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  #geom_text(data = data_nor[1:45,], aes(label = mean, x = period, y = mean + 1.5), check_overlap = TRUE) +
  facet_wrap(~sex) +
  ggtitle("已入组患者诊室收缩压、舒张压、心率变化趋势（按性别分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg或次/分)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0", "#fdbb30"), 
                      labels=c("收缩压", "舒张压", "心率")) +
  scale_x_discrete(labels = c("筛选", "基线", "4周", "8周", "12周")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

# BP & HR, stratify by randomization
data_nor$randomization <- factor(data_nor$randomization, levels = c("all", "spironolactone", "indapamide"), labels = c("总体", "螺内酯组", "吲哒帕胺组"))
tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/normal_BP & HR, stratify by randomization.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_nor[c(1:15, 46:75),], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_nor[c(1:15, 46:75),], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_nor[c(1:15, 46:75),], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~randomization) +
  ggtitle("已入组患者诊室收缩压、舒张压、心率变化趋势（按药物分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg或次/分)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0", "#fdbb30"), 
                      labels=c("收缩压", "舒张压", "心率")) +
  scale_x_discrete(labels = c("筛选", "基线", "4周", "8周", "12周")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

# only BP, stratify by sex
data_bp <- filter(data_nor, var != "hr")

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/normal_only BP, stratify by sex.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_bp[1:30,], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_bp[1:30,], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_bp[1:30,], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~sex) +
  ggtitle("已入组患者诊室收缩压、舒张压变化趋势（按性别分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0"), 
                      labels=c("收缩压", "舒张压")) +
  scale_x_discrete(labels = c("筛选", "基线", "4周", "8周", "12周")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

# only BP, stratify by randomization
tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/normal_only BP, stratify by randomization.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_bp[c(1:10, 31:50),], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_bp[c(1:10, 31:50),], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_bp[c(1:10, 31:50),], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~randomization) +
  ggtitle("已入组患者诊室收缩压、舒张压变化趋势（按药物分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0"), 
                      labels=c("收缩压", "舒张压")) +
  scale_x_discrete(labels = c("筛选", "基线", "4周", "8周", "12周")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

############################################ line chart of ambulatory BP ############################################
data_amb <- read.csv("ambulatory BP.csv")

data_amb$period <- factor(data_amb$period, levels = c("baseline_day", "baseline_night", "baseline_24", "week12_day", "week12_night", "week12_24"),
                          labels = c("基线(日)", "基线(夜)", "基线(24h)", "12周(日)", "12周(夜)", "12周(24)"))

# BP & HR, stratify by sex
data_amb$var <- factor(data_amb$var, levels = c("sbp", "dbp", "hr"), labels = c("收缩压", "舒张压", "心率"))
data_amb$sex <- factor(data_amb$sex, levels = c("all", "male", "female"), labels = c("总体", "男性", "女性"))
tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/ambulatory_BP & HR, stratify by sex.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_amb[1:54,], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_amb[1:54,], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_amb[1:54,], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~sex) +
  ggtitle("已入组患者24小时动态收缩压、舒张压、心率变化趋势（按性别分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg或次/分)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0", "#fdbb30")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

# BP & HR, stratify by randomization
data_amb$randomization <- factor(data_amb$randomization, levels = c("all", "spironolactone", "indapamide"), labels = c("总体", "螺内酯组", "吲哒帕胺组"))
tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/ambulatory_BP & HR, stratify by randomization.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_amb[c(1:18, 55:90),], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_amb[c(1:18, 55:90),], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_amb[c(1:18, 55:90),], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~randomization) +
  ggtitle("已入组患者诊室收缩压、舒张压、心率变化趋势（按药物分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg或次/分)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0", "#fdbb30")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

# only BP, stratify by sex
data_bp_amb <- filter(data_amb, var != "心率")

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/ambulatory_only BP, stratify by sex.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_bp_amb[1:36,], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_bp_amb[1:36,], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_bp_amb[1:36,], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~sex) +
  ggtitle("已入组患者诊室收缩压、舒张压变化趋势（按性别分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

# only BP, stratify by randomization
tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/ambulatory_only BP, stratify by randomization.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_errorbar(data = data_bp_amb[c(1:12, 37:60),], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_bp_amb[c(1:12, 37:60),], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_bp_amb[c(1:12, 37:60),], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~randomization) +
  ggtitle("已入组患者诊室收缩压、舒张压变化趋势（按药物分组）") +
  xlab("时期") +
  ylab("均值 ± 标准差 (mmHg)") +
  scale_colour_manual("体检指标",
                      values = c("#e64b50", "#0077c0")) +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

############################################ line chart of patients finished study ############################################
data5 <- read.csv("F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/code/data_htn_12 week finished2.csv")

data5$id <- as.character(data5$id)

data5_nor <- data5[1:75,]
data5_amb <- data5[76:165,]

# normal BP of all patients finished study
data5_nor$period <- factor(data5_nor$period, levels = c("screen", "baseline", "week4", "week8", "week12"),
                          labels = c("筛选", "基线", "4周", "8周", "12周"))
data5_nor$var <- factor(data5_nor$var, levels = c("sbp", "dbp", "hr"), labels = c("收缩压", "舒张压", "心率"))
data5_nor$sex <- factor(data5_nor$sex, levels = c("male", "female"), labels = c("男性", "女性"))

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/finished_normal_BP.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_point(data = filter(data5_nor, var != "心率"), aes(x = period, y = mean, colour = id, group = interaction(id, var), alpha = factor(var)), size = 2) +
  geom_line(data = filter(data5_nor, var != "心率"), aes(x = period, y = mean, colour = id, group = interaction(id, var), alpha = factor(var)), size = 1) +
  ggtitle("已完成研究患者的诊室血压波动情况") +
  xlab("时期") +
  ylab("血压测量值 (mmHg)") +
  scale_colour_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"), guide = "none") +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray')) +
  scale_alpha_manual("体检指标", values = c(1, 0.7))
dev.off()

# ambulatory BP of all patients finished study
data5_amb$period <- factor(data5_amb$period, levels = c("baseline_day", "baseline_night", "baseline_24", "week12_day", "week12_night", "week12_24"),
                           labels = c("基线(日)", "基线(夜)", "基线(24h)", "12周(日)", "12周(夜)", "12周(24h)"))
data5_amb$var <- factor(data5_amb$var, levels = c("sbp", "dbp", "hr"), labels = c("收缩压", "舒张压", "心率"))
data5_amb$sex <- factor(data5_amb$sex, levels = c("male", "female"), labels = c("男性", "女性"))

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/finished_ambulatory_BP.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_point(data = filter(data5_amb, var != "心率"), aes(x = period, y = mean, colour = id, group = interaction(id, var), alpha = factor(var)), size = 2) +
  geom_line(data = filter(data5_amb, var != "心率"), aes(x = period, y = mean, colour = id, group = interaction(id, var), alpha = factor(var)), size = 1) +
  ggtitle("已完成研究患者的24小时动态血压波动情况") +
  xlab("时期") +
  ylab("血压测量值 (mmHg)") +
  scale_colour_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"), guide = "none") +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray')) +
  scale_alpha_manual("体检指标", values = c(1, 0.7))
dev.off()

############################################ line chart of PWV ############################################
data_pwv <- read.csv("F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/code/data_htn_12 week PWV.csv")

data_pwv$id <- as.character(data_pwv$id)

data_pwv$period <- factor(data_pwv$period, levels = c("baseline", "week12"),
                           labels = c("基线", "12周"))

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/finished_pwv_seperate.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_point(data = data_pwv, aes(x = period, y = pwv, colour = id, group = id), size = 2) +
  geom_line(data = data_pwv, aes(x = period, y = pwv, colour = id, group = id), size = 1) +
  ggtitle("已完成研究患者的脉搏波传导速度波动情况") +
  xlab("时期") +
  ylab("脉搏波传导速度测量值") +
  scale_colour_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"), guide = "none") +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

data_pwv_sum <- data_pwv %>%
                  group_by(period) %>%
                    summarise(mean = mean(pwv), sd = sd(pwv))

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/finished_pwv_sum.tiff", res = 600, width = 8000, height = 4290)
ggplot(data = data_pwv, aes(x = period, y = pwv)) +
  geom_boxplot(aes(fill = period)) +
  ggtitle("已完成研究患者的脉搏波传导速度波动情况") +
  xlab("时期") +
  ylab("脉搏波传导速度测量值") +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/finished_pwv_sum_randomization.tiff", res = 600, width = 8000, height = 4290)
ggplot(data = data_pwv, aes(x = period, y = pwv)) +
  geom_boxplot(aes(fill = randomization)) +
  ggtitle("已完成研究患者的脉搏波传导速度波动情况") +
  xlab("时期") +
  ylab("脉搏波传导速度测量值") +
  theme(legend.position = "bottom", panel.background = element_rect(fill='transparent', color='gray'))
dev.off()

############################################ final revision on plots ############################################

# 01 boxplot
data_pwv$randomization <- factor(data_pwv$randomization, levels = c("spironolactone", "indapamide"), labels = c("螺内酯组", "吲哒帕胺组"))

windowsFonts(heiti = windowsFont(family = "SimHei"), yahei = windowsFont(family = "Microsoft YaHei"),
             dengxian = windowsFont(family = "Arial"), zhenghei = windowsFont(family = "Microsoft JhengHei"))

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/final/01 finished_pwv_sum_randomization.tiff", res = 600, width = 6000, height = 4290)
ggplot(data = data_pwv, aes(x = period, y = pwv)) +
  geom_boxplot(aes(fill = period), width = 0.35) +
  ggtitle("图3. 已完成研究患者的脉搏波传导速度变化情况") +
  xlab("") +
  ylab("脉搏波传导速度(米/秒)") +
  theme(panel.background = element_rect(fill='transparent', color='black'),
        panel.border = element_rect(fill='transparent', color='transparent'),
        panel.grid = element_blank(),
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5, family = "zhenghei"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.title = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.position = "bottom") +
        scale_fill_manual("药物", values = c("#439fd3", "#f6b128"), guide = "none")
dev.off()

# 02 ambulatory BP of all patients finished study

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/final/02 finished_ambulatory_BP.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_point(data = filter(data5_amb, var != "心率"), aes(x = period, y = mean, colour = id, group = id), size = 2) +
  geom_line(data = filter(data5_amb, var != "心率"), aes(x = period, y = mean, colour = id, group = id), size = 1) +
  facet_wrap(~ var, scales = "free") +
  ggtitle("图4. 已完成研究患者的24小时动态血压波动情况") +
  xlab("") +
  ylab("血压(mmHg)") +
  scale_colour_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"), guide = "none") +
  theme(panel.background = element_rect(fill='transparent', color='black'),
        panel.border = element_rect(fill='transparent', color='transparent'),
        panel.grid = element_blank(),
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5, family = "zhenghei"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        strip.background = element_rect(fill='transparent', color='black'),
        strip.text.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei")) +
        scale_alpha_manual("体检指标", values = c(1, 0.7))
dev.off()

# 03 BP & HR, stratify by sex and randomization

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/final/03 ambulatory_only BP, stratify by sex.tiff", res = 600, width = 10000, height = 4290)
ggplot() +
  geom_errorbar(data = data_bp_amb[1:36,], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_bp_amb[1:36,], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_bp_amb[1:36,], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~ sex) +
  ggtitle("图5. 已入组患者的动态血压波动情况") +
  xlab("") +
  ylab("血压 均值±标准差 (mmHg)") +
  scale_colour_manual("体检指标",
                      values = c("#e21737", "#0b648f"), 
                      labels=c("收缩压", "舒张压")) +
  scale_x_discrete(labels = c("基线(日)", "基线(夜)", "基线(24h)", "12周(日)", "12周(夜)", "12周(24h)")) +
  theme(panel.background = element_rect(fill='transparent', color='black'),
        panel.border = element_rect(fill='transparent', color='transparent'),
        panel.grid = element_blank(),
        plot.title = element_text(size = 18, face = "bold", color = "black", hjust = 0.5, family = "zhenghei"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        strip.background = element_rect(fill='transparent', color='black'),
        strip.text.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        legend.title = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.position = "bottom")
dev.off()

############################################ final revision on plots ############################################
############################################# plots title at buttom #############################################

# 01 boxplot
data_pwv$randomization <- factor(data_pwv$randomization, levels = c("spironolactone", "indapamide"), labels = c("螺内酯组", "吲哒帕胺组"))

windowsFonts(heiti = windowsFont(family = "SimHei"), yahei = windowsFont(family = "Microsoft YaHei"),
             dengxian = windowsFont(family = "Arial"), zhenghei = windowsFont(family = "Microsoft JhengHei"))

data_pwv <- data_pwv[3:15, ]

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/final/01 finished_pwv_sum_randomization.tiff", res = 600, width = 6000, height = 4290)
ggplot(data = data_pwv, aes(x = period, y = pwv)) +
  geom_boxplot(aes(fill = period), width = 0.35) +
  labs(caption = "图三：已完成研究患者的脉搏波传导速度变化情况") +
  xlab("") +
  ylab("脉搏波传导速度(米/秒)") +
  theme(panel.background = element_rect(fill='transparent', color='black'),
        panel.border = element_rect(fill='transparent', color='transparent'),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 18, face = "bold", color = "black", hjust = 0.5, family = "zhenghei"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.title = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.position = "bottom") +
  scale_fill_manual("药物", values = c("#439fd3", "#f6b128"), guide = "none")
dev.off()

# 02 ambulatory BP of all patients finished study

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/final/02 finished_ambulatory_BP.tiff", res = 600, width = 8000, height = 4290)
ggplot() +
  geom_point(data = filter(data5_amb, var != "心率"), aes(x = period, y = mean, colour = id, group = id), size = 2) +
  geom_line(data = filter(data5_amb, var != "心率"), aes(x = period, y = mean, colour = id, group = id), size = 1) +
  facet_wrap(~ var, scales = "free") +
  labs(caption = "图四：已完成研究患者的24小时动态血压波动情况") +
  xlab("") +
  ylab("血压(mmHg)") +
  scale_colour_manual(values = c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "#fc636b"), guide = "none") +
  theme(panel.background = element_rect(fill='transparent', color='black'),
        panel.border = element_rect(fill='transparent', color='transparent'),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 18, face = "bold", color = "black", hjust = 0.5, family = "zhenghei"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        strip.background = element_rect(fill='transparent', color='black'),
        strip.text.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei")) +
  scale_alpha_manual("体检指标", values = c(1, 0.7))
dev.off()

# 03 BP & HR, stratify by sex and randomization

tiff(file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/new plot/final/03 ambulatory_only BP, stratify by sex.tiff", res = 600, width = 10000, height = 4290)
ggplot() +
  geom_errorbar(data = data_bp_amb[1:36,], aes(x = period, ymin = mean - sd, ymax = mean + sd, colour = var, group = var), width = 0.1) +
  geom_point(data = data_bp_amb[1:36,], aes(x = period, y = mean, colour = var, group = var), size = 2) +
  geom_line(data = data_bp_amb[1:36,], aes(x = period, y = mean, colour = var, group = var), size = 1) +
  facet_wrap(~ sex) +
  labs(caption = "图五：已入组患者的动态血压波动情况") +
  xlab("") +
  ylab("血压 均值±标准差 (mmHg)") +
  scale_colour_manual("体检指标",
                      values = c("#e21737", "#0b648f"), 
                      labels=c("收缩压", "舒张压")) +
  scale_x_discrete(labels = c("基线(日)", "基线(夜)", "基线(24h)", "12周(日)", "12周(夜)", "12周(24h)")) +
  theme(panel.background = element_rect(fill='transparent', color='black'),
        panel.border = element_rect(fill='transparent', color='transparent'),
        panel.grid = element_blank(),
        plot.caption = element_text(size = 18, face = "bold", color = "black", hjust = 0.5, family = "zhenghei"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        axis.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        strip.background = element_rect(fill='transparent', color='black'),
        strip.text.x = element_text(size = 16, face = "bold", color = "black", family = "zhenghei"),
        legend.title = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.text = element_text(size = 14, face = "bold", color = "black", family = "zhenghei"),
        legend.position = "bottom")
dev.off()

############################################ summary statistics of PWV ############################################
data_pwv <- read.csv("F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/code/data_htn_12 week PWV.csv")

data_pwv$id <- as.character(data_pwv$id)

sd(data_pwv$pwv)

data_pwv %>%
  group_by(period) %>%
    summarise(mean = mean(pwv),
              sd = sd(pwv),
              median = median(pwv),
              min = min(pwv),
              max = max(pwv))

data_pwv %>%
  group_by(sex) %>%
  summarise(mean = mean(pwv),
            sd = sd(pwv),
            median = median(pwv),
            min = min(pwv),
            max = max(pwv))

data_pwv %>%
  group_by(randomization) %>%
  summarise(mean = mean(pwv),
            sd = sd(pwv),
            median = median(pwv),
            min = min(pwv),
            max = max(pwv))

data_pwv %>%
  group_by(interaction(period, sex)) %>%
  summarise(mean = mean(pwv),
            sd = sd(pwv),
            median = median(pwv),
            min = min(pwv),
            max = max(pwv))

data_pwv %>%
  group_by(interaction(period, randomization)) %>%
  summarise(mean = mean(pwv),
            sd = sd(pwv),
            median = median(pwv),
            min = min(pwv),
            max = max(pwv))

data_pwv <- data_pwv[c(1:5, 9:13),]
data_pwv_baseline <- data_pwv[1:5,]
data_pwv_week12 <- data_pwv[6:10,]
data_pwv_baseline$mean_dif <- data_pwv_week12$pwv - data_pwv_baseline$pwv
data_pwv_ind <- filter(data_pwv_baseline, randomization == "indapamide")
data_pwv_spi <- filter(data_pwv_baseline, randomization == "spironolactone")
t.test(data_pwv_ind$mean_dif, data_pwv_spi$mean_dif, paired=FALSE, alternative = "two.sided")

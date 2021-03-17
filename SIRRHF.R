require(dplyr)
require(ggplot2)

# import data
setwd("F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/data/dataset/csv version")

screen1 <- read.csv("screen1.csv", encoding = "UTF-8")
screen2 <- read.csv("screen2.csv", encoding = "UTF-8")
baseline <- read.csv("baseline.csv", encoding = "UTF-8")
randomization <- read.csv("randomization.csv", encoding = "UTF-8")
follow_up_4 <- read.csv("follow up 4 week.csv", encoding = "UTF-8")
blood_test_4 <- read.csv("blood test 4 week.csv", encoding = "UTF-8")
follow_up_8 <- read.csv("follow up 8 week.csv", encoding = "UTF-8")
blood_test_8 <- read.csv("blood test 8 week.csv", encoding = "UTF-8")
follow_up_12 <- read.csv("follow up 12 week.csv", encoding = "UTF-8")
blood_test_12 <- read.csv("blood test 12 week.csv", encoding = "UTF-8")
ambulatory_BP <- read.csv("ambulatory blood pressure.csv", encoding = "UTF-8")

############################################## dataset ##############################################
# baseline
baseline$SBP_baseline <- (baseline$B4_SBP_1st + baseline$B7_SBP_2nd + baseline$B10_SBP_3rd)/3
baseline$DBP_baseline <- (baseline$B5_DBP_1st + baseline$B8_DBP_2nd + baseline$B11_DBP_3rd)/3
baseline$HR_baseline <- (baseline$B6_HR_1st + baseline$B9_HR_2nd + baseline$B12_HR_3rd)/3

# 4 week follow up
follow_up_4$SBP_4week <- (follow_up_4$E3_SBP_1st + follow_up_4$E6_SBP_2nd + follow_up_4$E9_SBP_3rd)/3
follow_up_4$DBP_4week <- (follow_up_4$E4_DBP_1st + follow_up_4$E7_DBP_2nd + follow_up_4$E10_DBP_3rd)/3
follow_up_4$HR_4week <- (follow_up_4$E5_HR_1st + follow_up_4$E8_HR_2nd + follow_up_4$E11_HR_3rd)/3

# 8 week follow up
follow_up_8$SBP_8week <- (follow_up_8$E3_SBP_1st + follow_up_8$E6_SBP_2nd + follow_up_8$E9_SBP_3rd)/3
follow_up_8$DBP_8week <- (follow_up_8$E4_DBP_1st + follow_up_8$E7_DBP_2nd + follow_up_8$E10_DBP_3rd)/3
follow_up_8$HR_8week <- (follow_up_8$E5_HR_1st + follow_up_8$E8_HR_2nd + follow_up_8$E11_HR_3rd)/3

# 12 week follow up
follow_up_12$SBP_12week <- (follow_up_12$C1_SBP_1st + follow_up_12$C4_SBP_2nd + follow_up_12$C7_SBP_3rd)/3
follow_up_12$DBP_12week <- (follow_up_12$C2_DBP_1st + follow_up_12$C5_DBP_2nd + follow_up_12$C8_DBP_3rd)/3
follow_up_12$HR_12week <- (follow_up_12$C3_HR_1st + follow_up_12$C6_HR_2nd + follow_up_12$C9_HR_3rd)/3

# merge dataset
data_htn <- left_join(screen1[, c("id", "name", "A7_sex", "SBP_screen", "DBP_screen", "HR_screen")],
                      baseline[, c("id", "SBP_baseline", "DBP_baseline", "HR_baseline")],
                      by = "id")
data_htn <- left_join(data_htn,
                      follow_up_4[, c("id", "SBP_4week", "DBP_4week", "HR_4week")],
                      by = "id")
data_htn <- left_join(data_htn,
                      follow_up_8[, c("id", "SBP_8week", "DBP_8week", "HR_8week")],
                      by = "id")
data_htn <- left_join(data_htn,
                      follow_up_12[, c("id", "SBP_12week", "DBP_12week", "HR_12week")],
                      by = "id")
data_htn <- left_join(data_htn,
                      randomization[, c("id", "C1_randomization")],
                      by = "id")
data_htn <- left_join(data_htn,
                      ambulatory_BP,
                      by = "id")
colnames(data_htn) <- c("id", "name", "sex", "SBP_screen", "DBP_screen", "HR_screen", "SBP_baseline", "DBP_baseline",
                        "HR_baseline", "SBP_4week", "DBP_4week", "HR_4week", "SBP_8week", "DBP_8week", "HR_8week",
                        "SBP_12week", "DBP_12week", "HR_12week", "randomization", "baseline_dSBP", "baseline_dDBP",
                        "baseline_dPR", "baseline_nSBP", "baseline_nDBP", "baseline_nPR", "baseline_24SBP",
                        "baseline_24DBP", "baseline_24PR", "week12_dSBP", "week12_dDBP", "week12_dPR",
                        "week12_nSBP", "week12_nDBP", "week12_nPR", "week12_24SBP", "week12_24DBP",
                        "week12_24PR" )

# manipulate dataset
write.csv(data_htn, file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/code/data_htn.csv")

data_htn2 <- read.csv("F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/code/data_htn2.csv")

############################################## SBP ##############################################
# summary statistics_SBP
summary_SBP <- bind_rows(summary(data_htn$SBP_screen), summary(data_htn$SBP_baseline), summary(data_htn$baseline_dSBP),
                         summary(data_htn$baseline_nSBP), summary(data_htn$baseline_24SBP), summary(data_htn$SBP_4week), 
                         summary(data_htn$SBP_8week), summary(data_htn$SBP_12week), summary(data_htn$week12_dSBP),
                         summary(data_htn$week12_nSBP), summary(data_htn$week12_24SBP))
summary_SBP$period <- c("screen", "baseline", "baseline_day", "baseline_night", "baseline_24", "4 week", "8 week",
                        "12 week", "12 week_day", "12 week_night", "12 week_24")
write.csv(summary_SBP, file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/summary_SBP.csv")

# summary statistics_SBP_stratify by sex
data_htn_male <- filter(data_htn, sex == "male")
data_htn_female <- filter(data_htn, sex == "female")

summary_SBP_male <- bind_rows(summary(data_htn_male$SBP_screen), summary(data_htn_male$SBP_baseline), summary(data_htn_male$baseline_dSBP),
                         summary(data_htn_male$baseline_nSBP), summary(data_htn_male$baseline_24SBP), summary(data_htn_male$SBP_4week), 
                         summary(data_htn_male$SBP_8week), summary(data_htn_male$SBP_12week), summary(data_htn_male$week12_dSBP),
                         summary(data_htn_male$week12_nSBP), summary(data_htn_male$week12_24SBP))
summary_SBP_male$period <- c("screen", "baseline", "baseline_day", "baseline_night", "baseline_24", "4 week", "8 week",
                        "12 week", "12 week_day", "12 week_night", "12 week_24")

summary_SBP_female <- bind_rows(summary(data_htn_female$SBP_screen), summary(data_htn_female$SBP_baseline), summary(data_htn_female$baseline_dSBP),
                              summary(data_htn_female$baseline_nSBP), summary(data_htn_female$baseline_24SBP), summary(data_htn_female$SBP_4week), 
                              summary(data_htn_female$SBP_8week), summary(data_htn_female$SBP_12week), summary(data_htn_female$week12_dSBP),
                              summary(data_htn_female$week12_nSBP), summary(data_htn_female$week12_24SBP))
summary_SBP_female$period <- c("screen", "baseline", "baseline_day", "baseline_night", "baseline_24", "4 week", "8 week",
                             "12 week", "12 week_day", "12 week_night", "12 week_24")

write.csv(summary_SBP_male, file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/summary_SBP_male.csv")
write.csv(summary_SBP_female, file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/summary_SBP_female.csv")

# plot
summary_SBP$sex <- "all"
summary_SBP_male$sex <- "male"
summary_SBP_female$sex <- "female"
summary_SBP_all <- bind_rows(summary_SBP, summary_SBP_male, summary_SBP_female)

summary_SBP_clinic <- summary_SBP_all[c(1:2, 6:8, 12:13, 17:19, 23:24, 28:30), ]
summary_SBP_ambulatory <- summary_SBP_all[c(3:5, 9:11, 14:16, 20:22, 25:27, 31:33), ]

summary_SBP_clinic$period <- factor(summary_SBP_clinic$period, levels = c("screen", "baseline", "4 week", "8 week", "12 week"))
ggplot(data = summary_SBP_clinic, aes(x = period, y = Mean, colour = sex, group = sex)) +
  geom_point() +
  geom_line() +
  theme_classic()
ggplot(data = filter(summary_SBP_clinic, sex == "all"), aes(x = period, y = Mean, group = sex)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(legend.position="none")

summary_SBP_ambulatory$period <- factor(summary_SBP_ambulatory$period, levels = c("baseline_day", "baseline_night", "baseline_24", "12 week_day", "12 week_night", "12 week_24"))
ggplot(data = summary_SBP_ambulatory, aes(x = period, y = Mean, colour = sex, group = sex)) +
  geom_point() +
  geom_line() +
  theme_classic()
ggplot() +
  geom_point(data = summary_SBP_ambulatory[c(1,4,7,10,13,16), ], aes(x = period, y = Mean, group = sex)) +
  geom_line(data = summary_SBP_ambulatory[c(1,4,7,10,13,16), ], aes(x = period, y = Mean, group = sex)) +
  geom_point(data = summary_SBP_ambulatory[c(2,5,8,11,14,17), ], aes(x = period, y = Mean, group = sex)) +
  geom_line(data = summary_SBP_ambulatory[c(2,5,8,11,14,17), ], aes(x = period, y = Mean, group = sex)) +
  geom_point(data = summary_SBP_ambulatory[c(3,6,9,12,15,18), ], aes(x = period, y = Mean, group = sex)) +
  geom_line(data = summary_SBP_ambulatory[c(3,6,9,12,15,18), ], aes(x = period, y = Mean, group = sex))
  

###### drop NA
data_htn <- filter(data_htn, is.na(HR_12week) == FALSE)

# summary statistics_SBP
summary_SBP_nona <- bind_rows(summary(data_htn$SBP_screen), summary(data_htn$SBP_baseline), summary(data_htn$baseline_dSBP),
                         summary(data_htn$baseline_nSBP), summary(data_htn$baseline_24SBP), summary(data_htn$SBP_4week), 
                         summary(data_htn$SBP_8week), summary(data_htn$SBP_12week), summary(data_htn$week12_dSBP),
                         summary(data_htn$week12_nSBP), summary(data_htn$week12_24SBP))
summary_SBP_nona$period <- c("screen", "baseline", "baseline_day", "baseline_night", "baseline_24", "4 week", "8 week",
                        "12 week", "12 week_day", "12 week_night", "12 week_24")
write.csv(summary_SBP_nona, file = "F:/Box Sync/Duke Kunshan University Intern/1 SIRRHF Trial/data analysis/output/summary_SBP_nona.csv")

# plot
summary_SBP_clinic <- summary_SBP_all[c(1:2, 6:8), ]
summary_SBP_ambulatory <- summary_SBP_all[c(3:5, 9:11), ]

summary_SBP_clinic$period <- factor(summary_SBP_clinic$period, levels = c("screen", "baseline", "4 week", "8 week", "12 week"))
# ggplot(data = summary_SBP_clinic, aes(x = period, y = Mean, colour = sex, group = sex)) +
#   geom_point() +
#   geom_line() +
#   theme_classic()
ggplot(data = filter(summary_SBP_clinic, sex == "all"), aes(x = period, y = Mean, group = sex)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(legend.position="none")

summary_SBP_ambulatory$period <- factor(summary_SBP_ambulatory$period, levels = c("baseline_day", "baseline_night", "baseline_24", "12 week_day", "12 week_night", "12 week_24"))
ggplot(data = summary_SBP_ambulatory, aes(x = period, y = Mean, group = sex)) +
  geom_point() +
  geom_line() +
  theme_classic()
ggplot() +
  geom_point(data = summary_SBP_ambulatory[c(1,4,7,10,13,16), ], aes(x = period, y = Mean, group = sex)) +
  geom_line(data = summary_SBP_ambulatory[c(1,4,7,10,13,16), ], aes(x = period, y = Mean, group = sex)) +
  geom_point(data = summary_SBP_ambulatory[c(2,5,8,11,14,17), ], aes(x = period, y = Mean, group = sex)) +
  geom_line(data = summary_SBP_ambulatory[c(2,5,8,11,14,17), ], aes(x = period, y = Mean, group = sex)) +
  geom_point(data = summary_SBP_ambulatory[c(3,6,9,12,15,18), ], aes(x = period, y = Mean, group = sex)) +
  geom_line(data = summary_SBP_ambulatory[c(3,6,9,12,15,18), ], aes(x = period, y = Mean, group = sex))

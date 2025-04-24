library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)



# ************************************
# Post OP ****************************
# ************************************

sheets_list <- excel_sheets(path = "data/Asymmetry_DeepBrainStimulation.xlsx")

#  [1] "DEMOGRAPHIE "             "FACTEURSDERISQUE "        "ATCD_MED_CHIR"           
#  [4] "SOCIAL "                  "PDQ39-CGIS-SCOPA"         "PGI"                     
#  [7] "UPDRS II"                 "UPDRSIII_TOTAUX"          "UPDRSIII_COMPLET_V0_V1"  
# [10] "UPDRSI_II_IV"             "Hoehn&Yarh-S&E"           "EVA_FNM_V0_V1"           
# [13] "HAM-D"                    "HAM-A"                    "TCI_TCSP_V0"             
# [16] "Hallu_Miami"              "MoCA V0"                  "MoCA V1"                 
# [19] "Clox"                     "Boston_Fluence"           "PEROP_COMPLPEROP"        
# [22] "FREQUENCE_V0"             "FREQUENCE_V1"             "EVENEMENTSINDESIRABLES"  
# [25] "CONSO_SPE"                "PSYCHOTROPES"             "AUTRE_PARKINSON"         
# [28] "MEDICAMENTS dans Rapport" "DATES_DE_VISITES "


UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

names(UPDRSIII_COMPLET_V0_V1)

df_names <- names(UPDRSIII_COMPLET_V0_V1)

data.frame(df_names) %>%
  filter(grepl("3.11", df_names))

Item3.11 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, 
                                              OFF_3.11_,ON_3.11_, ON_3.11_1, ON_3.11_2,ON_3.11_3,ON_3.11_4,ON_3.11_5,
                                              OFF_3.11_1,ONOFF_3.11_,OFFON_3.11_, ON_3.11_6)


Item3.11 <- Item3.11[-1,]

names(Item3.11) <- c("SUBJID", "OFF_Before", "ON_15min_Before",
                     "ON_30min_Before","ON_45min_Before","ON_60min_Before",
                     "ON_90min_Before", "ON_120min_Before", "OFF_After",
                     "ONOFF_After", "OFFON_After", "ONON_After")

Item3.11 <- data.frame(Item3.11) %>% mutate_each(as.numeric, OFF_Before:ONON_After)


length(unique(Item3.11$SUBJID)) # 835

dim(Item3.11)[1] * dim(Item3.11)[2]

sum(is.na(Item3.11))

mean(Item3.11$OFF_Before,na.rm = T)
mean(Item3.11$ON_60min_Before,na.rm = T)




Item3.11_after <- Item3.11 %>% select(SUBJID, OFF_After, ONOFF_After, OFFON_After, ONON_After)

dim(Item3.11_after)[1] * dim(Item3.11_after)[2]

sum(is.na(Item3.11_after))

length(unique(Item3.11_after$SUBJID)) # 835

Item3.11_after <- Item3.11_after %>% drop_na()

length(unique(Item3.11_after$SUBJID)) # 520


mean(Item3.11_after$OFF_After,na.rm = T)
mean(Item3.11_after$ONOFF_After,na.rm = T)
mean(Item3.11_after$OFFON_After,na.rm = T)
mean(Item3.11_after$ONON_After,na.rm = T)


# Reshape data to long format
data_long <- Item3.11_after %>%
  pivot_longer(cols = c(OFF_After, ONOFF_After, OFFON_After, ONON_After),
               names_to = "Condition",
               values_to = "Value")

# Shapiro-Wilk normality test for each condition
shapiro.test(Item3.11_after$OFF_After) #W = 0.6474, p-value < 2.2e-16
shapiro.test(Item3.11_after$ONOFF_After) #W = 0.49251, p-value < 2.2e-16
shapiro.test(Item3.11_after$OFFON_After) #W = 0.32465, p-value < 2.2e-16
shapiro.test(Item3.11_after$ONON_After) #W = 0.2755, p-value < 2.2e-16


friedman.test(as.matrix(Item3.11_after[, 2:5]))  # Assuming columns 2:5 are conditions

# 	Friedman rank sum test
# 
# data:  as.matrix(Item3.11_after[, 2:5])
# Friedman chi-squared = 337.51, df = 3, p-value < 2.2e-16


pairwise.wilcox.test(data_long$Value, data_long$Condition, 
                      paired = TRUE, p.adjust.method = "bonferroni")

# 	Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  data_long$Value and data_long$Condition 
# 
#             OFF_After OFFON_After ONOFF_After
# OFFON_After < 2e-16   -           -          
# ONOFF_After < 2e-16   1.4e-09     -          
# ONON_After  < 2e-16   0.041       2.3e-13    
# 
# P value adjustment method: bonferroni 



data_long %>% group_by(Condition, Value) %>% count() %>%
  ungroup() %>% group_by(Condition) %>% mutate(tot=sum(n)) %>%
  mutate(perc=n/tot)




# Calculate mean and standard error of the mean (SEM)
summary_stats <- data_long %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )

# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                          ifelse(Condition=="OFFON_After", "Med-OFF/DBS-ON",
                                                 ifelse(Condition=="ONOFF_After", "Med-ON/DBS-OFF", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) %>%
ggplot(aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.4) +
  theme_minimal() +
  labs(x = "\n Post-OP Condition",
       y = "Mean ± SEM \n") +
  scale_fill_brewer(palette = "Set1") +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) +
  scale_fill_manual(values=c("#4a4e4d", "#f6cd61", "#0e9aa7", "#d11141")) 


ggsave(file="p1.svg", plot=plot1, width=3, height=5)


# Convert Value to numeric for calculations
data_long$Value <- as.numeric(as.character(data_long$Value))

# Calculate percentage breakdown of each score per condition
percentage_stats <- data_long %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 520) * 100)  # Always using 520 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="OFF_After", "OFF/OFF",
                                          ifelse(Condition=="OFFON_After", "Med-OFF/DBS-ON",
                                                 ifelse(Condition=="ONOFF_After", "Med-ON/DBS-OFF", "ON/ON")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF/OFF", "Med-ON/DBS-OFF","Med-OFF/DBS-ON", "ON/ON"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Post-OP Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.11") +
  scale_fill_brewer(palette = "Blues") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file="p1.svg", plot=plot1, width=4, height=5)

library(ordinal)


data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

model <- clmm(Value ~ Condition + (1 | SUBJID), data = data_long, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    data_long
# 
#  link  threshold nobs logLik   AIC     niter     max.grad cond.H 
#  logit flexible  2080 -1076.85 2169.71 640(5894) 4.06e-06 8.0e+02
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 54.09    7.354   
# Number of groups:  SUBJID 520 
# 
# Coefficients:
#                      Estimate Std. Error z value Pr(>|z|)    
# ConditionOFFON_After  -4.6054     0.3344 -13.772   <2e-16 ***
# ConditionONOFF_After  -2.2954     0.2395  -9.584   <2e-16 ***
# ConditionONON_After   -5.4386     0.3743 -14.528   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   5.2070     0.5097   10.22
# 1|2   7.4747     0.5909   12.65
# 2|3   9.4213     0.6534   14.42
# 3|4  10.7768     0.6958   15.49


library(emmeans)

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


# $emmeans
#  Condition   emmean    SE  df asymp.LCL asymp.UCL
#  OFF_After    -8.22 0.599 Inf     -9.39     -7.05
#  OFFON_After -12.83 0.814 Inf    -14.42    -11.23
#  ONOFF_After -10.52 0.723 Inf    -11.93     -9.10
#  ONON_After  -13.66 0.847 Inf    -15.32    -12.00
# 
# Confidence level used: 0.95 
# 
# $contrasts
#  contrast                  estimate    SE  df z.ratio p.value
#  OFF_After - OFFON_After      4.605 0.334 Inf  13.772  <.0001
#  OFF_After - ONOFF_After      2.295 0.240 Inf   9.584  <.0001
#  OFF_After - ONON_After       5.439 0.374 Inf  14.528  <.0001
#  OFFON_After - ONOFF_After   -2.310 0.279 Inf  -8.291  <.0001
#  OFFON_After - ONON_After     0.833 0.311 Inf   2.682  0.0439
#  ONOFF_After - ONON_After     3.143 0.313 Inf  10.031  <.0001
# 
# P value adjustment: bonferroni method for 6 tests 






# ************************************
# Before vs After surger *************
# ************************************





Item3.11_after

Item3.11_before_vs_after <- Item3.11

# Define the relevant columns
columns_to_check <- c("ON_15min_Before", "ON_30min_Before", "ON_45min_Before", 
                      "ON_60min_Before", "ON_90min_Before", "ON_120min_Before")

# Compute the max value for each SUBJID, ignoring NAs
Item3.11_before_vs_after <- Item3.11_before_vs_after %>%
  mutate(Min_ON_Before = pmin(!!!syms(columns_to_check), na.rm = TRUE))

# Define the relevant columns
columns_to_check <- c("OFFON_After", "ONOFF_After", "ONON_After")

# Compute the max value for each SUBJID, ignoring NAs
Item3.11_before_vs_after <- Item3.11_before_vs_after %>%
  mutate(Min_ON_After = pmin(!!!syms(columns_to_check), na.rm = TRUE))



# View the first few rows
head(Item3.11_before_vs_after)

Item3.11_before_vs_after <- Item3.11_before_vs_after %>% select(SUBJID, OFF_Before, OFF_After, Min_ON_Before, Min_ON_After)

Item3.11_before_vs_after <- Item3.11_before_vs_after %>% drop_na()

length(unique(Item3.11_before_vs_after$SUBJID)) # 527

# Wilcoxon Signed-Rank Test (Paired Test)
wilcox.test(Item3.11_before_vs_after$OFF_Before, Item3.11_before_vs_after$OFF_After, paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Item3.11_before_vs_after$OFF_Before and Item3.11_before_vs_after$OFF_After
# V = 10313, p-value = 0.1236
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(Item3.11_before_vs_after$Min_ON_Before, Item3.11_before_vs_after$Min_ON_After, paired = TRUE, alternative = "two.sided")

# 	Wilcoxon signed rank test with continuity correction
# 
# data:  Item3.11_before_vs_after$Min_ON_Before and Item3.11_before_vs_after$OFFON_After
# V = 200.5, p-value = 0.03326
# alternative hypothesis: true location shift is not equal to 0



# Reshape the data into long format
long_data <- Item3.11_before_vs_after %>%
  pivot_longer(cols = c(OFF_Before, OFF_After, Min_ON_Before, Min_ON_After),
               names_to = c("Condition"),
               values_to = "Value") %>%
  mutate(Group = ifelse(grepl("Before", Condition), "Before", "After"),
         Measure = case_when(
           grepl("OFF_", Condition) ~ "OFF",
           grepl("ON", Condition) ~ "ON"
         ))


# Wilcoxon Signed-Rank Test
wilcox.test(Value ~ Group, data = filter(long_data, Measure == "OFF"), paired = TRUE)


wilcox.test(Value ~ Group, data = filter(long_data, Measure == "ON"), paired = TRUE)



library(ordinal)


off_data <- long_data %>% filter(Measure=="OFF")
on_data <- long_data %>% filter(Measure=="ON")

data_long$Value <- factor(data_long$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)
off_data$Value <- factor(off_data$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

off_data %>% group_by(Condition) %>% summarise(Value=mean(as.numeric(Value)))

on_data$Value <- factor(on_data$Value, levels = c(0, 1, 2, 3, 4), ordered = TRUE)

on_data %>% group_by(Condition) %>% summarise(Value=mean(as.numeric(Value)))


model <- clmm(Value ~ Condition + (1 | SUBJID), data = off_data, link = "logit", control = clmm.control(method = "ucminf"))

summary(model)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    off_data
# 
#  link  threshold nobs logLik   AIC     niter     max.grad cond.H 
#  logit flexible  1054 -1087.09 2186.18 880(4065) 1.85e+00 1.2e+02
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 7.864    2.804   
# Number of groups:  SUBJID 527 
# 
# Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)
# ConditionOFF_Before   0.1790     0.1538   1.164    0.244
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   1.4826     0.2278   6.509
# 1|2   2.8622     0.2742  10.438
# 2|3   4.2719     0.3332  12.820
# 3|4   5.1534     0.3780  13.632
# 


model <- clmm(Value ~ Condition + (1 | SUBJID), data = on_data, link = "logit", control = clmm.control(method = "ucminf"))
 
summary(model)


# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Value ~ Condition + (1 | SUBJID)
# data:    on_data
# 
#  link  threshold nobs logLik  AIC    niter     max.grad cond.H 
#  logit flexible  1054 -155.76 321.52 448(2570) 3.79e-04 4.0e+02
# 
# Random effects:
#  Groups Name        Variance Std.Dev.
#  SUBJID (Intercept) 67.19    8.197   
# Number of groups:  SUBJID 527 
# 
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)   
# ConditionMin_ON_Before  -1.8024     0.5725  -3.149  0.00164 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1   8.2260     0.5861   14.04
# 1|2  11.2599     0.8422   13.37
# 2|4  13.8577     1.3205   10.49

long_data


# Calculate mean and standard error of the mean (SEM)
summary_stats <- long_data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(as.numeric(Value)),  # Convert ordered factor to numeric
    SEM = sd(as.numeric(Value)) / sqrt(n())  # Standard Error of Mean
  )


long_data %>% group_by(Condition) %>% summarise(mean=mean(Value))

# Plot bar graph with error bars
plot1 <- summary_stats %>% mutate(Condition=ifelse(Condition=="OFF_Before", "OFF pre-op",
                                          ifelse(Condition=="OFF_After", "OFF post-op",
                                                 ifelse(Condition=="Min_ON_Before", "Best-ON pre-op", "Best-ON post-op")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF pre-op", "OFF post-op","Best-ON pre-op", "Best-ON post-op"))) %>%
ggplot(aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.4) +
  theme_minimal() +
  labs(x = "\n Evaluation/Condition",
       y = "Mean ± SEM \n") +
  scale_fill_brewer(palette = "Set1") +
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values=c("#4a4e4d", "#f8dad0", "#0c5776", "#001c44")) +
  scale_fill_manual(values=c("#4a4e4d", "#f8dad0", "#0c5776", "#001c44")) 


ggsave(file="p1.svg", plot=plot1, width=3, height=5)





# Convert Value to numeric for calculations
long_data$Value <- as.numeric(as.character(long_data$Value))

# Calculate percentage breakdown of each score per condition
percentage_stats <- long_data %>%
  group_by(Condition, Value) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / 527) * 100)  # Always using 527 as denominator

# Convert Value to factor for proper ordering in the plot
percentage_stats$Value <- factor(percentage_stats$Value, levels = c(0, 1, 2, 3, 4))

# Stacked bar plot


percentage_stats <- percentage_stats %>% mutate(Condition=ifelse(Condition=="OFF_Before", "OFF pre-op",
                                          ifelse(Condition=="OFF_After", "OFF post-op",
                                                 ifelse(Condition=="Min_ON_Before", "Best-ON pre-op", "Best-ON post-op")))) %>%
  mutate(Condition=factor(Condition, levels=c("OFF pre-op", "OFF post-op","Best-ON pre-op", "Best-ON post-op"))) 

# Filter only Value = 0 for labeling
labels_data <- percentage_stats %>% filter(Value == 0)


plot1 <- percentage_stats %>% 
   ggplot(aes(x = Condition, y = Percentage, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
   geom_text(data = labels_data, aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.9), size = 3, fontface = "bold") +
  labs(x = "\n Evaluation/Condition",
       y = "Percentage (%) \n",
       fill = "Item 3.11") +
  scale_fill_brewer(palette = "Blues") +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(file="p2.svg", plot=plot1, width=4, height=5)



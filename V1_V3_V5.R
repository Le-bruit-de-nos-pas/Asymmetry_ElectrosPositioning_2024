library(readxl)
library(tidyverse)
library(data.table)
library(missMDA)
library(ISLR2)
library(leaps)
library(glmnet)


# Year 3 and Year 5 Asymmetry Visits -----------


UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V0_V1 <- names(UPDRSIII_COMPLET_V0_V1)
ONON_After_V0_V1 <- data.frame(df_names_V0_V1) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names_V0_V1)) %>%
  filter(grepl("3.3", df_names_V0_V1)|
           grepl("3.4", df_names_V0_V1)|
           grepl("3.5", df_names_V0_V1)|
           grepl("3.6", df_names_V0_V1)|
           grepl("3.7", df_names_V0_V1)|
           grepl("3.8", df_names_V0_V1)|
           grepl("3.15", df_names_V0_V1)|
           grepl("3.16", df_names_V0_V1)|
           grepl("3.17", df_names_V0_V1)
  ) %>%
  filter(grepl("Right", df_names_V0_V1)|grepl("right", df_names_V0_V1)|grepl("left", df_names_V0_V1)|grepl("Left", df_names_V0_V1)) %>%
  arrange(df_names_V0_V1)  %>%
  filter(!grepl("OFF", df_names_V0_V1)) 
toString(as.list(ONON_After_V0_V1))
match_V0_V1 <- c("ON_3.15_Left6", "ON_3.15_Right_6", "ON_3.16_Left6", "ON_3.16_Right6", 
                 "ON_3.17_Inf_Left_6", "ON_3.17_Inf_Right6", "ON_3.17_Sup_Left_6", 
                 "ON_3.17_Sup_Right6", "ON_3.3_Inf_Left", "ON_3.3_Inf_Right", "ON_3.3_S_Left", 
                 "ON_3.3_S_Right", "ON_3.4_Left_", "ON_3.4_Right_", "ON_3.5_Left_", "ON_3.5_Right_", 
                 "ON_3.6_Left_", "ON_3.6_Right_", "ON_3.7_Left", "ON_3.7_Right_", "ON_3.8_Left6", "ON_3.8_Right_6")
match <- append("SUBJID", match_V0_V1)
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)
ONON_After_V0_V1 <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After_V0_V1 <- ONON_After_V0_V1[-1,]


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1 %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
                                 group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V0_V1)

ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% mutate_each(as.numeric, ON_3.3_S_Right:ON_3.17_Inf_Left_6)
sum(is.na(ONON_After_V0_V1))
dim(ONON_After_V0_V1) 


for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V0_V1[,-1],ncp=2, scale = T)

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V0_V1))
sum(ONON_After_V0_V1<0)
ONON_After_V0_V1[ONON_After_V0_V1<0] <- 0
sum(ONON_After_V0_V1<0)


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V0_V1$Diff <- ONON_After_V0_V1$Right - ONON_After_V0_V1$Left
mean(ONON_After_V0_V1$Diff)



UPDRSIII_COMPLET_V3_V5 <- read_xlsx(path="Raw_Database/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V3_V5 <- names(UPDRSIII_COMPLET_V3_V5)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MSDROIT_RIG, ON_MSGCHE_RIG, ON_MIDROIT_RIG, ON_MIGCHE_RIG,
                                                            ON_MS_DROIT_DOIGT, ON_MSGCHE_DOIGT, ON_MSDROIT_MAINS, ON_MSGCHE_MAINS, ON_MSDROIT_MA, ON_MSGCHE_MA,
                                                            ON_MIDROIT_PIED, ON_MIGCHE_PIED, ON_MIDROIT_JAMBE, ON_MIGCHE_JAMBE, 
                                                            ON_TREMBLPOST_MSDROIT, ON_TREMBLPOST_MSGCHE, ON_TREMBLMAIN_MSGCHE, ON_TREMBLMAIN_MSDROIT,
                                                            ON_MSDROIT_AMPLI_TREMBL, ON_MSGCHE_AMPLI_TREMBL, ON_MIDROIT_AMPLI_TREMBL, ON_MIGCHE_AMPLI_TREMBL)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

ONON_After_V3_V5 <- UPDRSIII_COMPLET_V3_V5

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5 %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
                                 group_by(SUBJID, VISIT) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V3_V5)

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% mutate_each(as.numeric, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL)
sum(is.na(ONON_After_V3_V5))
dim(ONON_After_V3_V5) 


for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V3_V5[,-c(1,2)],ncp=2, scale = T)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V3_V5))
sum(ONON_After_V3_V5<0)
ONON_After_V3_V5[ONON_After_V3_V5<0] <- 0
sum(ONON_After_V3_V5<0)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% mutate(VISIT=ifelse(grepl("V3", VISIT), 3,5)) 

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(Var %in% c("ON_MSGCHE_RIG","ON_MIGCHE_RIG","ON_MSGCHE_DOIGT", "ON_MSGCHE_MAINS", 
                    "ON_MSGCHE_MA","ON_MIGCHE_PIED", "ON_MIGCHE_JAMBE", "ON_TREMBLPOST_MSGCHE",
                    "ON_TREMBLMAIN_MSGCHE","ON_MSGCHE_AMPLI_TREMBL","ON_MIGCHE_AMPLI_TREMBL")) %>% group_by(SUBJID, VISIT) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(Var %in% c("ON_MSDROIT_RIG","ON_MIDROIT_RIG","ON_MS_DROIT_DOIGT","ON_MSDROIT_MAINS" ,
                        "ON_MSDROIT_MA","ON_MIDROIT_PIED","ON_MIDROIT_JAMBE", "ON_TREMBLPOST_MSDROIT",
                        "ON_TREMBLMAIN_MSDROIT","ON_MSDROIT_AMPLI_TREMBL","ON_MIDROIT_AMPLI_TREMBL" )) %>% group_by(SUBJID, VISIT) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V3_V5$Diff <- ONON_After_V3_V5$Right - ONON_After_V3_V5$Left
mean(abs(ONON_After_V3_V5$Diff))

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% mutate(VISIT=1) %>% select(SUBJID, VISIT, Left, Right, Diff)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID) %>% distinct() %>% inner_join(ONON_After_V0_V1) %>%
  bind_rows(ONON_After_V3_V5)

unique(ONON_After_V3_V5$VISIT)

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=mean(Left), Right=mean(Right), Diff=mean(abs(Diff)))

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=sd(Left), Right=sd(Right), Diff=sd(Diff))

ONON_After_V3_V5 %>% arrange(SUBJID)


ONON_After_V3_V5 <- ONON_After_V3_V5 %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>%
  select(SUBJID) %>% left_join(ONON_After_V3_V5) %>% ungroup() #415



# LEFT ****************************************************

ONON_After_V3_V5 %>% group_by(VISIT) %>% summarise(mean=mean(Left), 
                                                   sd=sd(Left), 
                                                   median=median(Left),
                                                   Q0.25=quantile(Left, 0.25),
                                                   Q0.75=quantile(Left, 0.75)) %>% distinct()

# VISIT  mean    sd median Q0.25 Q0.75
#   1     1  3.87  3.23   3     1        6
# 2     3  9.28  7.65   6.95  5.16    12
# 3     5 10.4   7.92   6.95  6.95    12

friedman.test(y=ONON_After_V3_V5$Left, 
              groups=ONON_After_V3_V5$VISIT, 
              blocks=ONON_After_V3_V5$SUBJID)


# Friedman rank sum test
# 
# data:  ONON_After_V3_V5$Left, ONON_After_V3_V5$VISIT and ONON_After_V3_V5$SUBJID
# Friedman chi-squared = 332.93, df = 2, p-value < 2.2e-16

pairwise.wilcox.test(ONON_After_V3_V5$Left, ONON_After_V3_V5$VISIT, p.adj = "bonferroni", paired=T)
 
# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  ONON_After_V3_V5$Left and ONON_After_V3_V5$VISIT 
# 
# 1       3      
# 3 < 2e-16 -      
#   5 < 2e-16 0.00027
# 
# P value adjustment method: bonferroni 


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Diff) %>%
  filter(Var=="Left") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(Value, colour=VISIT, fill=VISIT)) +
  geom_density(alpha=0.4) +
  xlab("\n Left MDS UPDRS III") +
  ylab("Patient density \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0"))  +
  xlim(0,40)

ggsave(file="left.svg", plot=image, width=4, height=4)



image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Diff) %>%
  filter(Var=="Left") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Value), colour=VISIT, fill=VISIT)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.8, height=0.5, shape=1) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Left MDS UPDRS III \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  ylim(0,40)

ggsave(file="left.svg", plot=image, width=6, height=6)



df <- ONON_After_V3_V5

mean_stats <- aggregate(Left ~ VISIT, data = df, function(x) mean(x))
sd_stats <- aggregate(Left ~  VISIT, data = df, function(x) sd(x)/sqrt(415))
summary_stats <- merge(mean_stats, sd_stats, by = "VISIT", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                                       ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)
df <- df %>%  mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                  ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)

names(summary_stats)

image <- ggplot() +
  geom_jitter(data = df, 
              aes(x = VISIT, 
                  y = Left, color = VISIT),  show.legend = FALSE,  alpha=0.9, shape=1, size=2, height = 0.5, width=0.3) +
  geom_bar(data = summary_stats, aes(x = VISIT, y = Left_mean, fill=VISIT, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.7 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = VISIT, colour=VISIT, ymin = Left_mean  - Left_sd, ymax = Left_mean  + Left_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlab("\n Yearly Visit (1-3)") + ylab("Left MDS UPDRS III  \n")

ggsave(file="left.svg", plot=image, width=6, height=6)









# RIGHT ****************************************************


ONON_After_V3_V5 %>% group_by(VISIT) %>% summarise(mean=mean(Right), 
                                                   sd=sd(Right), 
                                                   median=median(Right),
                                                   Q0.25=quantile(Right, 0.25),
                                                   Q0.75=quantile(Right, 0.75)) %>% distinct()

# VISIT  mean    sd median Q0.25 Q0.75
#   1     1  2.79  2.79   2     1        4
# 2     3  7.15  6.39   5.42  4        8
# 3     5  8.22  7.73   5.42  5.42    10

friedman.test(y=ONON_After_V3_V5$Right, 
              groups=ONON_After_V3_V5$VISIT, 
              blocks=ONON_After_V3_V5$SUBJID)


# Friedman rank sum test
# 
# data:  ONON_After_V3_V5$Right, ONON_After_V3_V5$VISIT and ONON_After_V3_V5$SUBJID
# Friedman chi-squared = 329.93, df = 2, p-value < 2.2e-16

pairwise.wilcox.test(ONON_After_V3_V5$Right, ONON_After_V3_V5$VISIT, p.adj = "bonferroni", paired=T)

# 1      3     
# 3 <2e-16 -     
#   5 <2e-16 0.0019
# 
# P value adjustment method: bonferroni 


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Diff) %>%
  filter(Var=="Right") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(Value, colour=VISIT, fill=VISIT)) +
  geom_density(alpha=0.4) +
  xlab("\n Right MDS UPDRS III") +
  ylab("Patient density \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlim(0,40)

ggsave(file="right.svg", plot=image, width=4, height=4)


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Diff) %>%
  filter(Var=="Right") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Value), colour=VISIT, fill=VISIT)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.8, height=0.5, shape=1) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Right MDS UPDRS III \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  ylim(0,40)

ggsave(file="right.svg", plot=image, width=6, height=6)


df <- ONON_After_V3_V5

mean_stats <- aggregate(Right ~ VISIT, data = df, function(x) mean(x))
sd_stats <- aggregate(Right ~  VISIT, data = df, function(x) sd(x)/sqrt(415))
summary_stats <- merge(mean_stats, sd_stats, by = "VISIT", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                                       ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)
df <- df %>%  mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                  ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)

names(summary_stats)

image <- ggplot() +
  geom_jitter(data = df, 
              aes(x = VISIT, 
                  y = Right, color = VISIT),  show.legend = FALSE,  alpha=0.9, shape=1, size=2, height = 0.5, width=0.3) +
  geom_bar(data = summary_stats, aes(x = VISIT, y = Right_mean, fill=VISIT, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.7 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = VISIT, colour=VISIT, ymin = Right_mean  - Right_sd, ymax = Right_mean  + Right_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,40) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlab("\n Yearly Visit (1-3)") + ylab("Right MDS UPDRS III  \n")

ggsave(file="Right.svg", plot=image, width=6, height=6)











# ASYMMETRY ****************************************************

ONON_After_V3_V5$Diff <- abs(ONON_After_V3_V5$Diff)

ONON_After_V3_V5 %>% group_by(VISIT) %>% summarise(mean=mean(Diff), 
                                                   sd=sd(Diff), 
                                                   median=median(Diff),
                                                   Q0.25=quantile(Diff, 0.25),
                                                   Q0.75=quantile(Diff, 0.75)) %>% distinct()

# VISIT  mean    sd median Q0.25 Q0.75
# 1     1  2.17  1.92      2  1        3
# 2     3  4.11  4.56      2  1.52     6
# 3     5  4.15  4.99      2  1.52     6

friedman.test(y=ONON_After_V3_V5$Diff, 
              groups=ONON_After_V3_V5$VISIT, 
              blocks=ONON_After_V3_V5$SUBJID)


# Friedman rank sum test
# 
# data:  ONON_After_V3_V5$Diff, ONON_After_V3_V5$VISIT and ONON_After_V3_V5$SUBJID
# Friedman chi-squared = 70.695, df = 2, p-value = 4.454e-16


pairwise.wilcox.test(ONON_After_V3_V5$Diff, ONON_After_V3_V5$VISIT, p.adj = "bonferroni", paired=T)

# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  ONON_After_V3_V5$Diff and ONON_After_V3_V5$VISIT 
# 
# 1       3
# 3 < 2e-16 -
#   5 1.4e-14 1
# 
# P value adjustment method: bonferroni 


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Diff) %>%
  filter(Var=="Diff") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(abs(Value), colour=VISIT, fill=VISIT)) +
  geom_density(alpha=0.4) +
  xlab("\n Right-to-Left Asymmetry MDS UPDRS III") +
  ylab("Patient density \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlim(0,25)

ggsave(file="diff.svg", plot=image, width=4, height=4)


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Diff) %>%
  filter(Var=="Diff") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Value), colour=VISIT, fill=VISIT)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.8, height=0.5, shape=1) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Right-to-Left Asymmetry \n MDS UPDRS III \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  ylim(0,25)

ggsave(file="diff.svg", plot=image, width=6, height=6)


df <- ONON_After_V3_V5
df$Diff <- abs(df$Diff)
mean_stats <- aggregate(Diff ~ VISIT, data = df, function(x) mean(x))
sd_stats <- aggregate(Diff ~  VISIT, data = df, function(x) sd(x)/sqrt(415))
summary_stats <- merge(mean_stats, sd_stats, by = "VISIT", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                                       ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)
df <- df %>%  mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                  ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)

names(summary_stats)

image <- ggplot() +
  geom_jitter(data = df, 
              aes(x = VISIT, 
                  y = Diff, color = VISIT),  show.legend = FALSE,  alpha=0.9, shape=1, size=2, height = 0.5, width=0.3) +
  geom_bar(data = summary_stats, aes(x = VISIT, y = Diff_mean, fill=VISIT, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.7 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = VISIT, colour=VISIT, ymin = Diff_mean  - Diff_sd, ymax = Diff_mean  + Diff_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  ylim(0,25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlab("\n Yearly Visit (1-3)") + ylab("Right-to-Left Asymmetry \n MDS UPDRS III \n  \n")

ggsave(file="Diff.svg", plot=image, width=6, height=6)







# NORMALIWZED ASYMMETRY ****************************************************

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% mutate(TOT=Left+Right)
ONON_After_V3_V5 <- ONON_After_V3_V5 %>% mutate(Norm_Diff=abs(Diff)/TOT)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% filter(!is.na(Norm_Diff))


ONON_After_V3_V5 <- ONON_After_V3_V5 %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>%
  select(SUBJID) %>% left_join(ONON_After_V3_V5) %>% ungroup()

ONON_After_V3_V5 %>% group_by(VISIT) %>% summarise(mean=mean(Norm_Diff, na.rm=T), 
                                                   sd=sd(Norm_Diff, na.rm=T), 
                                                   median=median(Norm_Diff, na.rm=T),
                                                   Q0.25=quantile(Norm_Diff, 0.25, na.rm=T),
                                                   Q0.75=quantile(Norm_Diff, 0.75, na.rm=T)) %>% distinct()

# VISIT  mean    sd median Q0.25 Q0.75
# 1     1 0.456 0.359  0.333 0.156 0.75 
# 2     3 0.304 0.281  0.179 0.123 0.424
# 3     5 0.263 0.261  0.123 0.123 0.333

friedman.test(y=ONON_After_V3_V5$Norm_Diff, 
              groups=ONON_After_V3_V5$VISIT, 
              blocks=ONON_After_V3_V5$SUBJID)


# 
# Friedman rank sum test
# 
# data:  ONON_After_V3_V5$Norm_Diff, ONON_After_V3_V5$VISIT and ONON_After_V3_V5$SUBJID
# Friedman chi-squared = 56.058, df = 2, p-value = 6.716e-13

pairwise.wilcox.test(ONON_After_V3_V5$Norm_Diff, ONON_After_V3_V5$VISIT, p.adj = "bonferroni", paired=T)

# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  ONON_After_V3_V5$Diff and ONON_After_V3_V5$VISIT 
# 
# 1       3   
# 3 2.9e-12 -   
#   5 2.6e-15 0.08
# 
# P value adjustment method: bonferroni 


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Norm_Diff) %>%
  filter(Var=="Norm_Diff") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(abs(Value), colour=VISIT, fill=VISIT)) +
  geom_density(alpha=0.4) +
  xlab("\n Normalized \n Right-to-Left Asymmetry MDS UPDRS III") +
  ylab("Patient density \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0"))

ggsave(file="diff.svg", plot=image, width=4, height=4)


image <- ONON_After_V3_V5 %>% gather(Var, Value, Left:Norm_Diff) %>%
  filter(Var=="Norm_Diff") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Value), colour=VISIT, fill=VISIT)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.8, height=0.01, shape=1) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Normalized Right-to-Left Asymmetry \n MDS UPDRS III \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0"))


ggsave(file="diff.svg", plot=image, width=6, height=6)


df <- ONON_After_V3_V5
df$Diff <- abs(df$Norm_Diff)
mean_stats <- aggregate(Norm_Diff ~ VISIT, data = df, function(x) mean(x))
sd_stats <- aggregate(Norm_Diff ~  VISIT, data = df, function(x) sd(x)/sqrt(415))
summary_stats <- merge(mean_stats, sd_stats, by = "VISIT", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                                       ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)
df <- df %>%  mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                  ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)

names(summary_stats)

image <- ggplot() +
  geom_jitter(data = df, 
              aes(x = VISIT, 
                  y = Norm_Diff, color = VISIT),  show.legend = FALSE,  alpha=0.9, shape=1, size=2, height = 0.01, width=0.3) +
  geom_bar(data = summary_stats, aes(x = VISIT, y = Norm_Diff_mean, fill=VISIT, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.7 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = VISIT, colour=VISIT, ymin = Norm_Diff_mean   - Norm_Diff_sd, ymax = Norm_Diff_mean   + Norm_Diff_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlab("\n Yearly Visit (1-3)") + ylab("Normalized \n Right-to-Left Asymmetry \n MDS UPDRS III \n  \n")

ggsave(file="Diff.svg", plot=image, width=6, height=6)

# ----------------------
# Axial Score --------------------------

# Axial ********************************************

UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V0_V1)
Axials_V1 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ON_3.9_6, ON_3.10_6, ON_3.11_6, ON_3.12_6)
Axials_V1 <- Axials_V1[-1,]
Axials_V1$VISIT <- 1
Axials_V1 <- Axials_V1 %>% select(SUBJID, VISIT, ON_3.9_6, ON_3.10_6, ON_3.11_6, ON_3.12_6)


UPDRSIII_COMPLET_V3_V5 <- read_xlsx(path="Raw_Database/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
names(UPDRSIII_COMPLET_V3_V5)
Axials_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_LEVER, ON_MARCHE, ON_FREEZING, ON_STAB_POST)
Axials_V3_V5 <- Axials_V3_V5[-1,]
Axials_V3_V5 <- Axials_V3_V5 %>% mutate(VISIT=ifelse(grepl("V3", VISIT), 3,5)) 
names(Axials_V3_V5) <- c("SUBJID", "VISIT", "ON_3.9_6", "ON_3.10_6", "ON_3.11_6", "ON_3.12_6")
Axials <- Axials_V1 %>% bind_rows(Axials_V3_V5)

Axials$ON_3.9_6 <- as.numeric(Axials$ON_3.9_6) 
Axials$ON_3.10_6  <- as.numeric(Axials$ON_3.10_6 ) 
Axials$ON_3.11_6  <- as.numeric(Axials$ON_3.11_6 ) 
Axials$ON_3.12_6 <- as.numeric(Axials$ON_3.12_6) 

Axials <- Axials %>% drop_na() %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>% select(SUBJID) %>% 
  left_join(Axials) %>% ungroup()






# Asymmetry ********************************************



UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V0_V1 <- names(UPDRSIII_COMPLET_V0_V1)
ONON_After_V0_V1 <- data.frame(df_names_V0_V1) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names_V0_V1)) %>%
  filter(grepl("3.3", df_names_V0_V1)|
           grepl("3.4", df_names_V0_V1)|
           grepl("3.5", df_names_V0_V1)|
           grepl("3.6", df_names_V0_V1)|
           grepl("3.7", df_names_V0_V1)|
           grepl("3.8", df_names_V0_V1)|
           grepl("3.15", df_names_V0_V1)|
           grepl("3.16", df_names_V0_V1)|
           grepl("3.17", df_names_V0_V1)
  ) %>%
  filter(grepl("Right", df_names_V0_V1)|grepl("right", df_names_V0_V1)|grepl("left", df_names_V0_V1)|grepl("Left", df_names_V0_V1)) %>%
  arrange(df_names_V0_V1)  %>%
  filter(!grepl("OFF", df_names_V0_V1)) 
toString(as.list(ONON_After_V0_V1))
match_V0_V1 <- c("ON_3.15_Left6", "ON_3.15_Right_6", "ON_3.16_Left6", "ON_3.16_Right6", 
                 "ON_3.17_Inf_Left_6", "ON_3.17_Inf_Right6", "ON_3.17_Sup_Left_6", 
                 "ON_3.17_Sup_Right6", "ON_3.3_Inf_Left", "ON_3.3_Inf_Right", "ON_3.3_S_Left", 
                 "ON_3.3_S_Right", "ON_3.4_Left_", "ON_3.4_Right_", "ON_3.5_Left_", "ON_3.5_Right_", 
                 "ON_3.6_Left_", "ON_3.6_Right_", "ON_3.7_Left", "ON_3.7_Right_", "ON_3.8_Left6", "ON_3.8_Right_6")
match <- append("SUBJID", match_V0_V1)
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)
ONON_After_V0_V1 <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After_V0_V1 <- ONON_After_V0_V1[-1,]


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1 %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
                                 group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V0_V1)

ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% mutate_each(as.numeric, ON_3.3_S_Right:ON_3.17_Inf_Left_6)
sum(is.na(ONON_After_V0_V1))
dim(ONON_After_V0_V1) 


for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V0_V1[,-1],ncp=2, scale = T)

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V0_V1))
sum(ONON_After_V0_V1<0)
ONON_After_V0_V1[ONON_After_V0_V1<0] <- 0
sum(ONON_After_V0_V1<0)


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V0_V1$Diff <- ONON_After_V0_V1$Right - ONON_After_V0_V1$Left
mean(ONON_After_V0_V1$Diff)



UPDRSIII_COMPLET_V3_V5 <- read_xlsx(path="Raw_Database/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V3_V5 <- names(UPDRSIII_COMPLET_V3_V5)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MSDROIT_RIG, ON_MSGCHE_RIG, ON_MIDROIT_RIG, ON_MIGCHE_RIG,
                                                            ON_MS_DROIT_DOIGT, ON_MSGCHE_DOIGT, ON_MSDROIT_MAINS, ON_MSGCHE_MAINS, ON_MSDROIT_MA, ON_MSGCHE_MA,
                                                            ON_MIDROIT_PIED, ON_MIGCHE_PIED, ON_MIDROIT_JAMBE, ON_MIGCHE_JAMBE, 
                                                            ON_TREMBLPOST_MSDROIT, ON_TREMBLPOST_MSGCHE, ON_TREMBLMAIN_MSGCHE, ON_TREMBLMAIN_MSDROIT,
                                                            ON_MSDROIT_AMPLI_TREMBL, ON_MSGCHE_AMPLI_TREMBL, ON_MIDROIT_AMPLI_TREMBL, ON_MIGCHE_AMPLI_TREMBL)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

ONON_After_V3_V5 <- UPDRSIII_COMPLET_V3_V5

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5 %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
                                 group_by(SUBJID, VISIT) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V3_V5)

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% mutate_each(as.numeric, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL)
sum(is.na(ONON_After_V3_V5))
dim(ONON_After_V3_V5) 


for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V3_V5[,-c(1,2)],ncp=2, scale = T)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V3_V5))
sum(ONON_After_V3_V5<0)
ONON_After_V3_V5[ONON_After_V3_V5<0] <- 0
sum(ONON_After_V3_V5<0)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% mutate(VISIT=ifelse(grepl("V3", VISIT), 3,5)) 

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(Var %in% c("ON_MSGCHE_RIG","ON_MIGCHE_RIG","ON_MSGCHE_DOIGT", "ON_MSGCHE_MAINS", 
                    "ON_MSGCHE_MA","ON_MIGCHE_PIED", "ON_MIGCHE_JAMBE", "ON_TREMBLPOST_MSGCHE",
                    "ON_TREMBLMAIN_MSGCHE","ON_MSGCHE_AMPLI_TREMBL","ON_MIGCHE_AMPLI_TREMBL")) %>% group_by(SUBJID, VISIT) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(Var %in% c("ON_MSDROIT_RIG","ON_MIDROIT_RIG","ON_MS_DROIT_DOIGT","ON_MSDROIT_MAINS" ,
                        "ON_MSDROIT_MA","ON_MIDROIT_PIED","ON_MIDROIT_JAMBE", "ON_TREMBLPOST_MSDROIT",
                        "ON_TREMBLMAIN_MSDROIT","ON_MSDROIT_AMPLI_TREMBL","ON_MIDROIT_AMPLI_TREMBL" )) %>% group_by(SUBJID, VISIT) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V3_V5$Diff <- ONON_After_V3_V5$Right - ONON_After_V3_V5$Left
mean(abs(ONON_After_V3_V5$Diff))

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% mutate(VISIT=1) %>% select(SUBJID, VISIT, Left, Right, Diff)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID) %>% distinct() %>% inner_join(ONON_After_V0_V1) %>%
  bind_rows(ONON_After_V3_V5)

unique(ONON_After_V3_V5$VISIT)

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=mean(Left), Right=mean(Right), Diff=mean(abs(Diff)))

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=sd(Left), Right=sd(Right), Diff=sd(Diff))

ONON_After_V3_V5 %>% arrange(SUBJID)

length(unique(ONON_After_V3_V5$SUBJID))

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>%
  select(SUBJID) %>% left_join(ONON_After_V3_V5) %>% ungroup() #415



Axials <- ONON_After_V3_V5 %>% inner_join(Axials)

Axials <- Axials %>% mutate(Axial_Score=ON_3.9_6+ON_3.10_6+ON_3.11_6+ON_3.12_6)

Axials %>% group_by(VISIT) %>% summarise(n=mean(Axial_Score))



Axials <- Axials %>% mutate(Normalized=Diff/(Right+Left)) 
Axials <- Axials %>% mutate(Total=(Right+Left)) 


cor.test(abs(Axials$Diff[Axials$VISIT==1]) , Axials$Axial_Score[Axials$VISIT==1], method = c("spearman"))

# Spearman's rank correlation rho

# data:  abs(Axials$Diff[Axials$VISIT == 1]) and Axials$Axial_Score[Axials$VISIT == 1]
# S = 595505, p-value = 0.3399
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.07667351 

cor.test(abs(Axials$Diff[Axials$VISIT==3]) , Axials$Axial_Score[Axials$VISIT==3], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(Axials$Diff[Axials$VISIT == 3]) and Axials$Axial_Score[Axials$VISIT == 3]
# S = 533146, p-value = 0.02991
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1733599 

cor.test(abs(Axials$Diff[Axials$VISIT==5]) , Axials$Axial_Score[Axials$VISIT==5], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(Axials$Diff[Axials$VISIT == 5]) and Axials$Axial_Score[Axials$VISIT == 5]
# S = 590451, p-value = 0.2927
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.08450923 




summary(lm(Axial_Score ~ abs(Diff)  +  Total , data = Axials))

# Call:
#   lm(formula = Axial_Score ~ abs(Diff) + Total, data = Axials)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.3234 -1.2851 -0.7438  0.9412 10.7297 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.743760   0.157093   4.735 2.91e-06 ***
#   abs(Diff)   -0.075439   0.021110  -3.574 0.000389 ***
#   Total        0.093190   0.007115  13.097  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.227 on 468 degrees of freedom
# Multiple R-squared:  0.2884,	Adjusted R-squared:  0.2853 
# F-statistic: 94.82 on 2 and 468 DF,  p-value: < 2.2e-16


cor.test(abs(Axials$Normalized[Axials$VISIT==1]) , Axials$Axial_Score[Axials$VISIT==1], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(Axials$Normalized[Axials$VISIT == 1]) and Axials$Axial_Score[Axials$VISIT == 1]
# S = 574223, p-value = 0.03316
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1782706 

cor.test(abs(Axials$Normalized[Axials$VISIT==3]) , Axials$Axial_Score[Axials$VISIT==3], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(Axials$Normalized[Axials$VISIT == 5]) and Axials$Axial_Score[Axials$VISIT == 5]
# S = 708902, p-value = 0.02021
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1876309 

cor.test(abs(Axials$Normalized[Axials$VISIT==5]) , Axials$Axial_Score[Axials$VISIT==5], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(Axials$Normalized[Axials$VISIT == 5]) and Axials$Axial_Score[Axials$VISIT == 5]
# S = 708902, p-value = 0.02021
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1876309 



cor.test(abs(Axials$Diff) , Axials$Axial_Score, method = c("spearman"))


# Spearman's rank correlation rho
# 
# data:  abs(Axials$Diff) and Axials$Axial_Score
# S = 13285918, p-value = 1.931e-07
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2370747 


image <- Axials %>% filter(abs(Diff)<25) %>%
  ggplot(aes(abs(Diff), Axial_Score)) +
  geom_jitter(width=0.5, height=0.5, alpha=0.5, size=2, shape=1) +
  geom_smooth(method="lm", colour="#2841b0", fill="#2841b0") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Absolute \n Right-to-left Asymmetry") +
  ylab("Axial Score \n") +
  coord_cartesian(ylim=c(0,5)) +
  facet_wrap(~VISIT)

ggsave(file="axial_diff_years.svg", plot=image, width=12, height=6)




image <- Axials %>% drop_na() %>% filter(abs(Diff)<25) %>%
  ggplot(aes(abs(Normalized), Axial_Score)) +
  geom_jitter(width=0.01, height=0.5, alpha=0.5, size=2, shape=1) +
  geom_smooth(method="lm", colour="#2841b0", fill="#2841b0") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Normalized Absolute \n Right-to-left Asymmetry") +
  ylab("Axial Score \n") +
  coord_cartesian(ylim=c(0,5))   +
  facet_wrap(~VISIT)


ggsave(file="axial_norm_years.svg", plot=image, width=12, height=6)


library(car)
model <- lm(Axial_Score ~ abs(Diff) + Total, data = Axials)
plot <- crPlots(model)


Axials$Diff <- abs(Axials$Diff)

library(mediation)
med_model <- lm(Total ~ Diff, data = Axials)
outcome_model <- lm(Axial_Score ~ Diff + Total, data = Axials)
med_results <- mediation::mediate(med_model, outcome_model, treat = "Diff", mediator = "Total", boot = TRUE)
summary(med_results)

# Causal Mediation Analysis 
# 
# Nonparametric Bootstrap Confidence Intervals with the Percentile Method
# 
# Estimate 95% CI Lower 95% CI Upper p-value    
#   ACME             0.1507       0.1144         0.19  <2e-16 ***
#   ADE             -0.0754      -0.1243        -0.02    0.01 ** 
#   Total Effect     0.0753       0.0424         0.12  <2e-16 ***
#   Prop. Mediated   2.0022       1.1854         3.69  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Sample Size Used: 471 
# 
# 
# Simulations: 1000 

plot(med_results)



# Axials ****************************************************

Axials$Diff <- abs(Axials$Diff)

Axials %>% group_by(VISIT) %>% summarise(mean=mean(Axial_Score), 
                                         sd=sd(Axial_Score), 
                                         median=median(Axial_Score),
                                         Q0.25=quantile(Axial_Score, 0.25),
                                         Q0.75=quantile(Axial_Score, 0.75)) %>% distinct()

# VISIT  mean    sd median Q0.25 Q0.75
#   1     1 0.955  1.39      0     0     1
# 2     3 2.06   2.19      1     0     3
# 3     5 3.33   3.37      2     1     5

friedman.test(y=Axials$Axial_Score, 
              groups=Axials$VISIT, 
              blocks=Axials$SUBJID)

# 
# Friedman rank sum test
# 
# data:  Axials$Axial_Score, Axials$VISIT and Axials$SUBJID
# Friedman chi-squared = 111.7, df = 2, p-value < 2.2e-16

pairwise.wilcox.test(Axials$Axial_Score, Axials$VISIT, p.adj = "bonferroni", paired=T)


# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  Axials$Axial_Score and Axials$VISIT 
# 
# 1       3      
# 3 2.8e-10 -      
#   5 < 2e-16 4.4e-08
# 
# P value adjustment method: bonferroni 


image <- Axials %>% gather(Var, Value, Left:Axial_Score ) %>%
  filter(Var=="Axial_Score") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(Value, colour=VISIT, fill=VISIT)) +
  geom_density(alpha=0.4, adjust=2) +
  xlab("\n Axial Score") +
  ylab("Patient density \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) 


ggsave(file="axial.svg", plot=image, width=4, height=4)



image <- Axials %>% gather(Var, Value, Left:Axial_Score) %>%
  filter(Var=="Axial_Score") %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Value), colour=VISIT, fill=VISIT)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.8, height=0.5, shape=1) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Axial Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) 

ggsave(file="axial.svg", plot=image, width=6, height=6)


length(unique(Axials$SUBJID))
df <- Axials

mean_stats <- aggregate(Axial_Score ~ VISIT, data = df, function(x) mean(x))
sd_stats <- aggregate(Axial_Score ~  VISIT, data = df, function(x) sd(x)/sqrt(157))
summary_stats <- merge(mean_stats, sd_stats, by = "VISIT", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                                       ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)
df <- df %>%  mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                  ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)

names(summary_stats)

image <- ggplot() +
  geom_jitter(data = df, 
              aes(x = VISIT, 
                  y = Axial_Score, color = VISIT),  show.legend = FALSE,  alpha=0.9, shape=1, size=2, height = 0.5, width=0.3) +
  geom_bar(data = summary_stats, aes(x = VISIT, y = Axial_Score_mean, fill=VISIT, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.7 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = VISIT, colour=VISIT, ymin = Axial_Score_mean  - Axial_Score_sd, ymax = Axial_Score_mean  + Axial_Score_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlab("\n Yearly Visit (1-3)") + ylab("Axial Score  \n")

ggsave(file="axial.svg", plot=image, width=6, height=6)




Axials <- Axials %>% mutate(Gait=ifelse(ON_3.10_6>=2,1,0)) %>% mutate(Gait=as.factor(Gait))
Axials <- Axials %>% mutate(Freezing=ifelse(ON_3.11_6>=1,1,0)) %>% mutate(Freezing=as.factor(Freezing))


Axials %>% group_by(Gait) %>% summarise(mean=mean(Normalized, na.rm=T))


image <- Axials %>% select(VISIT, Gait, Diff) %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Diff), colour=Gait, fill=Gait)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.9, height=0.5, width=0.2, shape=1) +
  ylim(0,25) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Absolute \n Right-to-left Asymmetry \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc")) 

ggsave(file="gait.svg", plot=image, width=5, height=5)




wilcox.test(Diff ~ Gait, data = Axials[Axials$VISIT==1,]) 
wilcox.test(Diff ~ Gait, data = Axials[Axials$VISIT==3,]) 
wilcox.test(Diff ~ Gait, data = Axials[Axials$VISIT==5,]) 



image <- Axials %>% select(VISIT, Gait, Normalized) %>% drop_na() %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Normalized), colour=Gait, fill=Gait)) +
  geom_boxplot(alpha=0.4, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.9, height=0.1, width=0.2, shape=1) +
  # ylim(0,25) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Normalized Absolute \n Right-to-left Asymmetry \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc")) 

ggsave(file="normgait.svg", plot=image, width=5, height=5)


wilcox.test(Normalized ~ Gait, data = drop_na(Axials[Axials$VISIT==1,])) 
wilcox.test(Normalized ~ Gait, data = drop_na(Axials[Axials$VISIT==3,])) 
wilcox.test(Normalized ~ Gait, data = drop_na(Axials[Axials$VISIT==5,])) 











image <- Axials %>% select(VISIT, Freezing, Diff) %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Diff), colour=Freezing, fill=Freezing)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.9, height=0.5, width=0.2, shape=1) +
  ylim(0,25) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Absolute \n Right-to-left Asymmetry \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc")) 

ggsave(file="freeze.svg", plot=image, width=5, height=5)



wilcox.test(Diff ~ Freezing, data = Axials[Axials$VISIT==1,]) 
wilcox.test(Diff ~ Freezing, data = Axials[Axials$VISIT==3,]) 
wilcox.test(Diff ~ Freezing, data = Axials[Axials$VISIT==5,]) 



image <- Axials %>% select(VISIT, Freezing, Normalized) %>% drop_na() %>% ungroup() %>%
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(Normalized), colour=Freezing, fill=Freezing)) +
  geom_boxplot(alpha=0.4, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.9, height=0.1, width=0.2, shape=1) +
  # ylim(0,25) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("Normalized Absolute \n Right-to-left Asymmetry \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc")) 

ggsave(file="normfreeze.svg", plot=image, width=5, height=5)


wilcox.test(Normalized ~ Freezing, data = drop_na(Axials[Axials$VISIT==1,])) 
wilcox.test(Normalized ~ Freezing, data = drop_na(Axials[Axials$VISIT==3,])) 
wilcox.test(Normalized ~ Freezing, data = drop_na(Axials[Axials$VISIT==5,])) 



# --------------------
# PDQ39 ------------

PDQ39_CGIS_SCOPA <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "PDQ39-CGIS-SCOPA", skip=0, col_types = "text", trim_ws = TRUE)
PDQ39_CGIS_SCOPA <- PDQ39_CGIS_SCOPA %>% select(-c("...48", "CGIS" , "CGIS_NR", "SEVER_MALADIE" , "...52" ))
PDQ39_CGIS_SCOPA <- PDQ39_CGIS_SCOPA %>% select(-c("NUM_CENTRE", "NUM_PAT", "INIT_PAT", "SITUATION_MARITALE", "REF_SITU_FAM"))

PDQ39 <- PDQ39_CGIS_SCOPA[,c(1,2, 3:42)]
PDQ39 <- PDQ39 %>% group_by(SUBJID) %>% mutate(VISIT=row_number()-1) %>% ungroup()
PDQ39 <- PDQ39 %>% filter(VISIT==1)

PDQ39 <- PDQ39 %>% gather(item, value, PDQ39_1:PDQ39_SCORE) %>%
  mutate(value=ifelse(value=="Rarement", 1,
                      ifelse(value=="Parfois", 2,
                             ifelse(value=="Toujours ou ne peut jamais faire", 4,
                                    ifelse(value=="Souvent", 3,
                                           ifelse(value=="Jamais", 0, value))))))



PDQ39 <- PDQ39 %>% filter(item=="PDQ39_SCORE") 
PDQ39 <- PDQ39 %>% filter(value!=0)
PDQ39 <- PDQ39 %>% select(-item) %>% rename("PDQ39_SCORE"="value")


PDQ39_CGIS_SCOPA_V3_V5 <- read_xlsx(path="Raw_Database/Raquel_Margherita_Juil 24.xlsx",sheet = "PDQ39-CGIS-SCOPA _V3_V5", skip=0, col_types = "text", trim_ws = TRUE)
names(PDQ39_CGIS_SCOPA_V3_V5)
PDQ39_CGIS_SCOPA_V3_V5 <- PDQ39_CGIS_SCOPA_V3_V5 %>% select(SUBJID:PDQ39_SCORE)
PDQ39_CGIS_SCOPA_V3_V5 <- PDQ39_CGIS_SCOPA_V3_V5 %>% select(-c(SITUATION_MARITALE, REF_SITU_FAM,INIT_PAT))
unique(PDQ39_CGIS_SCOPA_V3_V5$VISIT)

PDQ39_CGIS_SCOPA_V3_V5 <- PDQ39_CGIS_SCOPA_V3_V5 %>% filter(VISIT!="Visit") %>%
  mutate(VISIT=ifelse(VISIT=="Visite Bilan à 3 ans - V3",3,5))

PDQ39_CGIS_SCOPA_V3_V5 <- PDQ39_CGIS_SCOPA_V3_V5 %>% select(SUBJID, VISIT,PDQ39_SCORE)
PDQ39_CGIS_SCOPA_V3_V5 <- PDQ39_CGIS_SCOPA_V3_V5 %>% drop_na()
PDQ39_CGIS_SCOPA_V3_V5 <- PDQ39_CGIS_SCOPA_V3_V5 %>% filter(PDQ39_SCORE!=0)


PDQ39 <- PDQ39 %>% bind_rows(PDQ39_CGIS_SCOPA_V3_V5) 















# Asymmetry ********************************************



UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V0_V1 <- names(UPDRSIII_COMPLET_V0_V1)
ONON_After_V0_V1 <- data.frame(df_names_V0_V1) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names_V0_V1)) %>%
  filter(grepl("3.3", df_names_V0_V1)|
           grepl("3.4", df_names_V0_V1)|
           grepl("3.5", df_names_V0_V1)|
           grepl("3.6", df_names_V0_V1)|
           grepl("3.7", df_names_V0_V1)|
           grepl("3.8", df_names_V0_V1)|
           grepl("3.15", df_names_V0_V1)|
           grepl("3.16", df_names_V0_V1)|
           grepl("3.17", df_names_V0_V1)
  ) %>%
  filter(grepl("Right", df_names_V0_V1)|grepl("right", df_names_V0_V1)|grepl("left", df_names_V0_V1)|grepl("Left", df_names_V0_V1)) %>%
  arrange(df_names_V0_V1)  %>%
  filter(!grepl("OFF", df_names_V0_V1)) 
toString(as.list(ONON_After_V0_V1))
match_V0_V1 <- c("ON_3.15_Left6", "ON_3.15_Right_6", "ON_3.16_Left6", "ON_3.16_Right6", 
                 "ON_3.17_Inf_Left_6", "ON_3.17_Inf_Right6", "ON_3.17_Sup_Left_6", 
                 "ON_3.17_Sup_Right6", "ON_3.3_Inf_Left", "ON_3.3_Inf_Right", "ON_3.3_S_Left", 
                 "ON_3.3_S_Right", "ON_3.4_Left_", "ON_3.4_Right_", "ON_3.5_Left_", "ON_3.5_Right_", 
                 "ON_3.6_Left_", "ON_3.6_Right_", "ON_3.7_Left", "ON_3.7_Right_", "ON_3.8_Left6", "ON_3.8_Right_6")
match <- append("SUBJID", match_V0_V1)
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)
ONON_After_V0_V1 <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After_V0_V1 <- ONON_After_V0_V1[-1,]


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1 %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
                                 group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V0_V1)

ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% mutate_each(as.numeric, ON_3.3_S_Right:ON_3.17_Inf_Left_6)
sum(is.na(ONON_After_V0_V1))
dim(ONON_After_V0_V1) 


for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V0_V1[,-1],ncp=2, scale = T)

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V0_V1))
sum(ONON_After_V0_V1<0)
ONON_After_V0_V1[ONON_After_V0_V1<0] <- 0
sum(ONON_After_V0_V1<0)


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V0_V1$Diff <- ONON_After_V0_V1$Right - ONON_After_V0_V1$Left
mean(ONON_After_V0_V1$Diff)



UPDRSIII_COMPLET_V3_V5 <- read_xlsx(path="Raw_Database/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V3_V5 <- names(UPDRSIII_COMPLET_V3_V5)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MSDROIT_RIG, ON_MSGCHE_RIG, ON_MIDROIT_RIG, ON_MIGCHE_RIG,
                                                            ON_MS_DROIT_DOIGT, ON_MSGCHE_DOIGT, ON_MSDROIT_MAINS, ON_MSGCHE_MAINS, ON_MSDROIT_MA, ON_MSGCHE_MA,
                                                            ON_MIDROIT_PIED, ON_MIGCHE_PIED, ON_MIDROIT_JAMBE, ON_MIGCHE_JAMBE, 
                                                            ON_TREMBLPOST_MSDROIT, ON_TREMBLPOST_MSGCHE, ON_TREMBLMAIN_MSGCHE, ON_TREMBLMAIN_MSDROIT,
                                                            ON_MSDROIT_AMPLI_TREMBL, ON_MSGCHE_AMPLI_TREMBL, ON_MIDROIT_AMPLI_TREMBL, ON_MIGCHE_AMPLI_TREMBL)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

ONON_After_V3_V5 <- UPDRSIII_COMPLET_V3_V5

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5 %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
                                 group_by(SUBJID, VISIT) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V3_V5)

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% mutate_each(as.numeric, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL)
sum(is.na(ONON_After_V3_V5))
dim(ONON_After_V3_V5) 


for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V3_V5[,-c(1,2)],ncp=2, scale = T)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V3_V5))
sum(ONON_After_V3_V5<0)
ONON_After_V3_V5[ONON_After_V3_V5<0] <- 0
sum(ONON_After_V3_V5<0)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% mutate(VISIT=ifelse(grepl("V3", VISIT), 3,5)) 

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(Var %in% c("ON_MSGCHE_RIG","ON_MIGCHE_RIG","ON_MSGCHE_DOIGT", "ON_MSGCHE_MAINS", 
                    "ON_MSGCHE_MA","ON_MIGCHE_PIED", "ON_MIGCHE_JAMBE", "ON_TREMBLPOST_MSGCHE",
                    "ON_TREMBLMAIN_MSGCHE","ON_MSGCHE_AMPLI_TREMBL","ON_MIGCHE_AMPLI_TREMBL")) %>% group_by(SUBJID, VISIT) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(Var %in% c("ON_MSDROIT_RIG","ON_MIDROIT_RIG","ON_MS_DROIT_DOIGT","ON_MSDROIT_MAINS" ,
                        "ON_MSDROIT_MA","ON_MIDROIT_PIED","ON_MIDROIT_JAMBE", "ON_TREMBLPOST_MSDROIT",
                        "ON_TREMBLMAIN_MSDROIT","ON_MSDROIT_AMPLI_TREMBL","ON_MIDROIT_AMPLI_TREMBL" )) %>% group_by(SUBJID, VISIT) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V3_V5$Diff <- ONON_After_V3_V5$Right - ONON_After_V3_V5$Left
mean(abs(ONON_After_V3_V5$Diff))

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% mutate(VISIT=1) %>% select(SUBJID, VISIT, Left, Right, Diff)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID) %>% distinct() %>% inner_join(ONON_After_V0_V1) %>%
  bind_rows(ONON_After_V3_V5)

unique(ONON_After_V3_V5$VISIT)

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=mean(Left), Right=mean(Right), Diff=mean(abs(Diff)))

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=sd(Left), Right=sd(Right), Diff=sd(Diff))

ONON_After_V3_V5 %>% arrange(SUBJID)


ONON_After_V3_V5 <- ONON_After_V3_V5 %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>%
  select(SUBJID) %>% left_join(ONON_After_V3_V5) %>% ungroup() #415


PDQ39 <- ONON_After_V3_V5 %>% inner_join(PDQ39)
PDQ39$PDQ39_SCORE <-as.numeric(PDQ39$PDQ39_SCORE)

PDQ39 %>% group_by(VISIT) %>% summarise(n=mean(PDQ39_SCORE, na.rm=T))

PDQ39 <- PDQ39 %>% mutate(Normalized=Diff/(Right+Left)) 
PDQ39 <- PDQ39 %>% mutate(Total=(Right+Left)) 
PDQ39 <- PDQ39 %>% mutate(Diff=abs(Diff)) 



cor.test(abs(PDQ39$Diff[PDQ39$VISIT==1]) , PDQ39$PDQ39_SCORE[PDQ39$VISIT==1], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Diff[PDQ39$VISIT == 1]) and PDQ39$PDQ39_SCORE[PDQ39$VISIT == 1]
# S = 10195917, p-value = 0.1907
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.06531545 


cor.test(abs(PDQ39$Diff[PDQ39$VISIT==3]) , PDQ39$PDQ39_SCORE[PDQ39$VISIT==3], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Diff[PDQ39$VISIT == 3]) and PDQ39$PDQ39_SCORE[PDQ39$VISIT == 3]
# S = 1571410, p-value = 0.4933
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.04775744 
# # 0.1733599 

cor.test(abs(PDQ39$Diff[PDQ39$VISIT==5]) , PDQ39$PDQ39_SCORE[PDQ39$VISIT==5], method = c("spearman"))


# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Diff[PDQ39$VISIT == 5]) and PDQ39$PDQ39_SCORE[PDQ39$VISIT == 5]
# S = 968560, p-value = 0.1884
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# 0.09686769 

cor.test(abs(PDQ39$Diff) , PDQ39$PDQ39_SCORE, method = c("spearman"))


# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Diff) and PDQ39$PDQ39_SCORE
# S = 74471458, p-value = 0.0008987
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1173941 


summary(lm(PDQ39_SCORE ~ Diff  +  Total , data = PDQ39))

# Call:
#   lm(formula = PDQ39_SCORE ~ Diff + Total, data = PDQ39)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -52.283 -16.816  -1.311  15.123  67.775 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 40.49482    1.13041   35.82   <2e-16 ***
#   Diff        -0.52411    0.23502   -2.23    0.026 *  
#   Total        0.79620    0.07432   10.71   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.53 on 794 degrees of freedom
# Multiple R-squared:  0.1441,	Adjusted R-squared:  0.142 
# F-statistic: 66.87 on 2 and 794 DF,  p-value: < 2.2e-16




cor.test(abs(PDQ39$Normalized[PDQ39$VISIT==1]) , PDQ39$PDQ39_SCORE[PDQ39$VISIT==1], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Normalized[PDQ39$VISIT == 1]) and PDQ39$PDQ39_SCORE[PDQ39$VISIT == 1]
# S = 10395767, p-value = 0.004463
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1457587 


cor.test(abs(PDQ39$Normalized[PDQ39$VISIT==3]) , PDQ39$PDQ39_SCORE[PDQ39$VISIT==3], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Normalized[PDQ39$VISIT == 3]) and PDQ39$PDQ39_SCORE[PDQ39$VISIT == 3]
# S = 1877015, p-value = 4.171e-07
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.3462986 

cor.test(abs(PDQ39$Normalized[PDQ39$VISIT==5]) , PDQ39$PDQ39_SCORE[PDQ39$VISIT==5], method = c("spearman"))

# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Normalized[PDQ39$VISIT == 5]) and PDQ39$PDQ39_SCORE[PDQ39$VISIT == 5]
# S = 1213287, p-value = 0.002051
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.2276998 

cor.test(abs(PDQ39$Normalized) , PDQ39$PDQ39_SCORE, method = c("spearman"))


# Spearman's rank correlation rho
# 
# data:  abs(PDQ39$Normalized) and PDQ39$PDQ39_SCORE
# S = 91910402, p-value = 1.381e-11
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.2414895 

unique(PDQ39$VISIT)

image <- PDQ39 %>% filter(Diff<25) %>%
  ggplot(aes(Diff, PDQ39_SCORE)) +
  geom_jitter(width=0.5, height=0.5, alpha=0.5, size=2, shape=1) +
  geom_smooth(method="lm", colour="#2841b0", fill="#2841b0") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Absolute \n Right-to-left Asymmetry") +
  ylab("PDQ39 Score \n") +
  facet_wrap(~VISIT)

ggsave(file="pdq39_years.svg", plot=image, width=12, height=6)





image <- PDQ39 %>% drop_na() %>% filter(Diff<25) %>%
  ggplot(aes(abs(Normalized), PDQ39_SCORE)) +
  geom_jitter(width=0.01, height=0.5, alpha=0.5, size=2, shape=1) +
  geom_smooth(method="lm", colour="#2841b0", fill="#2841b0") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Normalized Absolute \n Right-to-left Asymmetry") +
  ylab("PDQ39 Score \n") +
  facet_wrap(~VISIT)


ggsave(file="pdq39_norm.svg", plot=image, width=6, height=6)




library(car)
model <- lm(PDQ39_SCORE ~ Diff + Total, data = PDQ39)
plot <- crPlots(model)



library(mediation)
med_model <- lm(Total ~ Diff, data = PDQ39)
outcome_model <- lm(PDQ39_SCORE ~ Diff + Total, data = PDQ39)
med_results <- mediation::mediate(med_model, outcome_model, treat = "Diff", mediator = "Total", boot = TRUE)
summary(med_results)

# Causal Mediation Analysis 
# 
# Nonparametric Bootstrap Confidence Intervals with the Percentile Method
# 
# Estimate 95% CI Lower 95% CI Upper p-value    
#   ACME              1.380        1.068         1.74  <2e-16 ***
#   ADE              -0.524       -0.965        -0.07   0.018 *  
#   Total Effect      0.856        0.438         1.31  <2e-16 ***
#   Prop. Mediated    1.612        1.065         2.94  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Sample Size Used: 797 
# 
# 
# Simulations: 1000 


plot(med_results)


# PDQ39

PDQ39 %>% group_by(VISIT) %>% summarise(mean=mean(PDQ39_SCORE), 
                                        sd=sd(PDQ39_SCORE), 
                                        median=median(PDQ39_SCORE),
                                        Q0.25=quantile(PDQ39_SCORE, 0.25),
                                        Q0.75=quantile(PDQ39_SCORE, 0.75)) %>% distinct()

# VISIT  mean    sd median Q0.25 Q0.75
#   1     1  42.8  21.3   41    27    57  
# 2     3  50.9  22.6   49.5  35    67.2
# 3     5  57.7  24.7   55.5  39.2  76 



PDQ39 <- PDQ39 %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>% 
  ungroup() %>% left_join(PDQ39)

friedman.test(y=PDQ39$PDQ39_SCORE, 
              groups=PDQ39$VISIT, 
              blocks=PDQ39$SUBJID)

# Friedman rank sum test
# 
# data:  PDQ39$PDQ39_SCORE, PDQ39$VISIT and PDQ39$SUBJID
# Friedman chi-squared = 19.621, df = 2, p-value = 5.486e-05

pairwise.wilcox.test(PDQ39$PDQ39_SCORE, PDQ39$VISIT, p.adj = "bonferroni", paired=T)

# 
# Pairwise comparisons using Wilcoxon signed rank test with continuity correction 
# 
# data:  PDQ39$PDQ39_SCORE and PDQ39$VISIT 
# 
#        1       3     
#   3 0.0019  -     
#   5 1.2e-05 0.0235
# 
# P value adjustment method: bonferroni 


image <- PDQ39 %>% 
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(PDQ39_SCORE, colour=VISIT, fill=VISIT)) +
  geom_density(alpha=0.4, adjust=2) +
  xlab("\n PDQ39 Score") +
  ylab("Patient density \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) 


ggsave(file="pdq39.svg", plot=image, width=4, height=4)



image <- PDQ39 %>% 
  mutate(VISIT=as.factor(VISIT)) %>%
  mutate(VISIT=ifelse(VISIT==1, "Year 1",
                      ifelse(VISIT==3, "Year 3", "Year 5"))) %>%
  ggplot(aes(VISIT, abs(PDQ39_SCORE), colour=VISIT, fill=VISIT)) +
  geom_boxplot(alpha=0.6, notch = TRUE, width=0.5, outlier.colour = NULL, outlier.fill = NULL, outlier.alpha=0.00001) +
  geom_jitter(alpha=0.8, height=0.5, shape=1) +
  xlab("\n Yearly Visit (1-3)") +
  ylab("PDQ39 Score \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) 

ggsave(file="axial_years.svg", plot=image, width=6, height=6)


length(unique(PDQ39$SUBJID))

df <- PDQ39

mean_stats <- aggregate(PDQ39_SCORE ~ VISIT, data = df, function(x) mean(x))
sd_stats <- aggregate(PDQ39_SCORE ~  VISIT, data = df, function(x) sd(x)/sqrt(96))
summary_stats <- merge(mean_stats, sd_stats, by = "VISIT", suffixes = c("_mean", "_sd"))

summary_stats <- summary_stats %>% mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                                       ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)
df <- df %>%  mutate(VISIT=ifelse(VISIT == 1, "Year 1", 
                                  ifelse(VISIT==3, "Year 3", "Year 5"))) %>% arrange(VISIT)

names(summary_stats)

image <- ggplot() +
  geom_jitter(data = df, 
              aes(x = VISIT, 
                  y = PDQ39_SCORE, color = VISIT),  show.legend = FALSE,  alpha=0.9, shape=1, size=2, height = 0.5, width=0.3) +
  geom_bar(data = summary_stats, aes(x = VISIT, y = PDQ39_SCORE_mean, fill=VISIT, colour=NULL ), 
           stat = "identity", position = "dodge", show.legend = FALSE, alpha=0.7 , width = 0.5 ) +
  geom_errorbar(data = summary_stats, aes(x = VISIT, colour=VISIT, ymin = PDQ39_SCORE_mean  - PDQ39_SCORE_sd, ymax = PDQ39_SCORE_mean  + PDQ39_SCORE_sd), 
                position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  scale_fill_manual(values=c("#bbbbbb", "#4689cc", "#2841b0")) +
  xlab("\n Yearly Visit (1-3)") + ylab("PDQ39 Score  \n")

ggsave(file="axial_cols.svg", plot=image, width=6, height=6)

# ------------
# Predict Asymmetry Year 3 or Year 5 ---------------

UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V0_V1 <- names(UPDRSIII_COMPLET_V0_V1)
ONON_After_V0_V1 <- data.frame(df_names_V0_V1) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names_V0_V1)) %>%
  filter(grepl("3.3", df_names_V0_V1)|
           grepl("3.4", df_names_V0_V1)|
           grepl("3.5", df_names_V0_V1)|
           grepl("3.6", df_names_V0_V1)|
           grepl("3.7", df_names_V0_V1)|
           grepl("3.8", df_names_V0_V1)|
           grepl("3.15", df_names_V0_V1)|
           grepl("3.16", df_names_V0_V1)|
           grepl("3.17", df_names_V0_V1)
  ) %>%
  filter(grepl("Right", df_names_V0_V1)|grepl("right", df_names_V0_V1)|grepl("left", df_names_V0_V1)|grepl("Left", df_names_V0_V1)) %>%
  arrange(df_names_V0_V1)  %>%
  filter(!grepl("OFF", df_names_V0_V1)) 
toString(as.list(ONON_After_V0_V1))
match_V0_V1 <- c("ON_3.15_Left6", "ON_3.15_Right_6", "ON_3.16_Left6", "ON_3.16_Right6", 
                 "ON_3.17_Inf_Left_6", "ON_3.17_Inf_Right6", "ON_3.17_Sup_Left_6", 
                 "ON_3.17_Sup_Right6", "ON_3.3_Inf_Left", "ON_3.3_Inf_Right", "ON_3.3_S_Left", 
                 "ON_3.3_S_Right", "ON_3.4_Left_", "ON_3.4_Right_", "ON_3.5_Left_", "ON_3.5_Right_", 
                 "ON_3.6_Left_", "ON_3.6_Right_", "ON_3.7_Left", "ON_3.7_Right_", "ON_3.8_Left6", "ON_3.8_Right_6")
match <- append("SUBJID", match_V0_V1)
which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)
ONON_After_V0_V1 <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After_V0_V1 <- ONON_After_V0_V1[-1,]


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1 %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
                                 group_by(SUBJID) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V0_V1)

ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% mutate_each(as.numeric, ON_3.3_S_Right:ON_3.17_Inf_Left_6)
sum(is.na(ONON_After_V0_V1))
dim(ONON_After_V0_V1) 


for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V0_V1[,-1],ncp=2, scale = T)

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% select(SUBJID) %>% bind_cols(Imputed$completeObs)

for(i in 2:23){
  cat(i)
  print(round(mean(ONON_After_V0_V1[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V0_V1))
sum(ONON_After_V0_V1<0)
ONON_After_V0_V1[ONON_After_V0_V1<0] <- 0
sum(ONON_After_V0_V1<0)


ONON_After_V0_V1 <- data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(grepl("Left", Var)) %>% group_by(SUBJID) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V0_V1) %>% drop_na() %>% gather(Var, Value, ON_3.3_S_Right:ON_3.17_Inf_Left_6) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(grepl("Right", Var)) %>% group_by(SUBJID) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V0_V1$Diff <- ONON_After_V0_V1$Right - ONON_After_V0_V1$Left
mean(ONON_After_V0_V1$Diff)



UPDRSIII_COMPLET_V3_V5 <- read_xlsx(path="Raw_Database/Raquel_Margherita_Juil 24.xlsx",sheet = "UPDRSIII_COMPLET_V3_V5 ", skip=0, col_types = "text", trim_ws = TRUE)
df_names_V3_V5 <- names(UPDRSIII_COMPLET_V3_V5)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5 %>% select(SUBJID, VISIT, ON_MSDROIT_RIG, ON_MSGCHE_RIG, ON_MIDROIT_RIG, ON_MIGCHE_RIG,
                                                            ON_MS_DROIT_DOIGT, ON_MSGCHE_DOIGT, ON_MSDROIT_MAINS, ON_MSGCHE_MAINS, ON_MSDROIT_MA, ON_MSGCHE_MA,
                                                            ON_MIDROIT_PIED, ON_MIGCHE_PIED, ON_MIDROIT_JAMBE, ON_MIGCHE_JAMBE, 
                                                            ON_TREMBLPOST_MSDROIT, ON_TREMBLPOST_MSGCHE, ON_TREMBLMAIN_MSGCHE, ON_TREMBLMAIN_MSDROIT,
                                                            ON_MSDROIT_AMPLI_TREMBL, ON_MSGCHE_AMPLI_TREMBL, ON_MIDROIT_AMPLI_TREMBL, ON_MIGCHE_AMPLI_TREMBL)
UPDRSIII_COMPLET_V3_V5 <- UPDRSIII_COMPLET_V3_V5[-1,]

ONON_After_V3_V5 <- UPDRSIII_COMPLET_V3_V5

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5 %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
                                 group_by(SUBJID, VISIT) %>% summarise(n=sum(is.na(Value))) %>% filter(n<22)) %>% select(SUBJID) %>%
  inner_join(ONON_After_V3_V5)

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% mutate_each(as.numeric, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL)
sum(is.na(ONON_After_V3_V5))
dim(ONON_After_V3_V5) 


for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}


Imputed <- imputePCA(ONON_After_V3_V5[,-c(1,2)],ncp=2, scale = T)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID, VISIT) %>% bind_cols(Imputed$completeObs)

for(i in 3:24){
  cat(i)
  print(round(mean(ONON_After_V3_V5[,i], na.rm = T),5))
}

sum(is.na(ONON_After_V3_V5))
sum(ONON_After_V3_V5<0)
ONON_After_V3_V5[ONON_After_V3_V5<0] <- 0
sum(ONON_After_V3_V5<0)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% mutate(VISIT=ifelse(grepl("V3", VISIT), 3,5)) 

ONON_After_V3_V5 <- data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
  mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  filter(Var %in% c("ON_MSGCHE_RIG","ON_MIGCHE_RIG","ON_MSGCHE_DOIGT", "ON_MSGCHE_MAINS", 
                    "ON_MSGCHE_MA","ON_MIGCHE_PIED", "ON_MIGCHE_JAMBE", "ON_TREMBLPOST_MSGCHE",
                    "ON_TREMBLMAIN_MSGCHE","ON_MSGCHE_AMPLI_TREMBL","ON_MIGCHE_AMPLI_TREMBL")) %>% group_by(SUBJID, VISIT) %>% summarise(Left=sum(Value)) %>%
  inner_join(
    data.frame(ONON_After_V3_V5) %>% drop_na() %>% gather(Var, Value, ON_MSDROIT_RIG:ON_MIGCHE_AMPLI_TREMBL) %>%
      mutate(Value=as.numeric(Value)) %>% mutate(Value=ifelse(is.na(Value),0,Value)) %>%
      filter(Var %in% c("ON_MSDROIT_RIG","ON_MIDROIT_RIG","ON_MS_DROIT_DOIGT","ON_MSDROIT_MAINS" ,
                        "ON_MSDROIT_MA","ON_MIDROIT_PIED","ON_MIDROIT_JAMBE", "ON_TREMBLPOST_MSDROIT",
                        "ON_TREMBLMAIN_MSDROIT","ON_MSDROIT_AMPLI_TREMBL","ON_MIDROIT_AMPLI_TREMBL" )) %>% group_by(SUBJID, VISIT) %>% summarise(Right=sum(Value))
  ) 

ONON_After_V3_V5$Diff <- ONON_After_V3_V5$Right - ONON_After_V3_V5$Left
mean(abs(ONON_After_V3_V5$Diff))

ONON_After_V0_V1 <- ONON_After_V0_V1 %>% mutate(VISIT=1) %>% select(SUBJID, VISIT, Left, Right, Diff)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% select(SUBJID) %>% distinct() %>% inner_join(ONON_After_V0_V1) %>%
  bind_rows(ONON_After_V3_V5)

unique(ONON_After_V3_V5$VISIT)

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=mean(Left), Right=mean(Right), Diff=mean(abs(Diff)))

ONON_After_V3_V5 %>% group_by(VISIT) %>%
  summarise(Left=sd(Left), Right=sd(Right), Diff=sd(Diff))

ONON_After_V3_V5 %>% arrange(SUBJID)


ONON_After_V3_V5 <- ONON_After_V3_V5 %>% group_by(SUBJID) %>% count() %>% filter(n==3) %>%
  select(SUBJID) %>% left_join(ONON_After_V3_V5) %>% ungroup() #415

ONON_After_V3_V5$Diff <- abs(ONON_After_V3_V5$Diff)





DEMOGRAPHIE <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)
DEMOGRAPHIE <- ONON_After_V3_V5 %>% select(SUBJID) %>% inner_join(DEMOGRAPHIE)

DEMOGRAPHIE <- DEMOGRAPHIE %>% mutate(D_SCREEN=as.numeric(str_sub(D_SCREEN, 7L, 10L)))

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% left_join(DEMOGRAPHIE %>% select(SUBJID, AGE, SEXE) %>% mutate(AGE=as.numeric(AGE)) %>%
                                                     mutate(SEXE=ifelse(SEXE=="Homme",1,0))) %>% distinct()

DEMOGRAPHIE$DISEASE_DUR <- DEMOGRAPHIE$D_SCREEN - as.numeric(DEMOGRAPHIE$D_1ER_SYMPT)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% left_join(DEMOGRAPHIE %>% select(SUBJID, DISEASE_DUR) %>% distinct())




UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
df_names <- names(UPDRSIII_COMPLET_V0_V1)


OFF_before <- data.frame(df_names) %>%
  filter(grepl("^OFF_", df_names)) %>%
  filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)) %>%
  arrange(df_names) %>%
  filter(!grepl("ON", df_names)) %>%   filter(!grepl("1$", df_names))

toString(as.list(OFF_before))

match <- c("OFF_3.9_", "OFF_3.10_", "OFF_3.11_", "OFF_3.12_")

match <- append("SUBJID", match)

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFF_before <- UPDRSIII_COMPLET_V0_V1[which_names]
OFF_before <- OFF_before[-1,]


OFF_before <- OFF_before %>% mutate(OFF_3.9_  =as.numeric(OFF_3.9_  ),
                                    OFF_3.10_  =as.numeric(OFF_3.10_  ),
                                    OFF_3.11_  =as.numeric(OFF_3.11_  ),
                                    OFF_3.12_=as.numeric(OFF_3.12_)
) %>% drop_na()

OFF_before <- OFF_before %>% mutate(Axial_V0=OFF_3.9_ +OFF_3.10_ +OFF_3.11_ +OFF_3.12_)


ONON_After_V3_V5 <- ONON_After_V3_V5 %>% left_join(OFF_before %>% select(SUBJID, Axial_V0))


UPDRSIII_TOTAUX <- read_xlsx(path="Raw_Database/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSIII_TOTAUX <- ONON_After_V3_V5 %>% select(SUBJID) %>% inner_join(UPDRSIII_TOTAUX)
names(UPDRSIII_TOTAUX)
UPDRSIII_TOTAUX <-  UPDRSIII_TOTAUX %>% select(SUBJID, TOT_OFF_DRUG_V0)

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% left_join(UPDRSIII_TOTAUX %>% distinct())
sum(is.na(ONON_After_V3_V5))

ONON_After_V3_V5 <- ONON_After_V3_V5 %>% drop_na() %>% select(-c(Left, Right))
ONON_After_V3_V5$TOT_OFF_DRUG_V0 <- as.numeric(ONON_After_V3_V5$TOT_OFF_DRUG_V0)

Preds <- ONON_After_V3_V5 %>% select(-c(VISIT, Diff)) %>% distinct()

Preds <- Preds %>% left_join(ONON_After_V3_V5 %>% filter(VISIT==5)  %>% select(SUBJID, Diff) %>% distinct()) 

Preds <- Preds %>% select(-SUBJID)

summary(lm(Diff ~ AGE+SEXE+DISEASE_DUR+Axial_V0+TOT_OFF_DRUG_V0 , data = Preds))

# Call:
#   lm(formula = Diff ~ AGE + SEXE + DISEASE_DUR + Axial_V0 + TOT_OFF_DRUG_V0, 
#      data = Preds)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.569 -2.461 -1.373  1.388 24.083 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)      3.65298    1.88757   1.935  0.05366 . 
# AGE             -0.02643    0.02912  -0.908  0.36466   
# SEXE            -0.49760    0.44102  -1.128  0.25987   
# DISEASE_DUR      0.06496    0.05070   1.281  0.20084   
# Axial_V0        -0.17121    0.07486  -2.287  0.02271 * 
#   TOT_OFF_DRUG_V0  0.05282    0.01702   3.103  0.00205 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.128 on 400 degrees of freedom
# Multiple R-squared:  0.03755,	Adjusted R-squared:  0.02552 
# F-statistic: 3.121 on 5 and 400 DF,  p-value: 0.008911

summary(lm(Diff ~ AGE+SEXE+DISEASE_DUR+Axial_V0+TOT_OFF_DRUG_V0 , data = Preds))


Preds <- ONON_After_V3_V5 %>% select(-c(VISIT, Diff)) %>% distinct()

Preds <- Preds %>% left_join(ONON_After_V3_V5 %>% filter(VISIT==3)  %>% select(SUBJID, Diff) %>% distinct()) 

Preds <- Preds %>% select(-SUBJID)

Preds_standardized <- Preds
Preds_standardized[ , c("AGE", "DISEASE_DUR", "Axial_V0", "TOT_OFF_DRUG_V0")] <- scale(Preds_standardized[ , c("AGE", "DISEASE_DUR", "Axial_V0", "TOT_OFF_DRUG_V0")])

# Fit the linear model with standardized predictors
lm_standardized <- lm(Diff ~ AGE + SEXE + DISEASE_DUR + Axial_V0 + TOT_OFF_DRUG_V0, data = Preds_standardized)

# Summarize the model
summary(lm_standardized)

unstandardized_coef <- summary(lm(Diff ~ AGE + SEXE + DISEASE_DUR + Axial_V0 + TOT_OFF_DRUG_V0, data = Preds))$coefficients

# Get standardized coefficients
standardized_coef <- summary(lm_standardized)$coefficients

# Create a combined data frame
coef_summary <- data.frame(
  Predictor = rownames(unstandardized_coef),
  Unstandardized_Coefficients = unstandardized_coef[, "Estimate"],
  Standardized_Coefficients = standardized_coef[, "Estimate"]
)

print(coef_summary)

library(reshape2)

# Melt the data frame for ggplot
coef_melted <- melt(coef_summary, id.vars = "Predictor")

coef_melted <- coef_melted %>% filter(Predictor!="(Intercept)")

coef_melted <- coef_melted %>% mutate(Predictor=ifelse(Predictor=="SEXE", "Gender",
                                                       ifelse(Predictor=="DISEASE_DUR", "Disease_Duration_Baseline",
                                                              ifelse(Predictor=="Axial_V0", "Axial_Score_Baseline",
                                                                     ifelse(Predictor=="AGE", "Age", "MDS_UPDRS_III_OFF_Baseline")))))


coef_melted$alpha_value <- ifelse(coef_melted$Predictor %in% c("MDS_UPDRS_III_OFF_Baseline", "Axial_Score_Baseline"), 1, 0.1)  


image <- ggplot(coef_melted, aes(x = Predictor, y = value, fill = variable, alpha = alpha_value)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Baseline [Year 0] Coefficients Linear Regression \nYear 3 Absolute Motor Asymmetry",
       y = "\n Coefficient Value",
       x = "Predictors \n") +
  scale_fill_manual(values = c("Unstandardized_Coefficients" = "brown3", "Standardized_Coefficients" = "blue4"),
                    labels = c("Unstandardized Coefficients", "Standardized Coefficients")) +
  scale_alpha_identity()  +
  theme(legend.title = element_blank()) +
  ylim(-0.9,0.9)


ggsave(file="preds.svg", plot=image, width=14, height=6)
# -------------------------
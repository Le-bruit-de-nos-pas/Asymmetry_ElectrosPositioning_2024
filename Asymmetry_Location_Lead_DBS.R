library(readxl)
library(tidyverse)
library(data.table)

# Electrodes locations - Left And Right - All locations and all coordinates ---------

activeplots <- read_xlsx(path="activeplots.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
activeplots <- activeplots %>% mutate(PLOT=ifelse(grepl("G", PLOT), "L", "R"))
activeplots <- activeplots %>% mutate(CONTACT=paste0(CONTACT, PLOT)) %>% select(-PLOT)
activeplots %>% group_by(SUBJID) %>% count() %>% arrange(-n)

VTA_asymmetry <- read_xlsx(path="VTA_asymmetry.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
names(VTA_asymmetry)[1] <- "SUBJID"
VTA_asymmetry <- VTA_asymmetry %>% select(-c(group,  `...3`, electrode))
names(VTA_asymmetry)
VTA_asymmetry <- gather(VTA_asymmetry, feature, SITE, C0_LH:C3_RH_Z) 
VTA_asymmetry <- VTA_asymmetry %>% drop_na()
unique(VTA_asymmetry$feature)
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "LH", "L")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "RH", "R")
unique(VTA_asymmetry$feature)
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C0_", "0")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C1_", "1")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C2_", "2")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C3_", "3")
unique(VTA_asymmetry$feature)

VTA_asymmetry <- data.frame(VTA_asymmetry %>% left_join(activeplots) %>% arrange(SUBJID)) %>%
  filter(str_detect(feature, CONTACT)) 

dim(VTA_asymmetry) # 927, 4
unique(VTA_asymmetry$feature)

# Locations

SITES_df <-  VTA_asymmetry %>% filter(feature %in% c("0L", "1L", "2L", "3L", "0R", "1R", "2R", "3R")) %>% 
  select(SUBJID, feature, SITE) %>% distinct() 

dim(SITES_df) # 225, 3 

unique(SITES_df$SITE)

SITES_df$SITE <- str_replace_all(SITES_df$SITE, ",", "/")

unique(SITES_df$SITE)

SITES_df <- separate_rows(SITES_df, SITE, sep = "/", convert=T)

SITES_df$SITE <- str_replace_all(SITES_df$SITE, " ", "")

unique(SITES_df$SITE)

SITES_df <- SITES_df %>% distinct()

SITES_df %>% mutate(feature=str_sub(feature, 2L, 2L)) %>% distinct() %>%
  group_by(SUBJID, feature) %>% count() %>% arrange(-n)




# Groups of asymmetry change

VTA_asymmetry <- read_xlsx(path="VTA_asymmetry.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
groups <- VTA_asymmetry %>% select(ID, group) %>% rename("SUBJID"="ID")
length(unique(groups$SUBJID))

# Individual sites don't have enough data

groups %>% inner_join(SITES_df) %>%
  group_by(SITE,group) %>% count() %>%spread(key=group, value=n) %>%
  mutate(`Asym_to_Sym(A)_Perc` = `Asym_to_Sym(A)` / ( `Asym_to_Sym(A)` + `Aym_to_Asym(C)`) ) %>%
  mutate(`Asym_to_Asym(A)_Perc` =  `Aym_to_Asym(C)` / ( `Asym_to_Sym(A)` + `Aym_to_Asym(C)`) ) 
 
length(unique(SITES_df$SUBJID)) # 113



SITES_df %>% inner_join(groups) %>% select(SUBJID, group) %>% distinct() %>% group_by(group) %>% count()


SITES_df <- SITES_df %>% mutate(SITE=ifelse(SITE=="STN_SM", "STN_SM", "Other")) %>%
  mutate(feature=str_sub(feature, 2L, 2L)) %>%
  distinct()


SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  #group_by(group, L) %>% count()
  #group_by(group, R) %>% count()
  #group_by(group, L, R) %>% count() 
  filter(L==1|R==1) %>% group_by(group) %>% count()





data <- data.frame(
  group = c("Asym -> Sym", "Asym = Asym"),
  percentage = c(100, 100),  #  percentages
  absolute = c(82, 31)   #  absolute numbers
)


ggplot(data, aes(x = group, y = absolute, colour=group, fill=group)) +
  geom_bar(stat = "identity", alpha=0.8) +
  #geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +
  geom_text(aes(label = absolute), vjust = -0.5, size = 5, color = "black") +
  labs(title = "Absolute Patient Number per Group",
       x = "Group",
       y = "Absolute Number \n") +
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(3, 3, 3, 3, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))

data <- data.frame(
  group = c("Asym -> Sym", "Asym = Asym"),
  percentage = c(49, 55),  #  percentages
  absolute = c(40, 17)   #  absolute numbers
)


fisher.test(matrix(c(40, 42, 17, 14), nrow = 2,
                   dimnames = list(c("Sym", "Asym"), c("Yes", "No"))))



ggplot(data, aes(x = group, y = percentage, colour=group, fill=group)) +
  geom_bar(stat = "identity", alpha=0.8) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +
  geom_text(aes(label = absolute), vjust = 1.5, size = 5, color = "white") +
  labs(title = "SM on the Left",
       x = "Group",
       y = "Percentage \n") +
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(3, 3, 3, 3, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))


data <- data.frame(
  group = c("Asym -> Sym", "Asym = Asym"),
  percentage = c(37, 32),  #  percentages
  absolute = c(30, 10)   #  absolute numbers
)



fisher.test(matrix(c(30, 52, 10, 21), nrow = 2,
                   dimnames = list(c("Sym", "Asym"), c("Yes", "No"))))


ggplot(data, aes(x = group, y = percentage, colour=group, fill=group)) +
  geom_bar(stat = "identity", alpha=0.8) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +
  geom_text(aes(label = absolute), vjust = 1.5, size = 5, color = "white") +
  labs(title = "SM on the Right",
       x = "Group",
       y = "Percentage \n") +
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(3, 3, 3, 3, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))




data <- data.frame(
  group = c("Asym -> Sym", "Asym = Asym"),
  percentage = c(63, 65),  #  percentages
  absolute = c(52, 20)   #  absolute numbers
)


fisher.test(matrix(c(52, 30, 20, 11), nrow = 2,
                   dimnames = list(c("Sym", "Asym"), c("Yes", "No"))))


ggplot(data, aes(x = group, y = percentage, colour=group, fill=group)) +
  geom_bar(stat = "identity", alpha=0.8) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +
  geom_text(aes(label = absolute), vjust = 1.5, size = 5, color = "white") +
  labs(title = "SM on the Left OR Right",
       x = "Group",
       y = "Percentage \n") +
  theme_minimal() +
  ylim(0,100) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(3, 3, 3, 3, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))





data <- data.frame(
  group = c("Asym -> Sym", "Asym = Asym"),
  percentage = c(22, 23),  #  percentages
  absolute = c(18, 7)   #  absolute numbers
)


fisher.test(matrix(c(18, 64, 7, 24), nrow = 2,
                   dimnames = list(c("Sym", "Asym"), c("Yes", "No"))))


ggplot(data, aes(x = group, y = percentage, colour=group, fill=group)) +
  geom_bar(stat = "identity", alpha=0.8) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +
  geom_text(aes(label = absolute), vjust = 1.5, size = 5, color = "white") +
  labs(title = "SM on the Left AND Right",
       x = "Group",
       y = "Percentage \n") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  ylim(0,100) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(3, 3, 3, 3, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))





# --------------


# Absolute degree of asymetry SM vs no SM -------------------------------------

# SITES_df from above !


Asymmetry_Pre_vs_Post <- fread("Asymmetry_Pre_vs_Post.txt")


Asymmetry_Pre_vs_Post %>% inner_join(groups) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  group_by(group, Eval) %>% summarise(mean=mean(Asym), sd=sd(Asym))

test <- Asymmetry_Pre_vs_Post %>% inner_join(groups) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) 

unique(test$Eval)

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="Pre_OP"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="Pre_OP"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_ON"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_ON"]) ) 



SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Asymmetry_Pre_vs_Post) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  ggplot(aes(Asym, colour=Left_OR_Right_SM, fill=Left_OR_Right_SM )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Absolute Degree of \n Left-to-Right Motor Asymmetry") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))




Asymmetry_Pre_vs_Post %>% inner_join(groups) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") %>%  
  group_by(group, Eval) %>% summarise(mean=mean(Asym), sd=sd(Asym))

test <-  Asymmetry_Pre_vs_Post %>% inner_join(groups) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") 

unique(test$Eval)


wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]) ) 

wilcox.test(as.numeric(test$Asym[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_ON"]), 
            as.numeric(test$Asym[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_ON"]) ) 



SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Asymmetry_Pre_vs_Post) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") %>%
  ggplot(aes(Asym, colour=Left_OR_Right_SM, fill=Left_OR_Right_SM )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Change in Absolute \n Left-to-Right Motor Asymmetry \n From Pre OP \n") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))






SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(`Same_L/R`=ifelse(L==R,1,0)) %>%
  mutate(`Same_L/R`=as.factor(`Same_L/R`)) %>%
  mutate(`Same_L/R`=ifelse(`Same_L/R`==1, "Yes", "No")) %>%
  select(-c(L, R)) %>%
  group_by(group, `Same_L/R`) %>% count()



SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(`Same_L/R`=ifelse(L==R,1,0)) %>%
  mutate(`Same_L/R`=as.factor(`Same_L/R`)) %>%
  mutate(`Same_L/R`=ifelse(`Same_L/R`==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Asymmetry_Pre_vs_Post) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  ggplot(aes(Asym, colour=`Same_L/R`, fill=`Same_L/R` )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Absolute Degree of \n Left-to-Right Motor Asymmetry") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))





SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(`Same_L/R`=ifelse(L==R,1,0)) %>%
  mutate(`Same_L/R`=as.factor(`Same_L/R`)) %>%
  mutate(`Same_L/R`=ifelse(`Same_L/R`==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Asymmetry_Pre_vs_Post) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Asym, Pre_OP:OFF_OFF, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") %>%
  ggplot(aes(Asym, colour=`Same_L/R`, fill=`Same_L/R` )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Change in Absolute \n Left-to-Right Motor Asymmetry \n From Pre OP \n") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))









# Absolute degree of axial score SM vs no SM



UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
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



ONON_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^ON", df_names)) %>%
  filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)) %>%  arrange(df_names)  %>%
  filter(!grepl("OFF", df_names)) 


toString(as.list(ONON_After))

match <- c("ON_3.10_6", "ON_3.11_6", "ON_3.12_6", "ON_3.9_6")

match <- append("SUBJID", match)

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONON_After <- ONON_After[-1,]

ONON_After <- ONON_After %>% mutate(ON_3.9_6 =as.numeric(ON_3.9_6 ),
                                    ON_3.10_6 =as.numeric(ON_3.10_6 ),
                                    ON_3.11_6 =as.numeric(ON_3.11_6 ),
                                    ON_3.12_6=as.numeric(ON_3.12_6)
) %>% drop_na()





OFFOFF_After <- data.frame(df_names) %>%
  filter(row_number()>272) %>%
  filter(grepl("^OFF", df_names)) %>%
  filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)) %>%  arrange(df_names)  %>%
  filter(!grepl("ON", df_names)) 


toString(as.list(OFFOFF_After))

match <- c("OFF_3.10_1", "OFF_3.11_1", "OFF_3.12_1", "OFF_3.9_1")

match <- append("SUBJID", match)

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFOFF_After <- OFFOFF_After[-1,]


OFFOFF_After <- OFFOFF_After %>% mutate(OFF_3.9_1  =as.numeric(OFF_3.9_1  ),
                                        OFF_3.10_1  =as.numeric(OFF_3.10_1  ),
                                        OFF_3.11_1  =as.numeric(OFF_3.11_1  ),
                                        OFF_3.12_1=as.numeric(OFF_3.12_1)
) %>% drop_na()




ONOFF_After <- data.frame(df_names) %>%
  filter(grepl("^ONOFF", df_names)) %>%
  filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)
  ) %>%
  arrange(df_names) 

toString(as.list(ONOFF_After))

match <- c("ONOFF_3.10_", "ONOFF_3.11_", "ONOFF_3.12_", "ONOFF_3.9_")

match <- append("SUBJID", match)

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONOFF_After <- ONOFF_After[-1,]


ONOFF_After <- ONOFF_After %>% mutate(ONOFF_3.10_  =as.numeric(ONOFF_3.10_  ),
                                      ONOFF_3.11_  =as.numeric(ONOFF_3.11_  ),
                                      ONOFF_3.12_  =as.numeric(ONOFF_3.12_  ),
                                      ONOFF_3.9_=as.numeric(ONOFF_3.9_)
) %>% drop_na()




OFFON_After <- data.frame(df_names) %>%
  filter(grepl("^OFFON", df_names)) %>%
  filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)
  ) %>%
  arrange(df_names) 

toString(as.list(OFFON_After))

match <- c("OFFON_3.10_", "OFFON_3.11_", "OFFON_3.12_", "OFFON_3.9_")

match <- append("SUBJID", match)

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

OFFON_After <- UPDRSIII_COMPLET_V0_V1[which_names]
OFFON_After <- OFFON_After[-1,]


OFFON_After <- OFFON_After %>% mutate(OFFON_3.10_  =as.numeric(OFFON_3.10_  ),
                                      OFFON_3.11_  =as.numeric(OFFON_3.11_  ),
                                      OFFON_3.12_  =as.numeric(OFFON_3.12_  ),
                                      OFFON_3.9_=as.numeric(OFFON_3.9_)
) %>% drop_na()




OFF_before$AxialScoreOFFbefore <- OFF_before$OFF_3.9_ + OFF_before$OFF_3.10_ + OFF_before$OFF_3.11_ + OFF_before$OFF_3.12_
ONON_After$AxialScoreON <- ONON_After$ON_3.9_6 + ONON_After$ON_3.10_6 + ONON_After$ON_3.11_6 + ONON_After$ON_3.12_6
OFFOFF_After$AxialScoreOFF <- OFFOFF_After$OFF_3.9_1 + OFFOFF_After$OFF_3.10_1 + OFFOFF_After$OFF_3.11_1 + OFFOFF_After$OFF_3.12_1 
ONOFF_After$AxialScoreONOFF <- ONOFF_After$ONOFF_3.9_  + ONOFF_After$ONOFF_3.10_  + ONOFF_After$ONOFF_3.11_  + ONOFF_After$ONOFF_3.12_ 
OFFON_After$AxialScoreOFFON <- OFFON_After$OFFON_3.9_  + OFFON_After$OFFON_3.10_  + OFFON_After$OFFON_3.11_  + OFFON_After$OFFON_3.12_ 

Axial_scores <- OFF_before %>% select(SUBJID, AxialScoreOFFbefore) %>%
  inner_join(OFFOFF_After %>% select(SUBJID, AxialScoreOFF)) %>%
  inner_join(ONON_After %>% select(SUBJID, AxialScoreON)) %>%
  inner_join(ONOFF_After %>% select(SUBJID, AxialScoreONOFF)) %>%
  inner_join(OFFON_After %>% select(SUBJID, AxialScoreOFFON)) 

Axial_scores <- Axial_scores %>% 
  rename("Pre_OP"="AxialScoreOFFbefore") %>%
  rename("OFF_OFF"="AxialScoreOFF") %>%
  rename("ON_ON"="AxialScoreON") %>%
  rename("ON_DBS_OFF_Med"="AxialScoreONOFF") %>%
  rename("OFF_DBS_ON_Med"="AxialScoreOFFON")



test <- SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R)) %>%
  inner_join(Axial_scores) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE)




wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="Pre_OP"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="Pre_OP"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_ON"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_ON"]) ) 




SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R)) %>%
  inner_join(Axial_scores) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  group_by(group, Eval) %>% summarise(mean=mean(Axial),sd=sd(Axial))





SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Axial_scores) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  ggplot(aes(Axial, colour=Left_OR_Right_SM, fill=Left_OR_Right_SM )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Absolute Axial Score \n") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))



test <-   SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R)) %>%
  inner_join(Axial_scores) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP")


wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_OFF"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_DBS_OFF_Med"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="OFF_DBS_ON_Med"]) ) 

wilcox.test(as.numeric(test$Axial[test$group=="Asym_to_Sym(A)"&test$Eval=="ON_ON"]), 
            as.numeric(test$Axial[test$group!="Asym_to_Sym(A)"&test$Eval=="ON_ON"]) ) 




SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R)) %>%
  inner_join(Axial_scores) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") %>%
  group_by(group, Eval) %>% summarise(mean=mean(Axial),sd=sd(Axial))




SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(Left_OR_Right_SM=ifelse(L==1|R==1,1,0)) %>%
  mutate(Left_AND_Right_SM=ifelse(L==1&R==1,1,0)) %>%
  mutate(Left_OR_Right_SM=as.factor(Left_OR_Right_SM)) %>%
  mutate(Left_AND_Right_SM=as.factor(Left_AND_Right_SM)) %>%
  mutate(Left_OR_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  mutate(Left_AND_Right_SM=ifelse(Left_AND_Right_SM==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Axial_scores) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") %>%
  ggplot(aes(Axial, colour=Left_OR_Right_SM, fill=Left_OR_Right_SM )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Change in Absolute \n Axial Score \n From Pre OP \n") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))



SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(`Same_L/R`=ifelse(L==R,1,0)) %>%
  mutate(`Same_L/R`=as.factor(`Same_L/R`)) %>%
  mutate(`Same_L/R`=ifelse(`Same_L/R`==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Axial_scores) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  ggplot(aes(Axial, colour=`Same_L/R`, fill=`Same_L/R` )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Absolute Axial Score") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))





# Electrode coordinates 

activeplots <- read_xlsx(path="activeplots.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
activeplots <- activeplots %>% mutate(PLOT=ifelse(grepl("G", PLOT), "L", "R"))
activeplots <- activeplots %>% mutate(CONTACT=paste0(CONTACT, PLOT)) %>% select(-PLOT)
activeplots %>% group_by(SUBJID) %>% count() %>% arrange(-n)

VTA_asymmetry <- read_xlsx(path="VTA_asymmetry.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
names(VTA_asymmetry)[1] <- "SUBJID"
VTA_asymmetry <- VTA_asymmetry %>% select(-c(group,  `...3`, electrode))
names(VTA_asymmetry)
VTA_asymmetry <- gather(VTA_asymmetry, feature, SITE, C0_LH:C3_RH_Z) 
VTA_asymmetry <- VTA_asymmetry %>% drop_na()
unique(VTA_asymmetry$feature)
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "LH", "L")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "RH", "R")
unique(VTA_asymmetry$feature)
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C0_", "0")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C1_", "1")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C2_", "2")
VTA_asymmetry$feature <- str_replace_all(VTA_asymmetry$feature, "C3_", "3")
unique(VTA_asymmetry$feature)

VTA_asymmetry <- data.frame(VTA_asymmetry %>% left_join(activeplots) %>% arrange(SUBJID)) %>%
  filter(str_detect(feature, CONTACT)) 

dim(VTA_asymmetry) # 927, 4
unique(VTA_asymmetry$feature)


COORDS_df <-  VTA_asymmetry %>% filter(grepl("X", feature) | grepl("Y", feature) | grepl("Z", feature))

COORDS_df <- COORDS_df %>% 
  mutate(CONTACT=str_sub(CONTACT, 2L, 2L)) %>%
  mutate(feature=str_sub(feature, 4L, 4L)) %>%
  mutate(SITE=as.numeric(SITE)) %>%
  mutate(SITE=round(SITE, 2))


# Groups of asymmetry change

VTA_asymmetry <- read_xlsx(path="VTA_asymmetry.xlsx", skip=0, col_types = "text", trim_ws = TRUE)
groups <- VTA_asymmetry %>% select(ID, group) %>% rename("SUBJID"="ID")
length(unique(groups$SUBJID))

test <- groups %>% inner_join(COORDS_df) 



wilcox.test(as.numeric(test$SITE[test$group=="Asym_to_Sym(A)"&test$feature=="X"&test$CONTACT=="L"]), 
            as.numeric(test$SITE[test$group!="Asym_to_Sym(A)"&test$feature=="X"&test$CONTACT=="L"]) ) 

wilcox.test(as.numeric(test$SITE[test$group=="Asym_to_Sym(A)"&test$feature=="X"&test$CONTACT=="R"]), 
            as.numeric(test$SITE[test$group!="Asym_to_Sym(A)"&test$feature=="X"&test$CONTACT=="R"]) ) 


wilcox.test(as.numeric(test$SITE[test$group=="Asym_to_Sym(A)"&test$feature=="Y"&test$CONTACT=="L"]), 
            as.numeric(test$SITE[test$group!="Asym_to_Sym(A)"&test$feature=="Y"&test$CONTACT=="L"]) ) 

wilcox.test(as.numeric(test$SITE[test$group=="Asym_to_Sym(A)"&test$feature=="Y"&test$CONTACT=="R"]), 
            as.numeric(test$SITE[test$group!="Asym_to_Sym(A)"&test$feature=="Y"&test$CONTACT=="R"]) ) 


wilcox.test(as.numeric(test$SITE[test$group=="Asym_to_Sym(A)"&test$feature=="Z"&test$CONTACT=="L"]), 
            as.numeric(test$SITE[test$group!="Asym_to_Sym(A)"&test$feature=="Z"&test$CONTACT=="L"]) ) 

wilcox.test(as.numeric(test$SITE[test$group=="Asym_to_Sym(A)"&test$feature=="Z"&test$CONTACT=="R"]), 
            as.numeric(test$SITE[test$group!="Asym_to_Sym(A)"&test$feature=="Z"&test$CONTACT=="R"]) ) 



groups %>% inner_join(COORDS_df) %>%
  group_by(group, CONTACT, feature)  %>% summarise(mean=mean(SITE), sd=sd(SITE)) %>%
  arrange(CONTACT, feature)



groups %>% inner_join(COORDS_df) %>%
  mutate(group=ifelse(group=="Asym_to_Sym(A)", "Asym -> Sym", "Asym = Asym")) %>%
  filter(CONTACT=="L") %>%
  ggplot(aes(SITE, colour=group, fill=group )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~feature, ncol = 1) +
  xlab("\n Relative Location \n Left Side") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))




groups %>% inner_join(COORDS_df) %>%
  mutate(group=ifelse(group=="Asym_to_Sym(A)", "Asym -> Sym", "Asym = Asym")) %>%
  filter(CONTACT=="R") %>%
  ggplot(aes(SITE, colour=group, fill=group )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~feature, ncol = 1) +
  xlab("\n Relative Location \n Right Side") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))








SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM)) %>%
  inner_join(groups) %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  mutate(`Same_L/R`=ifelse(L==R,1,0)) %>%
  mutate(`Same_L/R`=as.factor(`Same_L/R`)) %>%
  mutate(`Same_L/R`=ifelse(`Same_L/R`==1, "Yes", "No")) %>%
  select(-c(L, R, group)) %>%
  inner_join(Axial_scores) %>%
  mutate(ON_DBS_OFF_Med=ON_DBS_OFF_Med-Pre_OP) %>%
  mutate(OFF_DBS_ON_Med =OFF_DBS_ON_Med -Pre_OP) %>%
  mutate(ON_ON =ON_ON -Pre_OP) %>%
  mutate(OFF_OFF=OFF_OFF-Pre_OP) %>%
  gather(Eval, Axial, Pre_OP:OFF_DBS_ON_Med, factor_key=TRUE) %>%
  filter(Eval!="Pre_OP") %>%
  ggplot(aes(Axial, colour=`Same_L/R`, fill=`Same_L/R` )) +
  geom_density(linewidth=2, alpha=0.25) +
  facet_wrap(~Eval) +
  xlab("\n Change in Absolute \n Axial Score \n From Pre OP \n") +
  ylab("\n Patient Density \n Gaussian kernel function \n") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size = 12) ,
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_fill_manual(values=c("#0099E0", "#D45769")) +
  scale_colour_manual(values=c("#0099E0", "#D45769"))

# ---------------------


# Cohort Clinical and Demographic characteristics -----------------------

groups <- groups %>% inner_join(SITES_df %>% select(SUBJID) %>% distinct())

# DEMOGRAPHICS
DEMOGRAPHIE <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "DEMOGRAPHIE ", skip=0, col_types = "text", trim_ws = TRUE)
DEMOGRAPHIE <- groups %>% inner_join(DEMOGRAPHIE)
DEMOGRAPHIE <- DEMOGRAPHIE %>% mutate(D_SCREEN=as.numeric(str_sub(D_SCREEN, 7L, 10L)))

# AGE
DEMOGRAPHIE %>% group_by(group) %>% summarise(mean=mean(as.numeric(AGE)), sd=sd(as.numeric(AGE)))

wilcox.test(as.numeric(DEMOGRAPHIE$AGE[DEMOGRAPHIE$group=="Asym_to_Sym(A)"]), 
            as.numeric(DEMOGRAPHIE$AGE[DEMOGRAPHIE$group!="Asym_to_Sym(A)"]) ) 

# GENDER
DEMOGRAPHIE %>% group_by(group, SEXE) %>% count()

fisher.test(matrix(c(61, 17, 21, 14), nrow = 2,
                   dimnames = list(c("Sym", "Asym"), c("Male", "Female"))))

# RACE
DEMOGRAPHIE %>% group_by(group, ETHNIE) %>% count()

fisher.test(matrix(c(77, 5, 31, 0), nrow = 2,
                   dimnames = list(c("Sym", "Asym"), c("Cauc", "other"))))


# Symptoms to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% 
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_1ER_SYMPT)), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_1ER_SYMPT)))

test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_1ER_SYMPT)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 


# Diagnosis to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% 
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_DIAG)), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_DIAG)))

test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_DIAG)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 


# L-DOPA Init to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% 
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_LDOPA), na.rm=T), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_LDOPA), na.rm=T))


test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_LDOPA)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 

# Any DOPA Init to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% filter(D_TTT_DOPAM!="003") %>%
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_TTT_DOPAM), na.rm=T), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_TTT_DOPAM), na.rm=T))


test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_TTT_DOPAM)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 

# Motor Fluct to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% filter(D_FLUCTU_MOTR>="19") %>%
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_FLUCTU_MOTR), na.rm=T), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_FLUCTU_MOTR), na.rm=T))

test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_FLUCTU_MOTR)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 


# Non-Motor Fluct to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% filter(D_FLUCTU_NONMOTR!="0"&D_FLUCTU_NONMOTR!="0000") %>%
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_FLUCTU_NONMOTR), na.rm=T), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_FLUCTU_NONMOTR), na.rm=T))

test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_FLUCTU_NONMOTR)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 


# Dyskinesia to 1st Screen
DEMOGRAPHIE %>% group_by(group) %>% filter(D_DYSKINESIE!="0") %>%
  summarise(mean=mean(as.numeric(D_SCREEN)-as.numeric(D_DYSKINESIE), na.rm=T), sd=sd(as.numeric(D_SCREEN)-as.numeric(D_DYSKINESIE), na.rm=T))


test <- DEMOGRAPHIE %>% mutate(diff=as.numeric(D_SCREEN)-as.numeric(D_DYSKINESIE)) %>% select(SUBJID, diff, group)

wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 


# Age DBS
DATES_DE_VISITES  <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "DATES_DE_VISITES ", skip=0, col_types = "text", trim_ws = TRUE)
DATES_DE_VISITES <- DATES_DE_VISITES %>% select(SUBJID, D_CHIR)
DATES_DE_VISITES %>% inner_join(DEMOGRAPHIE %>% select(SUBJID, DDN)) %>% 
  mutate(D_CHIR=as.numeric(str_sub(D_CHIR, 7L, 10L))) %>%
  mutate(DDN=as.numeric(str_sub(DDN, 4L, 7))) %>% 
  inner_join(groups) %>% group_by(group) %>%
  summarise(mean=mean(D_CHIR-DDN, na.rm=T), sd=sd(D_CHIR-DDN, na.rm=T))

test <- DATES_DE_VISITES %>% inner_join(DEMOGRAPHIE %>% select(SUBJID, DDN)) %>% 
  mutate(D_CHIR=as.numeric(str_sub(D_CHIR, 7L, 10L))) %>%
  mutate(DDN=as.numeric(str_sub(DDN, 4L, 7))) %>% 
  inner_join(groups) %>% mutate(diff=D_CHIR-DDN) %>% drop_na() %>% select(SUBJID, group, diff)


wilcox.test(as.numeric(test$diff[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$diff[test$group!="Asym_to_Sym(A)"]) ) 



# UPDRS III Total
UPDRSIII_TOTAUX <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_TOTAUX", skip=0, col_types = "text", trim_ws = TRUE)
UPDRSIII_TOTAUX <- groups %>% inner_join(UPDRSIII_TOTAUX)
names(UPDRSIII_TOTAUX)

# Pre OFF
UPDRSIII_TOTAUX %>% group_by(group) %>%
  summarise(mean=mean(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T), sd=sd(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T))

UPDRSIII_TOTAUX %>% filter(group=="Asym_to_Sym(A)") %>%summarise(mean=mean(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T), sd=sd(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T))
UPDRSIII_TOTAUX %>% filter(group!="Asym_to_Sym(A)") %>%summarise(mean=mean(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T), sd=sd(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0), na.rm=T))



wilcox.test(as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0[UPDRSIII_TOTAUX$group=="Asym_to_Sym(A)"&!is.na(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0)]), 
            as.numeric(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0[UPDRSIII_TOTAUX$group!="Asym_to_Sym(A)"&!is.na(UPDRSIII_TOTAUX$TOT_OFF_DRUG_V0)]) ) 



# Pre Best ON
UPDRSIII_TOTAUX %>% group_by(group) %>%
  summarise(mean=mean(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), na.rm=T), sd=sd(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V0), na.rm=T))


# Post OFF
UPDRSIII_TOTAUX %>% group_by(group) %>%
  summarise(mean=mean(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T), sd=sd(as.numeric(UPDRSIII_TOTAUX$OFF_TOTALCALC_V1), na.rm=T))


# Post Best ON
UPDRSIII_TOTAUX %>% group_by(group) %>%
  summarise(mean=mean(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), na.rm=T), sd=sd(as.numeric(UPDRSIII_TOTAUX$EVAL_MOT_BESTON_V1), na.rm=T))


# UMSARS II Total
UPDRSI_II <- fread("UPDRSI_II.txt")
UPDRSI_II <- UPDRSI_II %>% gather(item, value, MDS_2_4ON:MDS2_9ON) %>%
  mutate(state=ifelse(grepl("ON", item), "ON", "OFF")) %>%
  group_by(SUBJID, VISIT, state) %>% 
  summarise(value=sum(value)) %>%
  ungroup() %>%
  mutate(VISIT=ifelse(VISIT==0, "Pre_OP", "Post_OP")) 

groups %>% inner_join(UPDRSI_II) %>%
  group_by(group, VISIT, state) %>% summarise(mean=mean(value), sd=sd(value))

UPDRSI_II <- groups %>% inner_join(UPDRSI_II) 

wilcox.test(as.numeric(UPDRSI_II$value[UPDRSI_II$group=="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Pre_OP"&UPDRSI_II$state=="OFF"]), 
            as.numeric(UPDRSI_II$value[UPDRSI_II$group!="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Pre_OP"&UPDRSI_II$state=="OFF"]) )   

wilcox.test(as.numeric(UPDRSI_II$value[UPDRSI_II$group=="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Pre_OP"&UPDRSI_II$state=="ON"]), 
            as.numeric(UPDRSI_II$value[UPDRSI_II$group!="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Pre_OP"&UPDRSI_II$state=="ON"]) )   


wilcox.test(as.numeric(UPDRSI_II$value[UPDRSI_II$group=="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Post_OP"&UPDRSI_II$state=="OFF"]), 
            as.numeric(UPDRSI_II$value[UPDRSI_II$group!="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Post_OP"&UPDRSI_II$state=="OFF"]) )   

wilcox.test(as.numeric(UPDRSI_II$value[UPDRSI_II$group=="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Post_OP"&UPDRSI_II$state=="ON"]), 
            as.numeric(UPDRSI_II$value[UPDRSI_II$group!="Asym_to_Sym(A)"&UPDRSI_II$VISIT=="Post_OP"&UPDRSI_II$state=="ON"]) )   



# S&E
Hoehn_YarhS_E <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "Hoehn&Yarh-S&E", skip=0, col_types = "text", trim_ws = TRUE)
Hoehn_YarhS_E <- groups %>% inner_join(Hoehn_YarhS_E)
names(Hoehn_YarhS_E)


Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_OFF) %>% spread(key=VISIT, value=SCHWAB_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`))

test <- Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_OFF) %>% spread(key=VISIT, value=SCHWAB_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()


wilcox.test(as.numeric(test$`Visite de screening`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite de screening`[test$group!="Asym_to_Sym(A)"]) )   



Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_OFF) %>% spread(key=VISIT, value=SCHWAB_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`))


test <- Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_OFF) %>% spread(key=VISIT, value=SCHWAB_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()


wilcox.test(as.numeric(test$`Visite Bilan à 1 an - V1`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite Bilan à 1 an - V1`[test$group!="Asym_to_Sym(A)"]) )   


Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`))


test <-Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() 

wilcox.test(as.numeric(test$`Visite de screening`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite de screening`[test$group!="Asym_to_Sym(A)"]) )   


Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`))


test <-Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, SCHWAB_ON) %>% spread(key=VISIT, value=SCHWAB_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()

wilcox.test(as.numeric(test$`Visite Bilan à 1 an - V1`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite Bilan à 1 an - V1`[test$group!="Asym_to_Sym(A)"]) )   




# H&E
Hoehn_YarhS_E <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "Hoehn&Yarh-S&E", skip=0, col_types = "text", trim_ws = TRUE)
Hoehn_YarhS_E <- groups %>% inner_join(Hoehn_YarhS_E)
names(Hoehn_YarhS_E)


Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_OFF) %>% spread(key=VISIT, value=HOEHN_YAHR_OFF) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`))

test <-  Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_OFF) %>% spread(key=VISIT, value=HOEHN_YAHR_OFF) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()

wilcox.test(as.numeric(test$`Visite de screening`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite de screening`[test$group!="Asym_to_Sym(A)"]) )   



Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_OFF) %>% spread(key=VISIT, value=HOEHN_YAHR_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`))

test <-  Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_OFF) %>% spread(key=VISIT, value=HOEHN_YAHR_OFF) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()

wilcox.test(as.numeric(test$`Visite Bilan à 1 an - V1`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite Bilan à 1 an - V1`[test$group!="Asym_to_Sym(A)"]) )   



Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_ON) %>% spread(key=VISIT, value=HOEHN_YAHR_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite de screening`), sd=sd(`Visite de screening`))

test <-  Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_ON) %>% spread(key=VISIT, value=HOEHN_YAHR_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()

wilcox.test(as.numeric(test$`Visite de screening`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite de screening`[test$group!="Asym_to_Sym(A)"]) )   



Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_ON) %>% spread(key=VISIT, value=HOEHN_YAHR_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na() %>% group_by(group) %>%
  summarise(mean=mean(`Visite Bilan à 1 an - V1`), sd=sd(`Visite Bilan à 1 an - V1`))

test <-  Hoehn_YarhS_E %>% select(SUBJID, group, VISIT, HOEHN_YAHR_ON) %>% spread(key=VISIT, value=HOEHN_YAHR_ON) %>% 
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, "Stade ", "")) %>%
  mutate(`Visite Bilan à 1 an - V1` = str_replace(`Visite Bilan à 1 an - V1`, ",", ".")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, "Stade ", "")) %>%
  mutate(`Visite de screening` = str_replace(`Visite de screening`, ",", ".")) %>%
  mutate(`Visite Bilan à 1 an - V1`=parse_number(`Visite Bilan à 1 an - V1`)) %>%
  mutate(`Visite de screening`=parse_number(`Visite de screening`)) %>%
  drop_na()

wilcox.test(as.numeric(test$`Visite Bilan à 1 an - V1`[test$group=="Asym_to_Sym(A)"]), 
            as.numeric(test$`Visite Bilan à 1 an - V1`[test$group!="Asym_to_Sym(A)"]) )   

# MoCA

MoCA_V0 <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "MoCA V0", skip=0, col_types = "text", trim_ws = TRUE)
MoCA_V0 <- groups %>% inner_join(MoCA_V0) %>% select(SUBJID, group, MOCA_SCORE)
MoCA_V0 %>% group_by(group) %>% summarise(mean=mean(as.numeric(MOCA_SCORE), na.rm=T), sd=sd(as.numeric(MOCA_SCORE), na.rm=T))

wilcox.test(as.numeric(MoCA_V0$MOCA_SCORE[MoCA_V0$group=="Asym_to_Sym(A)"]), 
            as.numeric(MoCA_V0$MOCA_SCORE[MoCA_V0$group!="Asym_to_Sym(A)"]) )   


MoCA_V1 <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "MoCA V1", skip=0, col_types = "text", trim_ws = TRUE)
MoCA_V1 <- groups %>% inner_join(MoCA_V1) %>%  select(SUBJID, group, MOCA_SCORE)
MoCA_V1 %>% group_by(group) %>% summarise(mean=mean(as.numeric(MOCA_SCORE), na.rm=T), sd=sd(as.numeric(MOCA_SCORE), na.rm=T))

wilcox.test(as.numeric(MoCA_V1$MOCA_SCORE[MoCA_V1$group=="Asym_to_Sym(A)"]), 
            as.numeric(MoCA_V1$MOCA_SCORE[MoCA_V1$group!="Asym_to_Sym(A)"]) )   


# PDQ39
PDQ39 <- fread("PDQ39.txt")
PDQ39 <- PDQ39 %>% arrange(SUBJID, VISIT, item) %>%
  spread(key=item, value=value) %>% arrange(SUBJID)
PDQ39 <- PDQ39 %>% filter(PDQ39_SCORE!=0)
PDQ39 <- PDQ39 %>% select(SUBJID, VISIT, PDQ39_SCORE)
PDQ39 %>% inner_join(groups) %>% group_by(VISIT, group) %>% 
  summarise(mean=mean(as.numeric(PDQ39_SCORE), na.rm=T), sd=sd(as.numeric(PDQ39_SCORE), na.rm=T))

PDQ39 <- PDQ39 %>% inner_join(groups)

wilcox.test(as.numeric(PDQ39$PDQ39_SCORE[PDQ39$group=="Asym_to_Sym(A)"&PDQ39$VISIT==0]), 
            as.numeric(PDQ39$PDQ39_SCORE[PDQ39$group!="Asym_to_Sym(A)"&PDQ39$VISIT==0]) )   

wilcox.test(as.numeric(PDQ39$PDQ39_SCORE[PDQ39$group=="Asym_to_Sym(A)"&PDQ39$VISIT==1]), 
            as.numeric(PDQ39$PDQ39_SCORE[PDQ39$group!="Asym_to_Sym(A)"&PDQ39$VISIT==1]) )   

# ---------------
# Correlation VTA coverage for SNr vs Delta Axial Symptoms PreOP OFF vs PostOP ON DBS ------------
UPDRSIII_COMPLET_V0_V1 <- read_xlsx(path="Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)
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





ONOFF_After <- data.frame(df_names) %>%
  filter(grepl("^ONOFF", df_names)) %>%
  filter(grepl("3.9", df_names)|
           grepl("3.10", df_names)|
           grepl("3.11", df_names)|
           grepl("3.12", df_names)
  ) %>%
  arrange(df_names) 

toString(as.list(ONOFF_After))

match <- c("ONOFF_3.10_", "ONOFF_3.11_", "ONOFF_3.12_", "ONOFF_3.9_")

match <- append("SUBJID", match)

which_names <- which(names(UPDRSIII_COMPLET_V0_V1) %in%  match)

ONOFF_After <- UPDRSIII_COMPLET_V0_V1[which_names]
ONOFF_After <- ONOFF_After[-1,]


ONOFF_After <- ONOFF_After %>% mutate(ONOFF_3.10_  =as.numeric(ONOFF_3.10_  ),
                                      ONOFF_3.11_  =as.numeric(ONOFF_3.11_  ),
                                      ONOFF_3.12_  =as.numeric(ONOFF_3.12_  ),
                                      ONOFF_3.9_=as.numeric(ONOFF_3.9_)
) %>% drop_na()



OFF_before$AxialScoreOFFbefore <- OFF_before$OFF_3.9_ + OFF_before$OFF_3.10_ + OFF_before$OFF_3.11_ + OFF_before$OFF_3.12_
ONOFF_After$AxialScoreONOFF <- ONOFF_After$ONOFF_3.9_  + ONOFF_After$ONOFF_3.10_  + ONOFF_After$ONOFF_3.11_  + ONOFF_After$ONOFF_3.12_ 

Axial_scores <- OFF_before %>% select(SUBJID, AxialScoreOFFbefore) %>%
  inner_join(ONOFF_After %>% select(SUBJID, AxialScoreONOFF)) 

Axial_scores <- Axial_scores %>% 
  rename("Pre_OP"="AxialScoreOFFbefore") %>%
  rename("ON_DBS_OFF_Med"="AxialScoreONOFF") 

Axial_scores$Delta <- (Axial_scores$ON_DBS_OFF_Med - Axial_scores$Pre_OP)



VTA_patients_MB <- read_xlsx(path="VTA_patients_MB.xlsx", skip=0,  trim_ws = TRUE)

names(VTA_patients_MB)

VTA_patients_MB <- VTA_patients_MB[,c("ID",
                                      "Group",
                                      "Zhang_Right_lateral_SNc_somatomotor",
                                      "Zhang_Right_medial_SNc_limbic",
                                      "Zhang_Right_ventral_SN_associative",
                                      "Zhang_Left_lateral_SNc_somatomotor",
                                      "Zhang_Left_medial_SNc_limbic",
                                      "Zhang_Left_ventral_SN_associative")]

VTA_patients_MB <- VTA_patients_MB %>% filter(Group=="B")

VTA_patients_MB <- VTA_patients_MB %>% rename("SUBJID"="ID")

Axial_scores <- Axial_scores %>% select(c(SUBJID, Delta))

Axial_scores <- Axial_scores %>% inner_join(VTA_patients_MB)

Axial_scores %>% group_by(Group) %>% summarise(mean=mean(Delta))

Deltas <- Axial_scores$Delta  

Subs_nigra_cov <- Axial_scores[, c("Zhang_Right_lateral_SNc_somatomotor",
                                   "Zhang_Right_medial_SNc_limbic",
                                   "Zhang_Right_ventral_SN_associative",
                                   "Zhang_Left_lateral_SNc_somatomotor",
                                   "Zhang_Left_medial_SNc_limbic",
                                   "Zhang_Left_ventral_SN_associative")]



results <- data.frame(column = character(), correlation = numeric(), p_value = numeric())


for (col in colnames(Subs_nigra_cov)) {
  test <- cor.test(Deltas, Subs_nigra_cov[[col]], method = "spearman")
  results <- rbind(results, data.frame(column = col, correlation = test$estimate, p_value = test$p.value))
}

print(results)




# ---------------

# Compare VTA coverage for STN |OR| SNr and Groups Asym->Sym |OR| Asym->Asym ------------


VTA_patients_MB <- read_xlsx(path="VTA_patients_MB.xlsx", skip=0,  trim_ws = TRUE)

names(VTA_patients_MB)

VTA_patients_MB <- VTA_patients_MB[,c("ID",
                                      "Group",
                                      "Ewert_Right_STN_n",
                                      "Ewert_Right_STN_motor_n",
                                      "Ewert_Right_STN_associative_n",
                                      "Ewert_Right_STN_limbic_n",
                                      "Ewert_Left_STN_n",
                                      "Ewert_Left_STN_motor_n",
                                      "Ewert_Left_STN_associative_n",
                                      "Ewert_Left_STN_limbic_n")]




grouped_data <- split(VTA_patients_MB, VTA_patients_MB$Group)

cols_to_test <- colnames(VTA_patients_MB)[colnames(VTA_patients_MB) %in% c("Ewert_Right_STN_n", 
                                                                           "Ewert_Right_STN_motor_n", 
                                                                           "Ewert_Right_STN_associative_n", 
                                                                           "Ewert_Right_STN_limbic_n",
                                                                           "Ewert_Left_STN_n",
                                                                           "Ewert_Left_STN_motor_n",
                                                                           "Ewert_Left_STN_associative_n",
                                                                           "Ewert_Left_STN_limbic_n")]

results <- data.frame(column = character(), 
                      W = numeric(), 
                      p_value = numeric(),
                      Group1_Mean = numeric(), 
                      Group1_SD = numeric(),
                      Group2_Mean = numeric(), 
                      Group2_SD = numeric())


for (col in cols_to_test) {
  group1 <- grouped_data[[1]][[col]]  
  group2 <- grouped_data[[2]][[col]]  
  
  test <- wilcox.test(group1, group2)
  
  group1_mean <- mean(group1, na.rm = TRUE)
  group1_sd <- sd(group1, na.rm = TRUE)
  group2_mean <- mean(group2, na.rm = TRUE)
  group2_sd <- sd(group2, na.rm = TRUE)
  
  # Store the results
  results <- rbind(results, data.frame(column = col, 
                                       W = test$statistic, 
                                       p_value = round(test$p.value,4),
                                       Group1_Mean = round(group1_mean,2), 
                                       Group1_SD = round(group1_sd,2),
                                       Group2_Mean = round(group2_mean,2), 
                                       Group2_SD = round(group2_sd,2)))
}


results






VTA_patients_MB <- read_xlsx(path="VTA_patients_MB.xlsx", skip=0,  trim_ws = TRUE)

names(VTA_patients_MB)

VTA_patients_MB <- VTA_patients_MB[,c("ID",
                                      "Group",
                                      "Zhang_Right_lateral_SNc_somatomotor",
                                      "Zhang_Right_medial_SNc_limbic",
                                      "Zhang_Right_ventral_SN_associative",
                                      "Zhang_Left_lateral_SNc_somatomotor",
                                      "Zhang_Left_medial_SNc_limbic",
                                      "Zhang_Left_ventral_SN_associative")]




grouped_data <- split(VTA_patients_MB, VTA_patients_MB$Group)

cols_to_test <- colnames(VTA_patients_MB)[colnames(VTA_patients_MB) %in% c("Zhang_Right_lateral_SNc_somatomotor",
                                                                           "Zhang_Right_medial_SNc_limbic",
                                                                           "Zhang_Right_ventral_SN_associative",
                                                                           "Zhang_Left_lateral_SNc_somatomotor",
                                                                           "Zhang_Left_medial_SNc_limbic",
                                                                           "Zhang_Left_ventral_SN_associative")]

results <- data.frame(column = character(), 
                      W = numeric(), 
                      p_value = numeric(),
                      Group1_Mean = numeric(), 
                      Group1_SD = numeric(),
                      Group2_Mean = numeric(), 
                      Group2_SD = numeric())


for (col in cols_to_test) {
  group1 <- grouped_data[[1]][[col]]  
  group2 <- grouped_data[[2]][[col]]  
  
  test <- wilcox.test(group1, group2)
  
  group1_mean <- mean(group1, na.rm = TRUE)
  group1_sd <- sd(group1, na.rm = TRUE)
  group2_mean <- mean(group2, na.rm = TRUE)
  group2_sd <- sd(group2, na.rm = TRUE)
  
  # Store the results
  results <- rbind(results, data.frame(column = col, 
                                       W = test$statistic, 
                                       p_value = round(test$p.value,4),
                                       Group1_Mean = round(group1_mean,4), 
                                       Group1_SD = round(group1_sd,4),
                                       Group2_Mean = round(group2_mean,4), 
                                       Group2_SD = round(group2_sd,4)))
}


results
# -----------------------
# Other Asymetry SM vs no SM ----
Asymmetry_Pre_vs_Post


test <- SITES_df %>% mutate(exp=1) %>%
  spread(key=SITE, value=exp) %>%
  mutate(Other=ifelse(is.na(Other),0,Other)) %>%
  mutate(STN_SM=ifelse(is.na(STN_SM),0,STN_SM)) %>%
  mutate(SM=ifelse(STN_SM==1,1,0)) %>% select(-c(Other, STN_SM))  %>%
  spread(key=feature, value=SM) %>%
  mutate(L=ifelse(is.na(L),0,L)) %>%
  mutate(R=ifelse(is.na(R),0,R)) %>%
  inner_join(Asymmetry_Pre_vs_Post %>% select(SUBJID, ON_ON)) %>%
  mutate(Both=ifelse(L+R>1,1,0)) %>%
  mutate(Any=ifelse(L+R>0,1,0)) %>%
  select(-c(L,R))

range(test$ON_ON)

wilcox.test(as.numeric(test$ON_ON[test$Both==1]), 
            as.numeric(test$ON_ON[test$Both==0]) ) 

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  as.numeric(test$ON_ON[test$Both == 1]) and as.numeric(test$ON_ON[test$Both == 0])
# W = 1106.5, p-value = 0.9663
# alternative hypothesis: true location shift is not equal to 0


wilcox.test(as.numeric(test$ON_ON[test$Any==1]), 
            as.numeric(test$ON_ON[test$Any==0]) ) 

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  as.numeric(test$ON_ON[test$Any == 1]) and as.numeric(test$ON_ON[test$Any == 0])
# W = 1300.5, p-value = 0.2881
# alternative hypothesis: true location shift is not equal to 0

# ---
# --------------
# Correlation VTA coverage vs asymetry  ------------

VTA_patients_MB <- read_xlsx(path="VTA_patients_MB.xlsx", skip=0,  trim_ws = TRUE)

names(VTA_patients_MB)

VTA_patients_MB <- VTA_patients_MB[,c("ID",
                                      "Group",
                                      "Zhang_Right_lateral_SNc_somatomotor",
                                      "Zhang_Right_medial_SNc_limbic",
                                      "Zhang_Right_ventral_SN_associative",
                                      "Zhang_Left_lateral_SNc_somatomotor",
                                      "Zhang_Left_medial_SNc_limbic",
                                      "Zhang_Left_ventral_SN_associative")]

VTA_patients_MB <- VTA_patients_MB %>% filter(Group=="B")

VTA_patients_MB <- VTA_patients_MB %>% rename("SUBJID"="ID")

Asymmetry_Pre_vs_Post <- fread("Asymmetry_Pre_vs_Post.txt")
Asymmetry_Pre_vs_Post <- Asymmetry_Pre_vs_Post %>% select(SUBJID, ON_ON)

VTA_patients_MB <- Asymmetry_Pre_vs_Post %>% inner_join(VTA_patients_MB)

names(VTA_patients_MB)

cor.test(VTA_patients_MB$ON_ON,  VTA_patients_MB$Zhang_Right_lateral_SNc_somatomotor ,  method = "spearman")
# 	Spearman's rank correlation rho
# 
# data:  VTA_patients_MB$ON_ON and VTA_patients_MB$Zhang_Right_lateral_SNc_somatomotor
# S = 1131.2, p-value = 0.5068
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1673755 

cor.test(VTA_patients_MB$ON_ON,  VTA_patients_MB$Zhang_Right_medial_SNc_limbic , method = "spearman")
# 	Spearman's rank correlation rho
# 
# data:  VTA_patients_MB$ON_ON and VTA_patients_MB$Zhang_Right_medial_SNc_limbic
# S = 1148.6, p-value = 0.4616
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1853268 

cor.test(VTA_patients_MB$ON_ON,  VTA_patients_MB$Zhang_Right_ventral_SN_associative,  method = "spearman")
# 	Spearman's rank correlation rho
# 
# data:  VTA_patients_MB$ON_ON and VTA_patients_MB$Zhang_Right_ventral_SN_associative
# S = 1295.1, p-value = 0.1721
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.3365383 

cor.test(VTA_patients_MB$ON_ON,  VTA_patients_MB$Zhang_Left_lateral_SNc_somatomotor ,method = "spearman" )
# 	Spearman's rank correlation rho
# 
# data:  VTA_patients_MB$ON_ON and VTA_patients_MB$Zhang_Left_lateral_SNc_somatomotor
# S = 857.44, p-value = 0.6492
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.1151338 

cor.test(VTA_patients_MB$ON_ON,  VTA_patients_MB$Zhang_Left_medial_SNc_limbic,method = "spearman" )
# 	Spearman's rank correlation rho
# 
# data:  VTA_patients_MB$ON_ON and VTA_patients_MB$Zhang_Left_medial_SNc_limbic
# S = 1335.8, p-value = 0.1214
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.3785046 

cor.test(VTA_patients_MB$ON_ON,  VTA_patients_MB$Zhang_Left_ventral_SN_associative ,method = "spearman")
# 	Spearman's rank correlation rho
# 
# data:  VTA_patients_MB$ON_ON and VTA_patients_MB$Zhang_Left_ventral_SN_associative
# S = 1103.4, p-value = 0.5831
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.1387136 


# ---------------

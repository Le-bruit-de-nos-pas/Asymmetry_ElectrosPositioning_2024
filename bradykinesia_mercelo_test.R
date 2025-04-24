library(tidyverse)
library(data.table)

UPDRSIII_COMPLET_V0_V1 <- readxl::read_xlsx(path="data/Asymmetry_DeepBrainStimulation.xlsx",sheet = "UPDRSIII_COMPLET_V0_V1", skip=0, col_types = "text", trim_ws = TRUE)

names(UPDRSIII_COMPLET_V0_V1)

df_names <- names(UPDRSIII_COMPLET_V0_V1)


Item3.11 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.11_1,ONOFF_3.11_,OFFON_3.11_, ON_3.11_6)
Item3.11 <- Item3.11[-1,]


names(Item3.11) <- c("SUBJID", "OFF_After", "ONOFF_After", "OFFON_After", "ONON_After")
Item3.11 <- data.frame(Item3.11) %>% mutate_each(as.numeric, OFF_After:ONON_After)
names(Item3.11) <- c("SUBJID", "OFF_After_3.11", "ONOFF_After_3.11",  "OFFON_After_3.11", "ONON_After_3.11")



Item3.10 <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, OFF_3.10_1,ONOFF_3.10_,OFFON_3.10_, ON_3.10_6)
Item3.10 <- Item3.10[-1,]
names(Item3.10) <- c("SUBJID", "OFF_After", "ONOFF_After", "OFFON_After", "ONON_After")
Item3.10 <- data.frame(Item3.10) %>% mutate_each(as.numeric, OFF_After:ONON_After)
names(Item3.10) <- c("SUBJID", "OFF_After_3.10", "ONOFF_After_3.10",  "OFFON_After_3.10", "ONON_After_3.10")


Item3.4_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.4_Right_,	OFF_3.4_Right_1,	OFFON_3.4_Right_,	ON_3.4_Right_)
Item3.4_right <- Item3.4_right[-1,]
names(Item3.4_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.4_right <- data.frame(Item3.4_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.4_right) <- c("SUBJID", "ONOFF_After_3.4_right", "OFF_After_3.4_right",  "OFFON_After_3.4_right", "ONON_After_3.4_right")
Item3.4_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID,ONOFF_3.4_Left_	, OFF_3.4_Left_1	, OFFON_3.4_Left_	, ON_3.4_Left_)
Item3.4_left <- Item3.4_left[-1,]
names(Item3.4_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.4_left <- data.frame(Item3.4_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.4_left) <- c("SUBJID", "ONOFF_After_3.4_left", "OFF_After_3.4_left",  "OFFON_After_3.4_left", "ONON_After_3.4_left")


Item3.5_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.5_Right_	, OFF_3.5_Right_1	, OFFON_3.5_Right_	, ON_3.5_Right_)
Item3.5_right <- Item3.5_right[-1,]
names(Item3.5_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.5_right <- data.frame(Item3.5_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.5_right) <- c("SUBJID", "ONOFF_After_3.5_right", "OFF_After_3.5_right",  "OFFON_After_3.5_right", "ONON_After_3.5_right")

Item3.5_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.5_Left_	, OFF_3.5_Left_1	, OFFON_3.5_Left_	, ON_3.5_Left_)
Item3.5_left <- Item3.5_left[-1,]
names(Item3.5_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.5_left <- data.frame(Item3.5_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.5_left) <- c("SUBJID", "ONOFF_After_3.5_left", "OFF_After_3.5_left",  "OFFON_After_3.5_left", "ONON_After_3.5_left")


Item3.6_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.6_Right_	, OFF_3.6_Right_1	, OFFON_3.6_Right_	, ON_3.6_Right_)
Item3.6_right <- Item3.6_right[-1,]
names(Item3.6_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.6_right <- data.frame(Item3.6_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.6_right) <- c("SUBJID", "ONOFF_After_3.6_right", "OFF_After_3.6_right",  "OFFON_After_3.6_right", "ONON_After_3.6_right")
Item3.6_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.6_Left_	, OFF_3.6_Left_1	, OFFON_3.6_Left_	, ON_3.6_Left_)
Item3.6_left <- Item3.6_left[-1,]
names(Item3.6_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.6_left <- data.frame(Item3.6_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.6_left) <- c("SUBJID", "ONOFF_After_3.6_left", "OFF_After_3.6_left",  "OFFON_After_3.6_left", "ONON_After_3.6_left")


Item3.7_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.7_Right_	, OFF_3.7_Right_1	, OFFON_3.7_Right_	, ON_3.7_Right_)
Item3.7_right <- Item3.7_right[-1,]
names(Item3.7_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.7_right <- data.frame(Item3.7_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.7_right) <- c("SUBJID", "ONOFF_After_3.7_right", "OFF_After_3.7_right",  "OFFON_After_3.7_right", "ONON_After_3.7_right")
Item3.7_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.7_Left	, OFF_3.7_Left1	, OFFON_3.7_Left,	ON_3.7_Left)
Item3.7_left <- Item3.7_left[-1,]
names(Item3.7_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.7_left <- data.frame(Item3.7_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.7_left) <- c("SUBJID", "ONOFF_After_3.7_left", "OFF_After_3.7_left",  "OFFON_After_3.7_left", "ONON_After_3.7_left")


Item3.8_right <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.8_Right_	, OFF_3.8_Right_1	, OFFON_3.8_Right_	, ON_3.8_Right_6)
Item3.8_right <- Item3.8_right[-1,]
names(Item3.8_right) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.8_right <- data.frame(Item3.8_right) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.8_right) <- c("SUBJID", "ONOFF_After_3.8_right", "OFF_After_3.8_right",  "OFFON_After_3.8_right", "ONON_After_3.8_right")
Item3.8_left <- UPDRSIII_COMPLET_V0_V1 %>% select(SUBJID, ONOFF_3.8_Left	, OFF_3.8_Left1	, OFFON_3.8_Left	,ON_3.8_Left6)
Item3.8_left <- Item3.8_left[-1,]
names(Item3.8_left) <- c("SUBJID", "ONOFF_After", "OFF_After",  "OFFON_After", "ONON_After")
Item3.8_left <- data.frame(Item3.8_left) %>% mutate_each(as.numeric, ONOFF_After:ONON_After)
names(Item3.8_left) <- c("SUBJID", "ONOFF_After_3.8_left", "OFF_After_3.8_left",  "OFFON_After_3.8_left", "ONON_After_3.8_left")



Deltas_Gait <- Item3.10 %>% inner_join(Item3.11) %>%
  mutate(Gait_OFF=OFF_After_3.10+OFF_After_3.11 ) %>%
  mutate(Gait_Med=ONOFF_After_3.10+ONOFF_After_3.11 ) %>%
  mutate(Gait_Stim=OFFON_After_3.11+OFFON_After_3.11 ) %>%
  mutate(Gait_Med=100*(Gait_OFF-Gait_Med )/Gait_OFF)  %>%
  mutate(Gait_Stim=100*(Gait_OFF-Gait_Stim )/Gait_OFF)  %>%
  select(SUBJID, Gait_Med, Gait_Stim) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


BradyUp <- Item3.4_left %>% select(SUBJID, ONOFF_After_3.4_left, ONOFF_After_3.4_left, OFF_After_3.4_left, OFFON_After_3.4_left) %>%
  inner_join(Item3.4_right %>% select(SUBJID, ONOFF_After_3.4_right, OFF_After_3.4_right, OFFON_After_3.4_right)) %>%
  mutate(ONOFF_After_3.4=ONOFF_After_3.4_left+ONOFF_After_3.4_right) %>%
  mutate(OFFON_After_3.4=OFFON_After_3.4_left+OFFON_After_3.4_right) %>%
  mutate(OFF_After_3.4=OFF_After_3.4_left+OFF_After_3.4_right) %>%
  select(SUBJID, OFF_After_3.4, ONOFF_After_3.4, OFFON_After_3.4) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  inner_join(
    Item3.5_left %>% select(SUBJID, ONOFF_After_3.5_left, ONOFF_After_3.5_left, OFF_After_3.5_left, OFFON_After_3.5_left) %>%
  inner_join(Item3.5_right %>% select(SUBJID, ONOFF_After_3.5_right, OFF_After_3.5_right, OFFON_After_3.5_right)) %>%
  mutate(ONOFF_After_3.5=ONOFF_After_3.5_left+ONOFF_After_3.5_right) %>%
  mutate(OFFON_After_3.5=OFFON_After_3.5_left+OFFON_After_3.5_right) %>%
  mutate(OFF_After_3.5=OFF_After_3.5_left+OFF_After_3.5_right) %>%
  select(SUBJID, OFF_After_3.5, ONOFF_After_3.5, OFFON_After_3.5) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  inner_join(
    Item3.6_left %>% select(SUBJID, ONOFF_After_3.6_left, ONOFF_After_3.6_left, OFF_After_3.6_left, OFFON_After_3.6_left) %>%
  inner_join(Item3.6_right %>% select(SUBJID, ONOFF_After_3.6_right, OFF_After_3.6_right, OFFON_After_3.6_right)) %>%
  mutate(ONOFF_After_3.6=ONOFF_After_3.6_left+ONOFF_After_3.6_right) %>%
  mutate(OFFON_After_3.6=OFFON_After_3.6_left+OFFON_After_3.6_right) %>%
  mutate(OFF_After_3.6=OFF_After_3.6_left+OFF_After_3.6_right) %>%
  select(SUBJID, OFF_After_3.6, ONOFF_After_3.6, OFFON_After_3.6) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  mutate(OFF_After_BradyUp=OFF_After_3.6+OFF_After_3.5+OFF_After_3.4) %>%
  mutate(ONOFF_After_BradyUp=ONOFF_After_3.6+ONOFF_After_3.5+ONOFF_After_3.4) %>%
  mutate(OFFON_After_BradyUp=OFFON_After_3.6+OFFON_After_3.5+OFFON_After_3.4) %>%
  select(SUBJID, OFF_After_BradyUp, ONOFF_After_BradyUp, OFFON_After_BradyUp) %>%
  mutate(BradyUp_Med=100*(OFF_After_BradyUp-ONOFF_After_BradyUp )/OFF_After_BradyUp) %>%
  mutate(BradyUp_Stim=100*(OFF_After_BradyUp-OFFON_After_BradyUp )/OFF_After_BradyUp) %>%
  select(SUBJID, BradyUp_Med, BradyUp_Stim)  %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


BradyDown <- Item3.7_left %>% select(SUBJID, ONOFF_After_3.7_left, ONOFF_After_3.7_left, OFF_After_3.7_left, OFFON_After_3.7_left) %>%
  inner_join(Item3.7_right %>% select(SUBJID, ONOFF_After_3.7_right, OFF_After_3.7_right, OFFON_After_3.7_right)) %>%
  mutate(ONOFF_After_3.7=ONOFF_After_3.7_left+ONOFF_After_3.7_right) %>%
  mutate(OFFON_After_3.7=OFFON_After_3.7_left+OFFON_After_3.7_right) %>%
  mutate(OFF_After_3.7=OFF_After_3.7_left+OFF_After_3.7_right) %>%
  select(SUBJID, OFF_After_3.7, ONOFF_After_3.7, OFFON_After_3.7) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  inner_join(
    Item3.8_left %>% select(SUBJID, ONOFF_After_3.8_left, ONOFF_After_3.8_left, OFF_After_3.8_left, OFFON_After_3.8_left) %>%
  inner_join(Item3.8_right %>% select(SUBJID, ONOFF_After_3.8_right, OFF_After_3.8_right, OFFON_After_3.8_right)) %>%
  mutate(ONOFF_After_3.8=ONOFF_After_3.8_left+ONOFF_After_3.8_right) %>%
  mutate(OFFON_After_3.8=OFFON_After_3.8_left+OFFON_After_3.8_right) %>%
  mutate(OFF_After_3.8=OFF_After_3.8_left+OFF_After_3.8_right) %>%
  select(SUBJID, OFF_After_3.8, ONOFF_After_3.8, OFFON_After_3.8) %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  ) %>%
  mutate(OFF_After_BradyDown=OFF_After_3.7+OFF_After_3.8) %>%
  mutate(ONOFF_After_BradyDown=ONOFF_After_3.7+ONOFF_After_3.8) %>%
  mutate(OFFON_After_BradyDown=OFFON_After_3.7+OFFON_After_3.8) %>%
  select(SUBJID, OFF_After_BradyDown, ONOFF_After_BradyDown, OFFON_After_BradyDown) %>%
  mutate(BradyDown_Med=100*(OFF_After_BradyDown-ONOFF_After_BradyDown )/OFF_After_BradyDown) %>%
  mutate(BradyDown_Stim=100*(OFF_After_BradyDown-OFFON_After_BradyDown )/OFF_After_BradyDown) %>%
  select(SUBJID, BradyDown_Med, BradyDown_Stim)  %>%
  drop_na() %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


BradyDown
BradyUp
Deltas_Gait

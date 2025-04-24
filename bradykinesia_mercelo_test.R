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


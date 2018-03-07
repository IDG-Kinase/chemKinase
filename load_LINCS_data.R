library(BerginskiRMisc)
library(here)
library(tidyverse)

LINCS_KINOMEscan_Kd = read.csv.all(here('LINCS/Data Files/Kd/*'));

LINCS_KINOMEscan_Kd_noNA = LINCS_KINOMEscan_Kd %>%
  filter(!is.na(Kd))

write.csv(LINCS_KINOMEscan_Kd,here('LINCS_KINOMEscan_Kd.csv'), row.names = F)
write.csv(LINCS_KINOMEscan_Kd_noNA,here('LINCS_KINOMEscan_Kd_noNA.csv'), row.names = F)

LINCS_KINOMEscan_percent = read.csv.all(here('LINCS/Data Files/percent_control/*'));
LINCS_KINOMEscan_percent = LINCS_KINOMEscan_percent %>%
  rename(Percent.Control=X..Control)

write.csv(LINCS_KINOMEscan_percent,here('LINCS_KINOMEscan_percent.csv'), row.names = F)
library(BerginskiRMisc)
library(here)

LINCS_KINOMEscan_Kd = read.csv.all(here('LINCS/Data Files/Kd/*'));

LINCS_KINOMEscan_Kd_noNA = LINCS_KINOMEscan_Kd %>%
  filter(!is.na(Kd))

write.csv(LINCS_KINOMEscan_Kd,'LINCS_KINOMEscan_Kd.csv', row.names = F)
write.csv(LINCS_KINOMEscan_Kd_noNA,'LINCS_KINOMEscan_Kd_noNA.csv', row.names = F)

LINCS_KINOMEscan_percent = read.csv.all(here('LINCS/Data Files/percent_control/*'));

LINCS_KINOMEscan_percent_noNA = LINCS_KINOMEscan_percent

write.csv(LINCS_KINOMEscan_percent,'LINCS_KINOMEscan_percent.csv', row.names = F)
write.csv(LINCS_KINOMEscan_percent_noNA,'LINCS_KINOMEscan_percent_noNA.csv', row.names = F)
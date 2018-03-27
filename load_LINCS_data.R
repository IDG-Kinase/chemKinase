library(BerginskiRMisc)
library(here)
library(tidyverse)
library(readxl)

###############################################################################
# Data Loading and Processing
###############################################################################

LINCS_KINOMEscan_Kd = read.csv.all(here('LINCS/Data Files/Kd/*'));

LINCS_KINOMEscan_Kd_noNA = LINCS_KINOMEscan_Kd %>%
  filter(!is.na(Kd))

write.csv(LINCS_KINOMEscan_Kd,here('LINCS_KINOMEscan_Kd.csv'), row.names = F)
write.csv(LINCS_KINOMEscan_Kd_noNA,here('LINCS_KINOMEscan_Kd_noNA.csv'), row.names = F)

LINCS_KINOMEscan_percent = read.csv.all(here('LINCS/Data Files/percent_control/*'));
LINCS_KINOMEscan_percent = LINCS_KINOMEscan_percent %>%
  rename(Percent.Control=X..Control) %>%
  select(-one_of('Small.Molecule.HMS.LINCS.ID','Protein.HMS.LINCS.ID'))

write.csv(LINCS_KINOMEscan_percent,here('LINCS_KINOMEscan_percent.csv'), row.names = F)
saveRDS(LINCS_KINOMEscan_percent,here('LINCS_KINOMEscan_percent.rds'))

UNC_KINOMEscan_percent = read_excel(here('UNC/journal.pone.0181585.s004.xlsx'))
UNC_KINOMEscan_percent = UNC_KINOMEscan_percent %>% 
  select(-one_of('Chemotype','Smiles','>90','>80','>70','Regno')) %>%
  gather(Protein.Name,Percent.Control,-Compound) %>%
  rename(Small.Molecule.Name = Compound) %>%
  mutate(Assay.compound.conc = 1, Conc.unit = 'uM')

write.csv(LINCS_KINOMEscan_percent,here('UNC_KINOMEscan_percent.csv'), row.names = F)
saveRDS(LINCS_KINOMEscan_percent,here('UNC_KINOMEscan_percent.rds'))

###############################################################################
# Glue Data Sets Together and Save
###############################################################################

full_percent_set = rbind(LINCS_KINOMEscan_percent,UNC_KINOMEscan_percent)

saveRDS(full_percent_set,here('shiny','data','Full_KINOMEscan_percent.rds'))
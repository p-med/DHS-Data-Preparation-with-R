# =============================================================================
# DHS MOZAMBIQUE 2022 DATA PROCESSING SCRIPT
# =============================================================================
# Purpose: Process Mozambique DHS 2022 data to create cluster-level indicators
# Author: Paulo Medina
# Date: 9/10/2025
# Input: MZIR files (.CSV format) and GE file (.dbf)
# Output: Cluster-level aggregated indicators
# =============================================================================

# Load necessary libraries for data manipulation and analysis
library(tidyverse)  # most variable creation here uses tidyverse 
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)      # for creating tables with Haven labeled data
library(openxlsx)   # for exporting to excel
library(naniar)     # to use replace_with_na function
library(here)       # to get R project path
library(survey)     # survey weight data to find median ages
library(foreign)    # to read DBF file
library(stringr)    # to manage MCASEID and CASEID


# -----------------------------------------------------------------------------
# LOAD AND PREPARE RAW DATA
# -----------------------------------------------------------------------------

# Load household-level data files
hh_basic <- read.csv(here("Data", "MZIR80", "RECH0.CSV"), sep = ";")        # Basic household information
hh_schedule <- read.csv(here("Data", "MZIR80", "RECH1.CSV"), sep = ";")     # Household roster
hh_chars <- read.csv(here("Data", "MZIR80", "RECH3.CSV"), sep = ";")        # Household characteristics
child_nut_data <- read.csv(here("Data", "MZIR80", "RECH6.CSV"), sep = ";")  # Child nutrition measurements

# Load individual women's data files  
indiv_basic <- read.csv(here("Data", "MZIR80", "REC01.CSV"), sep = ";")     # Basic respondent data
indiv_basic_cont <- read.csv(here("Data", "MZIR80", "REC11.CSV"), sep = ";")# Basic respondent data continued
repro_history <- read.csv(here("Data", "MZIR80", "REC21.CSV"), sep = ";")   # Birth history
preg_history <- read.csv(here("Data", "MZIR80", "REC22.CSV"), sep = ";")    # Pregnancy outcomes
repro_chars <- read.csv(here("Data", "MZIR80", "REC23.CSV"), sep = ";")     # Reproductive characteristics
maternity <- read.csv(here("Data", "MZIR80", "REC41.CSV"), sep = ";")       # Maternity care
mrg_data <- read.csv(here("Data", "MZIR80", "REC51.CSV"), sep = ";")        # Marriage and sexual activity
dv_data <- read.csv(here("Data", "MZIR80", "RECDV.CSV"), sep = ";")         # Domestic Violence

# Load individual men's data files
indiv_basic_m <- read.csv(here("Data", "MZMR80", "MREC01.CSV"), sep = ";")   # Basic respondent data
indiv_basic_cont_m <- read.csv(here("Data", "MZMR80", "MREC11.CSV"), sep = ";")# Basic respondent data continued

# Load geographic data file containing cluster coordinates
ge_file <- read.dbf(here("Data", "GE_File", "MZGE81FL.dbf"))                # Cluster geographic coordinates

# Create standardized yes/no labels for binary indicators
yesno = c("Yes" = 1, "No" = 0)

# -----------------------------------------------------------------------------
# MERGE DATASETS
# -----------------------------------------------------------------------------

# Marriage and age at first birth data-----------------------------------------

# Merge marriage data with individual basic data to get current age (V012)
mrg_and_repro_data <- left_join(mrg_data, 
                                indiv_basic[, c("CASEID", "V012", "V005")], by = "CASEID")

# Add age at first birth (V212) from reproductive characteristics
mrg_and_repro_data <- left_join(mrg_and_repro_data, 
                                repro_chars[, c("CASEID", "V212")], by = "CASEID")

# Drop un-used variables
mrg_and_repro_data <- mrg_and_repro_data %>% 
  select(HHID, CASEID, V005, V012, V212, V511)

# Delivery data----------------------------------------------------------------

# Merge maternity data with reproductive history to get Current age of child in 
# months (B19) and months since pregnancy outcome (P19)

# Create unique identifiers by concatenating case ID with birth/pregnancy indices
temp_rep <- repro_history[, c("CASEID","BIDXP", "B19")] # B19 = child's current age in months
temp_rep$ID <- paste0(temp_rep$CASEID,temp_rep$BIDXP) # Create an ID concatenating HHID and Index

temp_preg <- preg_history[, c("CASEID", "PIDXB", "P19")] # P19 = months since pregnancy
temp_preg$ID <- paste0(temp_preg$CASEID,temp_preg$PIDXB) # Create an ID concatenating HHID and Index

# Extract delivery assistance variables
temp_mat <- maternity[,c('HHID', 'CASEID', 'MIDXP', 'M3A', 'M3B', 'M3C', 'M3D', 
                         'M3E', 'M3F', 'M3G', 'M3H', 'M3J', 'M3K', 'M3L', 
                         'M3M', 'M3N', 'M80')]
temp_mat$ID <- paste0(temp_mat$CASEID,temp_mat$MIDXP) # Create an ID concatenating HHID and Index

# Merge all delivery-related data using the constructed IDs
delivery_data <- temp_preg %>%
  left_join(temp_rep, by = "ID") %>%
  left_join(temp_mat, by = "ID")

# Delete table copies
rm(temp_rep)
rm(temp_preg)
rm(temp_mat)

# Child nutrition--------------------------------------------------------------

# Merge child nutrition data with household schedule to get Height/Age
# standard deviation (new WHO) (HC70) and stayed last night (HV103)

# Filter household schedule for eligible children (HV120=1 indicates eligible for nutrition module)
temp_sch <- subset(hh_schedule, HV120 == 1)
temp_sch$ID <- paste0(temp_sch$HHID, temp_sch$HVIDX) # Create an ID concatenating HHID and Index

nutrition_data <- child_nut_data # Copy nutrition data
nutrition_data$ID <- paste0(nutrition_data$HHID, nutrition_data$HC0) # Create an ID concatenating HHID and Index

# Merge to get both height-for-age z-score (HC70) and residence status (HV103)
nutrition_data <- left_join(nutrition_data[, c("ID","HHID", "HC70")], 
                            temp_sch[, c("ID", "HV103")], by = "ID")
# Delete table copy
rm(temp_sch)

# Literacy---------------------------------------------------------------------

# Merge basic data on respondent REC

W_literacy_data <- left_join(indiv_basic, indiv_basic_cont, by = "CASEID")

m_literacy_data <- left_join(indiv_basic_m, indiv_basic_cont_m, by = "MCASEID")

# Domestic Violence------------------------------------------------------------
# Data from women's basic information, marriage and sexual activity, DV

dv_data_prep <- indiv_basic %>%
  select(HHID, CASEID, V001, V044) %>%
  left_join(mrg_data[,c("CASEID", "V502")], by = "CASEID") %>%
  left_join(dv_data, by = "CASEID")

# Ever-married or ever had intimate partner denominator
dv_data_prep <- dv_data_prep %>%
  mutate(DV_SPV1_W_ANY_den = case_when(V044 == 1 & V502 %in% 1:2 | D100 %in% 1:2 ~ 1, TRUE ~ 0))

# Child registration-----------------------------------------------------------

# Data from hh schedule to be used, no merge needed

# Female and Male education attainment-----------------------------------------
 
# Data from hh schedule to be used, no merge needed

# -----------------------------------------------------------------------------
# CALCULATE INDICATORS AND AGGREGATE TO THE HOUSEHOLD LEVEL
# -----------------------------------------------------------------------------

# Marriage and age at first birth data-----------------------------------------

# Create age group indicator for women 20-24
mrg_and_repro_data <- mrg_and_repro_data %>%
  # V012 is current age of respondent in completed years
  # When the current age of the respondent is between 20 and 24 add 1 else 0
  mutate(age_20_24 = case_when(V012 %in% 20:24 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(age_20_24 = yesno) %>%
  set_variable_labels(age_20_24 = "Woman age 20-24")


# INDICATOR: Women married by age 15
mrg_and_repro_data <- mrg_and_repro_data %>%
  # V511 is age at first union/marriage
  # Only count women 20-24 who married before age 15
  mutate(MA_MBAY_W_B15_num = case_when(age_20_24 == 1 & V511<15 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(MA_MBAY_W_B15_num = yesno) %>%
  set_variable_labels(MA_MBAY_W_B15_num = "First marriage by age 15")

# Denominator: all women 20-24
mrg_and_repro_data$MA_MBAY_W_B15_den = mrg_and_repro_data$age_20_24

# INDICATOR: Women married by age 18
mrg_and_repro_data <- mrg_and_repro_data %>%
  # Only count women 20-24 who married before age 18
  mutate(MA_MBAY_W_B18_num = case_when(age_20_24 == 1 & V511<18 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(MA_MBAY_W_B18_num = yesno) %>%
  set_variable_labels(MA_MBAY_W_B18_num = "First marriage by age 18")

# Denominator: all women 20-24
mrg_and_repro_data$MA_MBAY_W_B18_den = mrg_and_repro_data$age_20_24


# INDICATOR: Women age 20-24 giving birth by age 18
mrg_and_repro_data <- mrg_and_repro_data %>%
  # V212 is age at first birth; >=0 excludes missing values, <18 captures early births
  mutate(FE_BBAY_W_A18_num = case_when(age_20_24 == 1 & V212>=0 & V212<18 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(FE_BBAY_W_A18_num = yesno) %>%
  set_variable_labels(FE_BBAY_W_A18_num = "Young women age 20-24 giving birth by age 18")

# Denominator: Total number of women age 15-49 including those without a birth
mrg_and_repro_data <- mrg_and_repro_data %>%
  mutate(FE_BBAY_W_A18_den = case_when(V012 %in% 20:24 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(FE_BBAY_W_A18_den = yesno) %>%
  set_variable_labels(FE_BBAY_W_A18_den = "Total number of women age 15-49")

# Get the cluster ID
mrg_and_repro_data <- merge(mrg_and_repro_data, hh_basic[,c("HHID", "HV001")], by = "HHID")

# Collapse data to the household ID
mrg_and_repro_data <-mrg_and_repro_data %>%
  # Remove records with missing values
  drop_na(MA_MBAY_W_B15_num,
          MA_MBAY_W_B15_den,
          MA_MBAY_W_B18_num,
          MA_MBAY_W_B18_den,
          FE_BBAY_W_A18_num,
          FE_BBAY_W_A18_den)%>% #Drop data for hh members with NA stunting outcomes
  group_by(HV001) %>% # Group by cluster ID
  summarise(MA_MBAY_W_B15_num = sum(MA_MBAY_W_B15_num),# Sum numerators and denominators
            MA_MBAY_W_B15_den = sum(MA_MBAY_W_B15_den),# within each cluster
            MA_MBAY_W_B18_num = sum(MA_MBAY_W_B18_num),
            MA_MBAY_W_B18_den = sum(MA_MBAY_W_B18_den),
            FE_BBAY_W_A18_num = sum(FE_BBAY_W_A18_num),
            FE_BBAY_W_A18_den = sum(FE_BBAY_W_A18_den))


# INDICATOR: Assistance during delivery by skilled provider
# Set reference period (24 months = 2 years before survey)
delivery_data <- delivery_data %>%
  mutate(period = 24) %>% # 24 Months for 2 years
  mutate(age = B19) # B19 = current age of child in months

# Categorize birth attendant type based on M3 series variables (Code from DHS GitHub)
delivery_data <- delivery_data %>%
  mutate(rh_del_pv =
           case_when(
             M3A == 1   ~ 1 ,
             M3B == 1 | M3C == 1 ~ 2,
             M3D == 1 | M3E == 1 | M3F == 1~ 3 ,
             M3G == 1 ~ 4 ,
             M3H == 1 | M3J == 1 | M3K == 1 | M3L == 1 | M3M == 1 ~ 5 ,
             M3N ==1 ~ 6,
             M3A ==8 | M3A==9 ~ 9 ,
             age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_pv = c(99))) %>%
  set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, 
                                 "Country specific health professional"=3, 
                                 "Traditional birth attendant"=4, 
                                 "Relative/other"=5, "No one"=6, 
                                 "Don't know/missing"=9  )) %>%
  set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

# Create skilled vs unskilled provider categories
delivery_data <- delivery_data %>%
  mutate(rh_del_pvskill =
           case_when(
             rh_del_pv %in% c(1,2)   ~ 1 ,
             rh_del_pv %in% c(3,4,5) ~ 2,
             rh_del_pv ==6 ~ 3,
             rh_del_pv==9 ~ 9,
             age>=period ~ 99)) %>%
  replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
  set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")

# Numerator: Count births with skilled attendance within reference period
delivery_data <- delivery_data %>%
  mutate(RH_DELA_C_SKP_num = case_when(rh_del_pvskill == 1 & age < period ~ 1, TRUE ~ 0)) %>%
  set_value_labels(RH_DELA_C_SKP_num = yesno) %>%
  set_variable_labels(RH_DELA_C_SKP_num = "Skilled assistance during delivery - Numerator")

# Denominator: all births in reference period with known outcome
delivery_data <- delivery_data %>%
  mutate(RH_DELA_C_SKP_den = case_when(M80 %in% 1:4 & age < 24 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(RH_DELA_C_SKP_den = yesno) %>%
  set_variable_labels(RH_DELA_C_SKP_den = "Skilled assistance during delivery - Denominator")

# Keep variables of interest
delivery_data <- delivery_data[, c("HHID", "CASEID", "RH_DELA_C_SKP_num", "RH_DELA_C_SKP_den")]

# Get the cluster ID
delivery_data <- merge(delivery_data, hh_basic[,c("HHID", "HV001")], by = "HHID")

# Collapse data to the household ID
delivery_data <-delivery_data %>%
  drop_na(RH_DELA_C_SKP_num,
          RH_DELA_C_SKP_den)%>% # Remove records with missing values
  group_by(HV001) %>% #Group by Cluster ID
  summarise(RH_DELA_C_SKP_num = sum(RH_DELA_C_SKP_num),
            RH_DELA_C_SKP_den = sum(RH_DELA_C_SKP_den)) 


# CHILDREN SEVERELY STUNTED----------------------------------------------------

# INDICATOR: Children severely stunted
# height-for-age z-score < -3 SD
nutrition_data <- nutrition_data %>%
  mutate(CN_NUTS_C_HA3_num =
           case_when(
             HV103==1 &  HC70< -300  ~ 1 ,             # Severely stunted (z-score < -3.00)
             HV103==1 &  HC70>= -300 & HC70<9996 ~ 0 , # Not severely stunted
             HC70>=9996 ~ 99)) %>%                     # Missing values
  replace_with_na(replace = list(CN_NUTS_C_HA3_num = c(99))) %>%
  set_value_labels(CN_NUTS_C_HA3_num = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(CN_NUTS_C_HA3_num = "Severely stunted child under 5 years")

# Denominator: children with valid measurements who slept in household last night
nutrition_data <- nutrition_data %>%
  # HV103=1 is slept in household last night; HC70<9990 excludes missing values
  mutate(CN_NUTS_C_HA3_den = case_when(HV103 == 1 & HC70 < 9990 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(CN_NUTS_C_HA3_den = yesno) %>%
  set_variable_labels(CN_NUTS_C_HA3_den = "Severely stunted child under 5 years - Denominator")

# Get the cluster ID
nutrition_data <- merge(nutrition_data, hh_basic[,c("HHID", "HV001")], by = "HHID")

# Collapse data to the household ID
nutrition_data <-nutrition_data %>%
  drop_na(CN_NUTS_C_HA3_num,
          CN_NUTS_C_HA3_den)%>% #Drop NA outcomes
  group_by(HV001) %>% #Group by Cluster ID
  summarise(CN_NUTS_C_HA3_num = sum(CN_NUTS_C_HA3_num),
            CN_NUTS_C_HA3_den = sum(CN_NUTS_C_HA3_den))

# CHILDREN REGISTERED ---------------------------------------------------------
# INDICATOR: Children with registered births
child_registration <- hh_schedule %>%
  mutate(CP_BREG_C_REG_num =
           case_when(
             HV102==1 & HV105<5 & HV140 %in% c(1,2) ~ 1, # Registered (has certificate or registered)
             HV102==1 & HV105<5 & HV140 %in% c(0,8,9) ~ 0)) %>% # Not registered/unknown
  set_value_labels(CP_BREG_C_REG_num = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(CP_BREG_C_REG_num = "Children whose births are registered - Numerator")

# Denominator: all children under 5 who are usual household members
child_registration <- child_registration %>%
  # HV102=1 is usual resident; HV105 is age in years
  mutate(CP_BREG_C_REG_den = case_when(HV102 == 1 & HV105 <5 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(CP_BREG_C_REG_den = yesno) %>%
  set_variable_labels(CP_BREG_C_REG_den = "Children whose births are registered - Denominator")

child_registration <- child_registration[, c("HHID", "CP_BREG_C_REG_num", "CP_BREG_C_REG_den")]

# Get the cluster ID
child_registration <- merge(child_registration, hh_basic[,c("HHID", "HV001")], by = "HHID")

# Collapse data to the household ID
child_registration <-child_registration %>%
  drop_na(CP_BREG_C_REG_num,
          CP_BREG_C_REG_den)%>% #Drop NA
  group_by(HV001) %>% #Group by Cluster ID
  summarise(CP_BREG_C_REG_num = sum(CP_BREG_C_REG_num),
            CP_BREG_C_REG_den = sum(CP_BREG_C_REG_den))

# LITERACY INDICATORS
# ED_LITR_W_LIT: Percentage of women who are literate

# Numerator
#  Number of women (or men) age 15-49 literate 
# (women: V106 = 3 or V155 in 1,2; men: MV106 = 3 or MV155 in 1,2)
# Source: https://www.dhsprogram.com/data/Guide-to-DHS-Statistics/index.htm#t=Literacy.htm

#w_literacy <- W_literacy_data %>%
#  mutate(ED_LITR_W_LIT_num = case_when(V012 %in% 15:49 & V106 == 3 | V155 == 2 ~ 1, TRUE ~ 0)) %>%
#  set_value_labels(ED_LITR_W_LIT_num = yesno) %>%
#  set_variable_labels(ED_LITR_W_LIT_num = "Number of women age 15-49 who are literate")

w_literacy <- W_literacy_data %>%
  mutate(rc_litr_cats = case_when(
    !V106 == 3 & V155 == 2 ~ 1,
    !V106 == 3 & V155 == 1 ~ 2,
    !V106 == 3 & V155 == 0 ~ 3,
    !V106 == 3 & V155 == 3 ~ 4,
    !V106 == 3 & V155 == 4 ~ 5,
    V106 == 3 ~ 0),
    rc_litr_cats = set_value_labels(rc_litr_cats, labels = c("Higher than secondary education"=0, "Can read a whole sentence"=1,
                                                       "Can read part of a sentence"=2, "Cannot read at all"=3,
                                                       "No card with required language"=4, "Blind/visually impaired"=5)),
    rc_litr_cats = set_variable_labels(rc_litr_cats, label = "Level of literacy")) %>%
  mutate(ED_LITR_W_LIT_num = case_when(
    V106==3 | V155==1 | V155==2 ~ 1, TRUE ~ 0),
    ED_LITR_W_LIT_num = set_value_labels(ED_LITR_W_LIT_num, labels = c("No"=0, "Yes"=1)),
    ED_LITR_W_LIT_num = set_variable_labels(ED_LITR_W_LIT_num, label = "Literate"))

# Denominator
# Number of women (or men) age 15-49

w_literacy <- w_literacy %>%
  mutate(ED_LITR_W_LIT_den = case_when(V012 %in% 15:49 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ED_LITR_W_LIT_den = yesno) %>%
  set_variable_labels(ED_LITR_W_LIT_den = "Number of women age 15-49")

# Select important variables
w_literacy <- w_literacy %>%
  select(HHID.x, CASEID, V012, V106, V155, ED_LITR_W_LIT_num, ED_LITR_W_LIT_den) %>%
  rename(HHID = HHID.x)

# Get the cluster ID
w_literacy <- merge(w_literacy, hh_basic[,c("HHID", "HV001")], by = "HHID")

# Collapse data to the household ID
w_literacy <-w_literacy %>%
  drop_na(ED_LITR_W_LIT_num,
          ED_LITR_W_LIT_den)%>% #Drop NA
  group_by(HV001) %>% #Group by Cluster ID
  summarise(ED_LITR_W_LIT_num = sum(ED_LITR_W_LIT_num),
            ED_LITR_W_LIT_den = sum(ED_LITR_W_LIT_den))


# ED_LITR_M_LIT: Percentage of men who are illiterate

m_literacy <- m_literacy_data %>%
  mutate(ED_LITR_M_LIT_num = case_when(MV012 %in% 15:49 & MV155 %in% 1:2 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ED_LITR_M_LIT_num = yesno) %>%
  set_variable_labels(ED_LITR_M_LIT_num = "Number of men age 15-49 who are literate")

# Denominator
# Number of women (or men) age 15-49

m_literacy <- m_literacy %>%
  mutate(ED_LITR_M_LIT_den = case_when(MV012 %in% 15:49 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ED_LITR_M_LIT_den = yesno) %>%
  set_variable_labels(ED_LITR_M_LIT_den = "Number of men age 15-49")

m_literacy <- m_literacy %>%
  rename(HV001 = MV001) %>%
  mutate(m_weight = MV005/1000000)

# Collapse data to the household ID
m_literacy <-m_literacy %>%
  drop_na(ED_LITR_M_LIT_num,
          ED_LITR_M_LIT_den,
          m_weight)%>% #Drop NA
  group_by(HV001) %>% #Group by Cluster ID
  summarise(ED_LITR_M_LIT_num = sum(ED_LITR_M_LIT_num),
            ED_LITR_M_LIT_den = sum(ED_LITR_M_LIT_den))


# DOMESTIC VIOLENCE
# DV_SPV1_W_ANY:Percentage of ever married women or never married women who have 
# experienced physical or sexual or emotional violence committed by their 
# husband/partner in the 12 months preceding the survey.

# Numerator
# One or more of the specified acts of intimate partner physical violence (any of d105a – f, j in 1:2)
# or one or more of the specified acts of intimate partner sexual violence (any of d105h, i, k in 1:2)
# or one or more of the specified acts of intimate partner emotional violence (any of d103a, b, c in 1:2)

# Initial Variables

# Any physical violence by partner
dv_data_prep <- dv_data_prep %>%
  mutate(dv_prtnr_phy_12m =
           case_when(
             D105A %in% 1:2 | D105B %in% 1:2 | D105C %in% 1:2 | D105D %in% 1:2 | 
               D105E %in% 1:2 | D105F %in% 1:2 | D105G %in% 1:2 |D105J %in% 1:2 ~ 1, 
             V044==1 & V502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_phy_12m = "Any physical violence in past 12 mos. by partner")

# Any physical violence by any partner
dv_data_prep <- dv_data_prep %>%
  mutate(dv_aprtnr_phy_12m =
           case_when(
             dv_prtnr_phy_12m==1 | D130A==1 ~ 1, 
             V044==1 & V502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_phy_12m = "Experienced physical violence in past 12 mos. by any partner")

# Any sexual violence in past 12 mos. by partner
dv_data_prep <- dv_data_prep %>%
  mutate(dv_prtnr_sex_12m =
           case_when(
             D105H %in% c(1,2) | D105I %in% c(1,2) | D105K %in% c(1,2)  ~ 1, 
             V044==1 & V502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_sex_12m = "Any sexual violence in past 12 mos. by partner")

# Experienced sexual violence in past 12 mos. by any partner
dv_data_prep <- dv_data_prep %>%
  mutate(dv_aprtnr_sex_12m =
           case_when(
             dv_prtnr_sex_12m==1 | D130B==1 ~ 1, 
             V044==1 & V502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_sex_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_sex_12m = "Experienced sexual violence in past 12 mos. by any partner")

# Any emotional violence in past 12 mos. by partner
dv_data_prep <- dv_data_prep %>%
  mutate(dv_prtnr_emot_12m =
           case_when(
             D103A %in% c(1,2) | D103B %in% c(1,2) | D103C %in% c(1,2)  ~ 1, 
             V044==1 & V502>0 ~ 0 )) %>% 
  set_value_labels(dv_prtnr_emot_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_prtnr_emot_12m = "Any emotional violence in past 12 mos. by partner")

# Experienced emotional violence in past 12 mos. by any partner
dv_data_prep <- dv_data_prep %>%
  mutate(dv_aprtnr_emot_12m =
           case_when(
             dv_prtnr_emot_12m==1 | D130C==1 ~ 1, 
             V044==1 & V502>0 ~ 0 )) %>% 
  set_value_labels(dv_aprtnr_emot_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_aprtnr_emot_12m = "Experienced emotional violence in past 12 mos. by any partner")

# Numerator

dv_data_prep <- dv_data_prep %>%
  mutate(DV_SPV1_W_ANY_num =
           case_when(
             dv_aprtnr_phy_12m==1 | dv_aprtnr_sex_12m==1 | dv_aprtnr_emot_12m==1 ~ 1, 
             V044==1 & V502 %in% 1:2 ~ 0 )) %>% 
  set_value_labels(DV_SPV1_W_ANY_num = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(DV_SPV1_W_ANY_num = "Experienced physical OR sexual OR emotional violence in past 12 mos. by any partner")

# Denominator
# Number of ever-married women age 15-49 and never-married women age 15-49 who 
# have ever had an intimate partner selected and interviewed for the domestic 
# violence module (v044 = 1 & (v502 in 1:2 or d100 in 1:2) )

#dv_data_prep <- dv_data_prep %>%
#  mutate(DV_SPV1_W_ANY_den = case_when(V044 == 1 & (V502 %in% 1:2 | D100 %in% 1:2) ~ 1, TRUE ~ 0)) %>%
#  set_value_labels(DV_SPV1_W_ANY_den = yesno) %>%
#  set_variable_labels(DV_SPV1_W_ANY_den = "Number of ever-married women age 15-49")
  
dv_data_prep <- dv_data_prep %>%
  rename(HV001 = V001) %>%
  mutate(dv_weight = D005/1000000)

# Collapse data to the household ID
dv_data_prep <-dv_data_prep %>%
  group_by(HV001) %>% #Group by Cluster ID
  summarise(DV_SPV1_W_ANY_num = sum(DV_SPV1_W_ANY_num, na.rm = T),
            DV_SPV1_W_ANY_den = sum(DV_SPV1_W_ANY_den))



# -----------------------------------------------------------------------------
# CLEAN AND SELECT FINAL VARIABLES FOR AGGREGATION TO THE CLUSTER LEVEL
# -----------------------------------------------------------------------------

# Merge household weights (HV005) with individual sampling weights (V005)
hh_basic_subsetted <- hh_basic %>%
  left_join(indiv_basic[,c("HHID", "V005")], by = "HHID")

# Convert weights from integer to decimal format (DHS standard: divide by 1,000,000)
hh_basic_subsetted <- hh_basic_subsetted %>%
  mutate(wt = HV005/1000000)


# Merge all tables
# Keep only the necessary columns
hh_basic_subsetted <- hh_basic_subsetted %>%
  select(wt, HV001) # HV001 = cluster ID

# Aggregate weights to cluster level
hh_basic_aggregated <- hh_basic_subsetted %>%
  group_by(HV001) %>% # Group by cluster ID
  summarise(
    wt = max(wt), # Use max() to get the consistent weight value
    .groups = 'drop' # Remove grouping after summarise
  )


# Merge all data sets to the hh_data-------------------------------------------

MZ2022DHS_Indicators <- hh_basic_aggregated %>%
  # Add all indicators
  left_join(w_literacy, by="HV001") %>%
  left_join(m_literacy, by="HV001") %>%
  left_join(dv_data_prep, by = "HV001") %>%
  left_join(child_registration, by="HV001") %>%
  left_join(nutrition_data, by="HV001") %>%
  left_join(mrg_and_repro_data, by="HV001") %>%
  left_join(delivery_data, by="HV001") %>%
  mutate(across(everything(), ~ replace_na(., 0))) # Replace NA values with 0 for missing clusters


MZ2022DHS_Indicators<-MZ2022DHS_Indicators %>%
  rename(
    DHSCLUST = HV001, #Rename HV001 variable to be DHSCLUST
  )


# Merge GE file to the MZ2022DHS indicators------------------------------------

MZ2022DHS_Indicators <- MZ2022DHS_Indicators %>%
  left_join(ge_file[,c("DHSID", "DHSCC", "DHSYEAR", "DHSCLUST", "SOURCE", "URBAN_RURA", "LATNUM", "LONGNUM")], by = "DHSCLUST") %>%
  mutate(DATASET = "MZGE81", # Data set identifier
         survey_id = "MZ2022DHS") %>% # Survey identifier
  select(1,21:27,3:20) # Reorder columns 

# CREATE COMPARISON TABLE

df_weighted <- MZ2022DHS_Indicators %>%
  # Multiply columns 13 to 32 by weight
  mutate(across(.cols = 13:32, .fns = ~ .x * wt, .names = "mult_{.col}")) %>%
  # Multiply columns 33 to 40 individual sampling weight.
  mutate(across(.cols = 33:40, .fns = ~ .x * women_wt, .names = "mult_{.col}")) %>%
  select(41:68)
  
# for each of the multiplied variables.
df_pivot <- df_weighted %>%
  # Use pivot_longer() to reshape the data, selecting only the new 'mult_' columns.
  pivot_longer(
    cols = starts_with("mult_"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  # Group the data by the 'variable' column
  group_by(variable) %>%
  # Summarize the data by calculating the sum of the 'value' column for each group.
  summarise(total_sum = sum(value))


comparison_indicators <- df_pivot %>%
  # Create a new column to identify the indicator and the type (numerator or denominator)
  mutate(
    Indicator = str_sub(variable, 1, -5),
    type = str_sub(variable, -3, -1)
  ) %>%
  # Pivot the data to have separate columns for 'denominator' and 'numerator'
  pivot_wider(
    id_cols = Indicator,
    names_from = type,
    values_from = total_sum
  ) %>%
  # Calculate the percentage and create the final dataframe
  mutate(Percentage = (num / den) * 100) %>%
  # Select only the indicator and the new percentage column
  select(Indicator, Percentage)
# Remove _mult prefix
comparison_indicators$Indicator <- gsub("mult_", "", comparison_indicators$Indicator)


# Save datasets

write.csv(MZ2022DHS_Indicators, "MZ2022DHS_Indicators.csv")
write.csv(comparison_indicators, "comparison_indicators.csv")

# =============================================================================
# END OF SCRIPT
# =============================================================================
# Final dataset contains:
# - 1 observation per cluster (n = number of clusters in survey)
# - Cluster-level aggregated indicators (numerators and denominators)
# - Survey weights for proper statistical analysis
# - Geographic coordinates for spatial analysis
# - Standard DHS variable naming for comparability
# - Comparison csv: Calculated percentages for each indicator at the national level
# =============================================================================
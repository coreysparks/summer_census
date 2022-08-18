
  

library(tidyverse)
#txlrs <- readRDS(here::here("data", "tx_lrs.rds"))
lrs <- read_csv("C:/Users/ozd504/OneDrive - University of Texas at San Antonio/projects/census_summer/data/pdb2021trv3_us.csv/pdb2021trv3_us.csv") 

tx_lrs <- lrs %>%
  filter(State == "48")




library(brms)

mod_dat <- tx_lrs %>%
  select(College_ACS_15_19, Prs_Blw_Pov_Lev_ACS_15_19, MrdCple_Fmly_HHD_ACS_15_19, NonFamily_HHD_ACS_15_19, Sngl_Prns_HHD_ACS_15_19, Rel_Family_HHD_ACS_15_19, Med_HHD_Inc_ACS_15_19, Renter_Occp_HU_ACS_15_19, Owner_Occp_HU_ACS_15_19, Single_Unit_ACS_15_19, MLT_U2_9_STRC_ACS_15_19, MLT_U10p_ACS_15_19, Med_House_Value_ACS_15_19, Civ_labor_16plus_ACS_15_19, Civ_emp_16plus_ACS_15_19, One_Health_Ins_ACS_15_19, Two_Plus_Health_Ins_ACS_15_19, No_Health_Ins_ACS_15_19, Mail_Return_Rate_CEN_2010) 



y <- data.frame(y =mod_dat$Mail_Return_Rate_CEN_2010)
x<- mod_dat[, -19]

f<-  as.formula(paste("Mail_Return_Rate_CEN_2010 ~ ", paste(names(mod_dat[, -19]), collapse= "+")))

f




m1 <- lm (formula = f, data=mod_dat)
modelsummary::modelsummary(m1)



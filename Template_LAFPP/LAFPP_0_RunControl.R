# Run control file of the LAFPP model

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")
options(dplyr.print_min = 60) # default is 10






source("Functions.R")


# Road map for model files (all with suffix "LAFPP_" and extension ".R")
 
 # Master files:
   # Master_singleTier
   # Master_allTiers

 # Data_RP2000
 # Data_PlanInfo 
 # Data_ImportMemberData

 # Model_decrements
 # Model_InvReturns
 # Model_PrepData
 # Model_Demographics
 # Model_ContingentAnnuity
 # Model_IndivLiab
 # Model_AggLiab
 # Model_Sim


# Notes on service retirement
 # initial service retirees are currently (June27, 2016) modeled as life annuitants. 
 #

# Notes on DROP
  # Major concerns 
   # effect on new entrants
   # effect on EEC
   # DROP participants in initial actives 

   # 1. DROP participants are treated as retirees in the model, they do not affect the determination of new entrants. 
   # 2. Plan to model DROP participants' payroll and EEC, which are not neglectible quantitatively. 
   # 3. When assuming DROP participants are retirees, the fund pays benefits to DROP accounts. 
  
   # 4. Salary growth rate for DROP participants is fixed at 4.75%.
   # 5. No EEC exemption age for contingent annuitants

# Notes on benefit for death before retirement




# Notes on disability benefits
# As of June26 2016
  # all disabilities are assumed as service connected (90% in the AV)
  # disability rate not applied to members eligible to DROP
  # Disability are modeled as life annuity. 
  # Plan to apply an adjustment factor to mortality after disability mortality 
  # # of disabled at age min.age(20) must be 0.
# AS of June30 2016
  # Model disability benefit as contingent annuity. 
  # Modeling method is the same as that for contingent annuity for service retirement benefit. 
  # For LAFPP, benefits for QSSs of disability retirees are simplified as a fixed proportion of disability retirees' benefit. 


# Notes on initial service retirees and disability retirees
  # A proportion of them are assumed to be life annuitants while the rest contingent annuitants.
  # As of 7/9/2016, the proportion of life annuitants is 20%. 


# Notes on modeling the ERC cap proposed in initiatives.
 # Descriptions in the initiatives:

 # "Government Pension Cap Act of 2016 (#15-0077)"
    # Limits government employers from making retirement benefit contributions of more than 13% of base compensation for new public safety employees, 
    # and not more than 11% for new general employees. All other costs, including unfunded liability costs, are the responsibility of the new employee, 
      # unless voters establish a new limit. New employees are considered those hired on or after 1/1/2019.
    # Limits government employers from paying more than 1/2 of the total cost of retirement benefits for new employees, unless voters approve a higher proportion.

 # "Voter Empowerment Act of 2016 (#15-0076)"
    # Limits government employers from paying more than 1/2 of the total cost of retirement benefits for new employees, unless voters approve a higher proportion.

 # How we interpret and model the provisions
    # All new Tier 6 employers are considered as a new tier, Tier 7, with its own fund that operate independently of other tiers. 
    # The ERC cap is determined by the smaller one of:
      # 13% of the total payroll of Tier 7. 
      # 50% of the total normal cost. (we interpret the "total cost of retirement benefits" as the the total normal cost.)
    # When the limit is triggered, the EEC of Tier 7 is calculated as the total ADC minus the capped ERC. 

    # Since we are not sure about our interpretation of "Total cost of retirement benefits", we will use a parameter to control whether this limit will be implemented. 










#### Model Parameters ####
#********************************************************************************
folder_run <- "."
filename_RunControl <- dir(folder_run, pattern = "^RunControl")
path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)

# Import global parameters
runList <- read_excel(path_RunControl, sheet="params", skip = 0) %>% filter(!is.na(runname), include == 1)
runList

# Import return scenarios
returnScenarios <- read_excel(path_RunControl, sheet="returns", skip = 0) %>% filter(!is.na(scenario))

# Import global parameters
Global_paramlist <- read_excel(path_RunControl, sheet="GlobalParams") %>% filter(!is.na(init.year)) %>% 
                 as.list


#### Run Models and Save  ####
#********************************************************************************

folder_save <- "Results/"



#####  Run Model ####
#*********************************************************************************************************


for(runName in runList$runname ){
  
  #runName <- "RS1"
  
  paramlist <- get_parmsList(runList, runName)
  
  paramlist$simTiers <- "separate"  # "joint"(defult) or "separate"

  if(paramlist$nyear.override != 0) Global_paramlist$nyear <- paramlist$nyear.override
  
  # Global_paramlist <- list(
  #   
  #   init.year = 2015,
  #   nyear     = 10,
  #   nsim      = 5,
  #   ncore     = 4,
  #   
  #   min.ea    = 20,
  #   max.ea    = 64, # Retirement rate is 100% at age 65 
  #   
  #   min.age   = 20,
  #   max.age   = 120 
  # )
  
  
  # paramlist <- list(
  #   
  #   runname = "LAFPP",
  #   #Tier_select = "t76",
  #   simTiers = "joint",
  #   useAVamort  = T, 
  #   useExtFund  = F,
  #   
  #   Grouping    = "fillin",
  #   
  #   r.min  = 41, # this is not required age of retirement benefit. 
  #   r.max  = 65, 
  #   
  #   #fasyears = 3,
  #   #cola     = 0.03,
  #   i = 0.075,
  #   
  #   infl = 0.0325,
  #   prod = 0.01,
  #   s.year = 7,
  #   s.lower  = 0.6,  # AVA is adjusted to be within 40% of MVA:
  #   s.upper  = 1.4,
  #   
  #   m = 20,
  #   
  #   r.full = 50, # age at which vested terms are assumed to retire(Temp, should use r.vben)
  #   r.vben = 50, # age at which vested terms are assumed to retire.
  #   
  #   #r.yos  = 5,
  #   #v.yos  = 5, 
  #   #r.age
  #   
  #   startingSal_growth = 0.038,
  #   w.salgrowth.method =  "simple", # "simple" or "withInit"
  #   
  #   actuarial_method = "EAN.CP",
  #   
  #   
  #   wf_growth = 0,
  #   no_entrance = "F",
  #   newEnt_byTier = c(t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, t6 = 1),
  #   #entrants_dist = rep(1/length(range_ea), length(range_ea)),
  #   
  #   pct.ca.M =  0.8, # proportion of males who opt for ca upon retirement
  #   pct.ca.F =  0.6,
  #   
  #   #factor.ca = 0.25,
  #   
  #   # Investment returns
  #   seed = 1234,
  #   ir.mean = 0.075,
  #   ir.sd   = 0,  # 0.12,
  #   
  #   
  #   init_MA = "AL_pct",
  #   MA_0_pct = 0.946,  # AV2015 pdf page 12
  #   init_EAA = "MA",
  #   
  #   
  #   smooth_method = "method1",
  #   salgrowth_amort = 0.04,
  #   amort_method = "cp",
  #   amort_type = "closed",
  #   nonNegC = "FALSE",
  #   EEC_fixed = "TRUE",
  #   ConPolicy = "ADC",
  #   EEC_rate = 0.05
  # )

  #paramlist %<>% within(
    
    #runname <- "LAFPP",
    #Tier_select <- "t76",
    #simTiers <- "joint",
    #paramlist$useAVamort  <- F 
    #paramlist$useExtFund  <- F
    
  paramlist$Grouping    <- "fillin"
    
  paramlist$r.min  <- 41 # this is not required age of retirement benefit. 
  paramlist$r.max  <- 65 
    

  paramlist$infl <- 0.0325
  paramlist$prod <- 0.01

  paramlist$s.lower  <- 0.6  # AVA is adjusted to be within 40% of MVA:
  paramlist$s.upper  <- 1.4
    

    
  paramlist$r.full <- 50 # age at which vested terms are assumed to retire(Temp, should use r.vben)
  paramlist$r.vben <- 50 # age at which vested terms are assumed to retire.
    
    
  paramlist$startingSal_growth <- 0.038
    
  paramlist$actuarial_method <- "EAN.CP"
    

  paramlist$newEnt_byTier <- c(t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, t6 = 1)

    
  paramlist$pct.ca.M <-  0.8 # proportion of males who opt for ca upon retirement
  paramlist$pct.ca.F <-  0.6
  
  # paramlist$pct.ca.M <-  0.8 # proportion of males who opt for ca upon retirement
  # paramlist$pct.ca.F <-  0.6  

    
    # Investment returns
  paramlist$seed <- 1234

  # paramlist$init_MA <- "AL_pct"
  # paramlist$init_AA <- "AL_pct"  # Later we may want to allow it to be "MA" (equal to MA), "AL" (equal to AL), "AA0"(preset AA value)
  # 
  # paramlist$MA_0_pct <- 0.946  # AV2015 pdf page 12
  # paramlist$AA_0_pct <- 0.915  # AV2015 pdf page 12

  paramlist$init_EAA <- "MA"
    
    
  paramlist$smooth_method <- "method1"
  paramlist$salgrowth_amort <- 0.04

  paramlist$amort_type <- "closed"

  #)

  
  # Parameters derived from the parameter list above. 
  paramlist$range_ea = with(Global_paramlist, min.ea:max.ea)
  paramlist$range_age = with(Global_paramlist, min.age:max.age)
  paramlist$range_age.r = with(paramlist, r.min:r.max)
  paramlist$v     = with(paramlist, 1/(1 + i))
  
  

  if(paramlist$tier == "sumTiers"){
    source("LAFPP_0_Master_allTiers.R")
    save(outputs_list, file = paste0(folder_save, "results_",  paramlist$tier, "_", runName, ".RData"))

  } else {
    Tier_select <- paramlist$tier
    source("LAFPP_0_Master_singleTier.R")
    save(outputs_list, file = paste0(folder_save, "results_",  paramlist$tier, runName, ".RData"))
  }

}





#paramlist$Tier

# Checking the importance of death benefit

# Tier_select <- "t5"
# source("Test_0_Master_singleTier.R")


# 
# 
# 
# 166987857/16955579066
# 
# (16955579066 + 541456502 - 798249899)*0.04





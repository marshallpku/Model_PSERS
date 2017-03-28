
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************


# Plan information
source("PSERS_Data_PlanInfo_AV2015.R")
source("PSERS_Data_MemberData_AV2015.R")

load("Data_inputs/PSERS_PlanInfo_AV2015.RData")    # for all tiers
load("Data_inputs/PSERS_MemberData_AV2015.RData")  # for all tiers


## Exclude selected type(s) of initial members
# init_actives_all %<>% mutate(nactives = 0) 
# init_retirees_all %<>% mutate(nretirees = 0)
# init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
# init_terms_all %<>% mutate(nterm = 0)
# init_disb_all  %<>% mutate(ndisb = 0) 


pct.init.ret.la <-  1
pct.init.ret.ca  <- 1 - pct.init.ret.la

pct.init.disb.la <-  1
pct.init.disb.ca  <- 1 - pct.init.disb.la

init_retirees.la_all <- init_retirees_all %>%
  mutate(nretirees.la = nretirees * pct.init.ret.la) %>% 
  select(-nretirees)

init_retirees.ca_all <- init_retirees_all %>%
  mutate(nretirees.ca = nretirees * pct.init.ret.ca) %>% 
  select(-nretirees)

init_disb.la_all <- init_disb_all %>%
  mutate(ndisb.la = ndisb * pct.init.disb.la) %>% 
  select(-ndisb)

init_disb.ca_all <- init_disb_all %>%
  mutate(ndisb.ca = ndisb * pct.init.disb.ca) %>% 
  select(-ndisb)


#*********************************************************************************************************
# 1.2 Create decrement tables ####
#*********************************************************************************************************

# Decrement tables
source("PSERS_Model_Decrements.R")

list.decrements.tCD <- get_decrements("tCD")
list.decrements.tE <- get_decrements("tE")
list.decrements.tF <- get_decrements("tF")



decrement.model.tCD      <- list.decrements.tCD$decrement.model
mortality.post.model.tCD <- list.decrements.tCD$mortality.post.model

decrement.model.tE      <- list.decrements.tE$decrement.model
mortality.post.model.tE <- list.decrements.tE$mortality.post.model

decrement.model.tF      <- list.decrements.tF$decrement.model
mortality.post.model.tF <- list.decrements.tF$mortality.post.model



#*****************************************************
##   Calibration and Modification of initial data ####
#*****************************************************

## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terms_all %<>% mutate(nterm = 0)
 

## Exclude initial terms with ea < 20: Data_population, line 504
 # init_terminated_all %<>% filter(age.term >= Global_paramlist$min.ea,
 #                                 ea >= Global_paramlist$min.ea)


# ## Exclude the initial amortization basis when testing the program.
#  if(!paramlist$useAVamort)  init_amort_raw %<>% mutate(amount.annual = 0) # CAUTION: For consistency check only; will make initial UAAL not amortized. 
# 
# 
# ## Exclude the external fund. (currently only STIP borrowing)
#  if(!paramlist$useExtFund) extFund %<>% mutate_each(funs(. * 0), -year)


## Matching Segal cash flow

# Matching Segal payroll 
  # Payroll from model:
    # Total: 11040551k
    # t76:   9094102199
    # tCD3:   1279359295 
    # tm13:   667089761   
  # Goal: 9659652k (from Segal projection and AV 2015) 
  
  # # Method: Applying adjustment factors to initial population and initial salary.
  #   # Adjustment factor for initial workforce, applied to all 3 tiers (NEXT STEP? apply only to t76 tier.)
  #     # Payroll of none-lab seg / Payroll of all segs(unit： $k): 
  #       f1 <- 9659652 / 9927833 # 0.972987  
  #   # Adjustment factor for initial salary
  #     # payroll from vii table / payroll from model (both for all segs, unit $k):
  #       f2 <- 9927833 / 11040551 # 0.8992154
  #   # Total adjustment factor is 
  #     # f1 * f2 = 0.8749248
  # 
  # # Adjusting initial workforce and salary:
  #   init_actives_all %<>% mutate(nactives = nactives * f1,
  #                                salary   = salary   * f2) 
  # 


# tier.param %<>% mutate(cola = cola - 0.0025)  

# 
# ### Calibration:
# 
# # 1. PVFB.retirees
#   # By calibrating benefit factor for survivors   
# 
# calibFactor_factor.ca <- 1
# tier.param %<>% mutate(factor.ca = pmin(1.1, factor.ca * calibFactor_factor.ca))
# row.names(tier.param) <- tier.param$tier
# 
# 
# # 2. PVFB.act
#   # By calibrating benefit factor for service retirees
# calibFactor_bfactor <- 1.08
# bfactor %<>% mutate_each(funs(pmin(1, .*calibFactor_bfactor)), -yos )
# 
# 
# # 3. EEC
#   # Aggregate EEC rate (total EEC / projected payroll) in 2016 AV is about 9.75%
#   # Tier specific ERC rates are no more than 9%. 
#   # Aggergate EEC rate is 9.64% after calibration  
# 
# calibFactor_EEC.rate <- 1.1
# tier.param %<>% mutate(EEC.rate = pmin(1.1, EEC.rate * calibFactor_EEC.rate))
# row.names(tier.param) <- tier.param$tier
# 
# 
# # 3. Initial benefit payments
#  # calibrated to match the budgeted non-DROP payment for FY2016-2017  
# init_beneficiaries_all %<>% mutate(benefit = benefit * 0.989589)
# init_retirees.ca_all   %<>% mutate(benefit = benefit * 0.989589)
# init_retirees.la_all   %<>% mutate(benefit = benefit * 0.989589)
# init_disb.ca_all       %<>% mutate(benefit = benefit * 0.989589)
# init_disb.la_all       %<>% mutate(benefit = benefit * 0.989589)



#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("PSERS_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.4 Create plan data ####
#*********************************************************************************************************
source("PSERS_Model_PrepData.R")

# Create data for each tier

salary.tCD  <- get_salary_proc("tCD")
salary.tE  <- get_salary_proc("tE")
salary.tF  <- get_salary_proc("tF")


benefit.tCD <- get_benefit_tier("tCD")
benefit.tE <- get_benefit_tier("tE")
benefit.tF <- get_benefit_tier("tF")


benefit.disb.tCD <- get_benefit.disb_tier("tCD")
benefit.disb.tE <- get_benefit.disb_tier("tE")
benefit.disb.tF <- get_benefit.disb_tier("tF")


init_pop.tCD <- get_initPop_tier("tCD")
init_pop.tE <- get_initPop_tier("tE")
init_pop.tF <- get_initPop_tier("tF")


entrants_dist.tCD <- numeric(length(paramlist$range_ea))
entrants_dist.tE  <- get_entrantsDist_tier("tE") * share.tE
entrants_dist.tF  <- get_entrantsDist_tier("tF") * share.tF


#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************

source("PSERS_Model_Demographics_allTiers.R")
pop <- get_Population_allTiers_PSERS()
  
gc()


#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("PSERS_Model_ContingentAnnuity.R")

range_age.r.ca <- min(paramlist$range_age.r):100
liab.ca.tCD  <- get_contingentAnnuity("tCD", tier.param["tCD", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.tCD)
liab.ca.tE  <- get_contingentAnnuity("tE", tier.param["tE", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.tE)
liab.ca.tF  <- get_contingentAnnuity("tF", tier.param["tF", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.tF)


range_age.disb.ca <-  min(paramlist$range_age): 100 #max(paramlist$range_age.r)
liab.disb.ca.tCD  <- get_contingentAnnuity("tCD", tier.param["tCD", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tCD) %>% rename(age.disb = age.r)
liab.disb.ca.tE  <- get_contingentAnnuity("tE", tier.param["tE", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tE) %>% rename(age.disb = age.r)
liab.disb.ca.tF  <- get_contingentAnnuity("tF", tier.param["tF", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tF) %>% rename(age.disb = age.r)




#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("PSERS_Model_IndivLiab.R")
gc()

liab.tCD <- get_indivLab("tCD",
                         decrement.model.tCD,
                         salary.tCD,
                         benefit.tCD,
                         benefit.disb.tCD,
                         bfactor.tCD,
                         mortality.post.model.tCD,
                         liab.ca.tCD,
                         liab.disb.ca.tCD)

liab.tE <- get_indivLab("tE",
                        decrement.model.tE,
                        salary.tE,
                        benefit.tE,
                        benefit.disb.tE,
                        bfactor.tE,
                        mortality.post.model.tE,
                        liab.ca.tE,
                        liab.disb.ca.tE)

liab.tF <- get_indivLab("tF",
                        decrement.model.tF,
                        salary.tF,
                        benefit.tF,
                        benefit.disb.tF,
                        bfactor.tF,
                        mortality.post.model.tF,
                        liab.ca.tF,
                        liab.disb.ca.tF)



#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("PSERS_Model_AggLiab.R")
gc()


AggLiab.tCD <- get_AggLiab("tCD",
                          liab.tCD,
                          liab.ca.tCD,
                          liab.disb.ca.tCD,
                          pop$pop.tCD,
                          mortality.post.model.tCD) 


AggLiab.tE <- get_AggLiab("tE",
                          liab.tE,
                          liab.ca.tE,
                          liab.disb.ca.tE,
                          pop$pop.tE,
                          mortality.post.model.tE) 


AggLiab.tF <- get_AggLiab("tF",
                          liab.tF,
                          liab.ca.tF,
                          liab.disb.ca.tF,
                          pop$pop.tF,
                          mortality.post.model.tF) 


AggLiab.sumTiers <- get_AggLiab_sumTiers(AggLiab.tCD, AggLiab.tE, AggLiab.tF,
                                         AggLiab.t4, AggLiab.t5, AggLiab.t6)



#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("PSERS_Model_Sim.R")

# source("LAFPP_Model_Sim_cap.R")
# if(paramlist$simTiers == "separate"){
#   penSim_results.tCD  <- run_sim("tCD",  AggLiab.tCD)
#   penSim_results.tE  <- run_sim("tE",  AggLiab.tE)
#   penSim_results.tF  <- run_sim("tF",  AggLiab.tF)
#   penSim_results.t4  <- run_sim("t4",  AggLiab.t4)
#   penSim_results.t5  <- run_sim("t5",  AggLiab.t5)
#   penSim_results.t6  <- run_sim("t6",  AggLiab.t6)
# }

if(!paramlist$ERC_cap.initiatives)
penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers) %>% 
                           select(runname, sim, year, Tier, everything())



if(paramlist$ERC_cap.initiatives){
  
  
  
  # penSim_results.xt7 <- penSim_results.sumTiers %>% 
  #                       mutate(Tier = "xt7")
  # 
  # penSim_results.t7 <- run_sim("t7", 
  #                               AggLiab.t7,
  #                               init_amort_raw_ = init_amort_raw %>% mutate(balance = 0, annual.payment = 0), 
  #                               init_unrecReturns.unadj_ = init_unrecReturns.unadj %>% mutate(DeferredReturn = 0)
  #                               ) %>%
  #                      mutate_all(funs(ifelse(is.nan(.), 0, .))) %>% 
  #                      select(runname, sim, year, Tier, everything())

  
  penSim_results.sumTiers <- run_sim.wt7("sumTiers",
                      AggLiab.xt7_ = AggLiab.sumTiers,
                      AggLiab.t7_  = AggLiab.t7,
                      i.r_ = i.r,
                      
                      init_amort_raw.xt7_ = init_amort_raw, # amount.annual, year.remaining 
                      init_unrecReturns.unadj.xt7_ = init_unrecReturns.unadj,
                      
                      init_amort_raw.t7_ = init_amort_raw %>% mutate(balance = 0, annual.payment = 0),
                      init_unrecReturns.unadj.t7_ = init_unrecReturns.unadj %>% mutate(DeferredReturn = 0),
                      
                      paramlist_ = paramlist,
                      Global_paramlist_ = Global_paramlist)
  
  
  penSim_results.xt7 <-  penSim_results.sumTiers %>% filter(Tier == "xt7")
  penSim_results.t7  <-  penSim_results.sumTiers %>% filter(Tier == "t7")
  
  penSim_results.sumTiers %<>%  
    select(runname, run.returnScn, run.policyScn, sim, year, Tier, everything())   %>% 
    group_by(runname, sim, year) %>% 
    summarise_at(c(7:(ncol(penSim_results.sumTiers))),  funs(sum(., na.rm = TRUE))) %>% 
    mutate(Tier    = "sumTiers",
           run.returnScn = paramlist$run.returnScn, 
           run.policyScn = paramlist$run.policyScn,
           FR      = 100 * AA / exp(log(AL)),
           FR_MA   = 100 * MA / exp(log(AL)),
           UAAL_PR = 100 * UAAL / PR,
           MA_PR   = 100 * MA / PR,
           AA_PR   = 100 * AA / PR,
           AL_PR   = 100 * AL / PR,
           AL.act_PR    = 100 * AL.act / PR,
           AL.la_PR    = 100 * AL.la / PR, 
           AL.ca_PR    = 100 * AL.ca / PR, 
           AL.term_PR   = 100 * AL.term / PR, 
           #AL._PR    = 100 * AL.Ben / PR,
           ADC_PR  = 100 * ADC / PR,
           NC_PR   = 100 * NC / PR,
           NC.laca_PR    = 100 * NC.laca / PR,
           NC.v_PR   = 100 * NC.v / PR,
           SC_PR   = 100 * SC / PR, 
           ERC_PR  = 100 * ERC / PR,
           EEC_PR  = 100 * EEC / PR, 
           C_PR    = 100 * C / PR,
           B_PR    = 100 * B / PR,
           ExF     = C - B,
           ExF_PR  = 100 * ExF / PR,
           ExF_MA  = 100 * ExF / MA,
           PR.growth = ifelse(year > 1, 100 * (PR / lag(PR) - 1), NA)) %>% 
    select(runname, run.returnScn, run.policyScn, sim, year, Tier, everything())
  
  }


# penSim_results.t7 %>% filter(sim == -1)
# penSim_results.sumTiers%>% filter(sim == -1)
# penSim_results.sumTiers1%>% filter(sim == -1)


outputs_list <- list(paramlist = paramlist, 
                     Global_paramlist = Global_paramlist,
                     
                     #decrement = decrement,
                     
                     results     = penSim_results.sumTiers
                     
                     #ind_active  = AggLiab$ind_active, 
                     #ind_retiree = AggLiab$ind_retiree,
                     #ind_term    = AggLiab$ind_term,
                     #demo_summary= pop$demo_summary,
                     
                     #liab  = if(paramlist$save.liab) liab else "Not saved",
                     #demo  = if(paramlist$save.demo) pop else "Not saved",
                     
                     #entrant_dist = entrants_dist
)

if(paramlist$ERC_cap.initiatives){
  outputs_list$results.xt7 <- penSim_results.xt7
  outputs_list$results.t7  <- penSim_results.t7
}
  
# x <- outputs_list
# x$results
# 
# load("./Results/results_sumTiers_RS1.RData")
# 
# outputs_list$results %>% filter(sim == 0) %>% mutate(EEC_PR = 100*EEC/PR) %>%  select(runname, sim, year, AL, MA, FR, ERC_PR, EEC_PR, ERC, PR, C_PR)
# 
# x$results %>% filter(sim == 1) %>% select(runname, sim, year, AL, MA, FR, ERC_PR, EEC_PR,ERC, PR, C_PR)
# x$results.xt7 %>% filter(sim == 0) %>% select(runname, sim, year, AL, MA, FR, ERC_PR, EEC_PR, ERC, PR, C_PR)
# x$results.t7 %>% filter(sim == 1) %>% select(runname, sim, year, AL, MA, FR, ERC_PR, EEC_PR, ERC, PR, C_PR)
# 
# x$results.t7 %>% filter(sim == 1)

#*********************************************************************************************************
# 7.1  Showing results: Joint simulation of all tiers ####
#*********************************************************************************************************
# Use full outputs include:
  # penSim_results.sumTiers
  # AggLiab.t76; AggLiab.tCD3; AggLiab.tm13
# NEXT STEP: extract useful variables from AggLiab.XXX files, so we can still see liability dynamics of each tier
#            when we simulate(do the loop) all tiers jointly. 


var_display1 <- c("runname",  "Tier", "sim", "year", "FR", "MA", "AA", 
                 "AL", 
                 # "AL.act", "AL.act.laca", 
                 "NC","SC", "NC.laca",   "AL.act", "AL.la", "AL.ca", "AL.term",
                 "PVFB", 
                 "B", # "B.la", "B.ca", "B.disb.la","B.disb.ca", 
                 "C",   
                 "PR", "NC_PR", "ERC_PR", "LG", "Amort_basis", "UAAL", "EUAAL")

var_display2 <- c("Tier", "sim", "year", "FR", "MA", "AL", "AL.act", "NC", "SC", "C", "B", "EEC","ERC","PR", "PR_DROP", "EEC_DROP", "ERC_PR" ) # , "ADC") # ,"Amort_basis")

var_display3 <- c("year", "nactives", "nretirees", "nla", "n.ca.R1", "n.ca.R0S1", 
                  "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1")

var_display.cali <- c("runname", "sim", "year", "FR", "MA", "AA", "AL", 
                      "AL.act", "AL.initDROP",
                      "PVFB", 
                      "B", # "B.la", "B.ca", "B.disb.la","B.disb.ca", 
                      # "C",   
                      "NC","SC", "ERC", "EEC",
                      "PR", "NC_PR", "ERC_PR")


kable(penSim_results.sumTiers %>% filter(sim == -1) %>% select(one_of(var_display2)), digits = 2) %>% print 
#kable(penSim_results.sumTiers %>% filter(sim == -1) %>% select(one_of(var_display2)), digits = 2) %>% print 

kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display3))) %>% print  # mutate(FR.AA = 100 * AA/AL) , digits = 2) %>%
kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display.cali)), digits = 2) %>% print 

kable(penSim_results.sumTiers %>% filter(sim == 1) %>% select(one_of(var_display1)), digits = 2) %>% print 
kable(penSim_results.sumTiers %>% filter(sim == 1) %>% select(one_of(var_display2)) %>% mutate(#ExF = C - B, 
                                                                                               #ExF_MA = 100 * ExF/MA,
                                                                                               PVFB.nonact = AL - AL.act,
                                                                                               DROP.PR   = PR_DROP/PR,
                                                                                               DROP.rate = EEC_DROP/EEC), digits = 2)  %>%  print 



#*********************************************************************************************************
# 7.1  Showing results: Separate simulations of each tier ####
#*********************************************************************************************************
# Currently for the purpose of checking model consistency. 
# To make sense of separate simulation of each tier, we must allocate initial assets and amortization payments 
# among the tiers. However, we currently lack information for doing this reasonably. 




# var_display <- c("Tier", "sim", "year", "FR", "MA", "AL", 
#                  #"AL.act", "AL.act.laca", "AL.act.v", "AL.act.LSC", "AL.la", "AL.ca", "AL.term", 
#                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
#                  "B", "B.la", "B.ca", "B.LSC", "B.v", 
#                  "nactives", "nterms", "PR", "NC_PR")
# 
# penSim_results_byTiers <- bind_rows(penSim_results.t76,
#                                     penSim_results.tCD3,
#                                     penSim_results.tm13)
# 
# penSim_results_sumTiers <- penSim_results_byTiers %>% 
#   group_by(sim, year) %>% 
#   summarise(MA = sum(MA)/1000,
#             AL = sum(AL)/1000,
#             NC = sum(NC)/1000,
#             PVFB = sum(PVFB)/1000,
#             B = sum(B)/1000,
#             PR = sum(PR)/1000,
#             nactives = sum(nactives)) %>% 
#   mutate(NC_PR = NC/PR * 100,
#          FR = MA/AL * 100)
# 
# 
# penSim_results.t76  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# penSim_results.tCD3  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# penSim_results.tm13 %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
# 
# penSim_results_sumTiers %>% filter(sim == -1) 



#write.xlsx2(penSim_results_sumTiers %>% filter(sim == -1), file = "Data/detective_constant_wf.xlsx", sheet = "Total")


# init_actives_all %>% summarise(avg.age = sum(age * nactives)/sum(nactives),
#                                avg.yos = sum(yos * nactives)/sum(nactives),
#                                avg.sal = sum(salary * nactives)/sum(nactives))
# 
# init_actives_all %>% 
#   group_by(planname) %>% 
#   summarise(avg.age = sum(age * nactives)/sum(nactives),
#                                avg.yos = sum(yos * nactives)/sum(nactives),
#                                avg.sal = sum(salary * nactives)/sum(nactives))



# npv <- function(x, i){
#   sum(x * (1/(1 + i))^(seq_along(x) - 1))
# }
# 
# 
# p.cp <- amort_cp(967866035, 0.075, 20, 0.04)
# npv(p.cp, 0.075)
# 
# 
# x <- colSums(SC_amort.init)
# npv(x, 0.075) #  1526077697
#               #  1561332747
# 
# 
# 
# SC_amort.init
# 
# init_amort_raw$balance %>% sum
# 
# 
# (607 - 546)/607
# (919 - 798)/919

# load("../Model_Main/IO_M2.1_new/Outputs_D1F075-average.RData")
# outputs_list$results %>% select(runname, sim ,year, AL, NC, B, MA, C, i.r) %>% head


# load("Results/results_sumTiers_RS1.RData")
# load("Results/results_sumTiers_RS1_cap.RData")
# load("Results/results_sumTiers_RS1_cap.allTiers.RData")
# outputs_list$results %>% filter(Tier == "sumTiers", sim == 1) %>% select(runname, sim, year, Tier, AL, MA, FR_MA, ERC_PR) %>%  head(100)





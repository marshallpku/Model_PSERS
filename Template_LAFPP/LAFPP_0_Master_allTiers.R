
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************



# Plan information
# source("LAFPP_Data_RP2000.R")
source("LAFPP_Data_PlanInfo_2016.R")
source("LAFPP_Data_ImportMemberData_2016.R")

load("Data_inputs/LAFPP_PlanInfo_2016.RData")    # for all tiers
load("Data_inputs/LAFPP_MemberData_2016.RData")  # for all tiers


## Exclude selected type(s) of initial members
# init_actives_all %<>% mutate(nactives = 0) 
# init_retirees_all %<>% mutate(nretirees = 0)
# init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
# init_terms_all %<>% mutate(nterm = 0)
# init_disb_all  %<>% mutate(ndisb = 0) 


pct.init.ret.la <-  0.2
pct.init.ret.ca  <- 1 - pct.init.ret.la

pct.init.disb.la <-  0.2
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
source("LAFPP_Model_Decrements.R")

list.decrements.t1 <- get_decrements("t1")
list.decrements.t2 <- get_decrements("t2")
list.decrements.t3 <- get_decrements("t3")
list.decrements.t4 <- get_decrements("t4")
list.decrements.t5 <- get_decrements("t5")
list.decrements.t6 <- get_decrements("t6")


decrement.model.t1      <- list.decrements.t1$decrement.model
mortality.post.model.t1 <- list.decrements.t1$mortality.post.model

decrement.model.t2      <- list.decrements.t2$decrement.model
mortality.post.model.t2 <- list.decrements.t2$mortality.post.model

decrement.model.t3      <- list.decrements.t3$decrement.model
mortality.post.model.t3 <- list.decrements.t3$mortality.post.model


decrement.model.t4      <- list.decrements.t4$decrement.model
mortality.post.model.t4 <- list.decrements.t4$mortality.post.model

decrement.model.t5      <- list.decrements.t5$decrement.model
mortality.post.model.t5 <- list.decrements.t5$mortality.post.model

decrement.model.t6      <- list.decrements.t6$decrement.model
mortality.post.model.t6 <- list.decrements.t6$mortality.post.model


# Modeling ERC cap: new hires of t6
list.decrements.t7      <- get_decrements("t6")
decrement.model.t7      <- list.decrements.t6$decrement.model
mortality.post.model.t7 <- list.decrements.t6$mortality.post.model

# decrement.model.t7 

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
    # t13:   1279359295 
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


### Calibration:

# 1. PVFB.retirees
  # By calibrating benefit factor for survivors   

calibFactor_factor.ca <- 1
tier.param %<>% mutate(factor.ca = pmin(1.1, factor.ca * calibFactor_factor.ca))
row.names(tier.param) <- tier.param$tier


# 2. PVFB.act
  # By calibrating benefit factor for service retirees
calibFactor_bfactor <- 1.08
bfactor %<>% mutate_each(funs(pmin(1, .*calibFactor_bfactor)), -yos )


# 3. EEC
  # Aggregate EEC rate (total EEC / projected payroll) in 2016 AV is about 9.75%
  # Tier specific ERC rates are no more than 9%. 
  # Aggergate EEC rate is 9.64% after calibration  

calibFactor_EEC.rate <- 1.1
tier.param %<>% mutate(EEC.rate = pmin(1.1, EEC.rate * calibFactor_EEC.rate))
row.names(tier.param) <- tier.param$tier


# 3. Initial benefit payments
 # calibrated to match the budgeted non-DROP payment for FY2016-2017  
init_beneficiaries_all %<>% mutate(benefit = benefit * 0.989589)
init_retirees.ca_all   %<>% mutate(benefit = benefit * 0.989589)
init_retirees.la_all   %<>% mutate(benefit = benefit * 0.989589)
init_disb.ca_all       %<>% mutate(benefit = benefit * 0.989589)
init_disb.la_all       %<>% mutate(benefit = benefit * 0.989589)



#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("LAFPP_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))



#*********************************************************************************************************
# 1.4 Create plan data ####
#*********************************************************************************************************
source("LAFPP_Model_PrepData.R")

# Create data for each tier

salary.t1  <- get_salary_proc("t1")
salary.t2  <- get_salary_proc("t2")
salary.t3  <- get_salary_proc("t3")
salary.t4  <- get_salary_proc("t4")
salary.t5  <- get_salary_proc("t5")
salary.t6  <- get_salary_proc("t6")

benefit.t1 <- get_benefit_tier("t1")
benefit.t2 <- get_benefit_tier("t2")
benefit.t3 <- get_benefit_tier("t3")
benefit.t4 <- get_benefit_tier("t4")
benefit.t5 <- get_benefit_tier("t5")
benefit.t6 <- get_benefit_tier("t6")

benefit.disb.t1 <- get_benefit.disb_tier("t1")
benefit.disb.t2 <- get_benefit.disb_tier("t2")
benefit.disb.t3 <- get_benefit.disb_tier("t3")
benefit.disb.t4 <- get_benefit.disb_tier("t4")
benefit.disb.t5 <- get_benefit.disb_tier("t5")
benefit.disb.t6 <- get_benefit.disb_tier("t6")


init_pop.t1 <- get_initPop_tier("t1")
init_pop.t2 <- get_initPop_tier("t2")
init_pop.t3 <- get_initPop_tier("t3")
init_pop.t4 <- get_initPop_tier("t4")
init_pop.t5 <- get_initPop_tier("t5")
init_pop.t6 <- get_initPop_tier("t6")


entrants_dist.t1  <- numeric(length(paramlist$range_ea))
entrants_dist.t2  <- numeric(length(paramlist$range_ea))
entrants_dist.t3  <- numeric(length(paramlist$range_ea))
entrants_dist.t4  <- numeric(length(paramlist$range_ea))
entrants_dist.t5  <- numeric(length(paramlist$range_ea))
entrants_dist.t6  <- get_entrantsDist_tier("t6")


get_tier.bfactor <- function(Tier_select_) bfactor %<>% select(yos, matches(Tier_select_)) %>%  rename_("bfactor" = paste0("bf.", Tier_select_))
bfactor.t1 <- get_tier.bfactor("t1")
bfactor.t2 <- get_tier.bfactor("t2")
bfactor.t3 <- get_tier.bfactor("t3")
bfactor.t4 <- get_tier.bfactor("t4")
bfactor.t5 <- get_tier.bfactor("t5")
bfactor.t6 <- get_tier.bfactor("t6")



# Modeling ERC cap: new hires of t6

if (paramlist$ERC_cap.initiatives){
 
 salary.t7  <- get_salary_proc("t6")
 benefit.t7 <- get_benefit_tier("t6")
 benefit.disb.t7 <- get_benefit.disb_tier("t6")
 bfactor.t7 <- get_tier.bfactor("t6")
 
 entrants_dist.t7  <- get_entrantsDist_tier("t6")
 #entrants_dist.t6  <- numeric(length(paramlist$range_ea))
 
 init_pop.t7       <- get_initPop_tier("t6")
 for (z in 1:length(init_pop.t7)) init_pop.t7[[z]][ , ] <- 0
 
 paramlist$newEnt_byTier_before2019 <- c(t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, t6 = 0, t7 = 1)
 paramlist$newEnt_byTier_after2019  <- c(t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, t6 = 0, t7 = 1)
 }

# # Chnange variable names
# make_tierDec <- function(Tier_select_, df = decrement.ucrp){
#   df %<>% rename_("pxT" = paste0("pxT.", Tier_select_),
#                   "qxr.la"   = paste0("qxr.la.", Tier_select_),
#                   "qxr.ca"   = paste0("qxr.ca.", Tier_select_),
#                   "qxr.LSC"  = paste0("qxr.LSC.", Tier_select_),
#                   "qxr"      = paste0("qxr.", Tier_select_),
#                   "qxt"      = paste0("qxt.", Tier_select_))
#   }
# decrement.ucrp.t76  <- make_tierDec("t76")
# decrement.ucrp.t13  <- make_tierDec("t13")
# decrement.ucrp.tm13 <- make_tierDec("tm13")



#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************

if (!paramlist$ERC_cap.initiatives){
  
  source("LAFPP_Model_Demographics_allTiers.R")
  pop <- get_Population_allTiers_LAFPP()
  } else {
  
  source("LAFPP_Model_Demographics_allTiers.ERC_cap.R")
  pop <- get_Population_allTiers_LAFPP()
  
  
}

 gc()


#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("LAFPP_Model_ContingentAnnuity.R")

range_age.r.ca <- min(paramlist$range_age.r):100
liab.ca.t1  <- get_contingentAnnuity("t1", tier.param["t1", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t1)
liab.ca.t2  <- get_contingentAnnuity("t2", tier.param["t2", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t2)
liab.ca.t3  <- get_contingentAnnuity("t3", tier.param["t3", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t3)
liab.ca.t4  <- get_contingentAnnuity("t4", tier.param["t4", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t4)
liab.ca.t5  <- get_contingentAnnuity("t5", tier.param["t5", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t5)
liab.ca.t6  <- get_contingentAnnuity("t6", tier.param["t6", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t6)


range_age.disb.ca <-  min(paramlist$range_age): 100 #max(paramlist$range_age.r)
liab.disb.ca.t1  <- get_contingentAnnuity("t1", tier.param["t1", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t1) %>% rename(age.disb = age.r)
liab.disb.ca.t2  <- get_contingentAnnuity("t2", tier.param["t2", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t2) %>% rename(age.disb = age.r)
liab.disb.ca.t3  <- get_contingentAnnuity("t3", tier.param["t3", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t3) %>% rename(age.disb = age.r)
liab.disb.ca.t4  <- get_contingentAnnuity("t4", tier.param["t4", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t4) %>% rename(age.disb = age.r)
liab.disb.ca.t5  <- get_contingentAnnuity("t5", tier.param["t5", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t5) %>% rename(age.disb = age.r)
liab.disb.ca.t6  <- get_contingentAnnuity("t6", tier.param["t6", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t6) %>% rename(age.disb = age.r)


if (paramlist$ERC_cap.initiatives){
  liab.ca.t7       <- get_contingentAnnuity("t7", tier.param["t7", "factor.ca"], range_age.r.ca, FALSE, decrement.model_ = decrement.model.t7)
  liab.disb.ca.t7  <- get_contingentAnnuity("t7", tier.param["t7", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.t7) %>% rename(age.disb = age.r)
  
}


#*********************************************************************************************************
# 3. Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_IndivLiab.R")
gc()

liab.t1 <- get_indivLab("t1",
                         decrement.model.t1,
                         salary.t1,
                         benefit.t1,
                         benefit.disb.t1,
                         bfactor.t1,
                         mortality.post.model.t1,
                         liab.ca.t1,
                         liab.disb.ca.t1)

liab.t2 <- get_indivLab("t2",
                        decrement.model.t2,
                        salary.t2,
                        benefit.t2,
                        benefit.disb.t2,
                        bfactor.t2,
                        mortality.post.model.t2,
                        liab.ca.t2,
                        liab.disb.ca.t2)

liab.t3 <- get_indivLab("t3",
                        decrement.model.t3,
                        salary.t3,
                        benefit.t3,
                        benefit.disb.t3,
                        bfactor.t3,
                        mortality.post.model.t3,
                        liab.ca.t3,
                        liab.disb.ca.t3)

liab.t4 <- get_indivLab("t4",
                        decrement.model.t4,
                        salary.t4,
                        benefit.t4,
                        benefit.disb.t4,
                        bfactor.t4,
                        mortality.post.model.t4,
                        liab.ca.t4,
                        liab.disb.ca.t4)

liab.t5 <- get_indivLab("t5",
                        decrement.model.t5,
                        salary.t5,
                        benefit.t5,
                        benefit.disb.t5,
                        bfactor.t5,
                        mortality.post.model.t5,
                        liab.ca.t5,
                        liab.disb.ca.t5)

liab.t6 <- get_indivLab("t6",
                        decrement.model.t6,
                        salary.t6,
                        benefit.t6,
                        benefit.disb.t6,
                        bfactor.t6,
                        mortality.post.model.t6,
                        liab.ca.t6,
                        liab.disb.ca.t6)

if (paramlist$ERC_cap.initiatives){
# liab.t7 <- get_indivLab("t7",
#                         decrement.model.t7,
#                         salary.t7,
#                         benefit.t7,
#                         benefit.disb.t7,
#                         bfactor.t7,
#                         mortality.post.model.t7,
#                         liab.ca.t7,
#                         liab.disb.ca.t7)
liab.t7 <- liab.t6

}


# liab.t5$disb.la %>% filter(year == 2015, !is.na(B.disb.la), B.disb.la !=0, age.disb == age)
# init_disb.la_all

#*********************************************************************************************************
# 5. Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("LAFPP_Model_AggLiab.R")
gc()


AggLiab.t1 <- get_AggLiab("t1",
                          liab.t1,
                          liab.ca.t1,
                          liab.disb.ca.t1,
                          pop$pop.t1,
                          mortality.post.model.t1) 


AggLiab.t2 <- get_AggLiab("t2",
                          liab.t2,
                          liab.ca.t2,
                          liab.disb.ca.t2,
                          pop$pop.t2,
                          mortality.post.model.t2) 


AggLiab.t3 <- get_AggLiab("t3",
                          liab.t3,
                          liab.ca.t3,
                          liab.disb.ca.t3,
                          pop$pop.t3,
                          mortality.post.model.t3) 


AggLiab.t4 <- get_AggLiab("t4",
                          liab.t4,
                          liab.ca.t4,
                          liab.disb.ca.t4,
                          pop$pop.t4,
                          mortality.post.model.t4) 


AggLiab.t5 <- get_AggLiab("t5",
                          liab.t5,
                          liab.ca.t5,
                          liab.disb.ca.t5,
                          pop$pop.t5,
                          mortality.post.model.t5) 


AggLiab.t6 <- get_AggLiab("t6",
                          liab.t6,
                          liab.ca.t6,
                          liab.disb.ca.t6,
                          pop$pop.t6,
                          mortality.post.model.t6)

if (paramlist$ERC_cap.initiatives){
AggLiab.t7 <- get_AggLiab("t7",
                          liab.t7,
                          liab.ca.t7,
                          liab.disb.ca.t7,
                          pop$pop.t7,
                          mortality.post.model.t7)
}


AggLiab.sumTiers <- get_AggLiab_sumTiers(AggLiab.t1, AggLiab.t2, AggLiab.t3,
                                         AggLiab.t4, AggLiab.t5, AggLiab.t6)



#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************
source("LAFPP_Model_Sim.R")
source("LAFPP_Model_Sim_cap.R")


# if(paramlist$simTiers == "separate"){
#   penSim_results.t1  <- run_sim("t1",  AggLiab.t1)
#   penSim_results.t2  <- run_sim("t2",  AggLiab.t2)
#   penSim_results.t3  <- run_sim("t3",  AggLiab.t3)
#   penSim_results.t4  <- run_sim("t4",  AggLiab.t4)
#   penSim_results.t5  <- run_sim("t5",  AggLiab.t5)
#   penSim_results.t6  <- run_sim("t6",  AggLiab.t6)
# }

if(!paramlist$ERC_cap.initiatives)
penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers) %>% 
                           select(runname, sim, year, Tier, everything())


##############################################################

#penSim_results.sumTiers %>% filter(sim == 0)

# x <- penSim_results.sumTiers %>% filter(sim == 0)
# penSim_results.xt7 %>% filter(sim == 1)
# penSim_results.t7 %>% filter(sim == 1)

## Save outputs from RS1

# list.RS1 <- list(liab.t1 = liab.t1, liab.t2 = liab.t2, liab.t3 = liab.t3,liab.t4 = liab.t4, liab.t5 = liab.t5, liab.t6 = liab.t6,
#                  AggLiab.t1 = AggLiab.t1, AggLiab.t2 = AggLiab.t2, AggLiab.t3 = AggLiab.t3, AggLiab.t4 = AggLiab.t4, AggLiab.t5 = AggLiab.t5, AggLiab.t6 = AggLiab.t6,
#                  AggLiab.sumTiers = AggLiab.sumTiers,
#                  liab.disb.ca.t6 = liab.disb.ca.t6,
#                  liab.ca.t6 = liab.ca.t6,
#                  pop = pop)
# 
# save(list.RS1, file = "list.RS1.RData")
# load("list.RS1.RData")
# 
# identical(list.RS1$liab.t6$disb.la, liab.t7$disb.la)
# 
# identical(list.RS1$AggLiab.t2, AggLiab.t2)
# 
# list.RS1$AggLiab.t6

 #identical(list.RS1$liab.disb.ca.t6, liab.disb.ca.t6)
 #identical(list.RS1$liab.ca.t6, liab.ca.t6)

##############################################################

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
  # AggLiab.t76; AggLiab.t13; AggLiab.tm13
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
#                                     penSim_results.t13,
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
# penSim_results.t13  %>% filter(sim == -1) %>% select(one_of(var_display)) %>% data.frame
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





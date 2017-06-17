
gc()

#*********************************************************************************************************
# 1.1 Load data,  for all tiers ####
#*********************************************************************************************************


# Plan information
source("PSERS_Data_PlanInfo_AV2016.R")
source("PSERS_Data_MemberData_AV2016.R")

load("Data_inputs/PSERS_PlanInfo_AV2016.RData")    # for all tiers
load("Data_inputs/PSERS_MemberData_AV2016.RData")  # for all tiers

# if(paramlist$i == 0.0725) load("Data_inputs/DC_rate.tot725.RData")
# if(paramlist$i == 0.0625) load("Data_inputs/DC_rate.tot625.RData")              




## DC contribution rate
 # 0. No DC reform
 # 1. Entry age specific total DC contribution rates obtained using PV approach with 7.25% discount rate
 # 2. Entry age specific total DC contribution rates obtained using PV approach with 6.25% discount rate
 # 3. Uniform 5% total DC contribution rate, with 3% employee DC contribution rate for new employees
 # 4. Uniform 5% total DC contribution rate, with employee DC contribution rate for new employees equal to ERC rate for the DB plan after reform

## parameter "DC_reform_all"
 # If true, DC reform is applied to all current and future members from year 1:
 # For current and future active members in all tiers:
   # - benefit factor is cut by half
   # - active members make EEC to the DC plan. (contribution rate specified by EEC_DC.rate)
   # - employer makes ERC to the DC plan. (contribution rate is equal to the total DC rate in DC_rate.tot minus the EEC rate for DC)
 # For current retirees and beneficiaries:
   # - Benefits for initial retirees and beneficiaries are cut by half
   # - Benefits for initial disabled remain the same. 


if(paramlist$DC.rate == 0) {
  load("Data_inputs/DC_rate.tot625.RData")
  DC_rate.tot %<>% mutate(DC_rate.tot = 0) 
} 

if(paramlist$DC.rate == 1){ 
  
  load("Data_inputs/DC_rate.tot725.RData")
  
  if(paramlist$DC_reform_all){
    tier.param %<>%
      mutate(bfactor     = c(0.0125, 0.01, 0.01, 0.01, 0.01),
             EEC_DC.rate = c(0.0375, 0.0375, 0.0515, 0.0375, 0.0515),
             EEC_rate    = c(0.0375, 0.0375, 0.0515, 0.0375, 0.0515)
      )
    rownames(tier.param) <- tier.param$tier
  
    init_beneficiaries_all %<>% mutate(benefit = 0.5 * benefit)
    init_retirees_all      %<>% mutate(benefit = 0.5 * benefit)
    
    }
  }



if(paramlist$DC.rate == 2) load("Data_inputs/DC_rate.tot625.RData")              

if(paramlist$DC.rate == 3) {
  load("Data_inputs/DC_rate.tot625.RData")
  DC_rate.tot %<>% mutate(DC_rate.tot = 0.05) 
  
  tier.param %<>%
    mutate(EEC_DC.rate = ifelse(tier %in% c("tNE", "tNF"), 0.03, 0 ))
  rownames(tier.param) <- tier.param$tier
 
  
  if(paramlist$DC_reform_all){
    tier.param %<>%
      mutate(bfactor     = c(0.0125, 0.01, 0.01, 0.01, 0.01),
             EEC_DC.rate = 0.03,
             EEC_rate    = c(0.0375, 0.0375, 0.0515, 0.0375, 0.0515)
             )
    rownames(tier.param) <- tier.param$tier
    
    init_beneficiaries_all %<>% mutate(benefit = 0.5 * benefit)
    init_retirees_all      %<>% mutate(benefit = 0.5 * benefit)
  }
  
}

if(paramlist$DC.rate == 4) {
  load("Data_inputs/DC_rate.tot625.RData")
  DC_rate.tot %<>% mutate(DC_rate.tot = 0.09) 
  
  if(paramlist$DC_reform_all){
    tier.param %<>%
      mutate(bfactor     = c(0.0125, 0.01, 0.01, 0.01, 0.01),
             EEC_DC.rate = c(0.0375, 0.0375, 0.0515, 0.0375, 0.0515),
             EEC_rate    = c(0.0375, 0.0375, 0.0515, 0.0375, 0.0515)
      )
    rownames(tier.param) <- tier.param$tier
    
    init_beneficiaries_all %<>% mutate(benefit = 0.5 * benefit)
    init_retirees_all      %<>% mutate(benefit = 0.5 * benefit)
  }

}







init_beneficiaries_all %<>% filter(age >= 25) 


## Exclude selected type(s) of initial members
 #init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terms_all %<>% mutate(nterm = 0)
 # init_disb_all  %<>% mutate(ndisb = 0) 

init_retirees_all <- bind_rows(
  init_retirees_all,
  init_retirees_all %>% filter(str_detect(planname, "tE|tF")) %>% 
    mutate(planname = str_replace(planname,"tE", "tNE"),
           planname = str_replace(planname,"tF", "tNF"),
           nretirees = 0,
           benefit   = 0)
) 


init_disb_all <- bind_rows(
  init_disb_all,
  init_disb_all %>% filter(str_detect(planname, "tE|tF")) %>% 
    mutate(planname = str_replace(planname,"tE", "tNE"),
           planname = str_replace(planname,"tF", "tNF"),
           ndisb = 0,
           benefit = 0)
) 

init_beneficiaries_all <- bind_rows(
  init_beneficiaries_all,
  init_beneficiaries_all %>% filter(str_detect(planname, "tE|tF")) %>% 
    mutate(planname = str_replace(planname,"tE", "tNE"),
           planname = str_replace(planname,"tF", "tNF"),
           nbeneficiaries = 0,
           benefit   = 0)
) 

init_terms_all <- bind_rows(
  init_terms_all,
  init_terms_all %>% filter(str_detect(planname, "tE|tF")) %>% 
    mutate(planname = str_replace(planname,"tE", "tNE"),
           planname = str_replace(planname,"tF", "tNF"),
           nterms = 0,
           benefit   = 0)
) 

init_actives_all <- bind_rows(
  init_actives_all,
  init_actives_all %>% filter(str_detect(planname, "tE|tF")) %>% 
    mutate(planname = str_replace(planname,"tE", "tNE"),
           planname = str_replace(planname,"tF", "tNF"),
           nactives = 0,
           salary   = 0)
) 




pct.init.ret.la  <-  0.75
pct.init.ret.ca  <- 1 - pct.init.ret.la

pct.init.disb.la  <- 1
pct.init.disb.ca  <- 1 - pct.init.disb.la

init_retirees.la_all  <- init_retirees_all %>%
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



if(paramlist$DC_reform){
  decrement.model.tNE      <- decrement.model.tE      
  mortality.post.model.tNE <- mortality.post.model.tE
  
  decrement.model.tNF      <- decrement.model.tF
  mortality.post.model.tNF <- mortality.post.model.tF
}



#*****************************************************
##   Calibration and Modification of initial data ####
#*****************************************************

# 1. PVFB and AL of actives
# paramlist$bfactor <- paramlist$bfactor * 1.1
tier.param %<>% mutate(bfactor = bfactor * 1.11) 
row.names(tier.param) <- tier.param$tier  # using "mutate" removes row names, need to add row names back


# 2. AL and PVFB for actives and NC 
# Issue: PVFB too high, AL too low => PVFNC too high
#        NC too highs

# Effect of calibrating factors:
#    - benefit factor: +AL, +PVFB, +PVFNC, +NC
#    - higher growth in early years and slower growth in later years, with the same career growth:
#      0PVFB, -PVFNC, +AL, 0NCRate, +NC

salgrowth.original <- salgrowth

salInc20_60 <- prod((salgrowth %>% filter(age %in% 19:60))$salgrowth + 1)

salInc20_60

sal.adj <- TRUE
f.adj <- 1
f1 <- 0.05   
f2 <- -0.06

f1 <- ifelse(sal.adj, 1 + f1 * f.adj, 1)
f2 <- ifelse(sal.adj, 1 + f2 * f.adj, 1)

#salgrowth <- salgrowth.original
salgrowth %<>% mutate(salgrowth.unadj = salgrowth,
                            
                            # adj.factor.add = 0, # 0.0075,
                            # salgrowth = salgrowth.unadj + adj.factor.add
                            
                            adj.factor = c(seq(f1, f2, length.out = 36), rep(f2, 20)),
                            salgrowth = salgrowth.unadj * adj.factor
)

# salgrowth

# prod((salgrowth %>% filter(age %in% 19:60))$salgrowth + 1)




## Exclude selected type(s) of initial members
 # init_actives_all %<>% mutate(nactives = 0) 
 # init_retirees_all %<>% mutate(nretirees = 0)
 # init_beneficiaries_all %<>% mutate(n.R0S1 = 0)
 # init_terms_all %<>% mutate(nterm = 0)
 

 





#*********************************************************************************************************
# 1.3  Actual investment return, for all tiers ####
#*********************************************************************************************************
source("PSERS_Model_InvReturns.R")
i.r <- gen_returns()
#i.r[, 3] <-  c(paramlist$ir.mean, paramlist$ir.mean/2, rep(paramlist$ir.mean, Global_paramlist$nyear - 2))


# Investment return (MV) for 2011 - 2015
# Return of 2011-2013: AV2016 pdf page 15
# Return of 2015: CAFR2015 pdf page 78 
# Return of 2016: CAFR2016 pdf page 76

i.r_supplement <-  
  cbind(rep(paramlist$i, 5),
        matrix(c(0.0343, 0.0796, 0.1491, 0.0304, 0.0129), 5, Global_paramlist$nsim + 1))

i.r_geoReturn <- rbind(i.r_supplement, i.r) %>% 
  as.data.frame %>% 
  mutate_all(funs(get_rollingReturns(., "moving", 10)))

i.r_geoReturn[1:9, ] <- rbind(i.r_supplement, i.r)[1:9,] %>% 
  as.data.frame %>% 
  mutate_all(funs(get_rollingReturns(., "expanding"))) %>% 
  as.matrix()

i.r_geoReturn <- i.r_geoReturn[-(1:5),]


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
init_pop.tE <-  get_initPop_tier("tE")
init_pop.tF <-  get_initPop_tier("tF")


entrants_dist.tCD <- numeric(length(paramlist$range_ea))
entrants_dist.tE  <- get_entrantsDist_tier("tE")
entrants_dist.tF  <- get_entrantsDist_tier("tF")
paramlist$newEnt_byTier   <- c(tCD = 0, tE = share.tE, tF = share.tF)



if (paramlist$DC_reform){
  
  salary.tNE  <- get_salary_proc("tE")
  benefit.tNE <- get_benefit_tier("tE")
  benefit.disb.tNE <- get_benefit.disb_tier("tE")
  entrants_dist.tNE  <- get_entrantsDist_tier("tE")

  salary.tNF  <- get_salary_proc("tF")
  benefit.tNF <- get_benefit_tier("tF")
  benefit.disb.tNF <- get_benefit.disb_tier("tF")
  entrants_dist.tNF  <- get_entrantsDist_tier("tF")
  
  init_pop.tNE       <- get_initPop_tier("tE")
  for (z in 1:length(init_pop.tNE)) init_pop.tNE[[z]][ , ] <- 0
  
  init_pop.tNF       <- get_initPop_tier("tF")
  for (z in 1:length(init_pop.tNF)) init_pop.tNF[[z]][ , ] <- 0
  
  paramlist$newEnt_byTier_preReform   <- c(tCD = 0, tE = share.tE, tF = share.tF, tNE = 0,        tNF = 0)
  paramlist$newEnt_byTier_postReform  <- c(tCD = 0, tE = 0,        tF = 0,        tNE = share.tE, tNF = share.tF)
}




#*********************************************************************************************************
# 2. Demographics ####
#*********************************************************************************************************


if(!paramlist$DC_reform){
  
  source("PSERS_Model_Demographics_allTiers.R")
  pop <- get_Population_allTiers_PSERS()

  } else {
  
    source("PSERS_Model_Demographics_allTiers_reform.R")
    pop <- get_Population_allTiers_PSERS()
}

gc()



#*********************************************************************************************************
# 4. Actuarial liabilities and benefits for contingent annuitants and survivors ####
#*********************************************************************************************************
source("PSERS_Model_ContingentAnnuity.R")

range_age.r.ca <- min(paramlist$range_age.r):100
liab.ca.tCD  <- get_contingentAnnuity("tCD", tier.param["tCD", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tCD)
liab.ca.tE   <- get_contingentAnnuity("tE",  tier.param["tE", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tE)
liab.ca.tF   <- get_contingentAnnuity("tF",  tier.param["tF", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tF)


range_age.disb.ca <-  min(paramlist$range_age): 100 #max(paramlist$range_age.r)
liab.disb.ca.tCD  <-  get_contingentAnnuity("tCD", tier.param["tCD", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tCD) %>% rename(age.disb = age.r)
liab.disb.ca.tE   <-  get_contingentAnnuity("tE", tier.param["tE", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tE) %>% rename(age.disb = age.r)
liab.disb.ca.tF   <-  get_contingentAnnuity("tF", tier.param["tF", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tF) %>% rename(age.disb = age.r)


if(paramlist$DC_reform){
  liab.ca.tNE   <- get_contingentAnnuity("tNE",  tier.param["tNE", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tNE)
  liab.ca.tNF   <- get_contingentAnnuity("tNF",  tier.param["tNF", "factor.ca"], range_age.r.ca, TRUE, decrement.model_ = decrement.model.tNF)
  
  liab.disb.ca.tNE   <-  get_contingentAnnuity("tNE", tier.param["tNE", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tNE) %>% rename(age.disb = age.r)
  liab.disb.ca.tNF   <-  get_contingentAnnuity("tNF", tier.param["tNF", "factor.ca.disb"], range_age.disb.ca, FALSE, decrement.model_ = decrement.model.tNF) %>% rename(age.disb = age.r)

}



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
                         mortality.post.model.tCD,
                         liab.ca.tCD,
                         liab.disb.ca.tCD)


liab.tE <- get_indivLab("tE",
                        decrement.model.tE,
                        salary.tE,
                        benefit.tE,
                        benefit.disb.tE,
                        mortality.post.model.tE,
                        liab.ca.tE,
                        liab.disb.ca.tE)


liab.tF <- get_indivLab("tF",
                        decrement.model.tF,
                        salary.tF,
                        benefit.tF,
                        benefit.disb.tF,
                        mortality.post.model.tF,
                        liab.ca.tF,
                        liab.disb.ca.tF)



if(paramlist$DC_reform){
  
   liab.tNE <- get_indivLab("tNE",
                           decrement.model.tNE,
                           salary.tNE,
                           benefit.tNE,
                           benefit.disb.tNE,
                           mortality.post.model.tNE,
                           liab.ca.tNE,
                           liab.disb.ca.tNE)
   
   
   liab.tNF <- get_indivLab("tNF",
                           decrement.model.tNF,
                           salary.tNF,
                           benefit.tNF,
                           benefit.disb.tNF,
                           mortality.post.model.tNF,
                           liab.ca.tNF,
                           liab.disb.ca.tNF)
}



# liab.tE$active %>% filter(year == 2017, ea == 30) %>% select(year, ea, age, DC_EEC)


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


if(paramlist$DC_reform){
  AggLiab.tNE <- get_AggLiab("tNE",
                            liab.tNE,
                            liab.ca.tNE,
                            liab.disb.ca.tNE,
                            pop$pop.tNE,
                            mortality.post.model.tNE) 
  
  
  AggLiab.tNF <- get_AggLiab("tNF",
                            liab.tNF,
                            liab.ca.tNF,
                            liab.disb.ca.tNF,
                            pop$pop.tNF,
                            mortality.post.model.tNF) 
}



if(!paramlist$DC_reform){
  
  AggLiab.sumTiers <- 
    get_AggLiab_sumTiers(AggLiab.tCD, AggLiab.tE, AggLiab.tF) 
  
  
  if(paramlist$tier == "sumTiers"){  
    PR.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, PR_tCD = PR.sum)  %>% 
      left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, PR_tE  = PR.sum)) %>% 
      left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, PR_tF  = PR.sum)) %>% 
      as.matrix
  } else {
    PR.Tiers <- NULL 
  }
  
  
  if(paramlist$tier == "sumTiers"){  
    DC.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, DC_EEC_tCD = DC_EEC.sum, DC_ERC_tCD = DC_ERC.sum)  %>% 
      left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, DC_EEC_tE  = DC_EEC.sum, DC_ERC_tE  = DC_ERC.sum)) %>% 
      left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, DC_EEC_tF  = DC_EEC.sum, DC_ERC_tF  = DC_ERC.sum)) %>% 
      as.matrix
  } else {
    DC.Tiers <- NULL 
  }
}

if(paramlist$DC_reform & !paramlist$SepNewHires){
  
  AggLiab.sumTiers <- 
    get_AggLiab_sumTiers(AggLiab.tCD, AggLiab.tE, AggLiab.tF, AggLiab.tNE, AggLiab.tNF) 
  
  
  if(paramlist$tier == "sumTiers"){  
    PR.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, PR_tCD = PR.sum)  %>% 
      left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, PR_tE  = PR.sum)) %>% 
      left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, PR_tF  = PR.sum)) %>% 
      left_join(AggLiab.tNE$active  %>% as.data.frame %>% select(year, PR_tNE  = PR.sum)) %>% 
      left_join(AggLiab.tNF$active  %>% as.data.frame %>% select(year, PR_tNF  = PR.sum)) %>% 
      as.matrix
  } else {
    PR.Tiers <- NULL 
  }
  
  
  if(paramlist$tier == "sumTiers"){  
    DC.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, DC_EEC_tCD = DC_EEC.sum, DC_ERC_tCD = DC_ERC.sum)  %>% 
      left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, DC_EEC_tE  = DC_EEC.sum, DC_ERC_tE  = DC_ERC.sum)) %>% 
      left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, DC_EEC_tF  = DC_EEC.sum, DC_ERC_tF  = DC_ERC.sum)) %>% 
      left_join(AggLiab.tNE$active  %>% as.data.frame %>% select(year, DC_EEC_tNE  = DC_EEC.sum, DC_ERC_tNE  = DC_ERC.sum)) %>% 
      left_join(AggLiab.tNF$active  %>% as.data.frame %>% select(year, DC_EEC_tNF  = DC_EEC.sum, DC_ERC_tNF  = DC_ERC.sum)) %>% 
      as.matrix
  } else {
    DC.Tiers <- NULL 
  }
  
}


if(paramlist$DC_reform & paramlist$SepNewHires){
  
  AggLiab.sumTiers.xNew <- 
    get_AggLiab_sumTiers(AggLiab.tCD, AggLiab.tE, AggLiab.tF) 
  
  AggLiab.sumTiers.New <- 
    get_AggLiab_sumTiers(AggLiab.tNE, AggLiab.tNF) 
  
  
  PR.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, PR_tCD = PR.sum)  %>% 
    left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, PR_tE  = PR.sum)) %>% 
    left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, PR_tF  = PR.sum)) %>% 
    left_join(AggLiab.tNE$active  %>% as.data.frame %>% select(year, PR_tNE  = PR.sum)) %>% 
    left_join(AggLiab.tNF$active  %>% as.data.frame %>% select(year, PR_tNF  = PR.sum)) %>% 
    as.matrix
    
  DC.Tiers <- AggLiab.tCD$active %>% as.data.frame %>% select(year, DC_EEC_tCD = DC_EEC.sum, DC_ERC_tCD = DC_ERC.sum)  %>% 
    left_join(AggLiab.tE$active  %>% as.data.frame %>% select(year, DC_EEC_tE  = DC_EEC.sum, DC_ERC_tE  = DC_ERC.sum)) %>% 
    left_join(AggLiab.tF$active  %>% as.data.frame %>% select(year, DC_EEC_tF  = DC_EEC.sum, DC_ERC_tF  = DC_ERC.sum)) %>% 
    left_join(AggLiab.tNE$active  %>% as.data.frame %>% select(year, DC_EEC_tNE  = DC_EEC.sum, DC_ERC_tNE  = DC_ERC.sum)) %>% 
    left_join(AggLiab.tNF$active  %>% as.data.frame %>% select(year, DC_EEC_tNF  = DC_EEC.sum, DC_ERC_tNF  = DC_ERC.sum)) %>% 
    as.matrix
  
}








#***************************************************************
## PSERS calibration: Initial vested who are not in pay status
#***************************************************************

# Assume the PVFB for initial vestees are paid up through out the next 50 years. 

AL.init.v <- 1829457000 # AV2016 pdf p17

df_init.vested <- data.frame(
  year = 1:51 + Global_paramlist$init.year - 1,
  B.init.v.sum = c(0, amort_cd(AL.init.v, paramlist$i, 50, TRUE))
) %>% 
  mutate(ALx.init.v.sum = ifelse(year == Global_paramlist$init.year, AL.init.v, 0))

for(i_v in 2:nrow(df_init.vested)){
  df_init.vested$ALx.init.v.sum[i_v] <- 
    with(df_init.vested, (ALx.init.v.sum[i_v - 1] - B.init.v.sum[i_v - 1]) * (1 + paramlist$i))
}


if(!paramlist$SepNewHires){
  
AggLiab.sumTiers$term %<>% 
  as.data.frame() %>% 
  left_join(df_init.vested) %>% 
  mutate_each(funs(na2zero)) %>% 
  mutate(ALx.v.sum = ALx.v.sum + ALx.init.v.sum,
         B.v.sum   = B.v.sum + B.init.v.sum) %>% 
  as.matrix

  } else {
    
  AggLiab.sumTiers.xNew$term %<>% 
    as.data.frame() %>% 
    left_join(df_init.vested) %>% 
    mutate_each(funs(na2zero)) %>% 
    mutate(ALx.v.sum = ALx.v.sum + ALx.init.v.sum,
           B.v.sum   = B.v.sum + B.init.v.sum) %>% 
    as.matrix
  } 



#************************************************************************
## PSERS calibration: year-1 lump sum benefit payment as external fund
#************************************************************************  

B.lumpSumY1 <- 686988000

if(!paramlist$SepNewHires){
  
AggLiab.sumTiers$la %<>%
  data.frame %>% 
  mutate(B.la.sum = ifelse(year == 2016, B.la.sum + B.lumpSumY1, B.la.sum),
         ALx.la.sum = ifelse(year == 2016, ALx.la.sum + B.lumpSumY1, ALx.la.sum)) %>% 
  as.matrix 
  
} else {
  
  AggLiab.sumTiers.xNew$la %<>%
    data.frame %>% 
    mutate(B.la.sum = ifelse(year == 2016, B.la.sum + B.lumpSumY1, B.la.sum),
           ALx.la.sum = ifelse(year == 2016, ALx.la.sum + B.lumpSumY1, ALx.la.sum)) %>% 
    as.matrix 
}

# AggLiab.tCD$active
# AggLiab.tE$active
# AggLiab.tF$active


#*********************************************************************************************************
# 6.  Simulation ####
#*********************************************************************************************************

# No hybrid plan reform
if(!paramlist$DC_reform){
  source("PSERS_Model_Sim.R")
  penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers)

  } 


# Hybrid plan reform. Finances of new hires ARE NOT modeld independently
if(paramlist$DC_reform & !paramlist$SepNewHires){
    source("PSERS_Model_Sim_reform.R")
    penSim_results.sumTiers <- run_sim("sumTiers", AggLiab.sumTiers)
}


# Hybrid plan reform. Finances of new hires ARE modeld independently
if(paramlist$DC_reform & paramlist$SepNewHires){
  source("PSERS_Model_Sim_reform.R")

 penSim_results.sumTiers.xNew <- run_sim("sumTiers.xNew", AggLiab.sumTiers.xNew)
 penSim_results.sumTiers.New  <- run_sim("sumTiers.New",  AggLiab.sumTiers.New)
 
 
 # returnScn = returnScn,
 # policy.SR = policy.SR,
 # policy.EL = policy.EL,
 # policy.reform = DC_reform,
 # Tier    = Tier_select_,
 
 penSim_results.sumTiers <- 
   bind_rows(penSim_results.sumTiers.New,
             penSim_results.sumTiers.xNew) 
 
 penSim_results.sumTiers %<>% 
   select(runname, returnScn, policy.SR, policy.EL, sim, year, Tier, everything())   %>% 
   group_by(runname, sim, year) %>% 
   summarise_at(c(8:(ncol(penSim_results.sumTiers))),  funs(sum(., na.rm = TRUE))) %>% 
   mutate(Tier    = "sumTiers",
          # returnScn = paramlist$returnScn),
          # policy.SR = paramlist$policy.SR,
          # policy.LE = paramlist$policy.EL,
          # policy.reform = paramlist$DC_reform,
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
          ADC_PR  = 100 * ADC / PR,
          
          NC_PR   = 100 * NC / PR,
          NC.laca_PR    = 100 * NC.laca / PR,
          NC.v_PR   = 100 * NC.v / PR,
          
          SC_PR   = 100 * SC / PR, 
          ERC_PR  = 100 * ERC / PR,
          ERC.final_PR = 100 * ERC.final / PR,
          EEC_PR  = 100 * EEC / PR, 
          
          C_PR    = 100 * C / PR,
          B_PR    = 100 * B / PR,
          
          ExF     = C - B,
          ExF_PR  = 100 * ExF / PR,
          ExF_MA  = 100 * ExF / MA,
          PR.growth = ifelse(year > 1, 100 * (PR / lag(PR) - 1), NA)) %>% 
   select(runname, sim, year, Tier, everything()) 
   #select(runname, returnScn, policy.SR, policy.EL, policy.reform, sim, year, Tier, everything()) 
}


penSim_results.sumTiers.New %>% filter(sim == 1) %>% select(one_of(var_display.cali1)) %>% print 



penSim_results.sumTiers %>% filter(sim == 0, year <= 2048) %>% select(one_of(var_display.cali1)) %>% print 

load("Results/results_sumTiers_SR1EL1.Reform_R725.d725.DC4.RData")
load("Results/results_sumTiers_SR1EL1.Reform_R725.d725.DC4.RData")
outputs_list$results %>%  filter(sim == 0, year <= 2048) %>% select(one_of(var_display.cali1)) %>% print 



outputs_list <- list(paramlist = paramlist, 
                     Global_paramlist = Global_paramlist,
                     results     = penSim_results.sumTiers)



#*********************************************************************************************************
# 7.1  Showing results: Joint simulation of all tiers ####
#*********************************************************************************************************
# Use full outputs include:
  # penSim_results.sumTiers
  # AggLiab.t76; AggLiab.tCD3; AggLiab.tm13
# NEXT STEP: extract useful variables from AggLiab.XXX files, so we can still see liability dynamics of each tier
#            when we simulate(do the loop) all tiers jointly. 



# penSim_results.sumTiers %<>% mutate() 

var_display1 <- c("sim", "year", "FR", "MA", "AL", 
                  # "AL.act", "AL.disb.la", "AL.act.disb", "AL.act.death", "AL.act.v", "AL.la", "AL.ca", "AL.term", "PVFB", "B",
                  # "AL.disb.la", "AL.disb.ca", "AL.death", "PVFB",
                  #"PVFB.laca", "PVFB.LSC", "PVFB.v", "PVFB", 
                  # "B", "B.la", "B.ca", "B.v", "B.disb.la","B.disb.ca", 
                  "PR.growth", "ERC.final",
                  "LG", "Amort_basis",
                  "PR", "NC_PR","SC_PR")



var_display2 <- c("Tier", "sim", "year", "FR_MA", "MA", "AL", "EEC","ERC","ERC_PR","B", "B.v", "SC", "C", 
                  "nactives", "nretirees", "nla", "n.ca.R1", "n.ca.R0S1", "nterms", 
                  "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )



var_display.cali1 <- c("sim", "year", 
                       #"FR",
                       "FR_MA", 
                       "MA", "AA", "AL","UAAL", 
                       "PVFB", "AL.act",
                      "NC","SC", "ERC", "ERC.final", "EEC",
                      #"PR",
                      #"NC","SC",
                      "ERC.final_PR",
                      #"DC_ERC_PR",
                      "NC_PR", 
                      "SC_PR", 
                      "EEC_PR",
                      "B")

var_display.cali2 <- c("sim", "year", 
                       "AL.act", "AL.act.v", "AL.la", "AL.ca", "AL.disb.la", "AL.term",
                       "PVFB.laca", "PVFB.disb",  
                       "B.la", "B.ca", "B.disb.la",   #  "B.disb.ca", 
                       "PR", "nactives", "nla")


kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display.cali1)), digits = 2) %>% print 
kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display.cali2)), digits = 2) %>% print 


kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(year, i.r, i.r_geoReturn,PR, PR_tCD,PR_tE,PR_tF, EEC.totRate_tCD, EEC.totRate_tE, EEC.totRate_tF), digits = 5) %>% print 



kable(penSim_results.sumTiers %>% filter(sim == -1) %>% select(one_of(var_display1)), digits = 2) %>% print 
kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_display2)), digits = 2) %>% print 


var_displayDC <- c("Tier", "sim", "year", "DC_ERC", "DC_EEC", "DC_EEC_tNE", "DC_EEC_tNF", "PR_tNE","DC_ERC_PR.tEF")
kable(penSim_results.sumTiers %>% filter(sim == 0) %>% select(one_of(var_displayDC)), digits = 2) %>% print 




#*********************************************************************************************************
#   8. Showing risk measures ####
#*********************************************************************************************************

df_all.stch <- penSim_results.sumTiers  %>% 
  filter(sim >= 0, year <= 2045)


df_all.stch %<>%   
  select(runname, sim, year, AL, MA, EEC, PR, ERC.final_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC.final_PR >= 40), 
         ERC_hike  = cumany(na2zero(ERC.final_PR - lag(ERC.final_PR, 5) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC.final_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC.final_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC.final_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC.final_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC.final_PR, 0.9, na.rm = T)
            
            
  ) %>% 
  ungroup()

df_all.stch




paramlist$salgrowth_amort


liab.tE$active %>% filter(year == 2017, ea == age) %>%  select(year, age, ea, sx, starts_with("NCx"))





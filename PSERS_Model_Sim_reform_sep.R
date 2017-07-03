# This script conducts the simulation of the finance of PSERS

# Current members and new employees are modeled seperately, while shared risk EEC rate and ERC restrictions are
# modeled based on aggregate values. 

# Reference: LAFPP_Model_sim_cap.R



run_sim <- function(Tier_select_,
                    AggLiab.xNew_,
                    AggLiab.New_,
                    PR.Tiers_ = PR.Tiers,
                    DC.Tiers_ = DC.Tiers,
                    i.r_ = i.r,
                    i.r_geoReturn_ = i.r_geoReturn,
                    
                    init_amort_raw.xNew_ = init_amort_raw.xNew, # amount.annual, year.remaining 
                    init_unrecReturns.unadj.xNew_ = init_unrecReturns.unadj.xNew,
                    
                    init_amort_raw.New_ = init_amort_raw.New, # amount.annual, year.remaining 
                    init_unrecReturns.unadj.New_ = init_unrecReturns.unadj.New,
                    
                    paramlist_ = paramlist,
                    Global_paramlist_ = Global_paramlist){

  # Run the section below when developing new features.

     # Tier_select_ =  "sumTiers" #  Tier_select
     # i.r_ = i.r
     # PR.Tiers_ = PR.Tiers
     # DC.Tiers_ = DC.Tiers
     # AggLiab.xNew_    = AggLiab.sumTiers.xNew
     # AggLiab.New_     = AggLiab.sumTiers.New
     # i.r_geoReturn_ = i.r_geoReturn
     # init_amort_raw.xNew_ = init_amort_raw.xNew
     # init_unrecReturns.unadj.xNew_ = init_unrecReturns.unadj.xNew
     # init_amort_raw.New_ = init_amort_raw.New
     # init_unrecReturns.unadj.New_ = init_unrecReturns.unadj.New
     # paramlist_      = paramlist
     # Global_paramlist_ = Global_paramlist
     # 

  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())
  
  # if(Tier_select_ != "sumTiers") init_amort_raw_ %<>% filter(tier == Tier_select_) 

  # if(Tier_select_ != "sumTiers"){
  #   if(!DC_reform) EEC_rate <- tier.param[Tier_select_, "EEC_rate"]
  #   if(DC_reform)  EEC_rate <- tier.param[Tier_select_, "ScnDC_EEC_DB.rate"]
  # } 
  
  # if(Tier_select_ != "sumTiers") EEC_rate <- tier.param[Tier_select_, "EEC_rate"]
  
  # if(Tier_select_ == "sumTiers"){
  # 
  #   EEC_baseRate_tCD <- tier.param["tCD", "EEC_rate"]
  #   EEC_baseRate_tE  <- tier.param["tE", "EEC_rate"]
  #   EEC_baseRate_tF  <- tier.param["tF", "EEC_rate"]
  #   EEC_baseRate_tNE  <- tier.param["tNE", "EEC_rate"]
  #   EEC_baseRate_tNF  <- tier.param["tNF", "EEC_rate"]
  #   
  # } 
  
  #*************************************************************************************************************
  #                                     Defining variables in simulation ####
  #*************************************************************************************************************  
  
  # Now we do the actuarial valuations 
  # In each period, following values will be caculated:
  # AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
  # NC: Normal Cost  
  # MA: Market value of assets.
  # AA: Actuarial value of assets.
  # EAA:Expected actuarial value of assets.
  # UAAL: Unfunded accrued actuarial liability, defined as AL - NC
  # EUAAL:Expected UAAL.
  # PR: payroll 
  # LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
  # AM: Amount to be amortized at period t. 
  # i is assumed interest rate. ELs of each period will be amortized seperately.  
  # SC: Supplement cost 
  # ADC: actuarially required contribution by employer. NC + SC - EEC
  # C : Actual contribution
  # C_ADC: shortfall in paying ADC
  # B : Total beneift Payment   
  # Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
  # Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
  # Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
  # I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
  # Funded Ratio: AA / AL
  # C_PR: contribution as % of payroll
  
  # Formulas
  # AL(t), NC(t), B(t) at each period are calculated using the workforce matrix and the liability matrix.
  # MA(t+1) = AA(t) + I(t) + C(t) - B(t), AA(1) is given
  # EAA(t+1)= AA(t) + EI(t)
  # AA(t+1) = (1-w)*EAA(t+1) + w*MA(t+1)
  # I.r(t) = i.r(t)*[AA(t) + C(t) - B(t)]
  # Ia(t) = i * AA(t)
  # Ib(t) = i * B(t)
  # Ic(t) = i * C(t)
  # EI(t) = Ia(t) - Ib(t) + Ic(t)
  # ADC   = NC(t) + SC(t)
  # ADC.ER = NC(t) + SC(t) - EEC(t)
  # C(t) = NC(t) + SC(t)
  # UAAL(t) = AL(t) - AA(t)
  # EUAAL(t) = [UAAL(t-1) + NC(t-1)](1+i(t-1)) - C(t-1) - Ic(t-1)
  # LG(t) =   UAAL(t) - EUAAL for t>=2 ; LG(1) = -UAAL(1) (LG(1) may be incorrect, need to check)
  # More on LG(t): When LG(t) is calculated, the value will be amortized thourgh m years. This stream of amortized values(a m vector) will be 
  # placed in SC_amort[t, t + m - 1]
  # SC = sum(SC_amort[,t])
  # ExF = B(j) - C(j)
  
  # About gains and losses
  # In this program, the only source of gain or loss is the difference between assumed interest rate i and real rate of return i.r,
  # which will make I(t) != Ia(t) + Ic(t) - Ib(t)
  
  
  # Set up data frame for current members
  penSim0.xNew <- data.frame(year = init.year:(init.year + nyear - 1)) %>%
    mutate(AL   = 0, #
           MA   = 0, #
           AA   = 0, #
           EAA  = 0, #
           FR   = 0, #
           ExF  = 0, # 
           UAAL = 0, #
           EUAAL= 0, #
           LG   = 0, #
           Amort_basis  = 0, # amount to be amortized: AM(t) = LG(t) + [ADC(t - 1) - C(t-1)]*[1 + i(t-1)], i.e. actuarial loss/gain plus shortfall in paying NC+SC in last period(plus interests) 
           # Switch_amort = 0, 
           NC   = 0, #
           SC   = 0, #
           EEC  = 0, #
           ERC  = 0, #
           ADC  = 0, #
           ADC.ER = 0, #
           C    = 0, #
           C_ADC= 0, #
           B    = 0, #                        
           I.r  = 0, #                        
           I.e  = 0, #
           I.dif= 0,
           Ia   = 0, #                         
           Ib   = 0, #                         
           Ic   = 0, #  
           i    = i,
           i.r  = 0,
           PR   = 0,
           ADC_PR = 0,
           C_PR = 0,
           nactives  = 0,
           nretirees = 0,
           nterms    = 0)
  # penSim0 <- as.list(penSim0)
  
  
  # Set up data frame for new members
  penSim0.New <- data.frame(year = init.year:(init.year + nyear - 1)) %>%
    mutate(AL   = 0, #
           MA   = 0, #
           AA   = 0, #
           EAA  = 0, #
           FR   = 0, #
           ExF  = 0, # 
           UAAL = 0, #
           EUAAL= 0, #
           LG   = 0, #
           Amort_basis  = 0, # amount to be amortized: AM(t) = LG(t) + [ADC(t - 1) - C(t-1)]*[1 + i(t-1)], i.e. actuarial loss/gain plus shortfall in paying NC+SC in last period(plus interests) 
           # Switch_amort = 0, 
           NC   = 0, #
           SC   = 0, #
           EEC  = 0, #
           ERC  = 0, #
           ADC  = 0, #
           ADC.ER = 0, #
           C    = 0, #
           C_ADC= 0, #
           B    = 0, #                        
           I.r  = 0, #                        
           I.e  = 0, #
           I.dif= 0,
           Ia   = 0, #                         
           Ib   = 0, #                         
           Ic   = 0, #  
           i    = i,
           i.r  = 0,
           PR   = 0,
           ADC_PR = 0,
           C_PR = 0,
           nactives  = 0,
           nretirees = 0,
           nterms    = 0)
  # penSim0 <- as.list(penSim0)
  
  
  
  
  # Vector used in asset smoothing
  s.vector <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector  # a vector containing the porportion of 
  
  s.vector.xNew <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector  # a vector containing the porportion of 
  s.vector.New <- seq(0,1,length = s.year + 1)[-(s.year+1)]; s.vector  # a vector containing the porportion of 
  

  
  
  #*************************************************************************************************************
  #                                 Defining variables in simulation  ####
  #*************************************************************************************************************
  
   # if(Tier_select_ == "sumTiers.New"){
   #   init_amort_raw_$balance <- 0
   #   MA_0 <- 0
   #   AA_0 <- 0
   # }
   
    MA_0.xNew <- MA_0
    AA_0.xNew <- AA_0
    
    MA_0.New <- 0
    AA_0.New <- 0
  
  
  # For PSERS
  # if(Tier_select_ == "sumTiers"){
    
    # include.old <- Tier_select_ %in% c("sumTiers", "sumTiers.xNew") 
    # include.new <- Tier_select_ %in% c("sumTiers", "sumTiers.New") 
  
  
    # Base employee contribution rates
    EEC_baseRate_tCD <- tier.param["tCD", "EEC_rate"]  #* include.old
    EEC_baseRate_tE  <- tier.param["tE",  "EEC_rate"]   #* include.old
    EEC_baseRate_tF  <- tier.param["tF",  "EEC_rate"]   #* include.old
    EEC_baseRate_tNE  <- tier.param["tNE", "EEC_rate"] #* include.new
    EEC_baseRate_tNF  <- tier.param["tNF", "EEC_rate"] #* include.new
    
    
    penSim0.xNew$PR_tCD <- PR.Tiers_[, "PR_tCD"]  #* include.old
    penSim0.xNew$PR_tE  <- PR.Tiers_[, "PR_tE"]   #* include.old
    penSim0.xNew$PR_tF  <- PR.Tiers_[, "PR_tF"]   #* include.old
    penSim0.New$PR_tNE  <- PR.Tiers_[, "PR_tNE"]  #* include.new
    penSim0.New$PR_tNF  <- PR.Tiers_[, "PR_tNF"]  #* include.new
    

    penSim0.xNew$EEC.totRate_tCD <- tier.param["tCD", "EEC_rate"]  #* include.old
    penSim0.xNew$EEC.totRate_tE  <- tier.param["tE",  "EEC_rate"]  #* include.old
    penSim0.xNew$EEC.totRate_tF  <- tier.param["tF",  "EEC_rate"]  #* include.old
    penSim0.New$EEC.totRate_tNE  <- tier.param["tNE", "EEC_rate"]  #* include.new
    penSim0.New$EEC.totRate_tNF  <- tier.param["tNF", "EEC_rate"]  #* include.new
    
   
    
    
    penSim0.xNew$DC_EEC_tCD <-  DC.Tiers_[, "DC_EEC_tCD"] #* include.old
    penSim0.xNew$DC_EEC_tE  <-  DC.Tiers_[, "DC_EEC_tE"]  #* include.old
    penSim0.xNew$DC_EEC_tF  <-  DC.Tiers_[, "DC_EEC_tF"]  #* include.old
    penSim0.New$DC_EEC_tNE  <-  DC.Tiers_[, "DC_EEC_tNE"] #* include.new
    penSim0.New$DC_EEC_tNF  <-  DC.Tiers_[, "DC_EEC_tNF"] #* include.new
    
    penSim0.xNew$DC_ERC_tCD <-  DC.Tiers_[, "DC_ERC_tCD"] #* include.old
    penSim0.xNew$DC_ERC_tE  <-  DC.Tiers_[, "DC_ERC_tE"]  #* include.old
    penSim0.xNew$DC_ERC_tF  <-  DC.Tiers_[, "DC_ERC_tF"]  #* include.old
    penSim0.New$DC_ERC_tNE  <-  DC.Tiers_[, "DC_ERC_tNE"] #* include.new
    penSim0.New$DC_ERC_tNF  <-  DC.Tiers_[, "DC_ERC_tNF"] #* include.new
    
    
    # Place holders
    penSim0.xNew$EEC_tCD <- rep(0, nyear)
    penSim0.xNew$EEC_tE  <- rep(0, nyear)
    penSim0.xNew$EEC_tF  <- rep(0, nyear)
    penSim0.New$EEC_tNE  <- rep(0, nyear)
    penSim0.New$EEC_tNF  <- rep(0, nyear)
    
    penSim0.xNew$sharedRisk.rate <- rep(0, nyear)
    penSim0.New$sharedRisk.rate  <- rep(0, nyear)
    
    #penSim0.New$sharedRisk.rate.New  <- rep(0, nyear) # shared risk rate for new employees, which can go down when returns are good. 
    
    penSim0.xNew$i.r_geoReturn <- rep(0, nyear) 
    penSim0.New$i.r_geoReturn  <- rep(0, nyear) 
    
    penSim0.xNew$ERC.final <- rep(0, nyear)
    penSim0.New$ERC.final  <- rep(0, nyear)
    
    penSim0.xNew$SharedRiskEval <- ((seq_len(nyear) + init.year - 1) - 2016) %% 3 == 0  # TRUE in the year to determine if the EEC rate should be changed
    penSim0.New$SharedRiskEval  <- ((seq_len(nyear) + init.year - 1) - 2016) %% 3 == 0  # TRUE in the year to determine if the EEC rate should be changed
    
    penSim0.xNew$AL.tot <- rep(0, nyear)
    penSim0.xNew$AA.tot <- rep(0, nyear)
        
 # }
  
  
  
  
  # For current members
  
  # AL(j)
  penSim0.xNew$AL.act.laca <- AggLiab.xNew_$active[, "ALx.laca.sum"]
  penSim0.xNew$AL.act.v    <- AggLiab.xNew_$active[, "ALx.v.sum"]
  penSim0.xNew$AL.act.death<- AggLiab.xNew_$active[, "ALx.death.sum"]
  penSim0.xNew$AL.act.disb <- AggLiab.xNew_$active[, "ALx.disb.sum"]
  penSim0.xNew$AL.act      <-  with(penSim0.xNew, AL.act.laca + AL.act.v + penSim0.xNew$AL.act.death + penSim0.xNew$AL.act.disb)
  
  penSim0.xNew$AL.la    <- AggLiab.xNew_$la[,   "ALx.la.sum"]
  penSim0.xNew$AL.ca    <- AggLiab.xNew_$ca[,   "liab.ca.sum"]
  penSim0.xNew$AL.term  <- AggLiab.xNew_$term[, "ALx.v.sum"]
  penSim0.xNew$AL.death <- AggLiab.xNew_$death[,"ALx.death.sum"]
  penSim0.xNew$AL.disb.la  <- AggLiab.xNew_$disb.la[, "ALx.disb.la.sum"]
  penSim0.xNew$AL.disb.ca  <- AggLiab.xNew_$disb.ca[, "ALx.disb.ca.sum"]
  
  penSim0.xNew$AL       <- with(penSim0.xNew, AL.act + AL.la + AL.ca +  AL.term + AL.death + AL.disb.la + AL.disb.ca)
  
  
  # NC(j)
  penSim0.xNew$NC.laca <- AggLiab.xNew_$active[, "NCx.laca.sum"]
  penSim0.xNew$NC.v    <- AggLiab.xNew_$active[, "NCx.v.sum"]
  penSim0.xNew$NC.death<- AggLiab.xNew_$active[, "NCx.death.sum"]
  penSim0.xNew$NC.disb <- AggLiab.xNew_$active[, "NCx.disb.sum"] 
  penSim0.xNew$NC      <-  with(penSim0.xNew, NC.laca + NC.v + NC.death + NC.disb)
  
  
  # PVFB(j)
  penSim0.xNew$PVFB.laca <- AggLiab.xNew_$active[, "PVFBx.laca.sum"]
  penSim0.xNew$PVFB.v    <- AggLiab.xNew_$active[, "PVFBx.v.sum"]
  penSim0.xNew$PVFB.death<- AggLiab.xNew_$active[, "PVFBx.death.sum"]
  penSim0.xNew$PVFB.disb <- AggLiab.xNew_$active[, "PVFBx.disb.sum"] 
  penSim0.xNew$PVFB      <-  with(penSim0.xNew, PVFB.laca + PVFB.v + PVFB.death + PVFB.disb) #Note this is the total PVFB for actives. PVFB for retirees/beneficiaries are the same as AL.
  
  # B(j)
  penSim0.xNew$B.la    <- AggLiab.xNew_$la[, "B.la.sum"]
  penSim0.xNew$B.ca    <- AggLiab.xNew_$ca[, "B.ca.sum"]
  penSim0.xNew$B.v     <- AggLiab.xNew_$term[, "B.v.sum"]
  penSim0.xNew$B.death <- AggLiab.xNew_$death[, "B.death.sum"]
  penSim0.xNew$B.disb.la  <- AggLiab.xNew_$disb.la[, "B.disb.la.sum"]
  penSim0.xNew$B.disb.ca  <- AggLiab.xNew_$disb.ca[, "B.disb.ca.sum"]
  penSim0.xNew$B       <- with(penSim0.xNew, B.la + B.ca + B.v + B.death + B.disb.la + B.disb.ca)
  
  # PR(j)
  penSim0.xNew$PR <- AggLiab.xNew_$active[, "PR.sum"]


  
  # nactives, nretirees, nterms
  penSim0.xNew$nactives  <- AggLiab.xNew_$active[,  "nactives"]
  penSim0.xNew$nla       <- AggLiab.xNew_$la[, "nla"]
  penSim0.xNew$n.ca.R1   <- AggLiab.xNew_$ca[, "n.R1"]
  penSim0.xNew$n.ca.R0S1 <- AggLiab.xNew_$ca[, "n.R0S1"]
  penSim0.xNew$nterms    <- AggLiab.xNew_$term[, "nterms"]
  penSim0.xNew$ndeathBen <- AggLiab.xNew_$death[, "ndeathBen"]
  penSim0.xNew$ndisb.la  <- AggLiab.xNew_$disb.la[,  "ndisb.la"]
  penSim0.xNew$ndisb.ca.R1   <- AggLiab.xNew_$disb.ca[,  "n.disb.R1"]
  penSim0.xNew$ndisb.ca.R0S1 <- AggLiab.xNew_$disb.ca[,  "n.disb.R0S1"]

  
  penSim0.xNew <- as.list(penSim0.xNew) # Faster to extract elements from lists than frame data frames.
  
  
  # For new members
  
  # AL(j)
  penSim0.New$AL.act.laca <- AggLiab.New_$active[, "ALx.laca.sum"]
  penSim0.New$AL.act.v    <- AggLiab.New_$active[, "ALx.v.sum"]
  penSim0.New$AL.act.death<- AggLiab.New_$active[, "ALx.death.sum"]
  penSim0.New$AL.act.disb <- AggLiab.New_$active[, "ALx.disb.sum"]
  penSim0.New$AL.act      <-  with(penSim0.New, AL.act.laca + AL.act.v + penSim0.New$AL.act.death + penSim0.New$AL.act.disb)
  
  penSim0.New$AL.la    <- AggLiab.New_$la[,   "ALx.la.sum"]
  penSim0.New$AL.ca    <- AggLiab.New_$ca[,   "liab.ca.sum"]
  penSim0.New$AL.term  <- AggLiab.New_$term[, "ALx.v.sum"]
  penSim0.New$AL.death <- AggLiab.New_$death[,"ALx.death.sum"]
  penSim0.New$AL.disb.la  <- AggLiab.New_$disb.la[, "ALx.disb.la.sum"]
  penSim0.New$AL.disb.ca  <- AggLiab.New_$disb.ca[, "ALx.disb.ca.sum"]
  
  penSim0.New$AL       <- with(penSim0.New, AL.act + AL.la + AL.ca +  AL.term + AL.death + AL.disb.la + AL.disb.ca)
  
  
  # NC(j)
  penSim0.New$NC.laca <- AggLiab.New_$active[, "NCx.laca.sum"]
  penSim0.New$NC.v    <- AggLiab.New_$active[, "NCx.v.sum"]
  penSim0.New$NC.death<- AggLiab.New_$active[, "NCx.death.sum"]
  penSim0.New$NC.disb <- AggLiab.New_$active[, "NCx.disb.sum"] 
  penSim0.New$NC      <-  with(penSim0.New, NC.laca + NC.v + NC.death + NC.disb)
  
  
  # PVFB(j)
  penSim0.New$PVFB.laca <- AggLiab.New_$active[, "PVFBx.laca.sum"]
  penSim0.New$PVFB.v    <- AggLiab.New_$active[, "PVFBx.v.sum"]
  penSim0.New$PVFB.death<- AggLiab.New_$active[, "PVFBx.death.sum"]
  penSim0.New$PVFB.disb <- AggLiab.New_$active[, "PVFBx.disb.sum"] 
  penSim0.New$PVFB      <-  with(penSim0.New, PVFB.laca + PVFB.v + PVFB.death + PVFB.disb) #Note this is the total PVFB for actives. PVFB for retirees/beneficiaries are the same as AL.
  
  # B(j)
  penSim0.New$B.la    <- AggLiab.New_$la[, "B.la.sum"]
  penSim0.New$B.ca    <- AggLiab.New_$ca[, "B.ca.sum"]
  penSim0.New$B.v     <- AggLiab.New_$term[, "B.v.sum"]
  penSim0.New$B.death <- AggLiab.New_$death[, "B.death.sum"]
  penSim0.New$B.disb.la  <- AggLiab.New_$disb.la[, "B.disb.la.sum"]
  penSim0.New$B.disb.ca  <- AggLiab.New_$disb.ca[, "B.disb.ca.sum"]
  penSim0.New$B       <- with(penSim0.New, B.la + B.ca + B.v + B.death + B.disb.la + B.disb.ca)
  
  # PR(j)
  penSim0.New$PR <- AggLiab.New_$active[, "PR.sum"]
  
  
  
  # nactives, nretirees, nterms
  penSim0.New$nactives  <- AggLiab.New_$active[,  "nactives"]
  penSim0.New$nla       <- AggLiab.New_$la[, "nla"]
  penSim0.New$n.ca.R1   <- AggLiab.New_$ca[, "n.R1"]
  penSim0.New$n.ca.R0S1 <- AggLiab.New_$ca[, "n.R0S1"]
  penSim0.New$nterms    <- AggLiab.New_$term[, "nterms"]
  penSim0.New$ndeathBen <- AggLiab.New_$death[, "ndeathBen"]
  penSim0.New$ndisb.la  <- AggLiab.New_$disb.la[,  "ndisb.la"]
  penSim0.New$ndisb.ca.R1   <- AggLiab.New_$disb.ca[,  "n.disb.R1"]
  penSim0.New$ndisb.ca.R0S1 <- AggLiab.New_$disb.ca[,  "n.disb.R0S1"]
  
  
  penSim0.New <- as.list(penSim0.New) # Faster to extract elements from lists than frame data frames.
  
  
  #*************************************************************************************************************
  #                                  Setting up initial amortization payments: current members ####
  #*************************************************************************************************************  
  
  # matrix representation of amortization: better visualization but larger size
  m.max.xNew <- max(init_amort_raw.xNew_$year.remaining, m)
  SC_amort0.xNew <- matrix(0, nyear + m.max.xNew, nyear + m.max.xNew)
  # SC_amort0
  
  # data frame representation of amortization: much smaller size, can be used in real model later.
  # SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))
  
  # Amortization payment amounts for all prior years. 
   SC_amort.init.xNew <- matrix(0, nrow(init_amort_raw.xNew_), nyear + m.max.xNew)
 
  # Adjustment factor for initial amortization payments (PSERS specific)
      # Factor is defined as the initial model UAAL as a proportion of UAAL in AV2015.
      # WARNING: Does not work with "method 2" for AA. 
   
   MA.year1.xNew <- switch(init_MA, 
                        MA0 = MA_0.xNew,                         # Use preset value
                        AL = penSim0.xNew$AL[1],                # Assume inital fund equals inital liability.
                        AL_pct = penSim0.xNew$AL[1] * MA_0_pct) # Inital MA is a proportion of inital AL
   
   AA.year1.xNew  <- ifelse(init_AA == "AL_pct",         penSim0.xNew$AL[1] * AA_0_pct, # Initial AA as a % of initial AL
                           ifelse(init_AA == "AA0", AA_0.xNew,                     # preset value of AA
                                                    MA.year1.New)  # # Assume inital AA equals inital liability.
   )
                                  
   AL.year1.xNew   <- penSim0.xNew$AL[1]
   UAAL.year1.xNew <- AL.year1.xNew - AA.year1.xNew
   
   factor.initAmort.xNew <- UAAL.year1.xNew/ 42723895000   # # AV2016 page17  AV2015 value: 37335764000 

   
  
   ## Adjustment for PSERS
   # The payments for 2016 experience and assumption changes will start in 2017(FY2017-2018), 
   # therefore the annual payment should be calculated based on the outstanding balance on 7/1/2017,
   # which is equal to the 2016 balance muliplied by (1 + discount rate 7.25%).
   # The payment schedule in Model_Sim must be adjusted accordingly. (payment starts in the 2nd column)
  
   init_amort_raw.xNew_ %<>% 
     mutate(balance = balance * factor.initAmort.xNew,
            balance = ifelse(year.est == 2017, balance * (1 + i), balance))
   
   if(useAVamort){
     SC_amort.init.list.xNew <- mapply(amort_LG, p = init_amort_raw.xNew_$balance , m = init_amort_raw.xNew_$year.remaining, method = init_amort_raw.xNew_$amort.method,
                                       MoreArgs = list(i = i, g = salgrowth_amort, end = FALSE), SIMPLIFY = F)

    for(j in 1:nrow(SC_amort.init.xNew)){
      
      # PSERS: payments for 2016 experience and changes start in 2017.
      if(j < 9)  SC_amort.init.xNew[j,  1:init_amort_raw.xNew_$year.remaining[j]] <- SC_amort.init.list.xNew[[j]]
      if(j >=9)  SC_amort.init.xNew[j, (1:init_amort_raw.xNew_$year.remaining[j]) + 1 ] <- SC_amort.init.list.xNew[[j]]
      
    }
  }

  # SC_amort.init

   
  nrow.initAmort.xNew <- nrow(SC_amort.init.xNew)

  SC_amort0.xNew <- rbind(SC_amort.init.xNew, SC_amort0.xNew)
  # # The amortization basis of year j should be placed in row nrow.initAmort + j - 1. 
  # # save(SC_amort0, file = "SC_amort0.RData")  
  
  
  #*************************************************************************************************************
  #                                  Setting up initial amortization payments: new members ####
  #*************************************************************************************************************  
  
  nrow.initAmort.New <- nrow.initAmort.xNew
  
  SC_amort0.New <- SC_amort0.xNew
  SC_amort0.New[,] <- 0
  
  
  SC_amort0.xNew
  penSim0.xNew
  
  #*************************************************************************************************************
  #                                       Simuation  ####
  #*************************************************************************************************************
    
  cl <- makeCluster(ncore) 
  registerDoParallel(cl)
  
  
  penSim_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr", "stringr")) %dopar% {
    
    # k <- 0
    # initialize
    penSim.xNew   <- penSim0.xNew
    SC_amort.xNew  <- SC_amort0.xNew
    
    penSim.New   <- penSim0.New
    SC_amort.New <- SC_amort0.New
    
    penSim.xNew$sim <- rep(k, nyear) 
    penSim.New$sim  <- rep(k, nyear)  
    
    if(k == -1) {
      SC_amort.xNew[,] <- 0
      SC_amort.New[,]  <- 0
    }
    
    penSim.xNew[["i.r"]] <- i.r_[, as.character(k)]
    penSim.xNew[["i.r_geoReturn"]] <- i.r_geoReturn_[, as.character(k)]
    
    penSim.New[["i.r"]] <- i.r_[, as.character(k)]
    penSim.New[["i.r_geoReturn"]] <- i.r_geoReturn_[, as.character(k)]
    
    
    source("Functions.R")
    
    for (j in 1:nyear){
        
        # j <- 2
        # j <- 8

      
      #*************************************************************************************************************
      #                                       Current tiers: Part 1  ####
      #*************************************************************************************************************
     
       # MA(j) and EAA(j) 
      if(j == 1) {penSim.xNew$MA[j]  <- ifelse(k == -1, penSim.xNew$AL[j],                   # k = -1 is for testing model consistency
                                          switch(init_MA, 
                                                     MA0 = MA_0.xNew,                        # Use preset value
                                                     AL = penSim.xNew$AL[j],                # Assume inital fund equals inital liability.
                                                     AL_pct = penSim.xNew$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
                                          ) 
                 penSim.xNew$EAA[j] <- switch(init_EAA,
                                         AL = EAA_0,                       # Use preset value 
                                         MA = penSim.xNew$MA[j])                # Assume inital EAA equals inital market value.
                 
                 penSim.xNew$AA[j]  <- ifelse(init_AA == "AL_pct" & k != -1, penSim.xNew$AL[j] * AA_0_pct, 
                                             ifelse(init_AA == "AA0" & k != -1, AA_0.xNew,
                                                    switch(smooth_method,
                                                           method1 =  with(penSim.xNew, MA[j]),   # we may want to allow for a preset initial AA.
                                                           method2 =  with(penSim.xNew, (1 - w) * EAA[j] + w * MA[j])
                                                    ) 
                                          )
      )
      } else {
        penSim.xNew$MA[j]  <- with(penSim.xNew, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim.xNew$EAA[j] <- with(penSim.xNew, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim.xNew$AA[j]  <- switch(smooth_method,
                                method1 = with(penSim.xNew, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),
                                method2 = with(penSim.xNew, (1 - w) * EAA[j] + w * MA[j]) 
        )
      }

      

      
      ## Initial unrecognized returns
      if((init_AA %in% c("AL_pct", "AA0")) & useAVunrecReturn & k != -1){

        # Adjusting initila unrecognized returns
        init_unrecReturns.adj.xNew <-  mutate(init_unrecReturns.unadj.xNew_, DeferredReturn = DeferredReturn * (penSim.xNew$MA[1] - penSim.xNew$AA[1])/sum(DeferredReturn),
                                                                        DeferredReturn.annualTot = sum(DeferredReturn) - cumsum(DeferredReturn) # Initial unrecognized return to be subtracted from AA in each year
                                         )

        # Adjust AA for inital unrecognized returns
        #mm <- j - 1
        if((j - 1 + init.year) %in% init_unrecReturns.adj.xNew$year) penSim.xNew$AA[j] <- penSim.xNew$AA[j] - with(init_unrecReturns.adj.xNew, DeferredReturn.annualTot[year == (j - 1 + init.year)])
            
           # init_unrecReturns.adj[init_unrecReturns.adj$year - init.year + 1 == j, "DeferredReturn"] #  )

      }
      
      #*************************************************************************************************************
      #                                       New tiers: Part 1  ####
      #*************************************************************************************************************
      
      # MA(j) and EAA(j) 
      if(j == 1) {penSim.New$MA[j]  <- ifelse(k == -1, penSim.New$AL[j],                   # k = -1 is for testing model consistency
                                          switch(init_MA, 
                                                 MA0 = MA_0.New,                        # Use preset value
                                                 AL = penSim.New$AL[j],                # Assume inital fund equals inital liability.
                                                 AL_pct = penSim.New$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
      ) 
      penSim.New$EAA[j] <- switch(init_EAA,
                              AL = EAA_0,                       # Use preset value 
                              MA = penSim.New$MA[j])                # Assume inital EAA equals inital market value.
      
      penSim.New$AA[j]  <- ifelse(init_AA == "AL_pct" & k != -1, penSim.New$AL[j] * AA_0_pct, 
                              ifelse(init_AA == "AA0" & k != -1, AA_0.New,
                                     switch(smooth_method,
                                            method1 =  with(penSim.New, MA[j]),   # we may want to allow for a preset initial AA.
                                            method2 =  with(penSim.New, (1 - w) * EAA[j] + w * MA[j])
                                     ) 
                              )
      )
      } else {
        penSim.New$MA[j]  <- with(penSim.New, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim.New$EAA[j] <- with(penSim.New, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim.New$AA[j]  <- switch(smooth_method,
                                method1 = with(penSim.New, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),
                                method2 = with(penSim.New, (1 - w) * EAA[j] + w * MA[j]) 
        )
      }
      
      
      
      
      # ## Initial unrecognized returns
      # if((init_AA %in% c("AL_pct", "AA0")) & useAVunrecReturn & k != -1 & Tier_select_ %in% c("sumTiers", "sumTiers.xNew")){
      #   
      #   # Adjusting initila unrecognized returns
      #   init_unrecReturns.adj <-  mutate(init_unrecReturns.unadj_, DeferredReturn = DeferredReturn * (penSim$MA[1] - penSim$AA[1])/sum(DeferredReturn),
      #                                    DeferredReturn.annualTot = sum(DeferredReturn) - cumsum(DeferredReturn) # Initial unrecognized return to be subtracted from AA in each year
      #   )
      #   
      #   # Adjust AA for inital unrecognized returns
      #   #mm <- j - 1
      #   if((j - 1 + init.year) %in% init_unrecReturns.adj$year) penSim$AA[j] <- penSim$AA[j] - with(init_unrecReturns.adj, DeferredReturn.annualTot[year == (j - 1 + init.year)])
      #   
      #   # init_unrecReturns.adj[init_unrecReturns.adj$year - init.year + 1 == j, "DeferredReturn"] #  )
      #   
      # }  
      
      
      
      #*************************************************************************************************************
      #            ## Apply corridor for AA  ####
      #*************************************************************************************************************
      
      # The corridor is applied based on the total AA and MA of the current tiers and tier 7. 
      
      if((penSim.xNew$AA[j] + penSim.New$AA[j]) > s.upper * (penSim.xNew$MA[j] + penSim.New$MA[j])){
        penSim.xNew$AA[j] <-  penSim.xNew$MA[j]
        penSim.New$AA[j]  <-  penSim.New$MA[j]
      } 
      
      if((penSim.xNew$AA[j] + penSim.New$AA[j]) < s.lower * (penSim.xNew$MA[j] + penSim.New$MA[j])){
        penSim.xNew$AA[j] <-  penSim.xNew$MA[j]
        penSim.New$AA[j]  <-  penSim.New$MA[j]
      } 
      
      
      
      # ## Apply corridor for MA, MA must not deviate from AA by more than 40%. 
      # 
      # penSim$AA[j] <- with(penSim, ifelse(AA[j] > s.upper * MA[j], MA[j], AA[j])) 
      # penSim$AA[j] <- with(penSim, ifelse(AA[j] < s.lower * MA[j], MA[j], AA[j]))
      # 
      
      
      
      
      #*************************************************************************************************************
      #                                       Current tiers: Part 2  ####
      #*************************************************************************************************************
      
      # UAAL(j)
      penSim.xNew$UAAL[j]    <- with(penSim.xNew, AL[j] - AA[j])
      # penSim.xNew$UAAL.MA[j] <- with(penSim.xNew, AL[j] - MA[j])
      
      
      # LG(j)
      # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
      if (j == 1){
        penSim.xNew$EUAAL[j]       <- 0
        penSim.xNew$LG[j]          <- with(penSim.xNew,  UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
        penSim.xNew$Amort_basis[j] <- with(penSim.xNew, LG[j])  # This will not be used for LAFPP since the amortization scheme for year 1 is provided by SC_amort.(from AV2016)
        
      } else {
        penSim.xNew$EUAAL[j]       <- with(penSim.xNew, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        
        penSim.xNew$LG[j]          <- with(penSim.xNew,  UAAL[j] - EUAAL[j])
        penSim.xNew$Amort_basis[j] <- with(penSim.xNew,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
      
      }   
      
      
      # # Amortize LG(j)
      if(j > 1){
      if(j > ifelse(useAVamort, 1, 0)){
        # if useAVamort is TRUE, AV amort will be used for j = 1, not the one calcuated from the model. This may cause inconsistency in the model results
        if(amort_type == "closed") SC_amort.xNew[nrow.initAmort.xNew + j - 1, j:(j + m - 1)] <- amort_LG(penSim.xNew$Amort_basis[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)
        }
      }
      
      # Supplemental cost in j
      penSim.xNew$SC[j] <- switch(amort_type,
                             closed = sum(SC_amort.xNew[, j]),
                             open   = amort_LG(penSim.xNew$UAAL[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
      
      
      
      #*************************************************************************************************************
      #                                       Current tiers: Part 2  ####
      #*************************************************************************************************************
      
      # UAAL(j)
      penSim.New$UAAL[j]    <- with(penSim.New, AL[j] - AA[j])
      # penSim.New$UAAL.MA[j] <- with(penSim.New, AL[j] - MA[j])
      
      
      # LG(j)
      # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
      if (j == 1){
        penSim.New$EUAAL[j]       <- 0
        penSim.New$LG[j]          <- with(penSim.New,  UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
        penSim.New$Amort_basis[j] <- with(penSim.New, LG[j])  # This will not be used for LAFPP since the amortization scheme for year 1 is provided by SC_amort.(from AV2016)
        
      } else {
        penSim.New$EUAAL[j]       <- with(penSim.New, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        
        penSim.New$LG[j]          <- with(penSim.New,  UAAL[j] - EUAAL[j])
        penSim.New$Amort_basis[j] <- with(penSim.New,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
        
      }   
      
      
      # # Amortize LG(j)
      if(j > 1){
        if(j > ifelse(useAVamort, 1, 0)){
          # if useAVamort is TRUE, AV amort will be used for j = 1, not the one calcuated from the model. This may cause inconsistency in the model results
          if(amort_type == "closed") SC_amort.New[nrow.initAmort.New + j - 1, j:(j + m - 1)] <- amort_LG(penSim.New$Amort_basis[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)
        }
      }
      
      
      # Supplemental cost in j
      penSim.New$SC[j] <- switch(amort_type,
                                  closed = sum(SC_amort.New[, j]),
                                  open   = amort_LG(penSim.New$UAAL[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
      
      
      
      
      #**************************************************************************************************************
      #                                        PSERS: shared-risk EEC rate 
      #**************************************************************************************************************
        penSim.xNew$AL.tot[j]  <-  penSim.xNew$AL[j] + penSim.New$AL[j]
        penSim.xNew$AA.tot[j]  <-  penSim.xNew$AA[j] + penSim.New$AA[j]
        
      
        if(j > 1){
          
          # in the re-evaluation year
          if(penSim.xNew$SharedRiskEval[j - 1]){
            
            # shared risk rate for class E and F
            penSim.xNew$sharedRisk.rate[j] <- ifelse(     penSim.xNew$i.r_geoReturn[j - 1] >= (i + 0.01), penSim.xNew$sharedRisk.rate[j - 1] - 0.005,
                                                   ifelse(penSim.xNew$i.r_geoReturn[j - 1] <  (i - 0.01), penSim.xNew$sharedRisk.rate[j - 1] + 0.005, 
                                                                                                          penSim.xNew$sharedRisk.rate[j - 1]))
            
            penSim.xNew$sharedRisk.rate[j] <- ifelse(            penSim.xNew$sharedRisk.rate[j] > SharedRisk_cap, SharedRisk_cap,
                                                         ifelse( penSim.xNew$sharedRisk.rate[j] < -0.02,   -0.02,
                                                                 penSim.xNew$sharedRisk.rate[j])
                                                )
            
            
            penSim.xNew$sharedRisk.rate[j] <- ifelse(penSim.xNew$AL.tot[j - 1] == 0, 0,
                          ifelse( (penSim.xNew$AA.tot[j - 1] / penSim.xNew$AL.tot[j - 1]) > 1 & penSim.xNew$sharedRisk.rate[j - 1] > 0, 0, penSim.xNew$sharedRisk.rate[j]))
              
            
            # shared risk/gain rate for new hybrid plan members
            
            penSim.New$sharedRisk.rate[j] <- ifelse(        penSim.xNew$i.r_geoReturn[j - 1] >= (i + 0.01), penSim.New$sharedRisk.rate[j - 1] - 0.0075,
                                                     ifelse(penSim.xNew$i.r_geoReturn[j - 1] <  (i - 0.01), penSim.New$sharedRisk.rate[j - 1] + 0.0075, 
                                                                                                            penSim.New$sharedRisk.rate[j - 1]))
            
            penSim.New$sharedRisk.rate[j] <- ifelse(           penSim.New$sharedRisk.rate[j] >  0.03,   0.03,
                                                       ifelse( penSim.New$sharedRisk.rate[j] < -0.03,  -0.03,
                                                               penSim.New$sharedRisk.rate[j])
            )
            
            
            penSim.New$sharedRisk.rate[j] <- ifelse(penSim.xNew$AL.tot[j - 1] == 0, 0,
                                                     ifelse( (penSim.xNew$AA.tot[j - 1] / penSim.xNew$AL.tot[j - 1]) > 1 & penSim.New$sharedRisk.rate[j - 1] > 0, 
                                                             0, 
                                                             penSim.New$sharedRisk.rate[j]))
            
            #penSim.New$sharedRisk.rate[j] <- penSim.xNew$sharedRisk.rate[j]  
              
            
            
          } else {
            # Not in the re-evaluation year  
            penSim.xNew$sharedRisk.rate[j] <-  penSim.xNew$sharedRisk.rate[j - 1]
            penSim.New$sharedRisk.rate[j]  <-  penSim.New$sharedRisk.rate[j - 1]
          }
        } 
        
        if(useSharedRisk){ 
         penSim.xNew$EEC.totRate_tE[j] <-  (EEC_baseRate_tE + penSim.xNew$sharedRisk.rate[j])  #* include.old
         penSim.xNew$EEC.totRate_tF[j] <-  (EEC_baseRate_tF + penSim.xNew$sharedRisk.rate[j])  #* include.old
         penSim.New$EEC.totRate_tNE[j] <- (EEC_baseRate_tNE + penSim.New$sharedRisk.rate[j]) #* include.new
         penSim.New$EEC.totRate_tNF[j] <- (EEC_baseRate_tNF + penSim.New$sharedRisk.rate[j]) #* include.new
        }
          
        penSim.xNew$EEC_tCD[j] <- with(penSim.xNew, PR_tCD[j] * EEC.totRate_tCD[j]) 
        penSim.xNew$EEC_tE[j]  <- with(penSim.xNew, PR_tE[j]  * EEC.totRate_tE[j])  
        penSim.xNew$EEC_tF[j]  <- with(penSim.xNew, PR_tF[j]  * EEC.totRate_tF[j])  
        penSim.New$EEC_tNE[j]  <- with(penSim.New,  PR_tNE[j]  * EEC.totRate_tNE[j]) 
        penSim.New$EEC_tNF[j]  <- with(penSim.New,  PR_tNF[j]  * EEC.totRate_tNF[j])
        
        # penSim$EEC[j] <- penSim$EEC_tCD[j] + penSim$EEC_tE[j] + penSim$EEC_tF[j] + penSim$EEC_tNE[j] + penSim$EEC_tNF[j]    
        
        penSim.xNew$EEC[j] <- penSim.xNew$EEC_tCD[j] + penSim.xNew$EEC_tE[j] + penSim.xNew$EEC_tF[j]    
        penSim.New$EEC[j]  <- penSim.New$EEC_tNE[j] + penSim.New$EEC_tNF[j]    
        
              
    
    
      #**************************************************************************************************************
      
      
      
      # ADC(j)
      
      if(nonNegC){
        # penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
        # penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
        
        
        # Calculate ADC as if current members and new members are modeled jointly.
        
        penSim.xNew$ADC[j] <- with(penSim.xNew, NC[j] + SC[j])
        penSim.New$ADC[j]  <- with(penSim.New,  NC[j] + SC[j])
        
        penSim.xNew$ADC.preTot[j] <- max(0,  with(penSim.xNew, ADC[j]) +  with(penSim.New, ADC[j]))
        
        penSim.xNew$ADC[j] <- ifelse(penSim.xNew$ADC.preTot[j] > 0, penSim.xNew$ADC[j], 0 )
        penSim.New$ADC[j]  <- ifelse(penSim.xNew$ADC.preTot[j] > 0, penSim.New$ADC[j],  0 )
        
        
        penSim.xNew$EEC.preTot[j] <- with(penSim.xNew, EEC[j]) +  with(penSim.New, EEC[j])
        penSim.xNew$ADC.ER.preTot[j]  <- with(penSim.xNew, ifelse(ADC.preTot[j] > EEC.preTot[j], ADC.preTot[j] - EEC.preTot[j], 0)) 
        
        penSim.xNew$ADC.ER[j] <- ifelse(penSim.xNew$ADC.preTot[j] > penSim.xNew$EEC.preTot[j], with(penSim.xNew, ADC[j] - EEC[j]), 0)
        penSim.New$ADC.ER[j]  <- ifelse(penSim.xNew$ADC.preTot[j] > penSim.xNew$EEC.preTot[j], with(penSim.New,  ADC[j] - EEC[j]), 0)
        
        
          
        # Adjustment of EEC (CAUTION: DO NOT use this for PSERS)
        if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
      
      } else {
        # Allow for negative ADC and C  (CAUTION: DO NOT use this for PSERS)
        penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
        
        if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
        # EEC is not fixed
          # 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
        } else if(with(penSim, ADC[j] > EEC[j])) {
          penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
          # 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
        } else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
          penSim$ADC.ER[j] <- 0
          penSim$EEC[j]    <- with(penSim, ADC[j])
          # 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
        } else if(with(penSim, ADC[j] <= 0)) {
          penSim$ADC.ER[j] <- with(penSim, ADC[j])
          penSim$EEC[j]    <- 0
        }
        
      }
      
      
      # ERC
      penSim.xNew$ERC[j] <- switch(ConPolicy,
                              ADC     = with(penSim.xNew, ADC.ER[j]),                          # Full ADC
                              ADC_cap = with(penSim.xNew, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                              Fixed   = with(penSim.xNew, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 
    
      penSim.New$ERC[j] <- switch(ConPolicy,
                              ADC     = with(penSim.New, ADC.ER[j]),                          # Full ADC
                              ADC_cap = with(penSim.New, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                              Fixed   = with(penSim.New, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 

      #**************************************************************************************************************
      #                                        PSERS: ERC cap 
      #**************************************************************************************************************
      # For fiscal years ending on or after June 30, 2014, the pension contribution rate can be no more than 4.5% of the total conpensation of all active members, 
      # greater than the prior year's final contribution rate. 
      # 
      
      if(useERC_cap & k!= -1){
        
        # aggregate variables
        
        penSim.xNew$PR.tot[j]  <- penSim.xNew$PR[j]  + penSim.New$PR[j]
        penSim.xNew$ERC.tot[j] <- penSim.xNew$ERC[j] + penSim.New$ERC[j]
        penSim.xNew$NC.tot[j] <-  penSim.xNew$NC[j]  + penSim.New$NC[j]
        penSim.xNew$EEC.tot[j] <- penSim.xNew$EEC[j] + penSim.New$EEC[j]
        
        if(j == 1){
          #PSERS: Limit ERC rate at model year 2016 (FY 2016-2017) to 29.5%
          
          if(penSim.xNew$PR.tot[j] > 0){
          penSim.xNew$ERC.final[j] <- ifelse(penSim.xNew$ERC.tot[j]/penSim.xNew$PR.tot[j] >= 0.295,
                                        0.295 * penSim.xNew$PR[j],
                                        penSim.xNew$ERC[j])
          
          penSim.New$ERC.final[j] <- ifelse(penSim.xNew$ERC.tot[j]/penSim.xNew$PR.tot[j] >= 0.295,
                                             0.295 * penSim.New$PR[j],
                                             penSim.New$ERC[j])
          
          } else {
            penSim.xNew$ERC.final[j] <- penSim.xNew$ERC[j]
            penSim.New$ERC.final[j]  <- penSim.New$ERC[j]
          }
          
        } else {
          
          penSim.xNew$ERC.final.tot[j-1] <- penSim.xNew$ERC.final[j-1] + penSim.New$ERC.final[j-1]
          
          # Constraint 1: ERC.final as a % of payroll year in j+1 cannot be greater than the rate + 4.5% in year j
          
          if(penSim.xNew$PR.tot[j] > 0 & penSim.xNew$PR.tot[j - 1] > 0 ){
          
            penSim.xNew$ERC.final[j] <- ifelse(penSim.xNew$ERC.tot[j]/penSim.xNew$PR.tot[j] >= (penSim.xNew$ERC.final.tot[j - 1]/penSim.xNew$PR.tot[j - 1] + 0.045),
                                              (penSim.xNew$ERC.final[j - 1]/penSim.xNew$PR[j - 1] + 0.045) * penSim.xNew$PR[j],
                                               penSim.xNew$ERC[j]) 
            
             if(penSim.New$PR[j] > 0 & penSim.New$PR[j - 1] > 0 ){
              penSim.New$ERC.final[j] <- ifelse(penSim.xNew$ERC.tot[j]/penSim.xNew$PR.tot[j] >= (penSim.xNew$ERC.final.tot[j - 1]/penSim.xNew$PR.tot[j - 1] + 0.045),
                                               (penSim.New$ERC.final[j - 1]/penSim.New$PR[j - 1] + 0.045) * penSim.New$PR[j],
                                                penSim.New$ERC[j])
             } else {penSim.New$ERC.final[j]  <- penSim.New$ERC[j]} 
          
          
          } else {
            penSim.xNew$ERC.final[j] <- penSim.xNew$ERC[j]
            penSim.New$ERC.final[j]  <- penSim.New$ERC[j]
            } 
          
          # Constraint 2: If contraint 1 is not triggered, then ERC.final should be at least as much as the employer NC rate (total NC - EEC).
          if(useERC_floor) {
            
            penSim.xNew$ERC.final.tot[j] <- penSim.xNew$ERC.final[j] + penSim.New$ERC.final[j]
            
            if(penSim.xNew$ERC.final.tot[j] == penSim.xNew$ERC.tot[j]){
              
              penSim.xNew$ERC.final[j] <- ifelse(penSim.xNew$ERC.final.tot[j] < (penSim.xNew$NC.tot[j] - penSim.xNew$EEC.tot[j]),   
                                                 penSim.xNew$NC[j] - penSim.xNew$EEC[j],
                                                 penSim.xNew$ERC.final[j]) 
              
              penSim.New$ERC.final[j]  <- ifelse(penSim.xNew$ERC.final.tot[j] < (penSim.xNew$NC.tot[j] - penSim.xNew$EEC.tot[j]),   
                                                 penSim.New$NC[j] - penSim.New$EEC[j],
                                                 penSim.New$ERC.final[j]) 
            }
          }
        }
        
      } else {
        penSim.xNew$ERC.final[j] <- penSim.xNew$ERC[j]
        penSim.New$ERC.final[j]  <- penSim.New$ERC[j]
        }
      
      
      
      
      
      #*************************************************************************************************************
      #                                       Current tiers: Part 3  ####
      #*************************************************************************************************************
      
      # C(j)
      
      penSim.xNew$C[j] <- with(penSim.xNew, EEC[j] + ERC.final[j])
      
      # C(j) - ADC(j)
      penSim.xNew$C_ADC[j] <- with(penSim.xNew, C[j] - ADC[j])
      
      # Ia(j), Ib(j), Ic(j)
      penSim.xNew$Ia[j] <- with(penSim.xNew,  MA[j] * i[j])
      penSim.xNew$Ib[j] <- with(penSim.xNew,  B[j] * i[j])
      penSim.xNew$Ic[j] <- with(penSim.xNew,  C[j] * i[j])
      
      
      # I.e(j)
      # penSim.xNew$I.e[j] <- with(penSim.xNew, Ia[j] + Ic[j] - Ib[j])
      penSim.xNew$I.e[j] <- with(penSim.xNew, i[j] *(MA[j] + C[j] - B[j]))
      
      # I.r(j)
      penSim.xNew$I.r[j] <- with(penSim.xNew, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
      
      # I.dif(j) = I.r(j) - I.e(j):  used in asset smoothing 
      penSim.xNew$I.dif[j] <- with(penSim.xNew, I.r[j] - I.e[j])
      
      
      #*************************************************************************************************************
      #                                       New tiers: Part 3  ####
      #*************************************************************************************************************
      
      # C(j)
      
      penSim.New$C[j] <- with(penSim.New, EEC[j] + ERC.final[j])
      
      # C(j) - ADC(j)
      penSim.New$C_ADC[j] <- with(penSim.New, C[j] - ADC[j])
      
      # Ia(j), Ib(j), Ic(j)
      penSim.New$Ia[j] <- with(penSim.New,  MA[j] * i[j])
      penSim.New$Ib[j] <- with(penSim.New,  B[j] * i[j])
      penSim.New$Ic[j] <- with(penSim.New,  C[j] * i[j])
      
      
      # I.e(j)
      # penSim.New$I.e[j] <- with(penSim.New, Ia[j] + Ic[j] - Ib[j])
      penSim.New$I.e[j] <- with(penSim.New, i[j] *(MA[j] + C[j] - B[j]))
      
      # I.r(j)
      penSim.New$I.r[j] <- with(penSim.New, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
      
      # I.dif(j) = I.r(j) - I.e(j):  used in asset smoothing 
      penSim.New$I.dif[j] <- with(penSim.New, I.r[j] - I.e[j])
    }
    
    # penSim_results[[k]] <- penSim
    # as.data.frame(penSim)
    
    bind_rows(
      list(xNew = as.data.frame(penSim.xNew) %>% mutate(Tier = "sumTiers.xNew"), 
           New  = as.data.frame(penSim.New)  %>% mutate(Tier = "sumTiers.New"))
    )
    
  }
  
  stopCluster(cl)
  

  #*************************************************************************************************************
  #                                  Combining results into a data frame.   ####
  #*************************************************************************************************************
  #penSim_results %>% head

  penSim_results <- bind_rows(penSim_results) %>% 
    mutate_all(funs(na2zero(.))) %>% 
    mutate(
           #sim     = rep(-1:nsim, each = nyear),
           runname = runname,
           returnScn = returnScn,
           policy.SR = policy.SR,
           policy.EL = policy.EL,
           policy.reform = DC_reform,
           # Tier    = Tier_select_,
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
           ERC.final_PR = 100 * ERC.final / PR,
           EEC_PR  = 100 * EEC / PR, 
           C_PR    = 100 * C / PR,
           B_PR    = 100 * B / PR,
           ExF     = C - B,
           ExF_PR  = 100 * ExF / PR,
           ExF_MA  = 100 * ExF / MA, 
           PR.growth = ifelse(year > 1, 100 * (PR / lag(PR) - 1), NA),
           
           DC_EEC = DC_EEC_tCD + DC_EEC_tE + DC_EEC_tF + DC_EEC_tNE + DC_EEC_tNF,
           DC_ERC = DC_ERC_tCD + DC_ERC_tE + DC_ERC_tF + DC_ERC_tNE + DC_ERC_tNF,
           
           DC_ERC_PR.tEF = 100 * DC_ERC / (PR_tNE + PR_tNF),
           DC_ERC_PR     = 100 * DC_ERC / PR
           
           ) %>%
    select(runname, sim, year, everything())
  
  return(penSim_results)
  
}


# 
 # start_time_loop <- proc.time()
 # 
 # penSim_results <- run_sim()
 # 
 # end_time_loop <- proc.time()
 # Time_loop <- end_time_loop - start_time_loop 
 # Time_loop
 # 
 # 
 # 


# penSim_results %>% select(sim, year, DC_EEC_tNE)

# This script conducts the simulation of the finance of UCRP



run_sim.wt7 <- function(Tier_select_,
                    AggLiab.xt7_,
                    AggLiab.t7_,
                    i.r_ = i.r,
                    init_amort_raw.xt7_ = init_amort_raw, # amount.annual, year.remaining 
                    init_unrecReturns.unadj.xt7_ = init_unrecReturns.unadj,
                    init_amort_raw.t7_, # amount.annual, year.remaining 
                    init_unrecReturns.unadj.t7_,
                    paramlist_ = paramlist,
                    Global_paramlist_ = Global_paramlist){

  # Run the section below when developing new features.
      # Tier_select_ =  "sumTiers" #  Tier_select
      # i.r_ = i.r
      # AggLiab.xt7_        = AggLiab.sumTiers
      # AggLiab.t7_         = AggLiab.t7
      # 
      # init_amort_raw.xt7_ = init_amort_raw
      # init_unrecReturns.unadj.xt7_ = init_unrecReturns.unadj
      # 
      # init_amort_raw.t7_ = init_amort_raw %>% mutate(balance = 0, annual.payment = 0)
      # init_unrecReturns.unadj.t7_ = init_unrecReturns.unadj %>% mutate(DeferredReturn = 0)
      # 
      # paramlist_      = paramlist
      # Global_paramlist_ = Global_paramlist

  
  
      # Tier_select_ =  "t7" #  Tier_select
      # i.r_ = i.r
      # AggLiab_        = AggLiab.t7
      # init_amort_raw_ = init_amort_raw %>% mutate(balance = 0, annual.payment = 0)
      # init_unrecReturns.unadj_ = init_unrecReturns.unadj %>% mutate(DeferredReturn = 0)
      # paramlist_      = paramlist
      # Global_paramlist_ = Global_paramlist
  
  assign_parmsList(Global_paramlist_, envir = environment())
  assign_parmsList(paramlist_,        envir = environment())
  
  #if(Tier_select_ != "sumTiers") init_amort_raw_ %<>% filter(tier == Tier_select_) 

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
  
  
  
  # Set up data frame for current tiers
  penSim0.xt7 <- data.frame(year = init.year:(init.year + nyear - 1)) %>%
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
  
  # Set up data frame for tier 7
  penSim0.t7 <- data.frame(year = init.year:(init.year + nyear - 1)) %>%
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
  
  
  
  
  # Vector used in asset smoothing
  s.vector.xt7 <- seq(0,1,length = s.year + 1)[-(s.year+1)]
  s.vector.t7  <- seq(0,1,length = s.year + 1)[-(s.year+1)] 
  s.vector  <- seq(0,1,length = s.year + 1)[-(s.year+1)] 
  
  #*************************************************************************************************************
  #                                   Introducing external fund   #### 
  #*************************************************************************************************************  
  
  # extFund$extFund <- rowSums(select(extFund, -year)) # total external fund
  # 
  # penSim0 %<>% left_join(extFund) %>% 
  #              mutate(extFund = na2zero(extFund))

  
  

  #*************************************************************************************************************
  #                                 Defining variables in simulation: Current tiers  ####
  #*************************************************************************************************************
  
  # AL(j)
  penSim0.xt7$AL.act.laca <- AggLiab.xt7_$active[, "ALx.laca.sum"]
  penSim0.xt7$AL.act.v    <- AggLiab.xt7_$active[, "ALx.v.sum"]
  penSim0.xt7$AL.act.death<- AggLiab.xt7_$active[, "ALx.death.sum"]
  penSim0.xt7$AL.act.disb <- AggLiab.xt7_$active[, "ALx.disb.sum"]
  penSim0.xt7$AL.act      <-  with(penSim0.xt7, AL.act.laca + AL.act.v + penSim0.xt7$AL.act.death + penSim0.xt7$AL.act.disb)
  
  penSim0.xt7$AL.la    <- AggLiab.xt7_$la[,   "ALx.la.sum"]
  penSim0.xt7$AL.ca    <- AggLiab.xt7_$ca[,   "liab.ca.sum"]
  penSim0.xt7$AL.term  <- AggLiab.xt7_$term[, "ALx.v.sum"]
  penSim0.xt7$AL.death <- AggLiab.xt7_$death[,"ALx.death.sum"]
  penSim0.xt7$AL.disb.la  <- AggLiab.xt7_$disb.la[, "ALx.disb.la.sum"]
  penSim0.xt7$AL.disb.ca  <- AggLiab.xt7_$disb.ca[, "ALx.disb.ca.sum"]
  
  penSim0.xt7$AL       <- with(penSim0.xt7, AL.act + AL.la + AL.ca +  AL.term + AL.death + AL.disb.la + AL.disb.ca)
  
  
  # NC(j)
  penSim0.xt7$NC.laca <- AggLiab.xt7_$active[, "NCx.laca.sum"]
  penSim0.xt7$NC.v    <- AggLiab.xt7_$active[, "NCx.v.sum"]
  penSim0.xt7$NC.death<- AggLiab.xt7_$active[, "NCx.death.sum"]
  penSim0.xt7$NC.disb <- AggLiab.xt7_$active[, "NCx.disb.sum"] 
  penSim0.xt7$NC      <-  with(penSim0.xt7, NC.laca + NC.v + NC.death + NC.disb)
  
  
  # PVFB(j)
  penSim0.xt7$PVFB.laca <- AggLiab.xt7_$active[, "PVFBx.laca.sum"]
  penSim0.xt7$PVFB.v    <- AggLiab.xt7_$active[, "PVFBx.v.sum"]
  penSim0.xt7$PVFB.death<- AggLiab.xt7_$active[, "PVFBx.death.sum"]
  penSim0.xt7$PVFB.disb <- AggLiab.xt7_$active[, "PVFBx.disb.sum"] 
  penSim0.xt7$PVFB      <-  with(penSim0.xt7, PVFB.laca + PVFB.v + PVFB.death + PVFB.disb) #Note this is the total PVFB for actives. PVFB for retirees/beneficiaries are the same as AL.
  
  # B(j)
  penSim0.xt7$B.la    <- AggLiab.xt7_$la[, "B.la.sum"]
  penSim0.xt7$B.ca    <- AggLiab.xt7_$ca[, "B.ca.sum"]
  penSim0.xt7$B.v     <- AggLiab.xt7_$term[, "B.v.sum"]
  penSim0.xt7$B.death <- AggLiab.xt7_$death[, "B.death.sum"]
  penSim0.xt7$B.disb.la  <- AggLiab.xt7_$disb.la[, "B.disb.la.sum"]
  penSim0.xt7$B.disb.ca  <- AggLiab.xt7_$disb.ca[, "B.disb.ca.sum"]
  penSim0.xt7$B       <- with(penSim0.xt7, B.la + B.ca + B.v + B.death + B.disb.la + B.disb.ca)
  
  # PR(j)
  penSim0.xt7$PR <- AggLiab.xt7_$active[, "PR.sum"]
  penSim0.xt7$PR_DROP <- (AggLiab.xt7_$la[, "sx_DROP.la.sum"] + AggLiab.xt7_$ca[, "sx_DROP.ca.sum"])*0.9  # payroll for DROP participants (LAFPP sepcific)
  
  # EEC(j) (LAFPP sepcific)
  penSim0.xt7$EEC      <- AggLiab.xt7_$active[,  "EEC.sum"]
  penSim0.xt7$EEC_DROP <- (AggLiab.xt7_$la[, "EEC_DROP.la.sum"] + AggLiab.xt7_$ca[, "EEC_DROP.ca.sum"]) * 0.9
  
  if(EEC_DROP){penSim0.xt7$EEC <- penSim0.xt7$EEC + penSim0.xt7$EEC_DROP
  penSim0.xt7$PR  <- penSim0.xt7$PR + penSim0.xt7$PR_DROP}
  
  # nactives, nretirees, nterms
  penSim0.xt7$nactives  <- AggLiab.xt7_$active[,  "nactives"]
  penSim0.xt7$nla       <- AggLiab.xt7_$la[, "nla"]
  penSim0.xt7$n.ca.R1   <- AggLiab.xt7_$ca[, "n.R1"]
  penSim0.xt7$n.ca.R0S1 <- AggLiab.xt7_$ca[, "n.R0S1"]
  penSim0.xt7$nterms    <- AggLiab.xt7_$term[, "nterms"]
  penSim0.xt7$ndeathBen <- AggLiab.xt7_$death[, "ndeathBen"]
  penSim0.xt7$ndisb.la  <- AggLiab.xt7_$disb.la[,  "ndisb.la"]
  penSim0.xt7$ndisb.ca.R1   <- AggLiab.xt7_$disb.ca[,  "n.disb.R1"]
  penSim0.xt7$ndisb.ca.R0S1 <- AggLiab.xt7_$disb.ca[,  "n.disb.R0S1"]
  
  penSim0.xt7 <- as.list(penSim0.xt7) # Faster to extract elements from lists than frame data frames.
  
  
  
  #*************************************************************************************************************
  #                                 Defining variables in simulation: Tier 7  ####
  #*************************************************************************************************************
  
  # AL(j)
  penSim0.t7$AL.act.laca <- AggLiab.t7_$active[, "ALx.laca.sum"]
  penSim0.t7$AL.act.v    <- AggLiab.t7_$active[, "ALx.v.sum"]
  penSim0.t7$AL.act.death<- AggLiab.t7_$active[, "ALx.death.sum"]
  penSim0.t7$AL.act.disb <- AggLiab.t7_$active[, "ALx.disb.sum"]
  penSim0.t7$AL.act      <-  with(penSim0.t7, AL.act.laca + AL.act.v + penSim0.t7$AL.act.death + penSim0.t7$AL.act.disb)
  
  penSim0.t7$AL.la    <- AggLiab.t7_$la[,   "ALx.la.sum"]
  penSim0.t7$AL.ca    <- AggLiab.t7_$ca[,   "liab.ca.sum"]
  penSim0.t7$AL.term  <- AggLiab.t7_$term[, "ALx.v.sum"]
  penSim0.t7$AL.death <- AggLiab.t7_$death[,"ALx.death.sum"]
  penSim0.t7$AL.disb.la  <- AggLiab.t7_$disb.la[, "ALx.disb.la.sum"]
  penSim0.t7$AL.disb.ca  <- AggLiab.t7_$disb.ca[, "ALx.disb.ca.sum"]
  
  penSim0.t7$AL       <- with(penSim0.t7, AL.act + AL.la + AL.ca +  AL.term + AL.death + AL.disb.la + AL.disb.ca)
  
  
  # NC(j)
  penSim0.t7$NC.laca <- AggLiab.t7_$active[, "NCx.laca.sum"]
  penSim0.t7$NC.v    <- AggLiab.t7_$active[, "NCx.v.sum"]
  penSim0.t7$NC.death<- AggLiab.t7_$active[, "NCx.death.sum"]
  penSim0.t7$NC.disb <- AggLiab.t7_$active[, "NCx.disb.sum"] 
  penSim0.t7$NC      <-  with(penSim0.t7, NC.laca + NC.v + NC.death + NC.disb)
  
  
  # PVFB(j)
  penSim0.t7$PVFB.laca <- AggLiab.t7_$active[, "PVFBx.laca.sum"]
  penSim0.t7$PVFB.v    <- AggLiab.t7_$active[, "PVFBx.v.sum"]
  penSim0.t7$PVFB.death<- AggLiab.t7_$active[, "PVFBx.death.sum"]
  penSim0.t7$PVFB.disb <- AggLiab.t7_$active[, "PVFBx.disb.sum"] 
  penSim0.t7$PVFB      <-  with(penSim0.t7, PVFB.laca + PVFB.v + PVFB.death + PVFB.disb) #Note this is the total PVFB for actives. PVFB for retirees/beneficiaries are the same as AL.
  
  # B(j)
  penSim0.t7$B.la    <- AggLiab.t7_$la[, "B.la.sum"]
  penSim0.t7$B.ca    <- AggLiab.t7_$ca[, "B.ca.sum"]
  penSim0.t7$B.v     <- AggLiab.t7_$term[, "B.v.sum"]
  penSim0.t7$B.death <- AggLiab.t7_$death[, "B.death.sum"]
  penSim0.t7$B.disb.la  <- AggLiab.t7_$disb.la[, "B.disb.la.sum"]
  penSim0.t7$B.disb.ca  <- AggLiab.t7_$disb.ca[, "B.disb.ca.sum"]
  penSim0.t7$B       <- with(penSim0.t7, B.la + B.ca + B.v + B.death + B.disb.la + B.disb.ca)
  
  # PR(j)
  penSim0.t7$PR <- AggLiab.t7_$active[, "PR.sum"]
  penSim0.t7$PR_DROP <- (AggLiab.t7_$la[, "sx_DROP.la.sum"] + AggLiab.t7_$ca[, "sx_DROP.ca.sum"])*0.9  # payroll for DROP participants (LAFPP sepcific)
  
  # EEC(j) (LAFPP sepcific)
  penSim0.t7$EEC      <- AggLiab.t7_$active[,  "EEC.sum"]
  penSim0.t7$EEC_DROP <- (AggLiab.t7_$la[, "EEC_DROP.la.sum"] + AggLiab.t7_$ca[, "EEC_DROP.ca.sum"]) * 0.9
  
  if(EEC_DROP){penSim0.t7$EEC <- penSim0.t7$EEC + penSim0.t7$EEC_DROP
  penSim0.t7$PR  <- penSim0.t7$PR + penSim0.t7$PR_DROP}
  
  # nactives, nretirees, nterms
  penSim0.t7$nactives  <- AggLiab.t7_$active[,  "nactives"]
  penSim0.t7$nla       <- AggLiab.t7_$la[, "nla"]
  penSim0.t7$n.ca.R1   <- AggLiab.t7_$ca[, "n.R1"]
  penSim0.t7$n.ca.R0S1 <- AggLiab.t7_$ca[, "n.R0S1"]
  penSim0.t7$nterms    <- AggLiab.t7_$term[, "nterms"]
  penSim0.t7$ndeathBen <- AggLiab.t7_$death[, "ndeathBen"]
  penSim0.t7$ndisb.la  <- AggLiab.t7_$disb.la[,  "ndisb.la"]
  penSim0.t7$ndisb.ca.R1   <- AggLiab.t7_$disb.ca[,  "n.disb.R1"]
  penSim0.t7$ndisb.ca.R0S1 <- AggLiab.t7_$disb.ca[,  "n.disb.R0S1"]
  
  
  penSim0.t7 <- as.list(penSim0.t7) # Faster to extract elements from lists than frame data frames.
  
  
  
  #***********************************************************************************************************************
  #                                  Adjust Benefit payments for DROP (LAFPP specific, only applied to current tiers) ####
  #***********************************************************************************************************************  

  B.model <- data.frame(year = 2016:2023, B = penSim0.xt7$B[1:8]) 
  B.model$B
  
  B.GASB <- data.frame(year = 2016:2023,
                       B    = 1e6*c(#970,
                                    1104,
                                    1050,
                                    1149,
                                    1267,
                                    1212,
                                    1283,
                                    1350,
                                    1416))
  
  # restriction 1: PVFB for 2015-2023
  R1.PVFB <- sum(B.model$B / (1 + i)^(2016:2023 - 2016))
  
  # restriction 2: Schedule of payments from GASB projection
  R2.GASB_scale <- B.GASB$B/B.GASB$B[1]
  
  # Adjustment factor
  adj.factor <- R1.PVFB/sum(R2.GASB_scale / (1 + i)^(2016:2023 - 2016))
  
  # Adjusted Benefits
  B.adj <-  data.frame(year = 2016:2023, 
                       B.adj1 = R2.GASB_scale* adj.factor)
  
  # Extra benefits: approximate DROP balance accumulated before 2016
    # B.adj %<>% mutate(B.extra = 0, B.extra.balance = 0) 
    # B.adj$B.extra.balance[1] <- 1369*3*6132*12
    # for(z in 1:5){
    #   B.adj$B.extra[z] <-  B.adj$B.extra.balance[z] / (5 - z + 1)
    #   if(z != nrow(B.adj)) B.adj$B.extra.balance[z + 1] <-  (B.adj$B.extra.balance[z] - B.adj$B.extra[z]) * 1.05
    # }

  
  
  B.adj %<>% mutate(B.extra = 0, B.extra.balance = 0) 
  B.adj$B.extra.balance[1] <- 239562356 +  282080479   # DROP balance as of 6/30/2016 CAFR2016 pdf p37, plus estimated benefit payment for remaining years in the DROP program.
  for(z in 1:5){
    
    if(z == 1){B.adj$B.extra[z] <- 105000000
    } else {
      B.adj$B.extra[z] <-  B.adj$B.extra.balance[z] / (5 - z + 1)} 
    
    if(z != nrow(B.adj)) B.adj$B.extra.balance[z + 1] <-  (B.adj$B.extra.balance[z] - B.adj$B.extra[z]) * 1.05
  }
  
  
  B.adj %<>% mutate(#B.extra = ifelse(year - 2015 < 5, B.extra/5, 0),
    B.adj2  = B.adj1 + B.extra)
  #if(Adj.benDROP) penSim0$B[1:9] <- B.adj$B.adj2
  
  penSim0.xt7$B.extra <- c(B.adj$B.extra, rep(0, nyear - nrow(B.adj)))
  penSim0.xt7$AL.initDROP <- order_by(-seq_len(nyear), cumsum(penSim0.xt7$B.extra/(1 + i)^(seq_len(nyear) - 1))) 
  
  
  #*************************************************************************************************************
  #                                  Setting up initial amortization payments: current tiers ####
  #*************************************************************************************************************  
  # matrix representation of amortization: better visualization but larger size
  m.max.xt7 <- max(init_amort_raw.xt7_$year.remaining)
  SC_amort0.xt7 <- matrix(0, nyear + m.max.xt7, nyear + m.max.xt7)
  # SC_amort0
  # data frame representation of amortization: much smaller size, can be used in real model later.
  # SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))
  
  # Amortization payment amounts for all prior years. 
  SC_amort.init.xt7 <- matrix(0, nrow(init_amort_raw.xt7_), nyear + m.max.xt7)
   
  # Adjustment factor for initial amortization payments (LAFPP specific)
    # Factor is defined as the initial model UAAL as a proportion of UAAL in AV2016.
    # CAUTION: the following formula only works when init_AA =  AL_pct, which is the case for LAFPP
  
  # factor.initAmort <- penSim0.xt7$AL[1]/ 18337507075
    factor.initAmort <- (penSim0.xt7$AL[1] + penSim0.xt7$AL.initDROP[1])/ 18798510534
    
  
  
  if(useAVamort){
    SC_amort.init.list.xt7 <- mapply(amort_LG, p = init_amort_raw.xt7_$balance * factor.initAmort , m = init_amort_raw.xt7_$year.remaining, method = init_amort_raw.xt7_$amort.method,
                                     MoreArgs = list(i = i, g = salgrowth_amort, end = FALSE), SIMPLIFY = F)
    
    for(j in 1:nrow(SC_amort.init.xt7)){
      SC_amort.init.xt7[j, 1:init_amort_raw.xt7_$year.remaining[j]] <- SC_amort.init.list.xt7[[j]]
    }
  }
  
  #17085208040/18337507075
  
  nrow.initAmort.xt7 <- nrow(SC_amort.init.xt7)
  
  SC_amort0.xt7 <- rbind(SC_amort.init.xt7, SC_amort0.xt7)
  # The amortization basis of year j should be placed in row nrow.initAmort + j - 1. 
  
  SC_amort0.xt7 %>% colSums()
  
  #*************************************************************************************************************
  #                                  Setting up initial amortization payments: Tier 7 ####
  #*************************************************************************************************************  
  # matrix representation of amortization: better visualization but larger size
  m.max.t7 <- max(init_amort_raw.t7_$year.remaining)
  SC_amort0.t7 <- matrix(0, nyear + m.max.t7, nyear + m.max.t7)
  # SC_amort0
  # data frame representation of amortization: much smaller size, can be used in real model later.
  # SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))
  
  # Amortization payment amounts for all prior years. 
  SC_amort.init.t7 <- matrix(0, nrow(init_amort_raw.t7_), nyear + m.max.t7)
  
  
  # Adjustment factor for initial amortization payments (LAFPP specific)
  # Factor is defined as the initial model UAAL as a proportion of UAAL in AV2016.
  # CAUTION: the following formula only works when init_AA =  AL_pct, which is the case for LAFPP
  
  factor.initAmort.t7 <- penSim0.t7$AL[1]/ 18798510534
  
  
  
  
  if(useAVamort){
    SC_amort.init.list.t7 <- mapply(amort_LG, p = init_amort_raw.t7_$balance * factor.initAmort.t7 , m = init_amort_raw.t7_$year.remaining, method = init_amort_raw.t7_$amort.method,
                                     MoreArgs = list(i = i, g = salgrowth_amort, end = FALSE), SIMPLIFY = F)
    
    for(j in 1:nrow(SC_amort.init.t7)){
      SC_amort.init.t7[j, 1:init_amort_raw.t7_$year.remaining[j]] <- SC_amort.init.list.t7[[j]]
    }
  }
  
  #17085208040/18337507075
  
  nrow.initAmort.t7 <- nrow(SC_amort.init.t7)
  
  SC_amort0.t7 <- rbind(SC_amort.init.t7, SC_amort0.t7)
  # The amortization basis of year j should be placed in row nrow.initAmort + j - 1. 

  
  
  #*************************************************************************************************************
  #                                       Simuation  ####
  #*************************************************************************************************************
    
  cl <- makeCluster(ncore) 
  registerDoParallel(cl)
  
  
  penSim_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
    # k <- 2
    # initialize
    penSim.xt7   <- penSim0.xt7
    SC_amort.xt7 <- SC_amort0.xt7
    
    penSim.t7   <- penSim0.t7
    SC_amort.t7 <- SC_amort0.t7
    
    
    
    if(k == -1) {SC_amort.xt7[,] <- 0; SC_amort.t7[,] <- 0}
    
    #if(Tier_select_ != "t7" & Adj.benDROP & k!= -1) penSim$B[1:9] <- B.adj$B.adj2  # Adjust benefit payments for DROP
    if(Tier_select_ != "t7" & Adj.benDROP & k!= -1){
      penSim.xt7$B[1:5] <- penSim.xt7$B[1:5] + B.adj$B.extra[1:5]  # Adjust benefit payments for DROP
      penSim.xt7$AL     <- penSim.xt7$AL + penSim.xt7$AL.initDROP
    }
    
    penSim.xt7[["i.r"]] <- i.r_[, as.character(k)]
    penSim.t7[["i.r"]]  <- i.r_[, as.character(k)]
    
    source("Functions.R")
    
    for (j in 1:nyear){
        
        # j <- 1
        # j <- 2
        # j <- 8
      #*************************************************************************************************************
      #                                       Current tiers: Part 1  ####
      #*************************************************************************************************************
      
      # MA(j) and EAA(j) 
      if(j == 1) {penSim.xt7$MA[j]  <- ifelse(k == -1, penSim.xt7$AL[j],                   # k = -1 is for testing model consistency
                                          switch(init_MA, 
                                                 MA = MA_0,                        # Use preset value
                                                 AL = penSim.xt7$AL[j],                # Assume inital fund equals inital liability.
                                                 AL_pct = penSim.xt7$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
                                          ) 
                 penSim.xt7$EAA[j] <- switch(init_EAA,
                                         AL = EAA_0,                       # Use preset value 
                                         MA = penSim.xt7$MA[j])                # Assume inital EAA equals inital market value.
                 
                 penSim.xt7$AA[j]  <- ifelse(init_AA == "AL_pct" & k != -1, penSim.xt7$AL[j] * AA_0_pct,
                                             ifelse(init_AA == "AA" & k != -1, AA_0, 
                                                    switch(smooth_method,
                                                           method1 =  with(penSim.xt7, MA[j]),   # we may want to allow for a preset initial AA.
                                                           method2 =  with(penSim.xt7, (1 - w) * EAA[j] + w * MA[j])
                                                           )
                                             )
      )
      } else {
        penSim.xt7$MA[j]  <- with(penSim.xt7, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim.xt7$EAA[j] <- with(penSim.xt7, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim.xt7$AA[j]  <- switch(smooth_method,
                                method1 = with(penSim.xt7, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),
                                method2 = with(penSim.xt7, (1 - w) * EAA[j] + w * MA[j]) 
        )
      }
      
      
      ## Initial unrecognized returns
      if((init_AA %in% c("AL_pct", "AA0")) & useAVunrecReturn & k != -1 & Tier_select_ == "sumTiers"){

        # Adjusting initila unrecognized returns
        init_unrecReturns.adj.xt7 <-  mutate(init_unrecReturns.unadj.xt7_, DeferredReturn = DeferredReturn * (penSim.xt7$MA[1] - penSim.xt7$AA[1])/sum(DeferredReturn),
                                                                   DeferredReturn.annualTot = sum(DeferredReturn) - cumsum(DeferredReturn) # Initial unrecognized return to be subtracted from AA in each year
                                         )

        # Adjust AA for inital unrecognized returns
        #mm <- j - 1
        if((j - 1 + init.year) %in% init_unrecReturns.adj.xt7$year) penSim.xt7$AA[j] <- penSim.xt7$AA[j] - with(init_unrecReturns.adj.xt7, DeferredReturn.annualTot[year == (j - 1 + init.year)])
            
           # init_unrecReturns.adj[init_unrecReturns.adj$year - init.year + 1 == j, "DeferredReturn"] #  )

      }
        
      
      #*************************************************************************************************************
      #                                       Tier 7: Part 1  ####
      #*************************************************************************************************************
      
      # MA(j) and EAA(j) 
      if(j == 1) {penSim.t7$MA[j]  <- ifelse(k == -1, penSim.t7$AL[j],                   # k = -1 is for testing model consistency
                                             switch(init_MA, 
                                                    MA = MA_0,                        # Use preset value
                                                    AL = penSim.t7$AL[j],                # Assume inital fund equals inital liability.
                                                    AL_pct = penSim.t7$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
      ) 
      penSim.t7$EAA[j] <- switch(init_EAA,
                                 AL = EAA_0,                       # Use preset value 
                                 MA = penSim.t7$MA[j])                # Assume inital EAA equals inital market value.
      
      penSim.t7$AA[j]  <- ifelse(init_AA == "AL_pct" & k != -1, 
                                 penSim.t7$AL[j] * AA_0_pct,
                                 switch(smooth_method,
                                        method1 =  with(penSim.t7, MA[j]),   # we may want to allow for a preset initial AA.
                                        method2 =  with(penSim.t7, (1 - w) * EAA[j] + w * MA[j])
                                 )
      )
      } else {
        penSim.t7$MA[j]  <- with(penSim.t7, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim.t7$EAA[j] <- with(penSim.t7, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim.t7$AA[j]  <- switch(smooth_method,
                                   method1 = with(penSim.t7, MA[j] - sum(s.vector[max(s.year + 2 - j, 1):s.year] * I.dif[(j-min(j, s.year + 1)+1):(j-1)])),
                                   method2 = with(penSim.t7, (1 - w) * EAA[j] + w * MA[j]) 
        )
      }
      
      
      ## Initial unrecognized returns
      # if((init_AA %in% c("AL_pct", "AA0")) & useAVunrecReturn & k != -1 & Tier_select_ == "sumTiers"){
      #   
      #   # Adjusting initila unrecognized returns
      #   init_unrecReturns.adj.t7 <-  mutate(init_unrecReturns.unadj.t7_, DeferredReturn = DeferredReturn * (penSim.t7$MA[1] - penSim.t7$AA[1])/sum(DeferredReturn),
      #                                        DeferredReturn.annualTot = sum(DeferredReturn) - cumsum(DeferredReturn) # Initial unrecognized return to be subtracted from AA in each year
      #   )
      #   
      #   # Adjust AA for inital unrecognized returns
      #   #mm <- j - 1
      #   if((j - 1 + init.year) %in% init_unrecReturns.adj.t7$year) penSim.t7$AA[j] <- penSim.t7$AA[j] - with(init_unrecReturns.adj.t7, DeferredReturn.annualTot[year == (j - 1 + init.year)])
      #   
      #   # init_unrecReturns.adj[init_unrecReturns.adj$year - init.year + 1 == j, "DeferredReturn"] #  )
      #   
      # }
      
      
      
      # ## Apply corridor for MA, MA must not deviate from AA by more than 40%. 
      # 
      # penSim.xt7$AA[j] <- with(penSim.xt7, ifelse(AA[j] > s.upper * MA[j], MA[j], AA[j])) 
      # penSim.xt7$AA[j] <- with(penSim.xt7, ifelse(AA[j] < s.lower * MA[j], MA[j], AA[j]))
      # 

      
      
      #*************************************************************************************************************
      #            ## Apply corridor for AA, AA must not deviate from MA by more than 40%.  ####
      #*************************************************************************************************************
      
      # The corridor is applied based on the total AA and MA of the current tiers and tier 7. 
      
      if((penSim.xt7$AA[j] + penSim.t7$AA[j]) > s.upper * (penSim.xt7$MA[j] + penSim.t7$MA[j])){
        penSim.xt7$AA[j] <-  penSim.xt7$MA[j]
        penSim.t7$AA[j]  <-  penSim.t7$MA[j]
      } else {
        penSim.xt7$AA[j] <-  penSim.xt7$AA[j]
        penSim.t7$AA[j]  <-  penSim.t7$AA[j]
      }
      
      
      if((penSim.xt7$AA[j] + penSim.t7$AA[j]) < s.lower * (penSim.xt7$MA[j] + penSim.t7$MA[j])){
        penSim.xt7$AA[j] <-  penSim.xt7$MA[j]
        penSim.t7$AA[j]  <-  penSim.t7$MA[j]
      } else {
        penSim.xt7$AA[j] <-  penSim.xt7$AA[j]
        penSim.t7$AA[j]  <-  penSim.t7$AA[j]
      }
      
      # 
      # penSim.xt7$AA[j] <- ifelse((penSim.xt7$AA[j] + penSim.t7$AA[j]) > s.upper * (penSim.xt7$MA[j] + penSim.t7$MA[j]), penSim.xt7$MA[j], penSim.xt7$AA[j])
      # penSim.t7$AA[j]  <- ifelse((penSim.xt7$AA[j] + penSim.t7$AA[j]) > s.upper * (penSim.xt7$MA[j] + penSim.t7$MA[j]), penSim.t7$MA[j],  penSim.t7$AA[j])
      # 
      # penSim.xt7$AA[j] <- ifelse((penSim.xt7$AA[j] + penSim.t7$AA[j]) < s.lower * (penSim.xt7$MA[j] + penSim.t7$MA[j]), penSim.xt7$MA[j], penSim.xt7$AA[j])
      # penSim.t7$AA[j]  <- ifelse((penSim.xt7$AA[j] + penSim.t7$AA[j]) < s.lower * (penSim.xt7$MA[j] + penSim.t7$MA[j]), penSim.t7$MA[j],  penSim.t7$AA[j])
      # 
      
      
      
    
      #*************************************************************************************************************
      #                                       Current tiers: Part 2  ####
      #*************************************************************************************************************
      
      # UAAL(j)
      penSim.xt7$UAAL[j]    <- with(penSim.xt7, AL[j] - AA[j])
      # penSim.xt7$UAAL.MA[j] <- with(penSim.xt7, AL[j] - MA[j])
      
      
      # LG(j)
      # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
      if (j == 1){
        penSim.xt7$EUAAL[j] <- 0
        penSim.xt7$LG[j] <- with(penSim.xt7,  UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
        penSim.xt7$Amort_basis[j] <- with(penSim.xt7, LG[j])  # This will not be used for UCRP since the amortization scheme for year 1 is provided by SC_amort.xt7.(from AV2015)
        
      } else {
        penSim.xt7$EUAAL[j] <- with(penSim.xt7, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        
        # if(j %in% (B.adj$year - init.year + 1 + 1)) penSim.xt7$EUAAL[j] <- penSim.xt7$EUAAL[j] + (B.adj[B.adj$year == j + init.year - 1 - 1,]$B.extra) * (1 + i) # For LAFPP. adjustment for initial DROP benefit balance is not used in the calculation of losses/gains.
        
        penSim.xt7$LG[j]    <- with(penSim.xt7,  UAAL[j] - EUAAL[j])
        penSim.xt7$Amort_basis[j]    <- with(penSim.xt7,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
      
        # penSim.xt7$EUAAL.MA[j] <- with(penSim.xt7, (UAAL.MA[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        # penSim.xt7$LG[j]    <- with(penSim.xt7,  UAAL.MA[j] - EUAAL.MA[j])
        # penSim.xt7$Amort_basis[j]    <- with(penSim.xt7,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
        
      }   
      
      # # Amortize LG(j)
    
      #if(j > 1){ 
      if(j > ifelse(useAVamort, 1, 0)){ 
        # if useAVamort is TRUE, AV amort will be used for j = 1, not the one calcuated from the model. This may cause inconsistency in the model results 
        if(amort_type == "closed") SC_amort.xt7[nrow.initAmort.xt7 + j - 1, j:(j + m - 1)] <- amort_LG(penSim.xt7$Amort_basis[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)  
        }
      
      # Supplemental cost in j
      penSim.xt7$SC[j] <- switch(amort_type,
                             closed = sum(SC_amort.xt7[, j], na.rm = TRUE),
                             open   = amort_LG(penSim.xt7$UAAL[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
      
      #**************************************************************************************************************
      
  
      
      
    
      
      #*************************************************************************************************************
      #                                       Tier 7: Part 2  ####
      #*************************************************************************************************************
      
      # UAAL(j)
      penSim.t7$UAAL[j]    <- with(penSim.t7, AL[j] - AA[j])
      # penSim.t7$UAAL.MA[j] <- with(penSim.t7, AL[j] - MA[j])
      
      
      # LG(j)
      # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
      if (j == 1){
        penSim.t7$EUAAL[j] <- 0
        penSim.t7$LG[j] <- with(penSim.t7,  UAAL[j])  # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
        penSim.t7$Amort_basis[j] <- with(penSim.t7, LG[j])  # This will not be used for UCRP since the amortization scheme for year 1 is provided by SC_amort.t7.(from AV2015)
        
      } else {
        penSim.t7$EUAAL[j] <- with(penSim.t7, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        penSim.t7$LG[j]    <- with(penSim.t7,  UAAL[j] - EUAAL[j])
        penSim.t7$Amort_basis[j]    <- with(penSim.t7,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
        
        # penSim.t7$EUAAL.MA[j] <- with(penSim.t7, (UAAL.MA[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        # penSim.t7$LG[j]    <- with(penSim.t7,  UAAL.MA[j] - EUAAL.MA[j])
        # penSim.t7$Amort_basis[j]    <- with(penSim.t7,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
        
      }   
      
      # # Amortize LG(j)
      
      #if(j > 1){ 
      if(j > ifelse(useAVamort, 1, 0)){ 
        # if useAVamort is TRUE, AV amort will be used for j = 1, not the one calcuated from the model. This may cause inconsistency in the model results 
        if(amort_type == "closed") SC_amort.t7[nrow.initAmort.t7 + j - 1, j:(j + m - 1)] <- amort_LG(penSim.t7$Amort_basis[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)  
      }
      
      # Supplemental cost in j
      penSim.t7$SC[j] <- switch(amort_type,
                                 closed = sum(SC_amort.t7[, j], na.rm = TRUE),
                                 open   = amort_LG(penSim.t7$UAAL[j], i, m, salgrowth_amort, end = FALSE, method = amort_method)[1])
      
      #**************************************************************************************************************
      
      
      
      #########################################################################################################################################
      
      #*************************************************************************************************************
      #        Calculating contributions for current tiers and tier 7 jointly (LAFPP specific)                  ####
      #*************************************************************************************************************
      

      # Employee contribution, based on payroll. May be adjusted later. 
      # penSim$EEC[j] <- with(penSim, PR[j] * EEC_rate)
      # penSim$EEC[j] <- with(penSim, EEC[j])
      
      
      # ADC(j)
      
      if(nonNegC){
        
        # Calculate ADC as if current tiers and tier 7 are modeled jointly.
        
        penSim.xt7$ADC[j] <- with(penSim.xt7, NC[j] + SC[j])
        penSim.t7$ADC[j]  <- with(penSim.t7,  NC[j] + SC[j])
        
        penSim.xt7$ADC.preTot[j] <- max(0,  with(penSim.xt7, ADC[j]) +  with(penSim.t7, ADC[j]))
        
        penSim.xt7$ADC[j] <- ifelse(penSim.xt7$ADC.preTot[j] > 0, penSim.xt7$ADC[j], 0 )
        penSim.t7$ADC[j]  <- ifelse(penSim.xt7$ADC.preTot[j] > 0, penSim.t7$ADC[j],  0 )
        
        
        penSim.xt7$EEC.preTot[j] <- with(penSim.xt7, EEC[j]) +  with(penSim.t7, EEC[j])
        penSim.xt7$ADC.ER.preTot[j]  <- with(penSim.xt7, ifelse(ADC.preTot[j] > EEC.preTot[j], ADC.preTot[j] - EEC.preTot[j], 0)) 
        
        penSim.xt7$ADC.ER[j] <- ifelse(penSim.xt7$ADC.preTot[j] > penSim.xt7$EEC.preTot[j], with(penSim.xt7, ADC[j] - EEC[j]), 0)
        penSim.t7$ADC.ER[j]  <- ifelse(penSim.xt7$ADC.preTot[j] > penSim.xt7$EEC.preTot[j], with(penSim.t7,  ADC[j] - EEC[j]), 0)
        
        
        # Adjustment of EEC (CAUTION: DO NOT use this for LAFPP)
        if(!EEC_fixed) penSim.xt7$EEC.preTot[j] <- with(penSim, ifelse(ADC.preTot[j] > EEC.preTot[j], EEC.preTot[j], ADC.preTot[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
        
        
        
      } else { #(CAUTION: DO NOT use this for LAFPP)
        # Allow for negative ADC and C  
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
      penSim.xt7$ERC[j] <- switch(ConPolicy,
                              ADC     = with(penSim.xt7, ADC.ER[j]),                          # Full ADC
                              ADC_cap = with(penSim.xt7, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                              Fixed   = with(penSim.xt7, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 
      
      penSim.t7$ERC[j] <- switch(ConPolicy,
                                  ADC     = with(penSim.t7, ADC.ER[j]),                          # Full ADC
                                  ADC_cap = with(penSim.t7, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                                  Fixed   = with(penSim.t7, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 
      
      
      
      # if(j %in% plan_contributions$year) {
      #   penSim$ERC[j] <- as.numeric(plan_contributions[j == plan_contributions$year, "pct_ADC"]) * penSim$ERC[j]
      # }
      
      
      
      # C(j)
      penSim.xt7$C[j] <- with(penSim.xt7, EEC[j] + ERC[j])
      penSim.t7$C[j]  <- with(penSim.t7,  EEC[j] + ERC[j])
      
      
      
      # ERC cap for LAFPP
      
      # The ERC cap is determined by the smaller one of:
      # 13% of the total payroll of Tier 7. 
      # 50% of the total normal cost. (we interpret the "total cost of retirement benefits" as the the total normal cost.)
      # When the limit is triggered, the EEC of Tier 7 is calculated as the total ADC minus the capped ERC. 
      
      if(ERC_cap_C50) penSim.t7$ERC_cap[j] <- min(0.13 * penSim.t7$PR[j], 0.5 *  max(0, penSim.t7$NC[j] + penSim.t7$SC[j])) else
                      penSim.t7$ERC_cap[j] <- 0.13 * penSim.t7$PR[j]
      
      if(ERC_cap.initiatives){
        penSim.t7$ERC[j] <- with(penSim.t7, ifelse(ERC[j] > ERC_cap[j], ERC_cap[j], ERC[j]))
        penSim.t7$EEC[j] <- with(penSim.t7, C[j] - ERC[j])
      }
      
      
      #################################################################################################################################
      
      
      
      #*************************************************************************************************************
      #                                       Current tiers: Part 3  ####
      #*************************************************************************************************************
    
      # C(j) - ADC(j)
      penSim.xt7$C_ADC[j] <- with(penSim.xt7, C[j] - ADC[j])
      
      # Ia(j), Ib(j), Ic(j)
      penSim.xt7$Ia[j] <- with(penSim.xt7,  MA[j] * i[j])
      penSim.xt7$Ib[j] <- with(penSim.xt7,  B[j] * i[j])
      penSim.xt7$Ic[j] <- with(penSim.xt7,  C[j] * i[j])
      
      
      # I.e(j)
      # penSim.xt7$I.e[j] <- with(penSim.xt7, Ia[j] + Ic[j] - Ib[j])
      penSim.xt7$I.e[j] <- with(penSim.xt7, i[j] *(MA[j] + C[j] - B[j]))
      
      # I.r(j)
      penSim.xt7$I.r[j] <- with(penSim.xt7, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
      
      # I.dif(j) = I.r(j) - I.e(j):  used in asset smoothing 
      penSim.xt7$I.dif[j] <- with(penSim.xt7, I.r[j] - I.e[j])
      
      
      
      
      
      #*************************************************************************************************************
      #                                       Tier 7: Part 3  ####
      #*************************************************************************************************************
      
      # C(j) - ADC(j)
      penSim.t7$C_ADC[j] <- with(penSim.t7, C[j] - ADC[j])
      
      # Ia(j), Ib(j), Ic(j)
      penSim.t7$Ia[j] <- with(penSim.t7,  MA[j] * i[j])
      penSim.t7$Ib[j] <- with(penSim.t7,  B[j] * i[j])
      penSim.t7$Ic[j] <- with(penSim.t7,  C[j] * i[j])
      
      
      # I.e(j)
      # penSim.t7$I.e[j] <- with(penSim.t7, Ia[j] + Ic[j] - Ib[j])
      penSim.t7$I.e[j] <- with(penSim.t7, i[j] *(MA[j] + C[j] - B[j]))
      
      # I.r(j)
      penSim.t7$I.r[j] <- with(penSim.t7, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
      
      # I.dif(j) = I.r(j) - I.e(j):  used in asset smoothing 
      penSim.t7$I.dif[j] <- with(penSim.t7, I.r[j] - I.e[j])
      
    }
    
    # penSim_results[[k]] <- penSim
    #as.data.frame(penSim)
    bind_rows(
    list(xt7 = as.data.frame(penSim.xt7) %>% mutate(Tier = "xt7"), 
          t7 = as.data.frame(penSim.t7) %>% mutate(Tier = "t7"))
    )
  }
  
  stopCluster(cl)
  
  
  
  
  #*************************************************************************************************************
  #                                  Combining results into a data frame.   ####
  #*************************************************************************************************************
  
  
  penSim_results <- bind_rows(penSim_results) %>% 
    mutate(sim     = rep(-1:nsim, each = 2*nyear),
           runname = runname,
           run.returnScn = run.returnScn,
           run.policyScn = run.policyScn,
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
    select(runname, sim, year, everything())
  
  return(penSim_results)
  
}


#x <- penSim_results %>% filter(sim == 3)

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



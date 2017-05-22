
# use run "RS1_SR1EL1" in RunControl.xlsx
# PSERS_0_RunColtrol without sourcing the master file.
# Run PSERS_0_Master_allTiers upto the line "source("PSERS_Model_IndivLiab.R")".




decrement.model_ = decrement.model.tE
salary_          = salary.tE
benefit_         = benefit.tE
benefit.disb_    = benefit.disb.tE
init_terms_all_ = init_terms_all # get_tierData(init_terms_all, Tier_select)
Tier_select_     = "tE"
mortality.post.model_ = mortality.post.model.tE
liab.ca_         = liab.ca.tE
liab.disb.ca_ = liab.disb.ca.tE
paramlist_       =  paramlist
Global_paramlist_ =  Global_paramlist


assign_parmsList(Global_paramlist_, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(paramlist_,        envir = environment())

# Choosing tier specific parameters and data
fasyears <- tier.param[Tier_select_, "fasyears"]
r.vben   <- tier.param[Tier_select_, "r.vben"]
#r.yos    <- tier.param[Tier_select_, "r.yos"]
#r.age    <- tier.param[Tier_select_, "r.age"]
v.yos    <- tier.param[Tier_select_, "v.yos"]
cola     <- tier.param[Tier_select_, "cola"]
#EEC.rate <- tier.param[Tier_select_, "EEC.rate"]



init_terminated_ <-  get_tierData(init_terms_all_, Tier_select_)

#i <- 0.0725
i <- 0.0625
bfactor <- tier.param[Tier_select_, "bfactor"]



#*************************************************************************************************************
#                               1. Preparation                        #####                  
#*************************************************************************************************************
# Starts with a simple case with only 2 entry ages: 20 and 74

min.year <- min(init.year - (max.age - (r.max - 1)), 
                init.year - (r.max - 1 - min.ea)) 
# min(init.year - (benefit_$age - (r.min - 1))))

## Track down to the year that is the smaller one of the two below: 
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
# the year a r.max year old active in year 1 enter the workforce at age min.ea 

# liab.ca %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)

#liab.active 
liab.active <- expand.grid(start.year = min.year:(init.year + nyear - 1) , 
                           ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>% 
  left_join(salary_) %>%
  # left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  left_join(decrement.model_) %>% 
  left_join(mortality.post.model_ %>% filter(age == age.r) %>% select(age, ax.r.W)) %>%
  left_join(liab.ca_ %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)) %>% 
  left_join(liab.disb.ca_ %>% filter(age == age.disb) %>% select(age, liab.disb.ca.sum.1 = liab.ca.sum.1)) %>% 
  group_by(start.year, ea) %>%
  
  # filter(start.year == 2015, ea == 73) %>% 
  
  # Calculate salary and benefits
  mutate(
    yos= age - ea,
    
    # years of service
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    
    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx    = na2zero(bfactor * yos * fas),                  # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    Bx2   = na2zero(0.5*bfactor * yos * fas),                  # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    
    CumSalwInt = ifelse(age == ea, 0, lag(get_cumAsset(sx, i - 0.015, TRUE))),  # cumulated salary with intrests
    
    bx = lead(Bx) - Bx,                           # benefit accrual at age x
    
    # actuarial present value of future benefit, for $1's benefit in the initial year. 
    ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale),    # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    ax.disb.la  = get_tla(pxm.d, i, COLA.scale),     
    ax.vben     = get_tla(pxm.term, i, COLA.scale),   
    # ax.r.W.ret is already in mortality.post.model_
    
    # ax.r = get_tla(pxm.r, i, COLA.scale),       # ax calculated with mortality table for retirees. 
    
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i,  sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    # axr = ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i), rep(0, max.age - r.vben + 1))),                   # Similar to axR, but based on r.vben.  For calculation of term benefits when costs are spread up to r.vben.        
    # axrs= ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i,  sx[age<r.vben]), rep(0, max.age - r.vben + 1))),  # Similar to axRs, but based on r.vben. For calculation of term benefits when costs are spread up to r.vben.
    
    axr = ifelse(ea >= age_superFirst, 0, c(get_tla(pxT[age < age_superFirst], i), rep(0, max.age - unique(age_superFirst) + 1))),                             # Similar to axR, but based  on age_superFirst. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)       
    axrs= ifelse(ea >= age_superFirst, 0, c(get_tla(pxT[age < age_superFirst], i,  sx[age < age_superFirst]), rep(0, max.age - unique(age_superFirst) + 1))),  # Similar to axRs, but based on age_superFirst. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )


#liab.active %>% select(ea, age, ax.disb.la, ax.vben)
#liab.active %>% filter(start.year == 2016, ea == 30) %>% select(start.year, ea, age, sx, Bx.DC, Bx)

#*************************************************************************************************************
#                        2.1  ALs and NCs of life annuity and contingent annuity for actives             #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.laca = ifelse(elig_super == 1, 1,
                           ifelse(elig_super == 0 & elig_early == 1, (1 - 0.03 * (age_superFirst - age)),  0)),
          # gx.laca = 0,
          Bx.laca  = gx.laca * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          TCx.la   = lead(Bx.laca) * qxr.la * lead(ax.r.W) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
          TCx.ca   = lead(Bx.laca) * qxr.ca * lead(liab.ca.sum.1) * v,  # term cost of contingent annuity at the internal retirement age x (start to claim benefit at age x + 1)
          TCx.laca = TCx.la + TCx.ca,
          
          DB.value_PVret.half.bf = 0.5 * lead(Bx.laca) * lead(ax.r.W), # PV of DB benefit with 1/2 benefit factor
          
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.laca  = c(get_PVFB(pxT[age <= r.max], v, TCx.laca[age <= r.max]), rep(0, max.age - r.max)),
          
          # Term cost for DC plan if the entier salary is contributed into the DC fund.
          gx.DC     = gx.laca, # for now, assume the same eligibility and retirement rates for DC benefit.
          TCx.DC    = lead(CumSalwInt * gx.DC) * qxr * v,
          PVFBx.DC  = c(get_PVFB(pxT[age <= r.max], v, TCx.DC[age <= r.max]), rep(0, max.age - r.max)),
        
          DC.value_CumSal  = lead(CumSalwInt * gx.DC), # DC balance at each age if the entire salary is contributed each year.
          DC.value_balance = lead(CumSalwInt * gx.DC), # DC balance at each age if a fraction of salary is contributed each year.
                    
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          # 
          # # NC and AL of EAN.CD
          # NCx.EAN.CD.laca = ifelse(age < r.max, PVFBx.laca[age == min(age)]/ayx[age == r.max], 0),
          # ALx.EAN.CD.laca = PVFBx.laca - NCx.EAN.CD.laca * axR,
          # 
          # # NC and AL of EAN.CP
          NCx.EAN.CP.laca   = ifelse(age < r.max, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
          ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 


liab.active %<>%
  #filter(start.year == 2017, ea == 30) %>% 
  mutate(
         # Total DC rate 1: 
         # DC balance and 1/2 DB balance have the same PV at entry age
     
         DC_rate.tot = 0.5 * PVFBx.laca/PVFBx.DC,
         DC.value_balance = DC.value_CumSal * DC_rate.tot[age == ea],
         # TCx2.DC = TCx2.DC * DC_rate.tot[age == ea],
         
         # Total DC rate2:
         # DC balance = accured liability at a certain age (the age first eligible for super annuity)
         
         DC_rate.tot2 =   DB.value_PVret.half.bf[age == age_superFirst] / DC.value_CumSal[age == age_superFirst],
         
         NC_PR = NCx.EAN.CP.laca/sx) %>% 
  select(start.year, ea, age, age_superFirst, 
         sx, Bx, 
         NCx.EAN.CP.laca,
         
         PVFBx.laca, 
         PVFBx.DC, 
         DB.value_PVret.half.bf,
         DC.value_balance,
         DC.value_CumSal,
         DC_rate.tot,
         DC_rate.tot2,
         NC_PR
         #ax.r.W, liab.ca.sum.1, CumSalwInt
         ) %>% 
  mutate(Rate.1_2 = DC_rate.tot2/DC_rate.tot)


liab.active %>% filter(start.year == 2017, ea == 30) 
liab.active %>% filter(start.year == 2017, ea == age) 


# liab.active %>% filter(start.year == 2017, ea %in% 30) %>% select(start.year, ea, age, sx, CumSalwInt, Bx, PVFBx.laca, PVFBx.DC, CumSalwInt, gx.DC, TCx.DC, qxr, TCx.ca) %>% 
#   mutate(DC_rate.tot = 0.5 * PVFBx.laca/PVFBx.DC )


# DC_rate.tot <- 
# liab.active %>% filter(start.year == 2017, ea %in% range_ea, age == ea) %>% 
#  # mutate(DC_rate.tot = 0.5 * PVFBx.laca/PVFBx.DC ) %>% 
#   ungroup %>% 
#   select(ea, DC_rate.tot, DC_rate.tot2)
# 
# save(DC_rate.tot, file = "Data_inputs/DC_rate.tot725.RData")


DC_rate.tot <-
  liab.active %>% filter(start.year == 2017, ea %in% range_ea, age == ea) %>%
  # mutate(DC_rate.tot = 0.5 * PVFBx.laca/PVFBx.DC ) %>%
  ungroup %>%
  select(ea, DC_rate.tot, DC_rate.tot2)

save(DC_rate.tot, file = "Data_inputs/DC_rate.tot625.RData")











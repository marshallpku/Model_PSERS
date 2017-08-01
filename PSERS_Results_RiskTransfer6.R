# Risk measures for PSERS

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

library(grid)
library(gridExtra)

library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(xlsx)
library("btools")
library("scales")

source("Functions.R")



#*****************************************************
##  Defining paths for inputs and outputs         ####
#*****************************************************
IO_folder       <- "Results/"
Outputs_folder  <- "Results/Graphs_report/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    # if("results.t7" %in% names(outputs_list)){
    #   df_out <- bind_rows(outputs_list$results,
    #                       outputs_list$results.t7,
    #                       outputs_list$results.xt7)
    #   return(df_out)
    # } else {
    #   return(outputs_list$results)
    # }
    
    return(outputs_list$results)
    
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}



results_all <- get_results(IO_folder, "results_") %>% select(runname, sim, year, everything())


## Loading revenue projection data
load("GenFund_proj.RData")

results_all %<>% left_join(df_revenue %>% select(year, GenFund = GenFund.proj)) %>% 
  mutate(ERC.final_GF = 100 * ERC.final/GenFund, 
         AL_GF  = 100 * AL/GenFund)


#results_all %>% head


#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}


centeringTitles <- function(){
  theme(
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5),
    plot.caption=element_text(hjust=0, size = 9))
}


# load("Results/results_sumTiers_RS1_SR1EL1_sep_R725.d725.RData")
# 
# outputs_list$results %>% head


#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

rn_policy <- c("SR0EL0",
               "SR1EL1",
               "SR0EL1",
               "SR1EL0",
               "SR2EL1",
               "SR1EL2")

# Runs used in the report 
runs_RS1 <- paste0("RS1_", rn_policy)
runs_RS2 <- paste0("RS2_", rn_policy)
runs_RS3 <- paste0("RS3_", rn_policy)

runs_alt <- c("RS1_SR1EL1.open", "RS1_SR1EL1.PR")


runs_reform <- c("SR1EL1.Reform_R725.d725.DC1",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: PVDC = PVDB 
                 "SR1EL1.Reform_R625.d725.DC1",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: PVDC = PVDB 
                 "SR1EL1.Reform_R625.d625.DC1",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: PVDC = PVDB  
                
                 "SR1EL1.Reform_R725.d725.DC3",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 5% 
                 "SR1EL1.Reform_R625.d725.DC3",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 5% 
                 "SR1EL1.Reform_R625.d625.DC3",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 5% 
                 
                 "SR1EL1.Reform_R725.d725.DC3a",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 5%; reform applied to all current and future members
                 "SR1EL1.Reform_R625.d725.DC3a",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 5%; reform applied to all current and future members 

                 "SR1EL1.Reform_R725.d725.DC4",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 9% 
                 "SR1EL1.Reform_R625.d725.DC4",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 9% 
                 "SR1EL1.Reform_R625.d625.DC4",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 9% 
                 
                 "SR1EL1.Reform_R725.d725.DC4a",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 9%; reform applied to all current and future members
                 "SR1EL1.Reform_R625.d725.DC4a",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 9%; reform applied to all current and future members
                 
                 "RS1_SR1EL1_R625.d725",       # No DC reform with expected return = 6.25%, discount rate = 7.25% 
                 "RS1_SR1EL1_R625.d625")       # No DC reform with expected return = 6.25%, discount rate = 6.25%


runs_reform_sep <- c(
                 "RS1_SR1EL1_sep_R725.d725",       # No DC reform with expected return = 7.25%, discount rate = 7.25%; new hires modeled separately
                 "RS1_SR1EL1_sep_R625.d725",       # No DC reform with expected return = 6.25%, discount rate = 7.25%; new hires modeled separately
                 "RS1_SR1EL1_sep_R625.d625",       # No DC reform with expected return = 6.25%, discount rate = 6.25%; new hires modeled separately
  
                 # "SR1EL1.Reform_sep_R725.d725.DC3",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 5% ; new hires modeled separately
                 # "SR1EL1.Reform_sep_R625.d725.DC3",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 5% ; new hires modeled separately
                 # "SR1EL1.Reform_sep_R625.d625.DC3",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 5% ; new hires modeled separately
                 # 
                 # "SR1EL1.Reform_sep_R725.d725.DC3a",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 5%; reform applied to all current and future members; new hires modeled separately
                 # "SR1EL1.Reform_sep_R625.d725.DC3a",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 5%; reform applied to all current and future members; new hires modeled separately 
                 # 
                 "SR1EL1.Reform_sep_R725.d725.DC4",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 9% ; new hires modeled separately
                 "SR1EL1.Reform_sep_R625.d725.DC4",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 9% ; new hires modeled separately
                 "SR1EL1.Reform_sep_R625.d625.DC4",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 9% ; new hires modeled separately
                 
                 #"SR1EL1.Reform_sep_R725.d725.DC4a",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 9%; reform applied to all current and future members; new hires modeled separately
                 #"SR1EL1.Reform_sep_R625.d725.DC4a",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 9%; reform applied to all current and future members; new hires modeled separately
        
                 # Versions with EEC sharedRisk turned off
                 "RS1_SR0EL1_sep_R725.d725",
                 "RS1_SR0EL1_sep_R625.d725",
                 "RS1_SR0EL1_sep_R525.d725",
                 
                 "SR0EL1.Reform_sep_R725.d725.DC4",
                 "SR0EL1.Reform_sep_R625.d725.DC4",
                 "SR0EL1.Reform_sep_R525.d725.DC4",
                 
                 "SR0EL1.Reform_sep_R725.d725.DC4a",
                 "SR0EL1.Reform_sep_R625.d725.DC4a",
                 "SR0EL1.Reform_sep_R525.d725.DC4a",
                 
                 "SR0EL1.Reform_sep_R725.d725.DC5",
                 "SR0EL1.Reform_sep_R625.d725.DC5",
                 
                 "SR0EL1.Reform_sep_R625.d625.DC4",
                 "RS1_SR0EL1_sep_R625.d625"
                 )         





runs_RS1_labels <- c("Assumption Achieved: Baseline",
                     "Assumption Achieved: Current Policy",
                     "Assumption Achieved: ERC limit Only",
                     "Assumption Achieved: Risk-sharing Only",
                     "Assumption Achieved: Risk-sharing 5% max",
                     "Assumption Achieved: No ERC floor")

runs_RS2_labels <- c("15 Years of Low Returns : Baseline",
                     "15 Years of Low Returns : Current Policy",
                     "15 Years of Low Returns : ERC limit Only",
                     "15 Years of Low Returns : Risk-sharing Only",
                     "15 Years of Low Returns : Risk-sharing 5% max",
                     "15 Years of Low Returns: No ERC floor")

runs_RS3_labels <- c("High Volatility: Baseline",
                     "High Volatility: Current Policy",
                     "High Volatility: ERC limit Only",
                     "High Volatility: Risk-sharing Only",
                     "High Volatility: Risk-sharing 5% max",
                     "High Volatility: No ERC floor")

runs_alt_labels    <- c("open amortization", "lower payroll growth assumption for amort")

runs_reform_labels <- c( "DC Reform; \nexpected return = 7.25%; \ndiscount rate = 7.25%, DC1", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%, DC1", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%, DC1", 
                         
                         "DC Reform; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC3", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC3", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%; DC3", 
                         
                         "DC Reform for all; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC3", 
                         "DC reform for all; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC3", 
                         
                         "DC Reform; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC4", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC4", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%; DC4", 
                         
                         "DC Reform for all; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC4", 
                         "DC reform for all; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC4", 
                         
                         "No DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%",
                         "No DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%")


runs_reform_sep_labels <- c(  "No DC reform; \nexpected return = 7.25%, \ndiscount rate = 7.25%; \nSep.new hires",
                          "No DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%; \nSep.new hires",
                          "No DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%; \nSep.new hires",
  
                         # "DC Reform; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC3; \nSep.new hires", 
                         # "DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC3; \nSep.new hires", 
                         # "DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%; DC3; \nSep.new hires", 
                         # 
                         # "DC Reform for all; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC3; \nSep.new hires", 
                         # "DC reform for all; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC3; \nSep.new hires", 
                         
                         "DC Reform; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC4; \nSep.new hires", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC4; \nSep.new hires", 
                         "DC reform; \nexpected return = 6.25%, \ndiscount rate = 6.25%; DC4; \nSep.new hires" 
                         
                         # "DC Reform for all; \nexpected return = 7.25%; \ndiscount rate = 7.25%; DC4", 
                         # "DC reform for all; \nexpected return = 6.25%, \ndiscount rate = 7.25%; DC4" 
                         
)


# lab_s1 <- "Scenario 1 \nAssumption Achieved: \nClosed Plan"
# lab_s2 <- "Scenario 2 \nAssumption Achieved: \nOpen Plan"
# lab_s3 <- "Scenario 3 \n15 Years of Low Returns"
# lab_s4 <- "Scenario 4 \nHigh Volatility"
# lab_s5 <- "Scenario 5 \nCurrent Return Assumption"
# lab_s6 <- "Scenario 6 \nLower Return Assumption"


runs_all <- c(runs_RS1, runs_RS2, runs_RS3, runs_alt, runs_reform, runs_reform_sep)
runs_all_labels <- c(runs_RS1_labels, runs_RS2_labels, runs_RS3_labels, runs_alt_labels, runs_reform_labels, runs_reform_sep_labels)



# Calculate total final ERC rate for runs with DC reform (include ERC to DC in ERC.final_PR)
results_all %<>%
  mutate(ERC.DB.final_PR = ERC.final_PR,
         ERC.DB.final_GF = ERC.final_GF,
         ERC.DB.final = ERC.final,
         ERC.tot.final = ERC.final + DC_ERC,
         
         DC_EEC_PR = 100 * DC_EEC / PR,
         
         DC_EEC_PR.tEF = DC_EEC/(PR_tNE + PR_tNF),
         DC_PR.tEF = (DC_EEC + DC_ERC)/(PR_tNE + PR_tNF),
         
         ERC.final_PR = 100 * ERC.tot.final/PR,
         ERC.final_GF = 100 * ERC.tot.final/GenFund
  )


results_all.sumTiers <- results_all %>% filter(Tier == "sumTiers")
results_all.xNew <- results_all %>% filter(Tier == "sumTiers.xNew")
results_all.New  <- results_all %>% filter(Tier == "sumTiers.New")


# df_all.stch <- results_all  %>% 
#   filter(runname %in% runs_all, 
#          sim > 0, 
#          year %in% 2016:2045)
# 
# 
# df_all.stch %<>%   
#   select(runname,Tier, returnScn, policy.SR, policy.EL, sim, year, FR_MA, AL, MA, ERC, EEC, PR, ERC_PR, ERC.final_PR, ERC.final_GF) %>%
#   group_by(runname, sim, Tier) %>% 
#   mutate(
#          #FR_MA     = 100 * MA / AL,
#          FR40less  = cumany(FR_MA <= 40),
#          FR100more = FR_MA >= 100,
#          ERC_high  = cumany(ERC.final_PR >= 50), 
#          ERC_hike     = cumany(na2zero(ERC.final_PR - lag(ifelse(year == 2016, NA, ERC.final_PR), 5) >= 10)),  # NA for 2016 value: excludes impact of new amort payment in 2017 
#          ERC_GF_hike  = cumany(na2zero(ERC.final_GF - lag(ifelse(year == 2016, NA, ERC.final_GF), 5) >= 5)),
#          EEC_PR       = 100 * EEC / PR
#          ) %>% 
#   group_by(runname, year, Tier) %>% 
#   summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
#             FR100more = 100 * sum(FR100more, na.rm = T)/n(),
#             ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
#             ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
#             ERC_GF_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
#             
#             FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
#             FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
#             FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
#             FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
#             FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
#             
#             ERC_PR.q10 = quantile(ERC.final_PR, 0.1, na.rm = T),
#             ERC_PR.q25 = quantile(ERC.final_PR, 0.25, na.rm = T),
#             ERC_PR.q50 = quantile(ERC.final_PR, 0.5, na.rm = T),
#             ERC_PR.q75 = quantile(ERC.final_PR, 0.75, na.rm = T),
#             ERC_PR.q90 = quantile(ERC.final_PR, 0.9, na.rm = T),
#             
#             EEC_PR.q10 = quantile(EEC_PR, 0.1, na.rm = T),
#             EEC_PR.q25 = quantile(EEC_PR, 0.25, na.rm = T),
#             EEC_PR.q50 = quantile(EEC_PR, 0.5, na.rm = T),
#             EEC_PR.q75 = quantile(EEC_PR, 0.75, na.rm = T),
#             EEC_PR.q90 = quantile(EEC_PR, 0.9, na.rm = T),
#             
#             ERC_GF.q10 = quantile(ERC.final_GF, 0.1, na.rm = T),
#             ERC_GF.q25 = quantile(ERC.final_GF, 0.25, na.rm = T),
#             ERC_GF.q50 = quantile(ERC.final_GF, 0.5, na.rm = T),
#             ERC_GF.q75 = quantile(ERC.final_GF, 0.75, na.rm = T),
#             ERC_GF.q90 = quantile(ERC.final_GF, 0.9, na.rm = T)
#             
#   ) %>% 
#   ungroup() %>%
#   mutate(runname.lab = factor(runname, 
#                               levels = runs_all)
#                               #labels = runs_all_labels
#          )


# Compare results: If asset side of new hires is modeled separately. 
# results_all %>% filter(runname == "RS1_SR1EL1", sim == 0)                                    %>% select(runname,Tier, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR)
# results_all %>% filter(runname == "RS1_SR1EL1_sep_R725.d725", sim == 0, Tier == "sumTiers")  %>% select(runname,Tier, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR)
# 
# 
# results_all %>% filter(runname == "SR1EL1.Reform_R725.d725.DC4", sim == 0)                          %>% select(runname,Tier, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.DB.final_PR)
# results_all %>% filter(runname == "SR1EL1.Reform_sep_R725.d725.DC4", sim == 0, Tier == "sumTiers")  %>% select(runname,Tier, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.DB.final_PR)
# 
# 
# results_all %>% filter(runname == "SR1EL1.Reform_R725.d725.DC4a", sim == 0)                               %>% select(runname,Tier, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.DB.final_PR)
# results_all %>% filter(runname == "SR1EL1.Reform_sep_R725.d725.DC4a", sim == 0, Tier == "sumTiers")  %>% select(runname,Tier, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.DB.final_PR)
# 
# 
# 
# results_all %>% filter(runname == "RS1_SR0EL1_sep_R725.d725",        sim == 0, Tier == "sumTiers.New", year <= 2048)  %>% select(runname,Tier, year, FR_MA, NC, SC, EEC, ERC, ERC.DB.final)
# results_all %>% filter(runname == "SR0EL1.Reform_sep_R725.d725.DC4", sim == 0, Tier == "sumTiers.New", year <= 2048)  %>% select(runname,Tier, year, FR_MA, NC, SC, EEC, ERC, ERC.DB.final)
# 
# 
# results_all %>% filter(runname == "RS1_SR0EL1_sep_R725.d725",    sim == 0, Tier == "sumTiers.xNew",  year == 2048)  %>% select(runname,Tier, year, FR_MA, NC,SC, EEC, ERC, ERC.DB.final)
# results_all %>% filter(runname == "RS1_SR0EL1_sep_R725.d725",    sim == 0, Tier == "sumTiers.New", year == 2048)    %>% select(runname,Tier, year, FR_MA, NC,SC, EEC, ERC, ERC.DB.final)
# 
# 
# results_all %>% filter(runname == "RS1_SR0EL1_sep_R725.d725",        sim == 0, Tier == "sumTiers.xNew") %>% filter(year <= 2048) %>%  summarise(ERC = sum(ERC.DB.final))
# results_all %>% filter(runname == "SR0EL1.Reform_sep_R725.d725.DC4", sim == 0, Tier == "sumTiers.xNew") %>% filter(year <= 2048) %>%  summarise(ERC = sum(ERC.DB.final))





# 
# 
# results_all %>% select(runname, sim, year, ERC.final.0 = ERC.final) %>% filter(sim > 0, runname == "RS1_SR0EL1") %>%
#   left_join(results_all %>% filter(sim > 0, runname == "RS1_SR1EL1")%>% select(sim, year, ERC.final.1 = ERC.final)) %>% 
#   group_by(sim) %>% 
#   summarise(PV.ERC.0 = sum(ERC.final.0 / 1e6 *(1 + 0.075)^(row_number() - 1)),
#          PV.ERC.1 = sum(ERC.final.1 / 1e6*(1 + 0.075)^(row_number() - 1))) %>%
#   mutate(diff.PV.ERC = 1 - PV.ERC.1 / PV.ERC.0 ) %>% 
#   ungroup() %>% 
#   summarise(diff.PV.ERC.q10   = quantile(diff.PV.ERC, 0.1,na.rm = T),
#             diff.PV.ERC.q25   = quantile(diff.PV.ERC, 0.25, na.rm = T),
#             diff.PV.ERC.q50   = quantile(diff.PV.ERC, 0.5, na.rm = T),
#             diff.PV.ERC.q75   = quantile(diff.PV.ERC, 0.75, na.rm = T),
#             diff.PV.ERC.q90   = quantile(diff.PV.ERC, 0.9, na.rm = T))




# #*************************************************************************
# ##             4. Determinitic measrues of risk transfer             ####
# #*************************************************************************
# 
# # load("Data_inputs/DC_rate.tot725.RData")
# # load("Data_inputs/DC_rate.tot625.RData")              
# # 
# # DC_rate.tot
# 
# runs_reform <- c("SR1EL1.Reform_R725.d725.DC1",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: PVDC = PVDB 
#                  "SR1EL1.Reform_R625.d725.DC1",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: PVDC = PVDB 
#                  "SR1EL1.Reform_R625.d625.DC1",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: PVDC = PVDB  
#                  
#                  "SR1EL1.Reform_R725.d725.DC3",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 5% 
#                  "SR1EL1.Reform_R625.d725.DC3",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 5% 
#                  "SR1EL1.Reform_R625.d625.DC3",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 5% 
#                  
#                  "SR1EL1.Reform_R725.d725.DC4",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 9% 
#                  "SR1EL1.Reform_R625.d725.DC4",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 9% 
#                  "SR1EL1.Reform_R625.d625.DC4",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 9% 
#                  
#                  "RS1_SR1EL1_R625.d725",       # No DC reform with expected return = 6.25%, discount rate = 7.25% 
#                  "RS1_SR1EL1_R625.d625")       # No DC reform with expected return = 6.25%, discount rate = 6.25%
# 
# 
# # 4.1 Deterministic run ####
# 
# # Impact on AL of DB plan
# results_all %>% filter(runname %in% c("RS1_SR1EL1", "SR1EL1.Reform_R725.d725.DC1"), sim == 0) %>% 
#   select(runname, sim, year, AL) %>% 
#   spread(runname, AL) %>% 
#   mutate(AL_reduction = SR1EL1.Reform_R725.d725.DC1/RS1_SR1EL1)
# 
# 
# # Impact on ERC
# results_all %>% filter(runname %in% c("RS1_SR1EL1", "SR1EL1.Reform_R725.d725.DC1", "SR1EL1.Reform_R725.d725.DC3"), sim == 0) %>% 
#   select(runname, sim, year, ERC.final_PR) %>% 
#   spread(runname, ERC.final_PR)
# 




# 4.4.3 Pew method, deterministic, sumTiers, hybrid for new hires; SR turned off ####

# deterministic Pew method; sumTiers
# 4.4 risk transfer: deterministic runs ####
# Employer contribution rate to DC: 3.75% for new Class E members, 5.15% for new Class F members. Average rate around 4%. 

riskTransfer_simPeriod <- 2017:2048

get_riskTransfer.IFO <- function(df, rn, year_range = 2017:2048){
  
    riskTransfer <-  df %>% filter(runname %in% rn, 
                                              sim == 0,
                                              year %in% year_range) %>% 
    mutate(runname = factor(runname, levels = rn)) %>% 
    group_by(runname) %>% 
    summarize(CF      = sum(ERC.tot.final, na.rm = TRUE)/1e9,
              PV725 = sum(ERC.tot.final / (1 + 0.0725)^(row_number() - 1), na.rm = TRUE)/1e9,
              PV37  = sum(ERC.tot.final / (1 + 0.037)^(row_number() - 1), na.rm = TRUE)/1e9
    ) %>% 
    mutate(runname = c("DB_high", "DB_low", "hybrid_high", "hybrid_low")) %>% 
    gather(var, value, -runname) %>% 
    mutate(runname = paste(runname, var, sep = "_")) %>% 
    select(-var) %>% 
    spread(runname, value) %>% 
    mutate(
      Diff.CL_CF = (DB_low_CF - DB_high_CF),
      Diff.PL_CF = (hybrid_low_CF - hybrid_high_CF),
      riskTansfer.dlr_CF = (Diff.CL_CF - Diff.PL_CF),
      riskTransfer.pct_CF = 100 * riskTansfer.dlr_CF / Diff.CL_CF,
      
      Diff.CL_PV725 = (DB_low_PV725 - DB_high_PV725),
      Diff.PL_PV725 = (hybrid_low_PV725 - hybrid_high_PV725),
      riskTansfer.dlr_PV725 = (Diff.CL_PV725 - Diff.PL_PV725),
      riskTransfer.pct_PV725 = 100 * riskTansfer.dlr_PV725 / Diff.CL_PV725,
      
      Diff.CL_PV37 = (DB_low_PV37 - DB_high_PV37),
      Diff.PL_PV37 = (hybrid_low_PV37 - hybrid_high_PV37),
      riskTansfer.dlr_PV37 = (Diff.CL_PV37 - Diff.PL_PV37),
      riskTransfer.pct_PV37 = 100 * riskTansfer.dlr_PV37 / Diff.CL_PV37
    ) %>% 
    t
    
    riskTransfer %<>%
      as.data.frame %>% 
      mutate(var = rownames(riskTransfer)) %>% 
      as.data.frame %>% 
      select(var, value = V1)
    
}

get_riskTransfer.pew <- function(df, rn, year_range = 2017:2048){
  
  riskTransfer <-  df %>% filter(runname %in% rn, 
                                 sim == 0,
                                 year %in% year_range) %>% 
    mutate(runname = factor(runname, levels = rn)) %>% 
    group_by(runname) %>% 
    summarize(CF      = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9,
              PV725 = sum(ERC.tot.final / (1 + 0.0725)^(row_number() - 1), na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9,
              PV37  = sum(ERC.tot.final / (1 + 0.037)^(row_number() - 1), na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9
    ) %>% 
    mutate(runname = c("DB_high", "DB_low", "hybrid_high", "hybrid_low")) %>% 
    gather(var, value, -runname) %>% 
    mutate(runname = paste(runname, var, sep = "_")) %>% 
    select(-var) %>% 
    spread(runname, value) %>% 
    mutate(
      Diff.CL_CF = (DB_low_CF - DB_high_CF),
      Diff.PL_CF = (hybrid_low_CF - hybrid_high_CF),
      riskTansfer.dlr_CF = (Diff.CL_CF - Diff.PL_CF),
      riskTransfer.pct_CF = 100 * riskTansfer.dlr_CF / Diff.CL_CF,
      
      Diff.CL_PV725 = (DB_low_PV725 - DB_high_PV725),
      Diff.PL_PV725 = (hybrid_low_PV725 - hybrid_high_PV725),
      riskTansfer.dlr_PV725 = (Diff.CL_PV725 - Diff.PL_PV725),
      riskTransfer.pct_PV725 = 100 * riskTansfer.dlr_PV725 / Diff.CL_PV725,
      
      Diff.CL_PV37 = (DB_low_PV37 - DB_high_PV37),
      Diff.PL_PV37 = (hybrid_low_PV37 - hybrid_high_PV37),
      riskTansfer.dlr_PV37 = (Diff.CL_PV37 - Diff.PL_PV37),
      riskTransfer.pct_PV37 = 100 * riskTansfer.dlr_PV37 / Diff.CL_PV37
    ) %>% 
    t
  
  riskTransfer %<>%
    as.data.frame %>% 
    mutate(var = rownames(riskTransfer)) %>% 
    as.data.frame %>% 
    select(var, value = V1)
  
}

get_riskTransfer.pew1 <- function(df, rn, year_range = 2017:2048){
  
  riskTransfer <-  df %>% filter(runname %in% rn, 
                                 sim == 0) %>%
                          mutate(UAAL = lead(UAAL)) %>% 
                          filter(year %in% year_range) %>% 
    mutate(runname = factor(runname, levels = rn)) %>% 
    group_by(runname) %>% 
    summarize(CF      = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9,
              PV725 = sum(ERC.tot.final / (1 + 0.0725)^(row_number() - 1), na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9,
              PV37  = sum(ERC.tot.final / (1 + 0.037)^(row_number() - 1), na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9
    ) %>% 
    mutate(runname = c("DB_high", "DB_low", "hybrid_high", "hybrid_low")) %>% 
    gather(var, value, -runname) %>% 
    mutate(runname = paste(runname, var, sep = "_")) %>% 
    select(-var) %>% 
    spread(runname, value) %>% 
    mutate(
      Diff.CL_CF = (DB_low_CF - DB_high_CF),
      Diff.PL_CF = (hybrid_low_CF - hybrid_high_CF),
      riskTansfer.dlr_CF = (Diff.CL_CF - Diff.PL_CF),
      riskTransfer.pct_CF = 100 * riskTansfer.dlr_CF / Diff.CL_CF,
      
      Diff.CL_PV725 = (DB_low_PV725 - DB_high_PV725),
      Diff.PL_PV725 = (hybrid_low_PV725 - hybrid_high_PV725),
      riskTansfer.dlr_PV725 = (Diff.CL_PV725 - Diff.PL_PV725),
      riskTransfer.pct_PV725 = 100 * riskTansfer.dlr_PV725 / Diff.CL_PV725,
      
      Diff.CL_PV37 = (DB_low_PV37 - DB_high_PV37),
      Diff.PL_PV37 = (hybrid_low_PV37 - hybrid_high_PV37),
      riskTansfer.dlr_PV37 = (Diff.CL_PV37 - Diff.PL_PV37),
      riskTransfer.pct_PV37 = 100 * riskTansfer.dlr_PV37 / Diff.CL_PV37
    ) %>% 
    t
  
  riskTransfer %<>%
    as.data.frame %>% 
    mutate(var = rownames(riskTransfer)) %>% 
    as.data.frame %>% 
    select(var, value = V1)
  
}



# 4.4.1 risk transfer: 5% total DC rate; costs of all, hybrid for new hires ####


# deterministic Pew method; sumTiers
riskTransfer.pew.DC4 <- 
  get_riskTransfer.pew(results_all.sumTiers, c("RS1_SR1EL1_sep_R725.d725",
                                               "RS1_SR1EL1_sep_R625.d725",
                                               "SR1EL1.Reform_sep_R725.d725.DC4",
                                               "SR1EL1.Reform_sep_R625.d725.DC4"),
                       riskTransfer_simPeriod
                       )

# deterministic Pew method; sumTiers
riskTransfer.pew.DC4.p1 <- 
  get_riskTransfer.pew(results_all.sumTiers, 
                       c("RS1_SR1EL1_sep_R725.d725",
                         "RS1_SR1EL1_sep_R625.d725",
                         "SR1EL1.Reform_sep_R725.d725.DC4",
                         "SR1EL1.Reform_sep_R625.d725.DC4"),
                       2017:2028
  )

riskTransfer.pew.DC4.p2 <- 
  get_riskTransfer.pew(results_all.sumTiers, 
                       c("RS1_SR1EL1_sep_R725.d725",
                         "RS1_SR1EL1_sep_R625.d725",
                         "SR1EL1.Reform_sep_R725.d725.DC4",
                         "SR1EL1.Reform_sep_R625.d725.DC4"),
                       2029:2038
  )

riskTransfer.pew.DC4.p3 <- 
  get_riskTransfer.pew(results_all.sumTiers, 
                       c("RS1_SR1EL1_sep_R725.d725",
                         "RS1_SR1EL1_sep_R625.d725",
                         "SR1EL1.Reform_sep_R725.d725.DC4",
                         "SR1EL1.Reform_sep_R625.d725.DC4"),
                       2039:2048
  )



riskTransfer.pew.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV725"))

riskTransfer.pew.DC4.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.p3 %>% filter(str_detect(var, "CF"))



# 4.4.2 risk transfer: 5% total DC rate; costs of new, hybrid for new hires ####


# deterministic Pew method; new hires only
riskTransfer.pew.DC4.new <- 
  get_riskTransfer.pew(results_all.New, c("RS1_SR1EL1_sep_R725.d725",
                                          "RS1_SR1EL1_sep_R625.d725",
                                          "SR1EL1.Reform_sep_R725.d725.DC4",
                                          "SR1EL1.Reform_sep_R625.d725.DC4")
  )

riskTransfer.pew.DC4.xNew <- 
  get_riskTransfer.pew(results_all.xNew, c("RS1_SR1EL1_sep_R725.d725",
                                          "RS1_SR1EL1_sep_R625.d725",
                                          "SR1EL1.Reform_sep_R725.d725.DC4",
                                          "SR1EL1.Reform_sep_R625.d725.DC4")
  )
riskTransfer.pew.DC4.xNew %>% filter(str_detect(var, "CF"))


# deterministic Pew method; New hires
riskTransfer.pew.DC4.new.p1 <- 
  get_riskTransfer.pew(results_all.New, 
                       c("RS1_SR1EL1_sep_R725.d725",
                         "RS1_SR1EL1_sep_R625.d725",
                         "SR1EL1.Reform_sep_R725.d725.DC4",
                         "SR1EL1.Reform_sep_R625.d725.DC4"),
                       2017:2028
  )

riskTransfer.pew.DC4.new.p2 <- 
  get_riskTransfer.pew(results_all.New, 
                       c("RS1_SR1EL1_sep_R725.d725",
                         "RS1_SR1EL1_sep_R625.d725",
                         "SR1EL1.Reform_sep_R725.d725.DC4",
                         "SR1EL1.Reform_sep_R625.d725.DC4"),
                       2029:2038
  )


riskTransfer.pew.DC4.new.p3 <- 
  get_riskTransfer.pew(results_all.New, 
                       c("RS1_SR1EL1_sep_R725.d725",
                         "RS1_SR1EL1_sep_R625.d725",
                         "SR1EL1.Reform_sep_R725.d725.DC4",
                         "SR1EL1.Reform_sep_R625.d725.DC4"),
                       2039:2048
  )


riskTransfer.pew.DC4.new %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4.new %>% filter(str_detect(var, "PV725"))


riskTransfer.pew.DC4.new.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.p3 %>% filter(str_detect(var, "CF"))


# 4.4.3 risk transfer: 5% total DC rate; costs of all, hybrid for new hires; SR turned off ####


riskTransfer.pew.DC4.SR0 <- 
  get_riskTransfer.pew(results_all.sumTiers, c("RS1_SR0EL1_sep_R725.d725",
                                               "RS1_SR0EL1_sep_R625.d725",
                                               "SR0EL1.Reform_sep_R725.d725.DC4",
                                               "SR0EL1.Reform_sep_R625.d725.DC4")
  )

# deterministic Pew method; sumTiers
riskTransfer.pew.DC4.SR0.p1 <- 
  get_riskTransfer.pew(results_all.sumTiers, 
                       c("RS1_SR0EL1_sep_R725.d725",
                         "RS1_SR0EL1_sep_R625.d725",
                         "SR0EL1.Reform_sep_R725.d725.DC4",
                         "SR0EL1.Reform_sep_R625.d725.DC4"),
                       2017:2028
  )

riskTransfer.pew.DC4.SR0.p2 <- 
  get_riskTransfer.pew(results_all.sumTiers, 
                       c("RS1_SR0EL1_sep_R725.d725",
                         "RS1_SR0EL1_sep_R625.d725",
                         "SR0EL1.Reform_sep_R725.d725.DC4",
                         "SR0EL1.Reform_sep_R625.d725.DC4"),
                       2029:2038
  )

riskTransfer.pew.DC4.SR0.p3 <- 
  get_riskTransfer.pew(results_all.sumTiers, 
                       c("RS1_SR0EL1_sep_R725.d725",
                         "RS1_SR0EL1_sep_R625.d725",
                         "SR0EL1.Reform_sep_R725.d725.DC4",
                         "SR0EL1.Reform_sep_R625.d725.DC4"),
                       2039:2048
  )

riskTransfer.pew.DC4.SR0  %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0.p3 %>% filter(str_detect(var, "CF"))




# 4.4.4 risk transfer: 5% total DC rate; costs of new, hybrid for new hires ; SR turned off ####


riskTransfer.pew.DC4.new.SR0 <- 
  get_riskTransfer.pew(results_all.New, c("RS1_SR0EL1_sep_R725.d725",
                                          "RS1_SR0EL1_sep_R625.d725",
                                          "SR0EL1.Reform_sep_R725.d725.DC4",
                                          "SR0EL1.Reform_sep_R625.d725.DC4")
  )

riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "CF"))



# deterministic Pew method; sumTiers
riskTransfer.pew.DC4.new.SR0.p1 <- 
  get_riskTransfer.pew(results_all.New, 
                       c("RS1_SR0EL1_sep_R725.d725",
                         "RS1_SR0EL1_sep_R625.d725",
                         "SR0EL1.Reform_sep_R725.d725.DC4",
                         "SR0EL1.Reform_sep_R625.d725.DC4"),
                       2017:2028
  )

riskTransfer.pew.DC4.new.SR0.p2 <- 
  get_riskTransfer.pew(results_all.New, 
                       c("RS1_SR0EL1_sep_R725.d725",
                         "RS1_SR0EL1_sep_R625.d725",
                         "SR0EL1.Reform_sep_R725.d725.DC4",
                         "SR0EL1.Reform_sep_R625.d725.DC4"),
                       2029:2038
  )

riskTransfer.pew.DC4.new.SR0.p3 <- 
  get_riskTransfer.pew(results_all.New, 
                       c("RS1_SR0EL1_sep_R725.d725",
                         "RS1_SR0EL1_sep_R625.d725",
                         "SR0EL1.Reform_sep_R725.d725.DC4",
                         "SR0EL1.Reform_sep_R625.d725.DC4"),
                       2039:2048
  )

riskTransfer.pew.DC4.new.SR0.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.SR0.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.SR0.p3 %>% filter(str_detect(var, "CF"))







#4.4.5 hybrid for all; costs of all ####
riskTransfer.pew2.DC4 <- 
  get_riskTransfer.pew(results_all.sumTiers, c("RS1_SR1EL1_sep_R725.d725",
                                               "RS1_SR1EL1_sep_R625.d725",
                                               "SR1EL1.Reform_sep_R725.d725.DC4a",
                                               "SR1EL1.Reform_sep_R625.d725.DC4a")
  )

riskTransfer.pew2.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV725"))


riskTransfer.pew2.DC4.SR0 <- 
  get_riskTransfer.pew(results_all.sumTiers, c("RS1_SR0EL1_sep_R725.d725",
                                               "RS1_SR0EL1_sep_R625.d725",
                                               "SR0EL1.Reform_sep_R725.d725.DC4a",
                                               "SR0EL1.Reform_sep_R625.d725.DC4a")
  )

riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "PV725"))








# 4.4.6 Display desterministic results: ####
 


# hybrid for new, all members, SR on
riskTransfer.pew.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV725"))

riskTransfer.pew.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.p3 %>% filter(str_detect(var, "CF"))


# hybrid for new, new hires only, SR on
riskTransfer.pew.DC4.new %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4.new %>% filter(str_detect(var, "PV725"))

riskTransfer.pew.DC4.new %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.p3 %>% filter(str_detect(var, "CF"))



# hybrid for new, all members, SR off
riskTransfer.pew.DC4.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4.SR0 %>% filter(str_detect(var, "PV725"))

riskTransfer.pew.DC4.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.SR0.p3 %>% filter(str_detect(var, "CF"))


# hybrid for new, new hires only, SR off
riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "PV725"))

riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.SR0.p1 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.SR0.p2 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4.new.SR0.p3 %>% filter(str_detect(var, "CF"))



# hybrid for all current members and new hires, all members

riskTransfer.pew2.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "CF"))

riskTransfer.pew2.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV725"))

riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "PV725"))





# reformatting results
reformat.riskTransfer <- function(df, name){
# df <- riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "CF"))
# df

DB_high <- df[str_detect(df$var, "DB_high"), "value" ]
DB_low  <- df[str_detect(df$var, "DB_low"), "value" ]

hybrid_high <- df[str_detect(df$var, "hybrid_high"), "value" ]
hybrid_low  <- df[str_detect(df$var, "hybrid_low"), "value" ]

Diff.CL  <- df[str_detect(df$var, "Diff.CL"), "value" ]
Diff.PL  <- df[str_detect(df$var, "Diff.PL"), "value" ]

riskTransfer      <- df[7, "value" ]
riskTransfer.pct  <- df[str_detect(df$var, "riskTransfer.pct"), "value" ]


df.o <- matrix(
       c(DB_high, hybrid_high,
         DB_low,  hybrid_low, 
         Diff.CL, Diff.PL, 
         0,       riskTransfer,
         0,       riskTransfer.pct),
       
       5, 2, 
       byrow = T
       ) %>% 
  as.data.frame %>% 
  mutate(name = name) %>% 
  select(name, everything())
}

riskTransfer.tab <- 
bind_rows(
RT.allTiers.DC4.SR1 <-    riskTransfer.pew.DC4 %>% filter(str_detect(var, "CF"))    %>% reformat.riskTransfer("RT.allTiers.DC4.SR1") %>% print,
RT.allTiers.DC4.SR1.p1 <- riskTransfer.pew.DC4.p1 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.allTiers.DC4.SR1.p1") %>% print,
RT.allTiers.DC4.SR1.p2 <- riskTransfer.pew.DC4.p2 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.allTiers.DC4.SR1.p2") %>% print,
RT.allTiers.DC4.SR1.p3 <- riskTransfer.pew.DC4.p3 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.allTiers.DC4.SR1.p3") %>% print,

RT.new.DC4.SR1    <- riskTransfer.pew.DC4.new %>% filter(str_detect(var, "CF"))   %>% reformat.riskTransfer("RT.new.DC4.SR1") %>% print,
RT.new.DC4.SR1.p1 <-riskTransfer.pew.DC4.new.p1 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.new.DC4.SR1.p1") %>% print,
RT.new.DC4.SR1.p2 <-riskTransfer.pew.DC4.new.p2 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.new.DC4.SR1.p2") %>% print,
RT.new.DC4.SR1.p3 <-riskTransfer.pew.DC4.new.p3 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.new.DC4.SR1.p3") %>% print,

RT.allTiers.DC4.SR0    <- riskTransfer.pew.DC4.SR0 %>% filter(str_detect(var, "CF"))    %>% reformat.riskTransfer("RT.allTiers.DC4.SR0") %>% print,
RT.allTiers.DC4.SR0.p1 <- riskTransfer.pew.DC4.SR0.p1 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.allTiers.DC4.SR0.p1") %>% print,
RT.allTiers.DC4.SR0.p2 <- riskTransfer.pew.DC4.SR0.p2 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.allTiers.DC4.SR0.p2") %>% print,
RT.allTiers.DC4.SR0.p3 <- riskTransfer.pew.DC4.SR0.p3 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.allTiers.DC4.SR0.p3") %>% print,

RT.new.DC4.SR0    <-riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "CF"))    %>% reformat.riskTransfer("RT.new.DC4.SR0 ") %>% print,
RT.new.DC4.SR0.p1 <-riskTransfer.pew.DC4.new.SR0.p1 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.new.DC4.SR0.p1") %>% print,
RT.new.DC4.SR0.p2 <-riskTransfer.pew.DC4.new.SR0.p2 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.new.DC4.SR0.p2") %>% print,
RT.new.DC4.SR0.p3 <-riskTransfer.pew.DC4.new.SR0.p3 %>% filter(str_detect(var, "CF")) %>% reformat.riskTransfer("RT.new.DC4.SR0.p3") %>% print,


RT.new.DC4a.SR1 <-  riskTransfer.pew2.DC4 %>% filter(str_detect(var, "CF"))    %>% reformat.riskTransfer("RT.new.DC4a.SR1 ") %>% print,
RT.new.DC4a.SR0 <-  riskTransfer.pew2.DC4.SR0 %>% filter(str_detect(var, "CF"))%>% reformat.riskTransfer("RT.new.DC4a.SR0")  %>% print
)


write.xlsx2(riskTransfer.tab, "Results/RiskTransfer/riskTransfer.tab.xlsx")




# 4.4.7 Mics ####

# benefit factor and DB EEC rate are reduced by 75% or 25%

riskTransfer.pew.DC5 <- 
  get_riskTransfer.pew(results_all.sumTiers, c("RS1_SR0EL1_sep_R725.d725",
                                               "RS1_SR0EL1_sep_R625.d725",
                                               "SR0EL1.Reform_sep_R725.d725.DC5",
                                               "SR0EL1.Reform_sep_R625.d725.DC5"),
                       riskTransfer_simPeriod
  )

riskTransfer.pew.DC5 %>% filter(str_detect(var, "CF"))


riskTransfer.pew.DC5.new <- 
  get_riskTransfer.pew(results_all.New, c("RS1_SR0EL1_sep_R725.d725",
                                          "RS1_SR0EL1_sep_R625.d725",
                                          "SR0EL1.Reform_sep_R725.d725.DC5",
                                          "SR0EL1.Reform_sep_R625.d725.DC5"),
                       2017:2048
  )

riskTransfer.pew.DC5.new %>% filter(str_detect(var, "CF"))


# Risk transfer based on 2% reduction of annual investment return. 
riskTransfer.2lowerReturn <- 
  get_riskTransfer.pew(results_all.sumTiers, c("RS1_SR0EL1_sep_R725.d725",
                                               "RS1_SR0EL1_sep_R525.d725",
                                               "SR0EL1.Reform_sep_R725.d725.DC4",
                                               "SR0EL1.Reform_sep_R525.d725.DC4"),
                       riskTransfer_simPeriod
  )


riskTransfer.2lowerReturn.new <- 
  get_riskTransfer.pew(results_all.New, c("RS1_SR0EL1_sep_R725.d725",
                                          "RS1_SR0EL1_sep_R525.d725",
                                          "SR0EL1.Reform_sep_R725.d725.DC4",
                                          "SR0EL1.Reform_sep_R525.d725.DC4"),
                       2039:2048
  )

riskTransfer.pew.DC4.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.2lowerReturn %>% filter(str_detect(var, "CF"))

riskTransfer.pew.DC4.new.SR0 %>% filter(str_detect(var, "CF"))
riskTransfer.2lowerReturn.new %>% filter(str_detect(var, "CF"))







## 5. Stochastic measures of risk transfer ####

# This section explores alternative measures of risk transfer under stochastic simulation approaches. 

# 5.1 Risk transfer as the reduction in uncertainty of employer pension costs  
# Starting from the same set of stochastic investment return series (2000 sims), comparing the 
# resulting distributions of 30-year employer pension costs under CL and PL. Possible measures of cost uncertainty:
#   = Standard deviation of total costs
#   = Difference between high and low percentiles (eg. 75th percentile minus 25th percentile)
#   = Difference between the median cost and a higher cost, eg. 
#      - 75th percentile; 
#      - cost from a simulation in which the 30-year compound return is 1 percentage point lower than the 30-year compound return in the simulation that generates the median cost.
# 
# Risk transfer can be defined as the difference between CL and PL in these measures of cost uncertainty. 

# Below we compare the distribution of employer pension costs in the following runs:
  # a. "RS1_SR1EL1": Pure DB plan; 7.25% expected return, 7.25% discount
  # b. "SR1EL1.Reform_R725.d725.DC4": DC contribution rate 9%, DC for new hires only, 7.25% expected return, 7.25% discount rate. 

# 5.1 measures of uncertainty ####
get_riskTransfer.stch <- function(df, rn, method, year_range = 2017:2048 ){
  # riskTransfer_simPeriod <- 2017:2048
  # rn <- c("RS1_SR1EL1", "SR1EL1.Reform_R725.d725.DC4"
  
  add.UAAL <- switch(method, IFO = 0, pew = 1)

  dist_cost <- 
    df %>% filter(runname %in% rn, 
                           sim > 0,
                           year %in% year_range) %>% 
    mutate(runname = factor(runname, levels = rn)) %>% 
    group_by(runname, sim) %>% 
    summarize(cost_0 = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9 * add.UAAL) %>% 
    group_by(runname) 
  
  dist_cost %<>% 
    summarise(pct90 = quantile(cost_0, 0.90),
              pct75 = quantile(cost_0, 0.75),
              pct50 = quantile(cost_0, 0.50),
              pct25 = quantile(cost_0, 0.25),
              avg   = mean(cost_0),
              SD    = sd(cost_0)
    ) %>% 
    mutate(
      diff_75.25 = pct75 - pct25,
      diff_90.50 = pct90 - pct50,
      diff_75.50 = pct75 - pct50,
      
      diff2_90.50 = pct90/pct50 - 1,
      diff2_75.50 = pct75/pct50 - 1)
  
}



dist_cost.hybNew.tot <- 
  get_riskTransfer.stch(results_all.sumTiers, 
                        c("RS1_SR1EL1_sep_R725.d725", "SR1EL1.Reform_sep_R725.d725.DC4"), "pew")

dist_cost.hybNew.new <- 
  get_riskTransfer.stch(results_all.New, 
                        c("RS1_SR1EL1_sep_R725.d725", "SR1EL1.Reform_sep_R725.d725.DC4"), "pew")


dist_cost.hybAll.tot <- 
  get_riskTransfer.stch(results_all.sumTiers, 
                        c("RS1_SR1EL1_sep_R725.d725", "SR1EL1.Reform_sep_R725.d725.DC4a"), "pew")


dist_cost.hybNew.tot.SR0 <- 
  get_riskTransfer.stch(results_all.sumTiers, 
                        c("RS1_SR0EL1_sep_R725.d725", "SR0EL1.Reform_sep_R725.d725.DC4"), "pew")

dist_cost.hybNew.new.SR0 <- 
  get_riskTransfer.stch(results_all.New, 
                        c("RS1_SR0EL1_sep_R725.d725", "SR0EL1.Reform_sep_R725.d725.DC4"), "pew")


dist_cost.hybAll.tot.SR0 <- 
  get_riskTransfer.stch(results_all.sumTiers, 
                        c("RS1_SR0EL1_sep_R725.d725", "SR0EL1.Reform_sep_R725.d725.DC4a"), "pew")


dist_cost.hybNew.tot
dist_cost.hybNew.new
dist_cost.hybAll.tot

dist_cost.hybNew.tot.SR0
dist_cost.hybNew.new.SR0
dist_cost.hybAll.tot.SR0

write.xlsx2(dist_cost.hybNew.tot, file = "Results/RiskTransfer/RiskTransfer_stch.tab.xlsx", sheetName = "hybNew.tot")
write.xlsx2(dist_cost.hybNew.new, file = "Results/RiskTransfer/RiskTransfer_stch.tab.xlsx", sheetName = "hybNew.new",  append = T)
write.xlsx2(dist_cost.hybAll.tot, file = "Results/RiskTransfer/RiskTransfer_stch.tab.xlsx", sheetName = "hybAll.tot",  append = T)

write.xlsx2(dist_cost.hybNew.tot.SR0, file = "Results/RiskTransfer/RiskTransfer_stch.tab.xlsx", sheetName = "hybNew.tot.SR0", append = T)
write.xlsx2(dist_cost.hybNew.new.SR0, file = "Results/RiskTransfer/RiskTransfer_stch.tab.xlsx", sheetName = "hybNew.new.SR0", append = T)
write.xlsx2(dist_cost.hybAll.tot.SR0, file = "Results/RiskTransfer/RiskTransfer_stch.tab.xlsx", sheetName = "hybAll.tot.SR0", append = T)




# 5.2 Plot the distributions of employer pension costs ####

fig.lab <- c("Pure DB plan",
             "DB/DC hybrid for new hires only",
             "DB/DC hybrid for all current and future employees")
fig.title <- "Distributions of 30-year employer pension costs (including unfunded liability in 2048)"

rn <- c("RS1_SR0EL1_sep_R725.d725", 
        "SR0EL1.Reform_sep_R725.d725.DC4",
        "SR0EL1.Reform_sep_R725.d725.DC4a")
year_range <- 2017:2048

dist.cost.tot <- 
  results_all.sumTiers %>% filter(runname %in% rn, 
                         sim > 0,
                         year %in% year_range) %>% 
  mutate(runname = factor(runname, levels = rn, labels = fig.lab)) %>% 
  group_by(runname, sim) %>% 
  summarize(cost_0 = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9) %>% 
  group_by(runname) %>% 
  ggplot(aes(cost_0)) + theme_bw() + 
  facet_wrap(~runname, nrow = 3) + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 20, boundary = 0) + 
  coord_cartesian(xlim = c(-400, 400)) + 
  scale_x_continuous(breaks = seq(-2000,2000,100)) + 
  labs(title = fig.title,
       x = "Employer pension cost",
       y = "Count of simulations") + 
  centeringTitles()

dist.cost.tot


fig.lab <- c("Pure DB plan",
             "DB/DC hybrid for new hires")
fig.title <- "Distributions of total employer pension costs in 2017-2048 \n(including unfunded liability in 2048)"
fig.subtitle <- "All current and future members"

rn <- c("RS1_SR0EL1_sep_R725.d725", 
        "SR0EL1.Reform_sep_R725.d725.DC4")
year_range <- 2017:2048

dist.cost.tot2 <- 
  results_all.sumTiers %>% filter(runname %in% rn, 
                                  sim > 0,
                                  year %in% year_range) %>% 
  mutate(runname = factor(runname, levels = rn, labels = fig.lab)) %>% 
  group_by(runname, sim) %>% 
  summarize(cost_0 = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9) %>% 
  group_by(runname) %>% 
  ggplot(aes(cost_0)) + theme_bw() + 
  facet_wrap(~runname, nrow = 3) + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 20, boundary = 0) + 
  coord_cartesian(xlim = c(-400, 400), ylim = c(0, 400)) + 
  scale_x_continuous(breaks = seq(-2000,2000,100)) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Employer pension cost",
       y = "Count of simulations") + 
  centeringTitles()


dist.cost.tot2




fig.lab <- c("Pure DB plan",
             "DB/DC hybrid for \all current and future members")
fig.title <- "Distributions of total employer pension costs in 2017-2048 \n(including unfunded liability in 2048)"
fig.subtitle <- "All current and future members are affected by the hybrid plan reform"

rn <- c("RS1_SR0EL1_sep_R725.d725", 
        "SR0EL1.Reform_sep_R725.d725.DC4a")
year_range <- 2017:2048

dist.cost.allHybrid <- 
  results_all.sumTiers %>% filter(runname %in% rn, 
                                  sim > 0,
                                  year %in% year_range) %>% 
  mutate(runname = factor(runname, levels = rn, labels = fig.lab)) %>% 
  group_by(runname, sim) %>% 
  summarize(cost_0 = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9) %>% 
  group_by(runname) %>% 
  ggplot(aes(cost_0)) + theme_bw() + 
  facet_wrap(~runname, nrow = 3) + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 20, boundary = 0) + 
  coord_cartesian(xlim = c(-400, 400)) + 
  scale_x_continuous(breaks = seq(-2000,2000,100)) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Employer pension cost",
       y = "Count of simulations") + 
  centeringTitles()
dist.cost.allHybrid




fig.lab <- c("Pure DB plan",
             "DB/DC hybrid")
fig.title <- "Distributions of total employer pension costs in 2017-2048 \n(including unfunded liability in 2048)"
fig.subtitle <- "Future members only"

rn <- c("RS1_SR0EL1_sep_R725.d725", 
        "SR0EL1.Reform_sep_R725.d725.DC4")
year_range <- 2017:2048

dist.cost.new <- 
  results_all.New %>% filter(runname %in% rn, 
                                  sim > 0,
                                  year %in% year_range) %>% 
  mutate(runname = factor(runname, levels = rn, labels = fig.lab)) %>% 
  group_by(runname, sim) %>% 
  summarize(cost_0 = sum(ERC.tot.final, na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9) %>% 
  group_by(runname) %>% 
  ggplot(aes(cost_0)) + theme_bw() + 
  facet_wrap(~runname, nrow = 3) + 
  geom_histogram(color = "black", fill = RIG.blue, binwidth = 5, boundary = 0) + 
  coord_cartesian(xlim = c(-100, 100), ylim = c(0, 400)) + 
  scale_x_continuous(breaks = seq(-2000,2000,20)) + 
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = "Employer pension cost",
       y = "Count of simulations") + 
  centeringTitles()




dist.cost.new
dist.cost.tot2


n1 <- "Notes:\n"
n2 <- "1. Employer pension cost: nominal value (no discounting) of the total employer contribution to PSERS during the \n"
n3 <- "simulation period of 2017-2048, plus the remaining unfunded liability at the end of 2048.\n"
n4 <- "2. There are negative values of total contribution because in simulations with very good realized investment returns in\n"
n5 <- "the 30-year period the plan can have a surplus in assets that is even larger than the total employer pension cost\n"
n6 <- "during the period. One reason for the existence of very large surplus is that our simulation model intentionally does\n" 
n7 <- "not allow for withdrawal from the assets when when there is a surplus (no amortization for the surplus).\n"
n8 <- "3. Total DC contribution rate = 5% (employer rate + employee rate).\n"
n9 <- "4. Shared-risk employee contribution rates are not modeled in these simulations."


notes <- grid.text(paste0(n1, n2, n3, n4, n5, n6, n7, n8, n9), 
    just = "left",
    x = 0.05) 

dist.cost.2cols <- grid.arrange(dist.cost.new, dist.cost.tot2, ncol = 2, bottom = notes)



ggsave(file = "Results/RiskTransfer/disb.cost.tot.png", dist.cost.tot, width = 8*0.9, height = 14*0.9)
ggsave(file = "Results/RiskTransfer/disb.cost.new.png", dist.cost.new, width = 8*0.9, height = 10*0.9)
ggsave(file = "Results/RiskTransfer/disb.cost.tot2.png", dist.cost.tot2,width = 8*0.9, height = 10*0.9)
ggsave(file = "Results/RiskTransfer/disb.cost.allHybrid.png", dist.cost.allHybrid,width = 8*0.9, height = 10*0.9)

ggsave(file = "Results/RiskTransfer/fig10.disb.cost.2cols.png", dist.cost.2cols, width = 12*0.9, height = 13*0.9)
ggsave(file = "Results/RiskTransfer/fig10.disb.cost.2cols.pdf", dist.cost.2cols, width = 12*0.9, height = 13*0.9)



# 5.3 How distribution changes over time

df_all.stch %>% filter(runname %in% c("RS1_SR0EL1_sep_R725.d725", 
                                      "SR0EL1.Reform_sep_R725.d725.DC4"),
                       Tier == "sumTiers") %>% 
  select(runname, year, ERC_PR.q10)
  ggplot(ase(x = year, y = ))
  


  # Distribution of total ERC as % Payroll
  fig.title    <- "Distribution of employer contribution as a percentage of payroll across simulations"
  fig.subtitle <- "Current PSERS funding policy and proposed pension reform"
  fig.ERC_PR.tot <- df_all.stch %>% filter(runname %in% c("RS1_SR0EL1_sep_R725.d725", 
                                                          "SR0EL1.Reform_sep_R725.d725.DC4"), 
                                           Tier == "sumTiers",
                                           year %in% 2017:2048) %>% 
    select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
    gather(type, value, -runname, -year) %>% 
    # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
    ggplot(aes(x = year, y = value,
               color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
               shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
    theme_bw() + 
    facet_grid(.~runname) + 
    geom_line() + 
    geom_point(size = 2) + 
    geom_hline(yintercept = 100, linetype = 2, size = 1) +
    coord_cartesian(ylim = c(0,50)) + 
    scale_x_continuous(breaks = c(2016, seq(2020, 2050, 5))) + 
    #scale_y_continuous(breaks = seq(0, 500, 5)) + 
    scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
                       label  = c("75th percentile", "50th percentile", "25th percentile")) + 
    scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
                       label  = c("75th percentile", "50th percentile", "25th percentile")) +
    labs(title = fig.title,
         subtitle = fig.subtitle,
         x = NULL, y = "Percent of payroll") + 
    theme(axis.text.x = element_text(size = 8)) + 
    RIG.theme()
  fig.ERC_PR.tot
  
  



  # Distribution of total ERC as % Payroll
  fig.title    <- "Distribution of employer contribution as a percentage of payroll across simulations"
  fig.subtitle <- "Current PSERS funding policy and proposed pension reform"
  fig.ERC_PR.new <- df_all.stch %>% filter(runname %in% c("RS1_SR0EL1_sep_R725.d725", 
                                                          "SR0EL1.Reform_sep_R725.d725.DC4"), 
                                           Tier == "sumTiers.New",
                                           year %in% 2017:2048) %>% 
    select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
    gather(type, value, -runname, -year) %>% 
    # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
    ggplot(aes(x = year, y = value,
               color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
               shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
    theme_bw() + 
    facet_grid(.~runname) + 
    geom_line() + 
    geom_point(size = 2) + 
    geom_hline(yintercept = 100, linetype = 2, size = 1) +
    coord_cartesian(ylim = c(0,10)) + 
    scale_x_continuous(breaks = c(2016, seq(2020, 2050, 5))) + 
    #scale_y_continuous(breaks = seq(0, 500, 5)) + 
    scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
                       label  = c("75th percentile", "50th percentile", "25th percentile")) + 
    scale_shape_manual(values = c(17, 16, 15),  name = NULL, 
                       label  = c("75th percentile", "50th percentile", "25th percentile")) +
    labs(title = fig.title,
         subtitle = fig.subtitle,
         x = NULL, y = "Percent of payroll") + 
    theme(axis.text.x = element_text(size = 8)) + 
    RIG.theme()
  fig.ERC_PR.new
  
  ggsave(file = "Results/RiskTransfer/ERC_PR.tot.png", fig.ERC_PR.tot, width = 14*0.9, height = 8*0.9)
  ggsave(file = "Results/RiskTransfer/ERC_PR.new.png", fig.ERC_PR.new, width = 14*0.9, height = 8*0.9)
  
  
  
## detective work ####
  
  df_all.stch %>% filter(runname %in% c("RS1_SR1EL1"), 
                         Tier == "sumTiers" ) %>% select(-ERC_GF_hike, -FR100more, -ERC_high, -Tier)
  df_all.stch %>% filter(runname %in% c("RS1_SR1EL1_sep_R725.d725"), 
                         Tier == "sumTiers" ) %>% select(-ERC_GF_hike, -FR100more, -ERC_high, -Tier)
  
  d.year <- 2017:2045# seq(2015, 2045, 5)
  d.sim  <- 10
  
  results_all.sumTiers %>% filter(runname == "RS1_SR1EL1", sim == d.sim, year %in% d.year)               %>% select(year, FR_MA, ERC.DB.final, ERC, EEC_PR,  ADC, SC, NC, sharedRisk.rate)
  results_all.sumTiers %>% filter(runname == "RS1_SR1EL1_sep_R725.d725", sim == d.sim, year %in% d.year) %>% select(year, FR_MA, ERC.DB.final, ERC, EEC_PR,  ADC, SC, NC, sharedRisk.rate)
  
  results_all.xNew %>% filter(runname == "RS1_SR1EL1_sep_R725.d725", sim == d.sim, year %in% d.year)   %>% select(year, FR_MA, ERC.DB.final,  ERC, EEC_PR,  ADC, SC, NC, sharedRisk.rate)
  results_all.New %>% filter(runname  == "RS1_SR1EL1_sep_R725.d725",  sim == d.sim, year %in% d.year)  %>% select(year, FR_MA, ERC.DB.final, ERC, EEC_PR,  ADC, SC, NC, sharedRisk.rate)
  
  
  # 1. lower bound of final.ERC
  # 2. upper bound of final.ERC
  # 3. reset shared risk rate to 0 when FR>100 for: easier to reach for new hires. 
  # Conclusion, need model ERC and EEC of old and new members jointly. 
  
  
  results_all.New %>% filter(runname == "RS1_SR0EL1_sep_R725.d725", sim == 0, year %in% d.year) %>% select(year, FR_MA, ERC.DB.final, ERC, EEC_PR,  ADC, SC, NC_PR, sharedRisk.rate, DC_EEC_PR)
  results_all.New %>% filter(runname == "SR0EL1.Reform_sep_R725.d725.DC4", sim == 0, year %in% d.year) %>% select(year, FR_MA, ERC.DB.final, ERC, EEC_PR,  ADC, SC, NC_PR, sharedRisk.rate, DC_EEC_PR)
  
  
# 6. Payroll and liability for new hires ####

  df_sumTiers <- results_all.sumTiers %>% filter(runname == "RS1_SR0EL1_sep_R725.d725", sim == 0, year %in% 2017:2055) %>% select(year, PR.tot = PR, AL.tot = AL, nactives.tot = nactives)
  df_New <- results_all.New %>% filter(runname == "RS1_SR0EL1_sep_R725.d725", sim == 0, year %in% 2017:2055) %>% select(year, PR.new = PR, AL.new = AL, nactives.new = nactives)
  
  df <- left_join(df_sumTiers, df_New)
  df %<>% mutate(PR_pct.new = 100 * PR.new / PR.tot,
                AL_pct.new = 100 * AL.new / AL.tot,
                nactives_pct.new = 100 * nactives.new / nactives.tot) %>% 
    select(year, PR_pct.new, AL_pct.new,  nactives_pct.new) 
  
  df.short <- df %>% filter(year %in% c(2017, seq(2020, 2045, 5), 2048))
  
  
  df
  df.short
  
  write.xlsx2(df,       file = "Results/RiskTransfer/RiskTransfer_shareNew.xlsx", sheetName = "long")
  write.xlsx2(df.short, file = "Results/RiskTransfer/RiskTransfer_shareNew.xlsx", sheetName = "short",  append = T)
  
  
# 7. Greg's question ####  

  df.IFO <- 
  results_all.sumTiers %>% filter(runname %in% c("RS1_SR0EL1_sep_R725.d725",
                                                 "RS1_SR0EL1_sep_R625.d625",
                                                 "SR0EL1.Reform_sep_R725.d725.DC4",
                                                 "SR0EL1.Reform_sep_R625.d625.DC4"),
                                  sim == 0,
                                  year %in% 2017:2048)  %>% 
    group_by(runname) %>% 
    summarise(NC = sum(NC-EEC)/1e9,
              SC = sum(SC)/1e9,
              UAAL30 = UAAL[year == max(year)]/1e9,
              ERC.tot = sum(ERC.DB.final)/1e9  + UAAL[year == max(year)]/1e9 * 1 ) %>% 
    mutate(ERC.xNC = ERC.tot - NC - UAAL30)
  df.IFO 
  
  
  
  df.pew <- 
    results_all.sumTiers %>% filter(runname %in% c("RS1_SR0EL1_sep_R725.d725",
                                                   "RS1_SR0EL1_sep_R625.d725",
                                                   "SR0EL1.Reform_sep_R725.d725.DC4",
                                                   "SR0EL1.Reform_sep_R625.d725.DC4"),
                                    sim == 0,
                                    year %in% 2017:2048)  %>% 
    group_by(runname) %>% 
    summarise(NC = sum(NC - EEC)/1e9,
              SC = sum(SC)/1e9,
              UAAL30 = UAAL[year == max(year)]/1e9,
              ERC.tot = sum(ERC.DB.final)/1e9  + UAAL[year == max(year)]/1e9) %>% 
    mutate(ERC.xNC = ERC.tot - NC - UAAL30)
  df.pew 
  
      
  # with UAAL
  diff.CL <- 166.7 - 132.4
  diff.PL <- 168.2 - 139.78
  diff.CL
  diff.PL
  diff.CL - diff.PL  
  
  diff.CL.xNC <-  
  diff.PL.xNC <- 
  diff.CL.xNC
  diff.PL.xNC
  diff.CL.xNC - diff.PL.xNC
    
  # without UAAL:
  
  diff.CL <- 169.61 - 134.99
  diff.PL <- 169.85 - 141.24
  diff.CL
  diff.PL
  diff.CL - diff.PL
  
  diff.CL.xNC <- 60.21 - 48.36
  diff.PL.xNC <- 89.20 - 77.37
  diff.CL.xNC
  diff.PL.xNC
  diff.CL.xNC - diff.PL.xNC
  
  
 
  
  df.IFO %>% filter(year == max(year)) %>% select(year, AL, FR_MA, UAAL)
  
  results_all.New %>% filter(runname == "RS1_SR0EL1_sep_R725.d725", sim == 0, year %in% 2017:2048) %>% 
    select(year, FR_MA, NC, SC, ERC, EEC, ERC.final, UAAL,ERC.DB.final_PR) %>% 
    mutate_all(.funs = funs(./1e9)) %>% 
    mutate(ERC.DB.final_PR = ERC.DB.final_PR*1e9) %>% 
    kable(digits = 2)

df.IFO.full <-     
results_all.sumTiers %>% filter( runname %in% c("RS1_SR0EL1_sep_R725.d725",
                                                 "RS1_SR0EL1_sep_R625.d625",
                                                 "SR0EL1.Reform_sep_R725.d725.DC4",
                                                 "SR0EL1.Reform_sep_R625.d625.DC4")) %>% 
  filter(sim == 0, year %in% 2017:2048) %>% 
  select(runname, year, ERC.DB.final, NC, EEC, UAAL) %>% 
  mutate(NC.ER = NC - EEC) %>% 
  mutate_at(vars(-runname, -year), funs(./1e9))
  
df.pew.full <- 
results_all.sumTiers %>% filter( runname %in% c("RS1_SR0EL1_sep_R725.d725",
                                                "RS1_SR0EL1_sep_R625.d725",
                                                "SR0EL1.Reform_sep_R725.d725.DC4",
                                                "SR0EL1.Reform_sep_R625.d725.DC4")) %>% 
  filter(sim == 0, year %in% 2017:2048) %>% 
  select(runname, year, ERC.DB.final, NC, EEC, UAAL) %>% 
  mutate(NC.ER = NC - EEC) %>% 
  mutate_at(vars(-runname, -year), funs(./1e9))

df.IFO.full
df.pew.full

  
write.xlsx2(df.IFO.full,  file = "Results/RiskTransfer/CompareApproaches.xlsx", sheetName = "DiscountRateLowered.raw")
write.xlsx2(df.pew.full,  file = "Results/RiskTransfer/CompareApproaches.xlsx", sheetName = "DiscountRateNotLowered.raw",  append = T)

  



####
results_all %>% filter(runname == "SR0EL1.Reform_sep_R725.d725.DC4a", sim == 0, Tier == "sumTiers.xNew") %>% filter(year <= 2051) %>% 
  select(Tier, year, FR_MA, AL, AL.act.laca, MA, AA, C, B, NC, NC.laca, AL.act.v, NC)















  
  
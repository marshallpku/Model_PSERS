# Risk measures for MISERS

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




# lab_s1 <- "Scenario 1 \nAssumption Achieved: \nClosed Plan"
# lab_s2 <- "Scenario 2 \nAssumption Achieved: \nOpen Plan"
# lab_s3 <- "Scenario 3 \n15 Years of Low Returns"
# lab_s4 <- "Scenario 4 \nHigh Volatility"
# lab_s5 <- "Scenario 5 \nCurrent Return Assumption"
# lab_s6 <- "Scenario 6 \nLower Return Assumption"


runs_all <- c(runs_RS1, runs_RS2, runs_RS3, runs_alt, runs_reform)
runs_all_labels <- c(runs_RS1_labels, runs_RS2_labels, runs_RS3_labels, runs_alt_labels, runs_reform_labels)



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


df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim > 0, year %in% 2016:2045)



df_all.stch %<>%   
  select(runname, returnScn, policy.SR, policy.EL, sim, year, FR_MA, AL, MA, ERC, EEC, PR, ERC_PR, ERC.final_PR, ERC.final_GF) %>%
  group_by(runname, sim) %>% 
  mutate(
         #FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more = FR_MA >= 100,
         ERC_high  = cumany(ERC.final_PR >= 50), 
         ERC_hike     = cumany(na2zero(ERC.final_PR - lag(ifelse(year == 2016, NA, ERC.final_PR), 5) >= 10)),  # NA for 2016 value: excludes impact of new amort payment in 2017 
         ERC_GF_hike  = cumany(na2zero(ERC.final_GF - lag(ifelse(year == 2016, NA, ERC.final_GF), 5) >= 5)),
         EEC_PR       = 100 * EEC / PR
         ) %>% 
  group_by(runname, returnScn, policy.SR, policy.EL, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            ERC_GF_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC.final_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC.final_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC.final_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC.final_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC.final_PR, 0.9, na.rm = T),
            
            EEC_PR.q10 = quantile(EEC_PR, 0.1, na.rm = T),
            EEC_PR.q25 = quantile(EEC_PR, 0.25, na.rm = T),
            EEC_PR.q50 = quantile(EEC_PR, 0.5, na.rm = T),
            EEC_PR.q75 = quantile(EEC_PR, 0.75, na.rm = T),
            EEC_PR.q90 = quantile(EEC_PR, 0.9, na.rm = T),
            
            ERC_GF.q10 = quantile(ERC.final_GF, 0.1, na.rm = T),
            ERC_GF.q25 = quantile(ERC.final_GF, 0.25, na.rm = T),
            ERC_GF.q50 = quantile(ERC.final_GF, 0.5, na.rm = T),
            ERC_GF.q75 = quantile(ERC.final_GF, 0.75, na.rm = T),
            ERC_GF.q90 = quantile(ERC.final_GF, 0.9, na.rm = T)
            
  ) %>% 
  ungroup() %>%
  mutate(runname.lab = factor(runname, 
                              levels = runs_all, 
                              labels = runs_all_labels))




# Check the impact of DC reform 
results_all %>% filter(runname == "RS1_SR1EL1", sim == 0)            %>% select(runname, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR, DC_ERC_PR.tEF, DC_PR.tEF, DC_ERC, ERC.DB.final, ExF_MA)
results_all %>% filter(runname == "RS1_SR1EL1_R625.d725", sim == 0)  %>% select(runname, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR, DC_ERC_PR.tEF, DC_PR.tEF, DC_ERC, ERC.DB.final, ExF_MA)
results_all %>% filter(runname == "RS1_SR1EL1_R625.d625", sim == 0)  %>% select(runname, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR, DC_ERC_PR.tEF, DC_PR.tEF, DC_ERC, ERC.DB.final, ExF_MA)

results_all %>% filter(runname == "SR1EL1.Reform_R725.d725.DC1", sim == 0)  %>% select(runname, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR, DC_ERC_PR.tEF, DC_PR.tEF, DC_ERC, ERC.DB.final, ExF_MA)


 # Reform for new hires and reform for all
results_all %>% filter(runname == "SR1EL1.Reform_R725.d725.DC3", sim == 0)   %>% select(runname, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR, DC_ERC_PR.tEF, DC_PR.tEF, DC_ERC, ERC.DB.final, ExF_MA)
results_all %>% filter(runname == "SR1EL1.Reform_R725.d725.DC3a", sim == 0)  %>% select(runname, year, FR_MA, AL, PR, NC_PR, B,SC, EEC_PR, ERC_PR, ERC.final_PR, DC_ERC_PR.tEF, DC_PR.tEF, DC_ERC, ERC.DB.final, ExF_MA)





results_all %>% select(runname, sim, year, ERC.final.0 = ERC.final) %>% filter(sim > 0, runname == "RS1_SR0EL1") %>%
  left_join(results_all %>% filter(sim > 0, runname == "RS1_SR1EL1")%>% select(sim, year, ERC.final.1 = ERC.final)) %>% 
  group_by(sim) %>% 
  summarise(PV.ERC.0 = sum(ERC.final.0 / 1e6 *(1 + 0.075)^(row_number() - 1)),
         PV.ERC.1 = sum(ERC.final.1 / 1e6*(1 + 0.075)^(row_number() - 1))) %>%
  mutate(diff.PV.ERC = 1 - PV.ERC.1 / PV.ERC.0 ) %>% 
  ungroup() %>% 
  summarise(diff.PV.ERC.q10   = quantile(diff.PV.ERC, 0.1,na.rm = T),
            diff.PV.ERC.q25   = quantile(diff.PV.ERC, 0.25, na.rm = T),
            diff.PV.ERC.q50   = quantile(diff.PV.ERC, 0.5, na.rm = T),
            diff.PV.ERC.q75   = quantile(diff.PV.ERC, 0.75, na.rm = T),
            diff.PV.ERC.q90   = quantile(diff.PV.ERC, 0.9, na.rm = T))




#*************************************************************************
##                        4. DC reform                                ####
#*************************************************************************

# load("Data_inputs/DC_rate.tot725.RData")
# load("Data_inputs/DC_rate.tot625.RData")              
# 
# DC_rate.tot

runs_reform <- c("SR1EL1.Reform_R725.d725.DC1",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: PVDC = PVDB 
                 "SR1EL1.Reform_R625.d725.DC1",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: PVDC = PVDB 
                 "SR1EL1.Reform_R625.d625.DC1",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: PVDC = PVDB  
                 
                 "SR1EL1.Reform_R725.d725.DC3",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 5% 
                 "SR1EL1.Reform_R625.d725.DC3",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 5% 
                 "SR1EL1.Reform_R625.d625.DC3",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 5% 
                 
                 "SR1EL1.Reform_R725.d725.DC4",  # DC reform with expected return = 7.25%, discount rate = 7.25%; DC rate: 9% 
                 "SR1EL1.Reform_R625.d725.DC4",  # DC reform with expected return = 6.25%, discount rate = 7.25%; DC rate: 9% 
                 "SR1EL1.Reform_R625.d625.DC4",  # DC reform with expected return = 6.25%, discount rate = 6.25%; DC rate: 9% 
                 
                 "RS1_SR1EL1_R625.d725",       # No DC reform with expected return = 6.25%, discount rate = 7.25% 
                 "RS1_SR1EL1_R625.d625")       # No DC reform with expected return = 6.25%, discount rate = 6.25%


# 4.1 Deterministic run ####

# Impact on AL of DB plan
results_all %>% filter(runname %in% c("RS1_SR1EL1", "SR1EL1.Reform_R725.d725.DC1"), sim == 0) %>% 
  select(runname, sim, year, AL) %>% 
  spread(runname, AL) %>% 
  mutate(AL_reduction = SR1EL1.Reform_R725.d725.DC1/RS1_SR1EL1)


# Impact on ERC
results_all %>% filter(runname %in% c("RS1_SR1EL1", "SR1EL1.Reform_R725.d725.DC1", "SR1EL1.Reform_R725.d725.DC3"), sim == 0) %>% 
  select(runname, sim, year, ERC.final_PR) %>% 
  spread(runname, ERC.final_PR)



# Decomposition of ERC, NO DC reform
results_all %>% filter(runname %in% c("RS1_SR1EL1"), sim == 0) %>% 
  select(runname, sim, year, C_PR, NC_PR, SC_PR, EEC_PR, ERC.DB.final_PR, DC_EEC_PR, DC_ERC_PR, ERC.final_PR, DC_EEC_PR.tEF, DC_ERC_PR.tEF)   #  %>% 
# spread(runname, ERC.final_PR)


# Decomposition of ERC, with DC reform
results_all %>% filter(runname %in% c("SR1EL1.Reform_R725.d725.DC1"), sim == 0) %>% 
  select(runname, sim, year, C_PR, NC_PR, SC_PR, EEC_PR, ERC.DB.final_PR, DC_EEC_PR, DC_ERC_PR, ERC.final_PR, DC_EEC_PR.tEF, DC_ERC_PR.tEF, AL, AA)   #  %>% 
  # spread(runname, ERC.final_PR)

results_all %>% filter(runname %in% c("SR1EL1.Reform_R725.d725.DC3"), sim == 0) %>% 
  select(runname, sim, year, C_PR, NC_PR, SC_PR, EEC_PR, ERC.DB.final_PR, DC_EEC_PR, DC_ERC_PR, ERC.final_PR, DC_EEC_PR.tEF, DC_ERC_PR.tEF, AL, AA)   #  %>% 
# spread(runname, ERC.final_PR)

results_all %>% filter(runname %in% c("SR1EL1.Reform_R625.d725.DC3"), sim == 0) %>% 
  select(runname, sim, year, C_PR, NC_PR, SC_PR, EEC_PR, ERC.DB.final_PR, DC_EEC_PR, DC_ERC_PR, ERC.final_PR, DC_EEC_PR.tEF, DC_ERC_PR.tEF)   #  %>% 
# spread(runname, ERC.final_PR)




# Decomposition of ERC, with DC reform
results_all %>% filter(runname %in% c("SR1EL1.Reform_R725.d725.DC1"), sim == 0, year <=2020) %>% 
  select(runname, sim, year,  AL, AA, AL.act.laca)

results_all %>% filter(runname %in% c("SR1EL1.Reform_R725.d725.DC3"), sim == 0, year <=2020) %>% 
  select(runname, sim, year,  AL, AA, AL.act.laca)



# 4.2 Distributions of FR and ERC ####


# Distribution of funded ratio 
fig.title <- "Distribution of funded ratios across simulations"
fig.subtitle <- "Current PSERS funding policy and proposed pension reform"
fig_reform.RS1.FRdist <- df_all.stch %>% filter(runname %in% c("RS1_SR1EL1", "RS1_SR1EL1.Reform725") ) %>% 
  left_join(results_all  %>% 
              filter(runname  %in% c("RS1_SR1EL1", "RS1_SR1EL1.Reform725"), sim == 0) %>% 
              select(runname, year, FR_det = FR_MA)) %>%  
  select(runname, year, FR.q25, FR.q50, FR.q75, FR_det) %>% 
  gather(type, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25", "FR_det"))
  )) + theme_bw() + 
  facet_grid(. ~ runname) + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,180)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_reform.RS1.FRdist



# Distribution of total ERC as % Payroll
fig.title    <- "Distribution of employer contribution as a percentage of payroll across simulations"
fig.subtitle <- "Current PSERS funding policy and proposed pension reform"
fig_reform.RS1.ERCdist <- df_all.stch %>% filter(runname %in% c("RS1_SR1EL1", "RS1_SR1EL1.Reform725")) %>% 
  left_join(results_all  %>%
              filter(runname  %in% c("RS1_SR1EL1", "RS1_SR1EL1.Reform725"), sim == 0) %>%
              select(runname, year, ERC_det = ERC.final_PR)) %>%
  select(runname, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75, ERC_det) %>% 
  gather(type, value, -runname, -year) %>% 
  # mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_det")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25", "ERC_det")))) + 
  theme_bw() + 
  facet_grid(.~runname) + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,50)) + 
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green, "black"),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile", "Deterministic")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Percent of payroll") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_reform.RS1.ERCdist



# 4.3 risk measures ####

# Risk of low funded ratio
fig.title <- "Probability of funded ratio below 40% in any year up to the given year"
fig.subtitle <- "Current PSERS funding policy and proposed pension reform"
fig_reform.RS1.FR40less <- df_all.stch %>% filter(runname %in% c("RS1_SR1EL1", "RS1_SR1EL1.Reform725")) %>%
  # mutate(runname = factor(runname, labels = c(lab.RS1, lab.RS2, lab.RS3))) %>%
  select(runname, year, FR40less) %>%
  #mutate(FR40less.det = 0) %>%
  #gather(variable, value, -year, -runname) %>%
  ggplot(aes(x = year, y = FR40less, color = runname, shape = runname)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line() +
  coord_cartesian(ylim = c(0,50)) +
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) +
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red),  name = "") +
  scale_shape_manual(values = c(17,16, 15),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") +
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_reform.RS1.FR40less
fig_reform.RS1.FR40less %>% filter(year == 2045)



# Risk of sharp increase in ERC/PR
fig.title <- "Probability of employer contribution rising more than 10% of payroll \nin a 5-year period at any time prior to and including the given year"
fig.subtitle <- "Current PSERS funding policy and proposed pension reform"
fig_reform.RS1.ERChike <- df_all.stch %>% filter(runname %in% c("RS1_SR1EL1", "RS1_SR1EL1.Reform725")) %>% 
  # mutate(policy.SR = factor(policy.SR, levels = c(1, 0), labels = c("Current policy", "No risk-sharing")),
  #        returnScn = factor(returnScn, levels = c("RS1", "RS2", "RS3"), labels = c(lab.RS1, lab.RS2, lab.RS3))) %>%  
  select(runname, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  # gather(variable, value, -year, -returnScn) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname, shape = runname)) + theme_bw() + 
  geom_point(size = 2) + geom_line() + 
  coord_cartesian(ylim = c(0,15)) + 
  scale_y_continuous(breaks = seq(0,200, 2)) +
  scale_x_continuous(breaks = c(2016, seq(2020, 2045, 5))) + 
  scale_color_manual(values = c(RIG.blue, RIG.green, RIG.red, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()
fig_reform.RS1.ERChike
fig_reform.RS1.ERChike$data



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
      riskTansfer_CF = (Diff.CL_CF - Diff.PL_CF),
      riskTransfer.pct_CF = 100 * riskTansfer_CF / Diff.CL_CF,
      
      Diff.CL_PV725 = (DB_low_PV725 - DB_high_PV725),
      Diff.PL_PV725 = (hybrid_low_PV725 - hybrid_high_PV725),
      riskTansfer_PV725 = (Diff.CL_PV725 - Diff.PL_PV725),
      riskTransfer.pct_PV725 = 100 * riskTansfer_PV725 / Diff.CL_PV725,
      
      Diff.CL_PV37 = (DB_low_PV37 - DB_high_PV37),
      Diff.PL_PV37 = (hybrid_low_PV37 - hybrid_high_PV37),
      riskTansfer_PV37 = (Diff.CL_PV37 - Diff.PL_PV37),
      riskTransfer.pct_PV37 = 100 * riskTansfer_PV37 / Diff.CL_PV37
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
      riskTansfer_CF = (Diff.CL_CF - Diff.PL_CF),
      riskTransfer.pct_CF = 100 * riskTansfer_CF / Diff.CL_CF,
      
      Diff.CL_PV725 = (DB_low_PV725 - DB_high_PV725),
      Diff.PL_PV725 = (hybrid_low_PV725 - hybrid_high_PV725),
      riskTansfer_PV725 = (Diff.CL_PV725 - Diff.PL_PV725),
      riskTransfer.pct_PV725 = 100 * riskTansfer_PV725 / Diff.CL_PV725,
      
      Diff.CL_PV37 = (DB_low_PV37 - DB_high_PV37),
      Diff.PL_PV37 = (hybrid_low_PV37 - hybrid_high_PV37),
      riskTansfer_PV37 = (Diff.CL_PV37 - Diff.PL_PV37),
      riskTransfer.pct_PV37 = 100 * riskTansfer_PV37 / Diff.CL_PV37
    ) %>% 
    t
  
  riskTransfer %<>%
    as.data.frame %>% 
    mutate(var = rownames(riskTransfer)) %>% 
    as.data.frame %>% 
    select(var, value = V1)
  
}


get_riskTransfer.pctile <- function(df, rn, discount, method, year_range = 2016:2048){

# df <- results_all
# rn <- c("RS1_SR1EL1",
#         "RS1_SR1EL1_R625.d625",
#         "SR1EL1.Reform_R725.d725.DC1",
#         "SR1EL1.Reform_R625.d625.DC1")
# 
# year_range <- 2016:2048
# 
# discount <- 0.0
# 
# method <- "IFO"

add.UAAL <- switch(method, IFO = 0, pew = 1)

riskTransfer <-  df %>% filter(runname %in% rn, 
                               year %in% year_range,
                               sim > 0) %>% 
  mutate(runname = factor(runname, levels = rn)) %>% 
  group_by(runname, sim) %>% 
  summarize(cost = sum(ERC.tot.final / (1 + discount)^(row_number() - 1), na.rm = TRUE)/1e9 + UAAL[year == max(year)]/1e9 * add.UAAL
  ) %>% 
  group_by(runname) %>% 
  summarize(pct75 = quantile(cost, 0.75),
            pct50 = quantile(cost, 0.50),
            pct25 = quantile(cost, 0.25)) %>% 
  mutate(runname = c("DB_high", "DB_low", "hybrid_high", "hybrid_low")) %>% 
  gather(var, value, -runname) %>% 
  mutate(runname = paste(runname, var, sep = "_")) %>% 
  select(-var) %>% 
  spread(runname, value) %>% 
  mutate(
    Diff.CL_pct75 = (DB_low_pct75 - DB_high_pct75),
    Diff.PL_pct75 = (hybrid_low_pct75 - hybrid_high_pct75),
    riskTansfer_pct75 = (Diff.CL_pct75 - Diff.PL_pct75),
    riskTransfer.pct_pct75 = 100 * riskTansfer_pct75 / Diff.CL_pct75,
    
    Diff.CL_pct50 = (DB_low_pct50 - DB_high_pct50),
    Diff.PL_pct50 = (hybrid_low_pct50 - hybrid_high_pct50),
    riskTansfer_pct50 = (Diff.CL_pct50 - Diff.PL_pct50),
    riskTransfer.pct_pct50 = 100 * riskTansfer_pct50 / Diff.CL_pct50,
    
    Diff.CL_pct25 = (DB_low_pct25 - DB_high_pct25),
    Diff.PL_pct25 = (hybrid_low_pct25 - hybrid_high_pct25),
    riskTansfer_pct25 = (Diff.CL_pct25 - Diff.PL_pct25),
    riskTransfer.pct_pct25 = 100 * riskTansfer_pct25 / Diff.CL_pct25
  )%>% 
  t

riskTransfer %<>%
  as.data.frame %>% 
  mutate(var = rownames(riskTransfer)) %>% 
  as.data.frame %>% 
  select(var, value = V1) %>% 
  separate(var, c("var","percentile"), "_pct") %>% 
  spread(percentile, value)
}




riskTransfer.pct.pew.DC1.CL
riskTransfer.pct.IFO.DC1.CL


# 4.4.1 risk transfer: 9% total DC rate ####

# deterministic IFO method
riskTransfer.IFO.DC4 <- 
get_riskTransfer.IFO(results_all, c("RS1_SR1EL1",
                                "RS1_SR1EL1_R625.d625",
                                "SR1EL1.Reform_R725.d725.DC4",
                                "SR1EL1.Reform_R625.d625.DC4"))

riskTransfer.IFO.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.IFO.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.IFO.DC4 %>% filter(str_detect(var, "PV725"))


# deterministic Pew method
riskTransfer.pew.DC4 <- 
  get_riskTransfer.pew(results_all, c("RS1_SR1EL1",
                                  "RS1_SR1EL1_R625.d725",
                                  "SR1EL1.Reform_R725.d725.DC4",
                                  "SR1EL1.Reform_R625.d725.DC4")
                       )

riskTransfer.pew.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV725"))


# deterministic Pew method, reform applied to all
riskTransfer.pew2.DC4 <- 
  get_riskTransfer.pew(results_all, c("RS1_SR1EL1",
                                      "RS1_SR1EL1_R625.d725",
                                      "SR1EL1.Reform_R725.d725.DC4a",
                                      "SR1EL1.Reform_R625.d725.DC4a")
  )




riskTransfer.pew2.DC4 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV725"))



# 4.4.2 risk transfer, 5% DC rate ####
# Employee contribution rate to DC: 3% for all new members. 

# deterministic IFO method
riskTransfer.IFO.DC3 <- 
  get_riskTransfer.IFO(results_all, c("RS1_SR1EL1",
                                      "RS1_SR1EL1_R625.d625",
                                      "SR1EL1.Reform_R725.d725.DC3",
                                      "SR1EL1.Reform_R625.d625.DC3"))

riskTransfer.IFO.DC3 %>% filter(str_detect(var, "CF"))
riskTransfer.IFO.DC3 %>% filter(str_detect(var, "PV37"))
riskTransfer.IFO.DC3 %>% filter(str_detect(var, "PV725"))


# deterministic pew method
riskTransfer.pew.DC3 <- 
  get_riskTransfer.pew(results_all, c("RS1_SR1EL1",
                                      "RS1_SR1EL1_R625.d725",
                                      "SR1EL1.Reform_R725.d725.DC3",
                                      "SR1EL1.Reform_R625.d725.DC3")
  )

riskTransfer.pew.DC3 %>% filter(str_detect(var, "CF"))
riskTransfer.pew.DC3 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew.DC3 %>% filter(str_detect(var, "PV725"))


# deterministic pew method, reform for all members
riskTransfer.pew2.DC3 <- 
  get_riskTransfer.pew(results_all, c("RS1_SR1EL1",
                                      "RS1_SR1EL1_R625.d725",
                                      "SR1EL1.Reform_R725.d725.DC3a",
                                      "SR1EL1.Reform_R625.d725.DC3a")
  )

riskTransfer.pew2.DC3 %>% filter(str_detect(var, "CF"))
riskTransfer.pew2.DC3 %>% filter(str_detect(var, "PV37"))
riskTransfer.pew2.DC3 %>% filter(str_detect(var, "PV725"))


# Display desterministic results:
# Approach 1: IFO
 riskTransfer.IFO.DC4 %>% filter(str_detect(var, "CF"))
 riskTransfer.IFO.DC3 %>% filter(str_detect(var, "CF"))

# Approach 2: Pew, but DC reform only on new hires
 riskTransfer.pew.DC4 %>% filter(str_detect(var, "CF"))
 riskTransfer.pew.DC3 %>% filter(str_detect(var, "CF"))

 # Approach 3: Pew, DC reform on all current and future members
 riskTransfer.pew2.DC4 %>% filter(str_detect(var, "CF"))
 riskTransfer.pew2.DC3 %>% filter(str_detect(var, "CF"))
 

 # Approach 4: IFO; PV
 riskTransfer.IFO.DC4 %>% filter(str_detect(var, "PV37"))
 riskTransfer.IFO.DC4 %>% filter(str_detect(var, "PV725"))
 
 # Approach 5: Pew, but DC reform only on new hires, PV
 riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV37"))
 riskTransfer.pew.DC4 %>% filter(str_detect(var, "PV725"))
 
 # Approach 6: Pew, DC reform on all current and future members; PV
 riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV37"))
 riskTransfer.pew2.DC4 %>% filter(str_detect(var, "PV725"))
 
 

 
 
 

# 4.5 Risk transfer under stochastic runs ####

# No discount
riskTransfer.pct.IFO.DC4 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d625",
                                        "SR1EL1.Reform_R725.d725.DC4",
                                        "SR1EL1.Reform_R625.d625.DC4"),
                          discount = 0,
                          method = "IFO")


riskTransfer.pct.pew.DC4 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d725",
                                        "SR1EL1.Reform_R725.d725.DC4",
                                        "SR1EL1.Reform_R625.d725.DC4"),
                          discount = 0,
                          method = "pew")


riskTransfer.pct.pew2.DC4 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d725",
                                        "SR1EL1.Reform_R725.d725.DC4a",
                                        "SR1EL1.Reform_R625.d725.DC4a"),
                          discount = 0,
                          method = "pew")


riskTransfer.pct.IFO.DC4
riskTransfer.pct.pew.DC4
riskTransfer.pct.pew2.DC4



# 3.7% discount
riskTransfer.pct.IFO.DC4.disc37 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d625",
                                        "SR1EL1.Reform_R725.d725.DC4",
                                        "SR1EL1.Reform_R625.d625.DC4"),
                          discount = 0.037,
                          method = "IFO")


riskTransfer.pct.pew.DC4.disc37 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d725",
                                        "SR1EL1.Reform_R725.d725.DC4",
                                        "SR1EL1.Reform_R625.d725.DC4"),
                          discount = 0.037,
                          method = "pew")


riskTransfer.pct.pew2.DC4.disc37 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d725",
                                        "SR1EL1.Reform_R725.d725.DC4a",
                                        "SR1EL1.Reform_R625.d725.DC4a"),
                          discount = 0.037,
                          method = "pew")


riskTransfer.pct.pew.DC4.disc37
riskTransfer.pct.IFO.DC4.disc37
riskTransfer.pct.pew2.DC4.disc37



# 7.25% discount
riskTransfer.pct.IFO.DC4.disc725 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d625",
                                        "SR1EL1.Reform_R725.d725.DC4",
                                        "SR1EL1.Reform_R625.d625.DC4"),
                          discount = 0.0725,
                          method = "IFO")


riskTransfer.pct.pew.DC4.disc725 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d725",
                                        "SR1EL1.Reform_R725.d725.DC4",
                                        "SR1EL1.Reform_R625.d725.DC4"),
                          discount = 0.0725,
                          method = "pew")


riskTransfer.pct.pew2.DC4.disc725 <-  
  get_riskTransfer.pctile(results_all,c("RS1_SR1EL1",
                                        "RS1_SR1EL1_R625.d725",
                                        "SR1EL1.Reform_R725.d725.DC4a",
                                        "SR1EL1.Reform_R625.d725.DC4a"),
                          discount = 0.0725,
                          method = "pew")


riskTransfer.pct.IFO.DC4.disc725
riskTransfer.pct.pew.DC4.disc725
riskTransfer.pct.pew2.DC4.disc725



# Display stochastic results


riskTransfer.pct.IFO.DC4
riskTransfer.pct.IFO.DC4.disc37
riskTransfer.pct.IFO.DC4.disc725


riskTransfer.pct.pew.DC4
riskTransfer.pct.pew.DC4.disc37
riskTransfer.pct.pew.DC4.disc725


riskTransfer.pct.pew2.DC4
riskTransfer.pct.pew2.DC4.disc37
riskTransfer.pct.pew2.DC4.disc725




















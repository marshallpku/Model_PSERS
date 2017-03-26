# Fiscal analysis of LAFPP

# Risk measures for LAFPP

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
Outputs_folder  <- "Results/Graphs_fiscal/"


#*****************************************************
##  Loading simulation data  ####
#*****************************************************

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    
    if("results.t7" %in% names(outputs_list)){
      df_out <- bind_rows(outputs_list$results,
                          outputs_list$results.t7,
                          outputs_list$results.xt7)
      return(df_out)
    } else {
      return(outputs_list$results)
    }
  }
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}




results_all <- get_results(IO_folder, "results_sumTiers_RS") %>% select(runname, Tier, sim, year, everything()) %>% 
  filter(run.policyScn != "FR075")
  


# Runs used in the report 
runs_RS <- paste0("RS", 1:5)
runs_policy <- c("noCap", "cap", "cap.allTiers")

runs_RS_labels <- c("Scenario 2: \nAssumption achieved", 
                    "Scenario 3: \n5 years of low returns", 
                    "Scenario 4: \n15 years of low returns",
                    "Scenario 5: \nHigh volatility \nreflecting market forecasts", 
                    "Scenario 6: \nLow expected return \nbased on LAFPP target portfolio")

runs_policy_labels <- c("without ERC cap", 
                        "ERC cap for new hires", 
                        "ERC cap for all tiers")


results_all %<>% mutate(run.policyScn.lab = factor(run.policyScn, levels = runs_policy, labels = runs_policy_labels),
                        run.returnScn.lab = factor(run.returnScn, levels = runs_RS,     labels = runs_RS_labels),
                        run.policyScn = factor(run.policyScn, levels = runs_policy),
                        run.returnScn = factor(run.returnScn, levels = runs_RS)
                        ) 


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


# demo.shape5 <- c(16, 16, 16, 15, 17) # 16-average, 15-mature, 17-immature 


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}



#**********************************************************************************************
##  Load revenue data and extend the projection into 2044  ####
#**********************************************************************************************

# source: City of LA Revenue Outlook, FY 2016-17
df_revenue <- read_ExcelRange("Data_inputs/LAFPP_PlanInfo_2016.xlsx", sheet = "Fiscal")

# extend the projection into 2044 using the projected growth rate of 2.9% in 2020 
rev.growth <- 0.029

df_revenue %<>% 
  mutate(GenFund.proj = 1000 * ifelse(year < 2021, GenFund.original, GenFund.original[year == 2020] * (1 + rev.growth)^(year - 2020))) 

df_revenue


#**********************************************************************************************
##  Fiscal analysis:  ####
#**********************************************************************************************

# Assumption about the non-pension ERC in the LAFPP system (mainly health subsidy):
  # Assume non-pension ERC is 25% of total LAFPP system ERC
share_LAFPP.health <- 0.25

# Assumption about the total contributions of LACERS:
  # Assume total LACERS contribution is 85% of total LAFPP system ERC (pension + health)
factor_LACERS <- 0.85 

df_estimates <- results_all %>% 
  filter(run.returnScn %in% paste0("RS", 1:5),run.policyScn == "noCap") %>%
  select(run.returnScn, sim, year, ERC) %>% 
  left_join(df_revenue) %>% 
  mutate(ERC.LAFPP.pension = ERC,
         ERC.LAFPP = ERC / (1-share_LAFPP.health),
         ERC.LAFPP.health = ERC.LAFPP - ERC.LAFPP.pension, 
         ERC.LACERS = ERC.LAFPP * factor_LACERS,
        #ERC.tot    = ERC.LAFPP + ERC.LACERS, 
        #ERC.LAFPP.pension_GenFund = 100 * ERC.LAFPP.pension / GenFund.proj,
        #ERC.LAFPP_GenFund = 100 * ERC.LAFPP / GenFund.proj,
         ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj) %>% 
        #ERC.tot_GenFund = 100 * ERC.tot / GenFund.proj
  select(-ERC, -ERC.LAFPP, -ERC.LAFPP.pension)


results_fiscal <- 
  results_all %>% 
  left_join(df_estimates) %>% 
  mutate(ERC.LAFPP.pension = ERC,
         ERC.LAFPP = ERC.LAFPP.pension + ERC.LAFPP.health,
         ERC.tot    = ERC.LAFPP + ERC.LACERS, 
         ERC.LAFPP.pension_GenFund = 100 * ERC.LAFPP.pension / GenFund.proj,
         ERC.LAFPP_GenFund = 100 * ERC.LAFPP / GenFund.proj,
         ERC.LACERS_GenFund = 100 * ERC.LACERS / GenFund.proj,
         ERC.tot_GenFund = 100 * ERC.tot / GenFund.proj) %>% 
  select(runname,run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab,
         Tier, sim, year, C, EEC, ERC, GenFund.proj, ERC_PR,
         ERC.LAFPP, ERC.LAFPP.health, ERC.LAFPP.pension,
         ERC.LACERS,
         ERC.tot,
         ERC.LAFPP.pension_GenFund,
         ERC.LAFPP_GenFund,
         ERC.LACERS_GenFund,
         ERC.tot_GenFund) 


fig_projGenFund <- 
results_fiscal %>% filter(runname == "RS1", sim == 0) %>% 
  ggplot(aes(x = year, y = GenFund.proj/1e6)) + 
  geom_bar(stat = "identity", fill = "skyblue2", color = "grey50", width = 0.5) + 
  theme_bw() + 
  RIG.theme() + 
  scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
  scale_y_continuous(breaks = seq(0, 20000, 1000)) + 
  labs(title = "Projected General Fund of the Los Angeles City",
       y = "$Million",
       x = "Year")
fig_projGenFund
  



#**********************************************************************************************
##  Fiscal analysis: Deterministic Run  ####
#**********************************************************************************************
## Deterministic run
 results_fiscal.det <- 
 results_fiscal %>%  filter(sim ==0, Tier == "sumTiers") %>% 
   select(runname, Tier, run.policyScn, run.returnScn, run.policyScn.lab, run.returnScn.lab, sim, year,
          ERC.LAFPP.pension_GenFund, 
          ERC.LAFPP_GenFund,
          ERC.tot_GenFund)


 # LAFPP ERC
fig_det_LAFPP <- 
 results_fiscal.det %>% 
   filter(run.returnScn %in% paste0("RS", c(1, 3, 5)), run.policyScn %in% c("noCap", "cap", "cap.allTiers" )) %>% 
   ggplot(aes(x = year, y = ERC.LAFPP_GenFund, color = run.returnScn.lab))  + 
   facet_grid(.~run.policyScn.lab) + 
   geom_point() + 
   geom_line()+ 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "Return scenarios") + 
   coord_cartesian(ylim = c(0,20)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
   labs(title = "ERC for LAFPP (pension and health) as a percentage of General Fund of LA",
        subtitle = "Deterministic runs",
        x = "Year",
        y = "Percent")
fig_det_LAFPP
 
 # LAFPP + LACERS ERC
fig_det_tot <- 
 results_fiscal.det %>% 
   filter(run.returnScn %in% paste0("RS", c(1, 3, 5)), run.policyScn %in% c("noCap", "cap", "cap.allTiers" )) %>% 
   ggplot(aes(x = year, y = ERC.tot_GenFund, color = run.returnScn.lab))  + 
   facet_grid(.~run.policyScn.lab) + 
   geom_point() + 
   geom_line()+ 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "Return scenarios") + 
   coord_cartesian(ylim = c(0,40)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 3)) + 
   labs(title = "ERC for LAFPP and LACERS (pension and health) as a percentage of General Fund of LA",
        subtitle = "Deterministic runs",
        x = "Year",
        y = "Percent")
fig_det_tot
   
#**********************************************************************************************
## Stochastic runs: policies  ####
#**********************************************************************************************


 ## Stochastic runs

 results_fiscal.stch <- 
   results_fiscal %>%  filter(Tier == "sumTiers", sim >0) %>%
   group_by(run.returnScn, run.policyScn, year) %>% 
   summarise(
          ERC.LAFPP_GenFund.q10   = quantile(ERC.LAFPP_GenFund, 0.1,  na.rm = T),
          ERC.LAFPP_GenFund.q25   = quantile(ERC.LAFPP_GenFund, 0.25, na.rm = T),
          ERC.LAFPP_GenFund.q50   = quantile(ERC.LAFPP_GenFund, 0.50, na.rm = T),
          ERC.LAFPP_GenFund.q75   = quantile(ERC.LAFPP_GenFund, 0.75, na.rm = T),
          ERC.LAFPP_GenFund.q90   = quantile(ERC.LAFPP_GenFund, 0.90, na.rm = T),
          
          ERC.tot_GenFund.q10   = quantile(ERC.tot_GenFund, 0.1,  na.rm = T),
          ERC.tot_GenFund.q25   = quantile(ERC.tot_GenFund, 0.25, na.rm = T),
          ERC.tot_GenFund.q50   = quantile(ERC.tot_GenFund, 0.50, na.rm = T),
          ERC.tot_GenFund.q75   = quantile(ERC.tot_GenFund, 0.75, na.rm = T),
          ERC.tot_GenFund.q90   = quantile(ERC.tot_GenFund, 0.90, na.rm = T))
          
 results_fiscal.stch

# Distribution under current policy and assumption is met
 
 
 # LAFPP ERC  
fig_stch.LAFPP <- 
 results_fiscal.stch %>% 
   filter(run.returnScn %in% paste0("RS", c(1)), run.policyScn %in% c("noCap")) %>% 
   select(run.returnScn, run.policyScn, year, 
          ERC.LAFPP_GenFund.q25, 
          ERC.LAFPP_GenFund.q50, 
          ERC.LAFPP_GenFund.q75) %>% 
   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP_GenFund.q75", "ERC.LAFPP_GenFund.q50", "ERC.LAFPP_GenFund.q25"),
                                labels = c("75th percentile", "50th percentile", "25th percentile"))) %>% 
   ggplot(aes(x = year, y = value , color = qtile))  + 
   geom_point() + 
   geom_line() + 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
   coord_cartesian(ylim = c(0,20)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
   labs(title = "Distribution of ERC for LAFPP (pension and health) \nas a percentage of General Fund of LA",
        subtitle = "Current policy; expected return = 7.5%",
        x = "Year",
        y = "Percent")
fig_stch.LAFPP
 
 # LAFPP + LACERS ERC
fig_stch.tot <- 
 results_fiscal.stch %>% 
   filter(run.returnScn %in% paste0("RS", c(1)), run.policyScn %in% c("noCap")) %>% 
   select(run.returnScn, run.policyScn, year, 
          ERC.tot_GenFund.q25, 
          ERC.tot_GenFund.q50, 
          ERC.tot_GenFund.q75) %>% 
   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
   mutate(qtile = factor(qtile, levels = c("ERC.tot_GenFund.q75", 
                                           "ERC.tot_GenFund.q50", 
                                           "ERC.tot_GenFund.q25"),
                         labels = c("75th percentile", "50th percentile", "25th percentile"))) %>% 
   ggplot(aes(x = year, y = value , color = qtile))  + 
   geom_point() + 
   geom_line() + 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 5)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
   coord_cartesian(ylim = c(0,40)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
   labs(title = "Distribution of ERC for LAFPP and LACERS (pension and health) \nas a percentage of General Fund of LA",
        subtitle = "Current policy; expected return = 7.5%",
        x = "Year",
        y = "Percent")
fig_stch.tot
  
 #**********************************************************************************************
 ## Low returns in early years ####
 #**********************************************************************************************
 labs.lowReturns <- c("Scenario 2: \nAssumption achieved", 
                      "Scenario 3: \n5 years of low returns", 
                      "Scenario 4: \n15 years of low returns")
 
 
 # LAFPP ERC
fig_lowR.LAFPP <- 
 results_fiscal.stch %>% 
   filter(run.returnScn %in% paste0("RS", c(1,2,3)), run.policyScn %in% c("noCap")) %>% 
   select(run.returnScn, run.policyScn, year,
          ERC.LAFPP_GenFund.q25, 
          ERC.LAFPP_GenFund.q50, 
          ERC.LAFPP_GenFund.q75) %>% 
   ungroup %>% 
   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP_GenFund.q75", 
                                           "ERC.LAFPP_GenFund.q50", 
                                           "ERC.LAFPP_GenFund.q25"),
                                labels = c("75th percentile", 
                                           "50th percentile", 
                                           "25th percentile")),
          run.returnScn = factor(run.returnScn, labels = labs.lowReturns)) %>% 
   ggplot(aes(x = year, y = value , color = qtile))  + 
   facet_grid(. ~ run.returnScn) + 
   geom_point() + 
   geom_line() + 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
   coord_cartesian(ylim = c(0,29)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
   labs(title = "Distribution of ERC for LAFPP (pension and health) \nas a percentage of General Fund of LA",
        subtitle = "Current policy; low expected returns in early years",
        x = "Year",
        y = "Percent")
 
 
 # LAFPP + LACERS ERC
fig_lowR.tot <- 
 results_fiscal.stch %>% 
   filter(run.returnScn %in% paste0("RS", c(1,2,3)), run.policyScn %in% c("noCap")) %>% 
   select(run.returnScn, run.policyScn, year,
          ERC.tot_GenFund.q25, 
          ERC.tot_GenFund.q50, 
          ERC.tot_GenFund.q75) %>% 
   ungroup %>% 
   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
   mutate(qtile = factor(qtile, levels = c("ERC.tot_GenFund.q75", 
                                           "ERC.tot_GenFund.q50", 
                                           "ERC.tot_GenFund.q25"),
                         labels = c("75th percentile", 
                                    "50th percentile", 
                                    "25th percentile")),
          run.returnScn = factor(run.returnScn, labels = labs.lowReturns)) %>% 
   ggplot(aes(x = year, y = value , color = qtile))  + 
   facet_grid(. ~ run.returnScn) + 
   geom_point() + 
   geom_line() + 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 5)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
   coord_cartesian(ylim = c(0,49)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
   labs(title = "Distribution of ERC for LAFPP and LACERS (pension and health) \nas a percentage of General Fund of LA",
        subtitle = "Current policy; low expected returns in early years",
        x = "Year",
        y = "Percent")
fig_lowR.tot
 
 #**********************************************************************************************
 ## Alternative risk-return profiles ####
 #**********************************************************************************************
 labs.altAssumptions <- c("Scenario 2: \nAssumption achieved", 
                          "Scenario 5: \nHigh volatility \nreflecting market forecasts", 
                          "Scenario 6: \nLow expected return \nbased on LAFPP target portfolio")
 
 
 # LAFPP ERC

fig_alt.LAFPP <- 
 results_fiscal.stch %>% 
   filter(run.returnScn %in% paste0("RS", c(1,4,5)), run.policyScn %in% c("noCap")) %>% 
   select(run.returnScn, run.policyScn, year,
          ERC.LAFPP_GenFund.q25, 
          ERC.LAFPP_GenFund.q50, 
          ERC.LAFPP_GenFund.q75) %>% 
   ungroup %>% 
   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
   mutate(qtile = factor(qtile, levels = c("ERC.LAFPP_GenFund.q75", 
                                           "ERC.LAFPP_GenFund.q50", 
                                           "ERC.LAFPP_GenFund.q25"),
                         labels = c("75th percentile", 
                                    "50th percentile", 
                                    "25th percentile")),
          run.returnScn = factor(run.returnScn, labels = labs.altAssumptions)) %>% 
   ggplot(aes(x = year, y = value , color = qtile))  + 
   facet_grid(. ~ run.returnScn) + 
   geom_point() + 
   geom_line() + 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 2)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
   coord_cartesian(ylim = c(0,29)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
   labs(title = "Distribution of ERC for LAFPP (pension and health) \nas a percentage of General Fund of LA",
        subtitle = "Current policy; alternative risk-return profiles",
        x = "Year",
        y = "Percent")
fig_alt.LAFPP 
 
 # LAFPP + LACERS ERC
fig_alt.tot <- 
 results_fiscal.stch %>% 
   filter(run.returnScn %in% paste0("RS", c(1,4,5)), run.policyScn %in% c("noCap")) %>% 
   select(run.returnScn, run.policyScn, year,
          ERC.tot_GenFund.q25, 
          ERC.tot_GenFund.q50, 
          ERC.tot_GenFund.q75) %>% 
   ungroup %>% 
   gather(qtile, value, -run.returnScn, -run.policyScn, -year) %>% 
   mutate(qtile = factor(qtile, levels = c("ERC.tot_GenFund.q75", 
                                           "ERC.tot_GenFund.q50", 
                                           "ERC.tot_GenFund.q25"),
                         labels = c("75th percentile", 
                                    "50th percentile", 
                                    "25th percentile")),
          run.returnScn = factor(run.returnScn, labels = labs.altAssumptions)) %>% 
   ggplot(aes(x = year, y = value , color = qtile))  + 
   facet_grid(. ~ run.returnScn) + 
   geom_point() + 
   geom_line() + 
   scale_x_continuous(breaks = c(seq(2015, 2040, 5), 2044)) + 
   scale_y_continuous(breaks = seq(0, 100, 5)) + 
   scale_color_manual(values = c(RIG.red, RIG.green, RIG.blue), name = "") + 
   coord_cartesian(ylim = c(0,49)) + 
   theme_bw() + 
   RIG.theme() +
   guides(color = guide_legend(keywidth = 1.5, keyheight = 2)) + 
   labs(title = "Distribution of ERC for LAFPP and LACERS (pension and health) \nas a percentage of General Fund of LA",
        subtitle = "Current policy; alternative risk-return profiles",
        x = "Year",
        y = "Percent")
 

ggsave(file = paste0(Outputs_folder, "fig_projGenFund.png"), fig_projGenFund, height = 6*0.9, width = 10*0.9)

ggsave(file = paste0(Outputs_folder, "fig_det_LAFPP.png"), fig_det_LAFPP, height = 6*0.8, width = 15*0.8)
ggsave(file = paste0(Outputs_folder, "fig_det_tot.png"), fig_det_tot, height = 6*0.8, width = 15*0.8)

ggsave(file = paste0(Outputs_folder, "fig_stch.LAFPP.png"), fig_stch.LAFPP, height = 7*0.8, width = 10*0.8)
ggsave(file = paste0(Outputs_folder, "fig_stch.tot.png"), fig_stch.tot, height = 7*0.8, width = 10*0.8)

ggsave(file = paste0(Outputs_folder, "fig_lowR.LAFPP.png"), fig_lowR.LAFPP, height = 6*0.8, width = 15*0.8)
ggsave(file = paste0(Outputs_folder, "fig_lowR.tot.png"), fig_lowR.tot, height = 6*0.8, width = 15*0.8)

ggsave(file = paste0(Outputs_folder, "fig_alt.LAFPP.png"), fig_alt.LAFPP, height = 6*0.8, width = 15*0.8)
ggsave(file = paste0(Outputs_folder, "fig_alt.tot.png"), fig_alt.tot, height = 6*0.8, width = 15*0.8)


#**************************************************************************
# For LAFPP ####
#**************************************************************************


df_det <- results_all  %>% 
  left_join(df_revenue) %>% 
  filter(runname == "RS5", sim == 0, year <= 2045) %>% 
  select(year, FR_MA, B, ERC,  ERC_PR, GenFund.proj ) %>%
  mutate(ERC_GenFund = 100 * ERC/GenFund.proj) %>% 
  mutate_at(vars(-year, -FR_MA, -ERC_PR, -ERC_GenFund), funs(./1e6)) %>% 
  select(year, FR_MA, B, ERC, GenFund.proj, ERC_PR, ERC_GenFund) 

df_det


write.csv(df_det, file = paste0(Outputs_folder, "LAFPP.csv"))

df_det %>% filter(year %in% c(2016, seq(2020, 2045, 5)))

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
# library(xlsx)
library("btools")

source("Functions.R")



#*****************************************************
##  Defining paths for inputs and outputs ####
#*****************************************************
IO_folder       <- "Results/"
outputs.folder  <- "Results/Graphs_report/"


#*****************************************************
##  Loading data  ####
#*****************************************************

## Loading and pre-processing simulation outputs
# This section will produce two .RData file under the folder "Analysis_Demo"
# 1. Demo_results_all.RData. Data frame "results_all" that contains results for pension finances. 
# 2. DemoSum_all.RData".     Various data frames that contains demographic data. 

## Outputs of pension finance  
get_results <- function(IO_folder, Pattern = "^Outputs"){
  
  fn <- function(x) {
    load(paste0(IO_folder, "/", x))
    outputs_list$results}
  
  file_select <- dir(IO_folder, Pattern)
  results_all <- adply(file_select, 1, fn) %>% select(-X1)
}


results_all <- get_results(IO_folder, "results_sumTiers_RS") 
# save(results_all, file = paste0(IO_folder, "/Analysis_Demo/Demo_results_all.RData"))



# results_all[1:80,] %>% select(runname, sim, year, MA, C, B)
# results_all[1:80,] %>% select(runname, sim, year, MA, C, B)
# 
# 
# i <- 0.075
# 
# B.model <- results_all %>% filter(runname == "RS1", sim == -1, year %in% 2015:2023) %>% select(year, B)
# 
# B.model$B
# 
# 1369*2.5*6000*12
# 
# B.GASB <- data.frame(year = 2015:2023,
#                      B    = 1e6*c(970,
#                               1104,
#                               1050,
#                               1149,
#                               1267,
#                               1212,
#                               1283,
#                               1350,
#                               1416))
# 
# 
# # restriction 1: PVFB for 2015-2023
# R1.PVFB <- sum(B.model$B / (1 + i)^(2015:2023 - 2015))
# 
# # restriction 2: Schedule of payments from GASB projection
# R2.GASB_scale <- B.GASB$B/B.GASB$B[1]
# 
# # Adjustment factor
# adj.factor <- R1.PVFB/sum(R2.GASB_scale / (1 + i)^(2015:2023 - 2015))
# 
# # Adjusted Benefits
# B.adj <-  data.frame(year = 2015:2023, 
#                      B.adj1 = R2.GASB_scale* adj.factor)
# 
# # Extra benefits: approximate DROP balance accumulated before 2015
# B.extra <- 1369*3*6132*12
# 
# B.adj %<>% mutate(B.extra = ifelse(year - 2015 < 5, B.extra/5, 0),
#                   B.adj2  = B.adj1 + B.extra) 
# 
# 
# R1.PVFB
# sum(B.adj$B.adj1 / (1 + i)^(2015:2023 - 2015))
# sum(B.adj$B.adj2 / (1 + i)^(2015:2023 - 2015))
# 
# B.adj







## Loading existing data. 

#load("Results/results_sumTiers_RS1.RData")


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
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"))
}


#*****************************************************
##  Selecting runs and calculating risk measures ####
#*****************************************************

runs_all <- paste0("RS", 1:5)

runs_all_labels <- c("Assumption Achieved",
                          "5 years of low returns",
                          "15 years of low returns",
                          "Callan",
                          "RVK")


df_all.stch <- results_all  %>% 
  filter(runname %in% runs_all, sim >= 0, year <= 2044)


df_all.stch %<>%   
  select(runname, sim, year, AL, MA, ERC_PR) %>% 
  group_by(runname, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         FR40less  = cumany(FR_MA <= 40),
         FR100more  = cumany(FR_MA >= 100),
         FR100more2 = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 50), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
            ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1),
            FR.q25   = quantile(FR_MA, 0.25),
            FR.q50   = quantile(FR_MA, 0.5),
            FR.q75   = quantile(FR_MA, 0.75),
            FR.q90   = quantile(FR_MA, 0.9),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1),
            ERC_PR.q25 = quantile(ERC_PR, 0.25),
            ERC_PR.q50 = quantile(ERC_PR, 0.5),
            ERC_PR.q75 = quantile(ERC_PR, 0.75),
            ERC_PR.q90 = quantile(ERC_PR, 0.9)
  ) %>% 
  ungroup() %>%
  mutate(runname = factor(runname, 
                          levels = runs_all, 
                          labels = runs_all_labels))




#*****************************************************
## Exploratory graphs  ####
#*****************************************************

g.FR40less <- 
  df_all.stch %>% 
  ggplot(aes(x = year, y = FR40less, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR40less


g.ERCsharpRise <- 
  df_all.stch %>% 
  ggplot(aes(x = year, y = ERC_hike, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.ERCsharpRise


g.ERChigh <- 
  df_all.stch %>%  
  ggplot(aes(x = year, y = ERC_high, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.ERChigh


g.FR.pctmore <-
  df_all.stch %>% 
  ggplot(aes(x = year, y = FR100more, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.pctmore

g.FR.pctmore2 <-
  df_all.stch %>% 
  ggplot(aes(x = year, y = FR100more2, color = runname)) + theme_bw() + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.pctmore2


g.FR.qts <- 
  df_all.stch %>% 
  select(runname, year, starts_with("FR.q")) %>% 
  gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 100, linetype = 2) + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.qts


g.FR.qts2 <- 
  df_all.stch %>% 
  select(runname, year, starts_with("FR.q")) %>% 
  gather(var, value, -runname, -year) %>% 
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 100, linetype = 2) + 
  coord_cartesian(ylim = c(0,200)) + 
  scale_x_continuous(breaks = seq(2015, 2100, 5))
g.FR.qts2


g.ERC_PR.qts <- 
  df_all.stch %>% 
  select(runname, year, starts_with("ERC_PR.q")) %>% 
  gather(var, value, -runname, -year) %>%
  ggplot(aes(x = year, y = value, color = var)) + theme_bw() + facet_grid(.~runname) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2015, 2100, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 10))
g.ERC_PR.qts



ggsave(g.FR40less,   file = paste0(outputs.folder, "g.FR40less.png"), width  = 10, height = 5)
ggsave(g.ERCsharpRise, file = paste0(outputs.folder, "g.ERCsharpRise.png"), width  = 10, height = 5)
ggsave(g.ERChigh,      file = paste0(outputs.folder, "g.ERChigh.png"), width  = 10, height = 5)
ggsave(g.FR.pctmore,   file = paste0(outputs.folder, "g.FR.pctmore.png"), width  = 10, height = 5)
ggsave(g.FR.pctmore2,   file = paste0(outputs.folder, "g.FR.pctmore2.png"), width  = 10, height = 5)

ggsave(g.FR.qts,     file = paste0(outputs.folder, "g.FR.qts.png"), width  = 15, height = 5)
ggsave(g.FR.qts2,     file = paste0(outputs.folder, "g.FR.qts2.png"), width  = 15, height = 5)
ggsave(g.ERC_PR.qts, file = paste0(outputs.folder, "g.ERC_PR.qts.png"), width  = 15, height = 5)





#*****************************************************
## Assumption achieved  ####
#*****************************************************






#*****************************************************
## Low returns in early years  ####
#*****************************************************




#********************************************************************
## Return scenarios based on capital market assumptions          ####
#********************************************************************










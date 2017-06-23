
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
##  Goals     ####
#*****************************************************

# 1. Total DC contribution rate that can make the hybrid benefit approx. equal to the pure DB benefit for 
#    a employee who enters the workforce at age 30 and retires at age 65. (benefit factor NOT calibrated.)


#*****************************************************
##  Loading Data        ####
#*****************************************************
load("Results/BenefitRisk/list.benRisk.R725.RData")



#*****************************************************
##  Benefit under pure DB plan         ####
#*****************************************************

# SR1EL1.Reform_sep_R725.d725.DC4.obf

i <- list.benRisk.R725$i.r[1,1]
i.reduction <- 0.015

df_BenRisk <- 
list.benRisk.R725$liab.tNE$active %>% 
  mutate(start.year = year - (age - ea)) %>% 
  filter(ea == 30, start.year == 2017) %>% 
  select(ea, age, year, sx, DB.value, Bx.laca) %>% 
  mutate(CumSalwInt = lag(get_cumAsset(sx, i - i.reduction, year_end = TRUE)),
         DC_rate = DB.value / CumSalwInt,
         DC_cont.72 = sx * 0.072,
         DC_cont.5 = sx * 0.05,
         DC_cont.x = sx * 0.1,
         DC_balance.72 = CumSalwInt * 0.072,
         DC_balance.5 = CumSalwInt * 0.05
         ) 

df_BenRisk %>% select(ea, age, year,DB.value, DC_balance.72)

DB.value <- with(df_BenRisk, df_BenRisk[age==65, "DB.value"]) %>% as.numeric
DB.value
#*****************************************************
##  Benefit       ####
#*****************************************************

# Contribution 
DC_cont.72 <- df_BenRisk$DC_cont.72
DC_cont.5 <- df_BenRisk$DC_cont.5
DC_cont.x <- df_BenRisk$DC_cont.x

DC_sx <- df_BenRisk$sx


list.return.R725 <- data.frame(i.r[-1,-(1:2)] - i.reduction) %>% unclass()

set.seed(1234)
list.return.R625 <- data.frame(matrix(rnorm(39*2000, 0.0697, 0.12), 39, 2000) - i.reduction) %>% unclass()

list.return.R725 %>% head


ben.DC72.R725 <- mapply(get_cumAsset, cf = list(DC_cont.72), i = list.return.R725,    MoreArgs = list(year_end = TRUE, constant_i = FALSE)) %>% data.frame %>% mutate(year = 2016 + row_number() + 1)
ben.DC5.R725  <- mapply(get_cumAsset,  cf = list(DC_cont.5),  i = list.return.R725,   MoreArgs = list(year_end = TRUE, constant_i = FALSE)) %>% data.frame %>% mutate(year = 2016 + row_number() + 1)
ben.DC72.R625 <- mapply(get_cumAsset, cf = list(DC_cont.72), i = list.return.R625,    MoreArgs = list(year_end = TRUE, constant_i = FALSE)) %>% data.frame %>% mutate(year = 2016 + row_number() + 1)
ben.DC5.R625  <- mapply(get_cumAsset,  cf = list(DC_cont.5),  i = list.return.R625,   MoreArgs = list(year_end = TRUE, constant_i = FALSE)) %>% data.frame %>% mutate(year = 2016 + row_number() + 1)

vec.ben.DC72.R725 <- ben.DC72.R725[ben.DC72.R725$year == 2052, ] %>% select(-year) %>% as.matrix() %>% as.vector()
vec.ben.DC5.R725  <- ben.DC5.R725[ben.DC72.R725$year  == 2052, ] %>% select(-year) %>% as.matrix() %>% as.vector()
vec.ben.DC72.R625 <- ben.DC72.R625[ben.DC72.R625$year == 2052, ] %>% select(-year) %>% as.matrix() %>% as.vector()
vec.ben.DC5.R625  <- ben.DC5.R625[ben.DC72.R625$year  == 2052, ] %>% select(-year) %>% as.matrix() %>% as.vector()


df_benDist <- data.frame(DC72.R725 = vec.ben.DC72.R725, 
                         DC5.R725  = vec.ben.DC5.R725,
                         
                         DC72.R625 = vec.ben.DC72.R625, 
                         DC5.R625  = vec.ben.DC5.R625)

df_benDist.pctiles <- 
df_benDist %>% summarise(
      DC72.R725.pct25 = quantile(DC72.R725, 0.25),
      DC72.R725.pct50 = quantile(DC72.R725, 0.50),
      DC72.R725.pct75 = quantile(DC72.R725, 0.75),
      
      DC5.R725.pct25 = quantile(DC5.R725, 0.25),
      DC5.R725.pct50 = quantile(DC5.R725, 0.50),
      DC5.R725.pct75 = quantile(DC5.R725, 0.75),
      
      DC72.R625.pct25 = quantile(DC72.R625, 0.25),
      DC72.R625.pct50 = quantile(DC72.R625, 0.50),
      DC72.R625.pct75 = quantile(DC72.R625, 0.75),
      
      DC5.R625.pct25 = quantile(DC5.R625, 0.25),
      DC5.R625.pct50 = quantile(DC5.R625, 0.50),
      DC5.R625.pct75 = quantile(DC5.R625, 0.75)
    ) %>% 
      t %>% data.frame

df_benDist.pctiles %<>% rename_("qtiles" = ".") %>% 
  mutate(DB.pct = 100 * qtiles/DB.value)
  
df_benDist.pctiles


# Find a total DC rate that can ensure the 25th/10th percentile of hybrid benefit is approx. equal to the pure DB benefit.  

# 7.25% return
 # 25th percentile: x = 9.25% 
 # 10th percenitle: x = 12%   (101.56% of DB)

# 6.25% return
 # 25th percentile: x = 11% 
 # 10th percenitle: x = 14%   (100.52% of DB)

DC_rate.x <- 0.11

df_BenRisk <- 
  list.benRisk.R725$liab.tNE$active %>% 
  mutate(start.year = year - (age - ea)) %>% 
  filter(ea == 30, start.year == 2017) %>% 
  select(ea, age, year, sx, DB.value, Bx.laca) %>% 
  mutate(CumSalwInt = lag(get_cumAsset(sx, i - i.reduction, year_end = TRUE)),
         DC_rate = DB.value / CumSalwInt,
         DC_cont.72 = sx * 0.072,
         DC_cont.5 = sx * 0.05,
         DC_cont.x = sx * DC_rate.x,
         DC_balance.72 = CumSalwInt * 0.072,
         DC_balance.5 = CumSalwInt * 0.05
  ) 

# Contribution 
DC_cont.x <- df_BenRisk$DC_cont.x

list.return.x <- list.return.R625

ben.DCx <- mapply(get_cumAsset, cf = list(DC_cont.x), i = list.return.x,    MoreArgs = list(year_end = TRUE, constant_i = FALSE)) %>% data.frame %>% mutate(year = 2016 + row_number() + 1)
vec.ben.DCx <- ben.DCx[ben.DCx$year == 2052, ] %>% select(-year) %>% as.matrix() %>% as.vector()

df_benDist.x <- data.frame(DCx = vec.ben.DCx)

df_benDist.x.pctiles <- 
  df_benDist.x %>% summarise(
    DCx.pct10 = quantile(DCx, 0.10),
    DCx.pct25 = quantile(DCx, 0.25),
    DCx.pct50 = quantile(DCx, 0.50),
    DCx.pct75 = quantile(DCx, 0.75)) %>%  
  t %>% data.frame

df_benDist.x.pctiles %<>% rename_("qtiles" = ".") %>% 
  mutate(DB.pct = 100 * qtiles/DB.value)

df_benDist.x.pctiles





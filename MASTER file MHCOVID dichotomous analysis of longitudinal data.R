#-------------------------------------------------------------------------------
# File  : MASTER file MHCOVID dichotomous analysis of longitudinal data.R
#
# File for the analysis of the data from the MHCOVID project, only dichotomous outcomes
# and only longitudinal data
# Written by Georgia Salanti 
#  
#-------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

#Get functions
source("util.R")

### libraries

library(tidyr)
library(tibble)
library(stringr)
library(readxl)
library(grid)
library(dplyr)
library(netmeta)
library(Matrix)
library(knitr)
library(kableExtra)
library(rms) 
library(meta)
library(devtools)
library(dosresmeta)
library(tools)
library(metafor)
library(rje)
library(gtools)

#obtain the data from two waves (before and after march 2021) and prepare them for analysis. 
source("clean_dichotomous_data.R")

#Do meta-analyses and meta-regressions and dose-response meta-analyses 
source("run_analysis longitudinal.R")



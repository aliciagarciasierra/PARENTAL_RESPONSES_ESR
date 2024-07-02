

####################################################################
### Parental responses to children’s early health disadvantages: ###
######## evidence from a British twin study ########################
####################################################################

# Author: Alicia García-Sierra 

# Journal: European Sociological Review (2024)

# DOI: https://doi.org/10.1093/esr/jcae016

# Data: TEDS dataset 
  # TEDS data is available upon request and approval 
  # from the pertinent organism. 
  # The TEDS data access policy can be found here:
  # https://www.teds.ac.uk/researchers/teds-data-access-policy.

#######################################################
#########   PREPARE THE ENVIRONMENT ################
######################################################

# CLEAN

rm(list=ls()) 

# LOAD PACKAGES

library(haven)
library(dplyr)
library("irr")
library(ggplot2)
library(hexbin)
library(fixest)
library(plm)
library(lmtest)
library(stargazer)
library(sjPlot)
library(ggiplot)


# SET WD

setwd("/Users/agarcias/Library/CloudStorage/OneDrive-UniversitédeLausanne/DPHIL/FIRST PAPER/TEDS/REPLICATION")

# OPEN TEDS DATA

data <- read_sav("529 Alicia Garcia Sierra early health and parenting Oct2021.sav")

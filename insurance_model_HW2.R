

library(haven) #necessary for import of SAS
library(tidyverse) # necessary for life in R
# library(brglm)

rm(list=ls()) # for cleaning global environment, to guarantee a clean slate

sp_path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Logistic\\logistic-insurance\\LogisticsHW1.RData"
gf_path <- "C:\\Users\\Grant\\Downloads\\MSA2019LogisticData\\data"
wj_path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project\\"
gw_path <- "C:\\Users\\gavin\\Desktop\\Logisitic_Regression_Data\\"
mr_path <- "C:\\Users\\molly\\"

load(sp_path)

############################################
#################  Results  ################
############################################
#By S.Powell
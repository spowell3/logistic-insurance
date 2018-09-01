rm(list=ls()) #cleaning the environment for a guaranteed fresh start

library(dplyr)
library(ggplot2)

# import the file
# PLEASE just add a new variable for your file path
sp_path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\data\\Logistic Data"
gf_path <- "C:\\Users\\Grant\\Downloads\\MSA2019LogisticData\\data"
#ADD HERE
setwd(file.path(sp_path)) #CHANGE INITIALS
getwd()
file <- paste(sp_path, "\\insurance_t.sas7bdat",sep='')
insurance_t <- read_sas(file)

insurance_spvar <- select(insurance_t, INCOME, HMOWN, LORES, HMVAL, AGE, CRSCORE, MOVED, INAREA, RES, INS)
head(insurance_spvar)
summary(insurance_spvar)
lapply(insurance_spvar, sd, na.rm=TRUE)

# R doesn't have it's own mode function which is wierd
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# univariate summary
univariate_summary <- function(data){
  num <- length(data)-sum(is.na(data))
  missing <- sum(is.na(data))
  mean <- mean(data, na.rm=TRUE)
  max <- max(data, na.rm=TRUE)
  mode <- getmode(data[is.na(data)==FALSE])
  std <- sd(data, na.rm=TRUE)
  iqr <- IQR(data, na.rm=TRUE)
  l1 <- list(n_obs=num, missing=missing, mean=mean, max=max, mode=mode, std=std, iqr=iqr)
  
}

sapply(insurance_spvar, univariate_summary) # when this print, can copy into Excel and use text to columns to format


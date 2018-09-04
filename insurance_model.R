
# install.packages("brglm") #necessary for bias reduction, penalized likelihood
library(haven) #necessary for import of SAS
library(tidyverse) # necessary for life in R
# library(brglm)

rm(list=ls()) # for cleaning global environment, to guarantee a clean slate

# import the file
# PLEASE just add a new variable for your file path
sp_path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\data\\Logistic Data"
gf_path <- "C:\\Users\\Grant\\Downloads\\MSA2019LogisticData\\data"
setwd(file.path(sp_path))
getwd()
file <- paste(sp_path, "\\insurance_t.sas7bdat",sep='')
insurance_t <- read_sas(file)
head(insurance_t)
str(insurance_t)

# TODO set alpha, or ensure we know what alpha we're using

#quick fit of Powell variables
fit_sp <- glm(INS ~ 
                INCOME + HMOWN + LORES + HMVAL + AGE + CRSCORE + MOVED + INAREA + RES,
              data = insurance_t, family = binomial(link = "logit"))
summary(fit_sp)

# RECOMMENDED VARS
#WJ 1-9 recommended vars : DDA + DDABAL
#MR 10-18 recommended vars : NSFAMT	+ PHONE	+ TELLER +	SAV +	SAVBAL + ATMAMT + POSAMT
#GF 19-27 recommended vars : CD + INV + IRA + CDBAL + IRABAL
#GW 28-38 recommended vars : ILSBAL + MM + MTG + CC + SDB
#SP 38-47 almost all vars : INCOME + HMOWN + LORES + HMVAL + AGE + CRSCORE + MOVED + INAREA + INS + BRANCH + RES
insurance.sel <- select(insurance_t,
                             DDA, DDABAL, 
                             NSFAMT, PHONE, TELLER,	SAV,	SAVBAL, ATMAMT, POSAMT,
                             CD, INV, IRA, CDBAL, IRABAL,
                             ILSBAL, MM, MTG, CC, SDB, 
                             INCOME, HMVAL, INAREA,
                           INS)
insurance.sel2 <- select(insurance_t,
                            DDA, DDABAL, 
                             PHONE, TELLER,	SAV,	SAVBAL, ATMAMT, 
                             CD, INV, IRA, CDBAL, 
                             MM, MTG, CC,
                            INS)

#Filtering into two groups to possible identify source of missing
insurance_t.missing <- insurance_t %>%
  mutate(missing_group = is.na(PHONE)) %>%
  filter(missing_group==TRUE)

insurance_t.complete <- insurance_t %>%
  mutate(missing_group = is.na(PHONE)) %>%
  filter(missing_group==FALSE)

#comparing summaries of individual variables (large differences would indicate source of missing)
summary(insurance_t.complete$ACCTAGE) - summary(insurance_t.missing$ACCTAGE)
summary(insurance_t.complete$HMVAL) - summary(insurance_t.missing$HMVAL)
summary(insurance_t.complete$LORES) - summary(insurance_t.missing$LORES)
summary(insurance_t.complete$INAREA) - summary(insurance_t.missing$INAREA)
summary(insurance_t.complete$MOVED) - summary(insurance_t.missing$MOVED)
summary(insurance_t.complete$AGE) - summary(insurance_t.missing$AGE)
table(insurance_t.complete$BRANCH)
table(insurance_t.missing$BRANCH) #BRANCH matches the problem

#missing value summary worthy of finalization
insurance.branch <- insurance_t %>%
  group_by(BRANCH) %>%
  summarize(Count=n(),
            PHONE.mean=mean(PHONE, na.rm=TRUE), 
            PHONE.sum=sum(PHONE, na.rm=TRUE), 
            PHONE.NA=sum(is.na(PHONE)),
            CC.NA=sum(is.na(CC)),
            CC.TRUE=sum(INV==1),
            CC.FALSE=sum(INV==0),
            INV.NA=sum(is.na(INV)),
            INV.TRUE=sum(INV==1),
            INV.FALSE=sum(INV==0)
            )
View(insurance.branch)

# Cleaning datasets for easier use and comparison
insurance.na.omit <- na.omit(insurance_t) #if we want datasets without 'missingness'
insurance.sel.na.omit <- na.omit(insurance.sel) #if we want datasets without 'missingness'

# ITERATION 1
#Building model from all variables that were recommended from data-exploration
fit1 <- glm(INS ~ DDA + DDABAL + 
              NSFAMT	+ PHONE	+ TELLER +	SAV +	SAVBAL + ATMAMT + POSAMT +
              CD + INV + IRA + CDBAL + IRABAL+
              ILSBAL + MM + MTG + CC + SDB + 
            INCOME + HMVAL + INAREA,
            data = insurance.sel, family = binomial(link = "logit"))
summary(fit1)

#Iteration 2: manual reduction of variables
fit2 <- glm(INS ~ DDA + DDABAL + 
              PHONE	+ TELLER +	SAV +	SAVBAL + ATMAMT + 
              CD + INV + IRA + CDBAL + 
              MM + MTG + CC, 
            data = insurance.sel, family = binomial(link = "logit"))
summary(fit2)


# ANOVA of all models generated
#anova(fit2, test = "LRT")

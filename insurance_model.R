
install.packages("brglm") #necessary for bias reduction, penalized likelihood
library(haven) #necessary for import of SAS
library(tidyverse) # necessary for life in R
library(brglm)

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
#View(insurance_t)
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


#Iteration 3: step-wise from all_recommended
#will get warnings 'fitted probabilities numerically 0 or 1 occurred' (linearly separated?) 
fit3 <- glm(INS ~ DDA + DDABAL + 
              NSFAMT	+ PHONE	+ TELLER +	SAV +	SAVBAL + ATMAMT + POSAMT +
              CD + INV + IRA + CDBAL + IRABAL+
              ILSBAL + MM + MTG + CC + SDB + 
              INCOME + HMVAL + INAREA, 
            data = insurance.sel.na.omit, family = binomial(link = "logit"))
model.null = glm(INS ~ 1, 
                 data=insurance.sel.na.omit,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=fit3),
     direction="both",
     test="Chisq",
     data=Data)

summary(fit3)

#Iteration 4:  brglm() instead of glm() but still has same warnings
fit4 <- brglm(INS ~ DDA + DDABAL + 
              NSFAMT	+ PHONE	+ TELLER +	SAV +	SAVBAL + ATMAMT + POSAMT +
              CD + INV + IRA + CDBAL + IRABAL+
              ILSBAL + MM + MTG + CC + SDB + 
              INCOME + HMVAL + INAREA, 
            data = insurance.sel.na.omit, family = binomial(link = "logit"))
model.null = glm(INS ~ 1, 
                 data=insurance.sel.na.omit,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=fit4),
     direction="both",
     test="Chisq",
     data=insurance.sel.na.omit)

summary(fit3)

# ANOVA of all models generated
anova(fit2, test = "LRT")
anova(fit3, fit4, test = "LRT")

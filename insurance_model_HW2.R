
library(haven) #necessary for import of SAS
library(Hmisc) # for rcorr.cens()
library(ROCR) # necessary for performance() func
library(tidyverse) # necessary for life in R
library(DescTools)
library(visreg) #necessary for visreg()
library(car) #necessary for dfbetasPlots func

#install.packages("Hmisc")
# library(brglm)

rm(list=ls()) # for cleaning global environment, to guarantee a clean slate

path <- getwd()

# IF YOU ARE USING R STUDIO, SKIP THESE AND USE THE AUTOMATED HOTNESS BELOW
# If you aren't using R Studio, chose your path wisely...
# path <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Logistic\\logistic-insurance\\"
# path <- "C:\\Users\\Grant\\Downloads\\MSA2019LogisticData\\data\\"
# path <- "C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Logistic Regression\\Final Project\\"
#path <- "C:\\Users\\gavin\\Desktop\\Logisitic_Regression_Data\\"
# path <- "C:\\Users\\molly\\folderino7000\\"

path <- dirname(rstudioapi::getActiveDocumentContext()$path) #AUTOMATED HOTNESS
setwd(path)
load("LogisticsHW1.RData")

############################################
############   DIAGNOSTICS  ################
############################################
# By B.Jenista
summary(fit2)   ## AIC 8331.9

insurance.sel <- select(insurance_t,
                        DDA, DDABAL, 
                        NSFAMT, PHONE, TELLER,	SAV,	SAVBAL, ATMAMT, POSAMT,
                        CD, INV, IRA, CDBAL, IRABAL,
                        ILSBAL, MM, MTG, CC, SDB, 
                        INCOME, HMVAL, INAREA,
                        INS)

## testing various interaction terms
fit3 <- glm(INS ~ DDA + DDABAL + 
              PHONE	+ TELLER + DDA*(SAV + SAVBAL) + ATMAMT + 
              CD + INV + CD*IRA + CDBAL + 
              CD*MM + MTG + CC, 
            data = insurance.sel, family = binomial(link = "logit"))
summary(fit3)    ## AIC 8297.2  --> performs marginally better

## Note: from the previous homework, the fit1 model had an AIC of 6712.2 (which is significantly better than either model here)
## What level of effort do we want to go into to find the best model?
## End Bill's code

#######################################################################
######### Partial Residuals Plots for Continuous Variables ############


####### Gavin Code ############

visreg(fit2, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot of DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit2, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot of SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

###### This plot is showing that SAVBAL may be be linear #######

visreg(fit2, "CDBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot of CDBAL",
       x = "CDBAL", y = "partial (deviance) residuals")

visreg(fit2, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot of ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

####### This plot is showing that ATMAMT may not be linear #########

visreg(fit2, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot of TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit2, "PHONE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "Partial Residual Plot of PHONE",
       x = "PHONE", y = "partial (deviance) residuals")

# Alternative Plot Method that is more appropriate for reports
# Mainly wanted Coloring of 1s and 0s, beter axes and better labels

options(scipen=10000) # Sets higher penatly for scientific notation on plots. Only need to do this once before plotting.

visreg(fit2, "DDABAL", gg = TRUE) +
  geom_point(aes(color=as.factor(fit2$y))) +
  geom_smooth(col = "red", fill = "red") +
  labs(title = "Partial Residual Plot of DDABAL",
       x = "Checking Account Balance, DDABAL ($)", y = "Partial (Deviance) Residuals",
       color="Product \nPurchased\n(INS)")

options(scipen=0) #resetting scipen option. Can do this at the end of all plots with scientific notation 

####################################
########## DF Beats Code ###########

influence.measures(fit2) # What was found from this? Did we plot it?

dfbetasPlots(fit2, id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))
# anything noteworthy 

######################################################
########## Getting Separation and Cooks D ############
library("brglm")
qplot(separation.detection(fit2))

### Based on the function there is no separation between our data #####


### plot Cook's distance
plot(fit2, 4, n.id = 5) 

#### the top most influential points from our model are 1547, 1721, 406

PseudoR2(fit2, which = c("Cox", "Nagelkerke", "McFadden"))
PseudoR2(fit3, which = c("Cox", "Nagelkerke", "McFadden"))

AIC(fit2, fit3)
BIC(fit2, fit3)

summary(fit2)

### Even thought fit 3 has a lower AIC and BIC, it was not drastically better than fit 2.  Including the interaction did not imporve the AIC that much, so to keep simplicity
### I would say that we continue to run with fit 2.

######### End Gavin Code #############

####### DECISION OF THE FINAL MODEL #########

final.model <- fit2

############################################
############   ASSESSMENT  #################
############################################
#By S.Powell

# SCORING
scores <- predict(final.model, newdata = insurance_v, type = "response") 
summary(scores)

#Brier score
insurance_scores <- cbind(scores, insurance_v)
brier_score1 <- mean((insurance_scores$INS - insurance_scores$scores)^2, na.rm=TRUE)
brier_score1

# COEFFICIENT OF DISCRIMINATION
D <- mean(scores[insurance_v$INS == 1], na.rm = TRUE) - mean(scores[insurance_v$INS == 0], na.rm = TRUE)
D <- round(D, 5) #rounding for plot labelling later
D

# PLOT OF DISCRIMINATION
df <- data.frame(INS = insurance_v$INS,
                 phat = scores)
ggplot(df, aes(phat, fill = factor(INS))) +
  geom_density(alpha = 0.2) +
  labs(title="Density Plot of Predicted Probabilities",
       subtitle=paste("Coef. of Discrimination = ",D,sep=""),
       x = "Predicted Probability",
       y= "Density",
       fill = "Product \nPurchased\n(INS)") +
  theme_minimal()
# Not great discrimination, could be better, obv

# c-STATISTIC
plot(scores, insurance_v$INS)
c.stat <- rcorr.cens(scores, insurance_v$INS)[1]
c.stat #0.761637

# BRIER SCORE
### Brier score function ###
brier_score <- function(obj, new_x = NULL, new_y = NULL){
  # computes [scaled] brier score
  #
  # inputs:
  # 1. obj: either a model from glm() or a data frame.
  #         the data frame must have a vector responses "y" and a vector of
  #         either probabilities "p" or linear predictor "lp".
  # 2. new_x: specify new dataset to get predicted probabilities for new obs.
  #             if NULL, the estimated probabilities from original obs will
  #             be used.
  # 3. new_y: use new responses. if NULL, original ones will be used.
  #
  # output:
  #   brier score, scaled brier score
  
  if(is.null(new_y)){
    y <- obj$y
  } else {
    y <- new_y
  }
  
  p_obs <- mean(y)
  
  if(any(class(obj) == "glm")){
    if(is.null(new_x)){
      p <- predict(obj, newdata = new_x, type = "response")
      lp <- predict(obj, newdata = new_x, type = "link")
    } else {
      lp <- obj$linear
      p <- fitted(obj)
    }
  } else if(is.null(obj$p)) {
    lp <- obj$lp
    p <- fitted(obj)
  } else {
    p <- obj$p
    lp <- obj$linear
  }
  
  # brier score
  brier_score <- mean((y - p)^2)
  
  # max brier score is just the observed proportion
  brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  
  # scaled brier score
  # ranges from 0 to 1---lower is better
  brier_scaled <- brier_score/brier_max
  # essentially, 1 - brier_scaled is the %improvement over null model
  
  res <- data.frame(brier_score = brier_score,
                    brier_max = brier_max,
                    brier_scaled = brier_scaled)
  res
}

brier_score_garbage <- brier_score(final.model, new_x=insurance_v, new_y = insurance_v$INS)
brier_score_garbage #Do not trust this number. it says that we did worse than randome guessing,
brier_score1 #is more reliable

### ROC CURVES ###
# actual outcomes must be a factor (b/c considered classification?)
pred <- prediction(scores, factor(insurance_v$INS))

# then in performance, "measure" is the y-axis, and "x.measure" is the x-axis
# for a roc curve, we want tpr vs. fpr. ("sens" and "spec" also work)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Plot of ROC and 45-degree line to reference random guessing
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)

# AUC
auc <- performance(pred, measure = "auc")@y.values
auc
c.stat # Equals c.stat. YAY!
help(performance)

### classification table ###
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]],
                            fpr = perf@x.values[[1]])
# TODO I want to simplify this from something of 2,000 values to something of 50-100 values for the report. But whatever

#colnames(classif_table) <- c("Threshold", "True Positive Rate", "True Negative Rate", "False Positive Rate")

# YOUDEN'S INDEX: 
# added hypothetical weights for tpr (sens) and tnr (spec), but not req'd in HW
false.neg.cost <- (-20) # cost of false neg: $25 of potential profit not realized 
false.pos.cost <- (-5)  # cost of false pos: $5 of marketing cost wasted
wt <- 0.8 # Need to confirm math of wt...
# TODO Check with Matt how hypothetical marketing.cost and ins.profit might affect wt calc
classif_table$youdenJ <- with(classif_table, (2*(wt*tpr + (1-wt)*tnr) - 1))

# find row with max
classif_table[which.max(classif_table$youdenJ),]
max.tpr <- classif_table[which.max(classif_table$youdenJ),]$tpr
max.tnr <- classif_table[which.max(classif_table$youdenJ),]$tnr
with(classif_table, plot(threshold, youdenJ)) 

# MEDIOCRE ROC PLOT
# plot(perf, colorize = TRUE)
# abline(a = 0, b = 1, lty = 2)
# points((1-max.tnr), max.tpr, type = "p", cex=1.5, col = "red", lwd = 2)
# title("ROC Curve of Validation Data")
# legend("bottomright", inset = 0.03, type(), col = c("grey","red"),
#        legend = c("Random guessing", "Ideal with assumed costs"),
#        lty = c(2,NA), pch= c(NA, 1), lwd = c(1,2))

# FINAL ROC PLOT w/ ideal threshold
ggplot(classif_table) +
  geom_line(aes(x=fpr, y=tpr, color=threshold), size=2) +
  scale_color_gradient(low="green", high="red") +
  labs(x="False Positive Rate", y= "False Negative Rate", color="alpha",
       title="ROC Curve of Validation Data") +
  geom_point(x= (1-max.tnr), y= max.tpr, size=3, shape=17)+
  geom_text(x= .03+(1-max.tnr), y= max.tpr, hjust=0, vjust=1, label="Ideal threshold for \nassumed costs")+
  theme_minimal()


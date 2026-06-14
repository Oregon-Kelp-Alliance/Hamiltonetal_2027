library(tidyverse)
library(glmtoolbox)
library(glmmTMB)
library(MuMIn)
#library(lme4)
library(performance)
library(tweedie)
library(knitr)
library(kableExtra)
###########################################################################################################
#Load Data
#Updated March 20 2026 to check that we are drawing most up to date data.
#####################################################################################################
#load environmental variables
temp <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithTempMetrics_20132023.csv")
sal <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithSalinityMetrics_20132018.csv")
no3 <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithNO3Metrics_20132023.csv")
wave <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithWaveMetrics_20132020.csv")
kd <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithKd490Metrics_20132023.csv")
#load biotic variables
bio <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSite_WithBiologicalMetrics_2023_updated.csv")
bio <- bio[,-1]
kw <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithKWKelpChangeMetrics_201013to202023_updated.csv")
#put it all into a single dataframe
sites <- merge(temp, sal)
sites <- merge(sites, no3)
sites <- merge(sites, wave)
sites <- merge(sites, kd)
sites <- merge(sites, bio)
#Cape lookout was surveyed in 2022 not 2023 and can't be included in this analysis. 
sites <- sites %>% filter(Site != "Cape Lookout")

kw_sites <- merge(sites, kw, by  = "Site")
###############################################################################
#GLMM with a tweedie distribution for kw percent change with region as
# a random variable
#Now using these vars based on PCA: Mean_Kd490_Growing, P90_temp_growing, P10_NO3, P10_growing,Waves_P90
###############################################################################
#assign regions to each site
kw_sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                  "PortOrford","PortOrford","Depoe","North","PortOrford","PortOrford","Rogue","Arago")
#start with variable by variable
mod1 <- glmmTMB(Percentage_Of1013 ~ Mean_Kd490_Growing + (1|Region),
              family = tweedie(link = "log"), data = kw_sites)
summary(mod1)
#urchin mean: AIC = 168.0, pval = 0.000228 Urchin mean effect = -0.18 +/- 0.05
  # Intercept is 6.66 +/- 0.55
  #interesting, the random effects are tiny... is that because of the log link? 
  # Urchin med: 168.2, urchin mean + intercept are signif, med effect = -0.21
#temp: AIC = 173.5, pval = 0.301, est = -1.98 +/- 1.917
#sal: AIC = 170.7, pval = 0.00657, est = -0.757 +- 0.279..... fascinating...
#waves: AIC = 174.4, pval = 0.628, est = 0.259 +/ 0.535
#NO3: AIC = 174.6, pval = 0.925, est = 0.042 +- 0.448
#Kd: AIC = 174.6, pval = 0.992, est = -0.033 +/- 3.54


#Variables+ urchin mean
mod1 <- glmmTMB(Percentage_Of1013 ~ P10_growing +urchin_mean + (1|Region),
                family = tweedie(link = "log"), data = kw_sites)
summary(mod1)
plot(resid(mod1))
mod1
#Urch + temp: AIC = 169.8, temp pval = 0.609
#Urch + sal: AIC = 169.4, sal pval = 0.394, doesn't actually improve model....
#Urch + NO3: AIC = 170.0, N03 pval = 0.869
#Urch + waves: AIC = 170.0, urc signif, waves not signif
#Urch + Kd: AIC = 170, urch signif, kd not signif

#All vars
mod1 <- glmmTMB(Percentage_Of1013 ~ Mean_Kd490 + P90_temp_growing + P10_NO3 + Waves_P90 + P10_growing +urchin_mean + (1|Region),
                family = tweedie(link = "log"), data = kw_sites)
summary(mod1)
#AIC = 173.6, unable to estimate pval for the variables
###################################################################################
#Make a little figure + table for tweedie GLMM modeling kw percent change and its coefficients
################################################################################
#Create dataframe showing coefficients and pval for each GLMM with single variable
#fyi the r2 I'm calculating is the marginal r2 (only reports on fixed effects)
vars = c("Mean Urchin Dens.","90th Perc. Temp. - Growing","10th Perc. Sal. - Growing",
         "90th Perc. Waves","10th. Perc. NO3", "Mean Turb. - Growing")
estimate = as.vector(NA)
ci1 = as.vector(NA)
ci2 = as.vector(NA)
pval = as.vector(NA)
r2 = as.vector(NA)
aic = as.vector(NA)


mod_urch  <- glmmTMB(Percentage_Of1013 ~ urchin_mean + (1|Region),
                           family = tweedie(link = "log"), data = kw_sites)
estimate[1] <- summary(mod_urch)$coefficients$cond[2,1]
ci1[1] <- confint(mod_urch)[2,1]
ci2[1] <- confint(mod_urch)[2,2]
pval[1] <- summary(mod_urch)$coefficients$cond[2,4]
r2[1] <- as.numeric(r2_nakagawa(mod_urch, tolerance = 1e-09)[[2]])
aic[1] <- summary(mod_urch)$AIC[1]

mod_temp  <- glmmTMB(Percentage_Of1013 ~ P90_temp_growing + (1|Region),
                     family = tweedie(link = "log"), data = kw_sites)
estimate[2] <- summary(mod_temp)$coefficients$cond[2,1]
ci1[2] <- confint(mod_temp)[2,1]
ci2[2] <- confint(mod_temp)[2,2]
pval[2] <- summary(mod_temp)$coefficients$cond[2,4]
r2[2] <- as.numeric(r2_nakagawa(mod_temp, tolerance = 1e-09)[[2]])
aic[2] <- summary(mod_temp)$AIC[1]

mod_sal  <- glmmTMB(Percentage_Of1013 ~ P10_growing + (1|Region),
                     family = tweedie(link = "log"), data = kw_sites)
estimate[3] <- summary(mod_sal)$coefficients$cond[2,1]
ci1[3] <- confint(mod_sal)[2,1]
ci2[3] <- confint(mod_sal)[2,2]
pval[3] <- summary(mod_sal)$coefficients$cond[2,4]
r2[3] <- as.numeric(r2_nakagawa(mod_sal, tolerance = 1e-09)[[2]])
aic[3] <- summary(mod_sal)$AIC[1]

mod_waves  <- glmmTMB(Percentage_Of1013 ~ Waves_P90 + (1|Region),
                     family = tweedie(link = "log"), data = kw_sites)
estimate[4] <- summary(mod_waves)$coefficients$cond[2,1]
ci1[4] <- confint(mod_waves)[2,1]
ci2[4] <- confint(mod_waves)[2,2]
pval[4] <- summary(mod_waves)$coefficients$cond[2,4]
r2[4] <- as.numeric(r2_nakagawa(mod_waves, tolerance = 1e-09)[[2]])
aic[4] <- summary(mod_waves)$AIC[1]

mod_no3  <- glmmTMB(Percentage_Of1013 ~ P10_NO3 + (1|Region),
                     family = tweedie(link = "log"), data = kw_sites)
estimate[5] <- summary(mod_no3)$coefficients$cond[2,1]
ci1[5] <- confint(mod_no3)[2,1]
ci2[5] <- confint(mod_no3)[2,2]
pval[5] <- summary(mod_no3)$coefficients$cond[2,4]
r2[5] <- as.numeric(r2_nakagawa(mod_no3, tolerance = 1e-09)[[2]])
aic[5] <- summary(mod_no3)$AIC[1]

mod_turb  <- glmmTMB(Percentage_Of1013 ~ Mean_Kd490_Growing + (1|Region),
                      family = tweedie(link = "log"), data = kw_sites)
estimate[6] <- summary(mod_turb)$coefficients$cond[2,1]
ci1[6] <- confint(mod_turb)[2,1]
ci2[6] <- confint(mod_turb)[2,2]
pval[6] <- summary(mod_turb)$coefficients$cond[2,4]
r2[6] <- as.numeric(r2_nakagawa(mod_turb, tolerance = 1e-09)[[2]])
aic[6] <- summary(mod_turb)$AIC[1]

df = data.frame("Variable" = vars, "Coefficient" = signif(estimate,3), "Lower CI" = signif(ci1,3), 
                "Upper CI" = signif(ci2,3), "P-value" = signif(pval,2), "Marg R2" = signif(r2,3), "AIC" = signif(aic,4))
write.csv(df, "C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/GLMMTable_PercentChangeTweedie.csv")
library(knitr)
library(kableExtra)
kable(df, format = "html")
df %>%
  kbl() %>%
  column_spec(5, bold = ifelse(df$P.value < 0.05 , TRUE, FALSE)) %>%
  column_spec(7, bold = ifelse(df$AIC == min(df$AIC) , TRUE, FALSE)) %>%
  kable_styling()
#plot it all out
p <- ggplot(df, aes(x = estimate, y = vars))+
  geom_point()+
  geom_errorbar(aes(xmin = ci1, xmax = ci2), color = "gray20")+
  geom_vline(xintercept =  0, linetype = "dashed", color = "red")+
  labs(x = "Estimated Coefficient",y = "Variable", title = "GLMM - Percent Change in Canopy Area")+
  annotate("text", x = -2, y = 5, label = "***", size = 6)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/GLMMCoefficients_PercentChange.jpeg", plot = p,
       width = 7.5, height = 2.5,dpi = 600)
################################################################################
#Figure of modeled urchin/percent change relationship for tweedie GLMM modeling kw percent change
#################################################################################
kw_sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                     "PortOrford","PortOrford","Depoe","North","PortOrford","PortOrford","Rogue","Arago")
kw_sites$Region <- factor(kw_sites$Region, levels = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))

mod_urch  <- glmmTMB(Percentage_Of1013 ~ urchin_mean + (1|Region),
                     family = tweedie(link = "log"), data = kw_sites)
modeled <- predict(mod_urch, kw_sites)
fd_n_u <- seq(0,28, by = 1)
fd_n_r <- rep("North", 29)
fakedata_north <- data.frame("urchin_mean" = fd_n_u, 
                             "Region" = fd_n_r)
modeled_north <- predict(mod_urch, fakedata_north)
fakedata_north$Log_Percentage1013 <- modeled_north
fakedata_north$Percentage_Of1013 <- 2.71828^(modeled_north)
fakedata_depoe <- data.frame("urchin_mean" = seq(0,28, by = 1), 
                                               "Region" = rep("Foulweather",29))
modeled_depoe <- predict(mod_urch, fakedata_depoe)
fakedata_depoe$Log_Percentage1013 <- modeled_depoe
fakedata_depoe$Percentage_Of1013 <- 2.71828^(modeled_depoe)
fakedata_arago <- data.frame("urchin_mean" = seq(0,28, by = 1), 
                                               "Region" = rep("Arago",29))
modeled_arago <- predict(mod_urch, fakedata_arago)
fakedata_arago$Log_Percentage1013 <- modeled_arago
fakedata_arago$Percentage_Of1013 <- 2.71828^(modeled_arago)
fakedata_po <- data.frame("urchin_mean" = seq(0,28, by = 1), 
                             "Region" = rep("PortOrford",29))
modeled_po <- predict(mod_urch, fakedata_po)
fakedata_po$Log_Percentage1013 <- modeled_po
fakedata_po$Percentage_Of1013 <- 2.71828^(modeled_po)
fakedata_rogue <- data.frame("urchin_mean" = seq(0,28, by = 1), 
                          "Region" = rep("Rogue",29))
modeled_rogue <- predict(mod_urch, fakedata_rogue)
fakedata_rogue$Log_Percentage1013 <- modeled_rogue
fakedata_rogue$Percentage_Of1013 <- 2.71828^(modeled_rogue)
fakedata_brookings <- data.frame("urchin_mean" = seq(0,28, by = 1), 
                             "Region" = rep("Brookings",29))
modeled_brookings <- predict(mod_urch, fakedata_brookings)
fakedata_brookings$Log_Percentage1013 <- modeled_brookings
fakedata_brookings$Percentage_Of1013 <- 2.71828^(modeled_brookings)

#Initiate a final dataframe that will hold all data
fakedata = data.frame("urchin_mean" = seq(0,28, by = 1),
                      "North" = fakedata_north$Percentage_Of1013,
                      "Depoe" = fakedata_depoe$Percentage_Of1013,
                      "Arago" = fakedata_arago$Percentage_Of1013,
                      "PortOrford" = fakedata_po$Percentage_Of1013,
                      "Rogue" = fakedata_rogue$Percentage_Of1013,
                      "Brookings" = fakedata_brookings$Percentage_Of1013)
fakedata <- pivot_longer(data = fakedata, cols = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))
fakedata$Region <- factor(fakedata$name, levels = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))

#Plot it up
p <- ggplot(fakedata, aes(x = urchin_mean, y = value))+
  geom_line(linewidth = 1, color = "black")+
  geom_point(data = kw_sites, aes(x = urchin_mean, y = Percentage_Of1013, fill = Region,),shape = 21, color = 'gray10',size = 3)+
  scale_fill_manual(
    name = "Regions",
    values = c("North" = "#440154", "Depoe" = "#414487", "Arago" = "#2a788e", 
               "PortOrford" = "#22a884", "Rogue" = "#7ad151", "Brookings" = "#fde725"),
    labels = c("North", "Depoe", "Arago", "Port Orford", "Rogue", "Brookings")
  )+
  labs(x = bquote("Mean Urchin Density "(urchin/m^2)), y = "Percent Change in Kelp Canopy from 2010-2013 to 2020-2023")+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/KelpCanopyUrchinRelationship.jpeg", plot = p,
       width = 6, height = 5,dpi = 600)


################################################################################
#Figure of modeled salinity/percent change relationship for tweedie GLMM modeling kw percent change
#################################################################################
kw_sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                     "PortOrford","PortOrford","Depoe","North","PortOrford","PortOrford","Rogue","Arago")
kw_sites$Region <- factor(kw_sites$Region, levels = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))

mod_urch  <- glmmTMB(Percentage_Of1013 ~ P10_growing + (1|Region),
                     family = tweedie(link = "log"), data = kw_sites)
modeled <- predict(mod_urch, kw_sites)
fd_n_u <- seq(27,33, by = 0.25)
fd_n_r <- rep("North", 25)
fakedata_north <- data.frame("P10_growing" = fd_n_u, 
                             "Region" = fd_n_r)
modeled_north <- predict(mod_urch, fakedata_north)
fakedata_north$Log_Percentage1013 <- modeled_north
fakedata_north$Percentage_Of1013 <- 2.71828^(modeled_north)
fakedata_depoe <- data.frame("P10_growing" =seq(27,33, by = 0.25), 
                             "Region" = rep("Foulweather",25))
modeled_depoe <- predict(mod_urch, fakedata_depoe)
fakedata_depoe$Log_Percentage1013 <- modeled_depoe
fakedata_depoe$Percentage_Of1013 <- 2.71828^(modeled_depoe)
fakedata_arago <- data.frame("P10_growing" = seq(27,33, by = 0.25), 
                             "Region" = rep("Arago",25))
modeled_arago <- predict(mod_urch, fakedata_arago)
fakedata_arago$Log_Percentage1013 <- modeled_arago
fakedata_arago$Percentage_Of1013 <- 2.71828^(modeled_arago)
fakedata_po <- data.frame("P10_growing" = seq(27,33, by = 0.25), 
                          "Region" = rep("PortOrford",25))
modeled_po <- predict(mod_urch, fakedata_po)
fakedata_po$Log_Percentage1013 <- modeled_po
fakedata_po$Percentage_Of1013 <- 2.71828^(modeled_po)
fakedata_rogue <- data.frame("P10_growing" = seq(27,33, by = 0.25), 
                             "Region" = rep("Rogue",25))
modeled_rogue <- predict(mod_urch, fakedata_rogue)
fakedata_rogue$Log_Percentage1013 <- modeled_rogue
fakedata_rogue$Percentage_Of1013 <- 2.71828^(modeled_rogue)
fakedata_brookings <- data.frame("P10_growing" = seq(27,33, by = 0.25), 
                                 "Region" = rep("Brookings",25))
modeled_brookings <- predict(mod_urch, fakedata_brookings)
fakedata_brookings$Log_Percentage1013 <- modeled_brookings
fakedata_brookings$Percentage_Of1013 <- 2.71828^(modeled_brookings)

#Initiate a final dataframe that will hold all data
fakedata = data.frame("urchin_mean" =  seq(27,33, by = 0.25),
                      "North" = fakedata_north$Percentage_Of1013,
                      "Depoe" = fakedata_depoe$Percentage_Of1013,
                      "Arago" = fakedata_arago$Percentage_Of1013,
                      "PortOrford" = fakedata_po$Percentage_Of1013,
                      "Rogue" = fakedata_rogue$Percentage_Of1013,
                      "Brookings" = fakedata_brookings$Percentage_Of1013)
fakedata <- pivot_longer(data = fakedata, cols = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))
fakedata$Region <- factor(fakedata$name, levels = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))

#Plot it up
p <- ggplot(fakedata, aes(x = urchin_mean, y = value))+
  geom_line(linewidth = 1, color = "black")+
  geom_point(data = kw_sites, aes(x = P10_growing, y = Percentage_Of1013, fill = Region),shape = 21, color = 'gray10',size = 3)+
  scale_fill_manual(
    name = "Regions",
    values = c("North" = "#440154", "Depoe" = "#414487", "Arago" = "#2a788e", 
               "PortOrford" = "#22a884", "Rogue" = "#7ad151", "Brookings" = "#fde725"),
    labels = c("North", "Depoe", "Arago", "Port Orford", "Rogue", "Brookings")
  )+
  labs(x = "10th Percentile of Growing Season Salinity (psu)", y = "Percent Change in Kelp Canopy from 2010-2013 to 2020-2023")+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/KelpCanopySalinityRelationship.jpeg", plot = p,
       width = 6, height = 5,dpi = 600)

################################################################################
# GLMMs for 2023 subtidal count data
#Checking overdispersion on a GLMM witha poisson family on subtidal count data with
# "Region" as the random variable 
#################################################################################
#OVERDISPERSION WITH SUBTIDAL COUNT DATA
hist(sites$kelp_mean)
sites$kelp_mean_count <- round(sites$kelp_mean * 60)
sites$Region <- c("PortOrford","Foulweather","North","Brookings","Arago","Arago","Brookings",
                  "PortOrford","PortOrford","Foulweather","North","PortOrford","PortOrford","Rogue","Arago")
#Using A poisson model
mod1 <- glmmTMB(kelp_mean_count ~ P10_salinity +  (1|Region),
                family = poisson(link = "log"), data = sites)
#A model may be overdispersed if the value of the Pearson Chi2 divided by the degrees of freedom 
#(dof) is greater than 1.0. The quotient is called the dispersion. Small amounts of overdispersion 
#are of little concern; however, if the dispersion statistic is greater than 1.25 for moderate-sized models, 
#then a correction may be warranted. Models with large numbers of observations may be overdispersed with a dispersion statistic of 1.05.
pearsonchi <- sum(residuals(mod1, type="pearson")^2)
dispersion = pearsonchi/11 #(11 is the dof) 
#Is 24, which would indicated its very overdispersed. 
#Using a negative binomial model 
mod1 <- glmmTMB(kelp_mean_count ~ P10_salinity +  (1|Region),
                family = nbinom2(link = "log"), data = sites)
pearsonchi <- sum(residuals(mod1, type="pearson")^2)
dispersion = pearsonchi/11 #(11 is the dof) 
#Is 0.67 , which would indicates it is not overdispersed (these kinds of stats hold
#true if you sub out several variables like urchins, temp, salinity)

#################################################################################
#GLMMs for 2023 subtidal count data
#GLMM with a negative binomial family on subtidal count data with "Region" as the random variable
#Now using these vars based on PCA: Mean_Kd490-Growing, P90_temp_growing, P10_NO3, P10_growing,Waves_P90
##################################################################################
#need to convert kelp density to kelp count so multiply density metric by area (60m2 transects)
sites$kelp_mean_count <- round(sites$kelp_mean * 60)
#label which sites are in which regions
sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                  "PortOrford","PortOrford","Depoe","North", "PortOrford","PortOrford","Rogue","Arago")

#Ok start with variable by variable (just going to show with a single variable here and record results of 
#all below) 
mod2a <- glmmTMB(kelp_mean_count ~ Mean_Kd490_Growing + (1|Region) ,
                family = nbinom2(link = "log"), data = sites)
summary(mod2a)
plot(resid(mod2a))
deviance(mod2a)
#urchin: AIC = 139.2, urch is signif, estimate = -0.17
#temp: AIC = 144.4, pval = 0.918, est = 0.229 +- 2.22
#sal: AIC = 143.9, pval = 0.476, estimate = -0.394 +- 0.552 
#NO3: AIC = 143.5, pval = 0.346, est = -0.363 +- 0.385
#waves: AiC = 144, waves are not signif, estimate = 0.33
#kd: AIC = 143.9, pval = 0.46, estimate = 21.781 +/- 2.41

#Urchin mean + each var individually. .
mod2b <- glmmTMB(kelp_mean_count ~ Mean_Kd490 + urchin_mean + (1|Region),
                family = nbinom2(link = "log"), data = sites)
summary(mod2b)
#Urc + temp: AIC = 140.5, temp not signif. Temp est = 3.3, urch est = -0.2
#Urch + sal: AIC = 139.8, salinity not signif. Sal est = 0.33, urch est = -0.19
#Urch + waves: AIC = 138.6, waves almost signif. Wave est = 0.84, urc est = -0.19
#Urch + Kd: AIC = 141.1,KD not signf. Kd est = -1.1, urch est = -0.17

#All vars
mod2c <- glmmTMB(kelp_mean_count ~ Mean_Kd490 + P90_temp_growing + P10_NO3 + Waves_P90 + P10_growing +urchin_mean + (1|Region),
                 family = nbinom2(link = "log"), data = sites)
summary(mod2c)
###################################################################################
#Make a little figure + table for nbinom GLMM modeling 2023 subtidal kelp count and its coefficients
################################################################################
vars = c("Mean Urchin Dens.","90th Perc. Temp. - Growing","10th Perc. Sal. - Growing",
         "90th Perc. Waves","10th. Perc. NO3", "Mean Turb. - Growing")
estimate = as.vector(NA)
ci1 = as.vector(NA)
ci2 = as.vector(NA)
pval = as.vector(NA)
r2 = as.vector(NA)
aic = as.vector(NA)

mod_urch  <- glmmTMB(kelp_mean_count ~ urchin_mean + (1|Region),
                     family = nbinom2(link = "log"), data = sites)
estimate[1] <- summary(mod_urch)$coefficients$cond[2,1]
ci1[1] <- confint(mod_urch)[2,1]
ci2[1] <- confint(mod_urch)[2,2]
pval[1] <- summary(mod_urch)$coefficients$cond[2,4]
r2[1] <- as.numeric(r2_nakagawa(mod_urch, tolerance = 1e-09)[[2]])
aic[1] <- summary(mod_urch)$AIC[1]

mod_temp  <- glmmTMB(kelp_mean_count ~ P90_temp_growing + (1|Region),
                     family = nbinom2(link = "log"), data = sites)
estimate[2] <- summary(mod_temp)$coefficients$cond[2,1]
ci1[2] <- confint(mod_temp)[2,1]
ci2[2] <- confint(mod_temp)[2,2]
pval[2] <- summary(mod_temp)$coefficients$cond[2,4]
r2[2] <- as.numeric(r2_nakagawa(mod_temp, tolerance = 1e-09)[[2]])
aic[2] <- summary(mod_temp)$AIC[1]

mod_sal  <- glmmTMB(kelp_mean_count ~ P10_growing + (1|Region),
                    family = nbinom2(link = "log"), data = sites)
estimate[3] <- summary(mod_sal)$coefficients$cond[2,1]
ci1[3] <- confint(mod_sal)[2,1]
ci2[3] <- confint(mod_sal)[2,2]
pval[3] <- summary(mod_sal)$coefficients$cond[2,4]
r2[3] <- as.numeric(r2_nakagawa(mod_sal, tolerance = 1e-16)[[2]])
aic[3] <- summary(mod_sal)$AIC[1]

mod_waves  <- glmmTMB(kelp_mean_count ~ Waves_P90 + (1|Region),
                      family = nbinom2(link = "log"), data = sites)
estimate[4] <- summary(mod_waves)$coefficients$cond[2,1]
ci1[4] <- confint(mod_waves)[2,1]
ci2[4] <- confint(mod_waves)[2,2]
pval[4] <- summary(mod_waves)$coefficients$cond[2,4]
r2[4] <- as.numeric(r2_nakagawa(mod_waves, tolerance = 1e-09)[[2]])
aic[4] <- summary(mod_waves)$AIC[1]

mod_no3  <- glmmTMB(kelp_mean_count ~ P10_NO3 + (1|Region),
                     family = nbinom2(link = "log"), data = sites)
estimate[5] <- summary(mod_no3)$coefficients$cond[2,1]
ci1[5] <- confint(mod_no3)[2,1]
ci2[5] <- confint(mod_no3)[2,2]
pval[5] <- summary(mod_no3)$coefficients$cond[2,4]
r2[5] <- as.numeric(r2_nakagawa(mod_no3, tolerance = 1e-09)[[2]])
aic[5] <- summary(mod_no3)$AIC[1]

mod_turb  <- glmmTMB(kelp_mean_count ~ Mean_Kd490_Growing + (1|Region),
                     family = nbinom2(link = "log"), data = sites)
estimate[6] <- summary(mod_turb)$coefficients$cond[2,1]
ci1[6] <- confint(mod_turb)[2,1]
ci2[6] <- confint(mod_turb)[2,2]
pval[6] <- summary(mod_turb)$coefficients$cond[2,4]
r2[6] <- as.numeric(r2_nakagawa(mod_turb, tolerance = 1e-09)[[2]])
aic[6] <- summary(mod_turb)$AIC[1]


#MAKE A FIGURE
df = data.frame(vars, estimate, ci1, ci2, pval)

#resolution of 600 x 195
p <- ggplot(df, aes(x = estimate, y = vars))+
  geom_point()+
  geom_errorbar(aes(xmin = ci1, xmax = ci2), color = "gray20")+
  geom_vline(xintercept =  0, linetype = "dashed", color = "red")+
  labs(x = "Estimated Coefficient",y = "Variable", title = "GLMM - 2023 SCUBA Kelp Abundance")+
  annotate("text", x = -2, y = 5, label = "**", size = 6)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/GLMMCoefficients_KelpCount.jpeg", plot = p,
       width = 7.5, height = 2.5,dpi = 600)


#MAKE A TABLE
df = data.frame("Variable" = vars, "Coefficient" = signif(estimate,3), "Lower CI" = signif(ci1,3), 
                "Upper CI" = signif(ci2,3), "P-value" = signif(pval,2), "Marg R2" = signif(r2,3), "AIC" = signif(aic,4))
write.csv(df, "C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/GLMMTable_KelpCountNBinom.csv")
df %>%
  kbl() %>%
  column_spec(5, bold = ifelse(df$P.value < 0.05 , TRUE, FALSE)) %>%
  column_spec(7, bold = ifelse(df$AIC == min(df$AIC) , TRUE, FALSE)) %>%
  kable_styling()
################################################################################
#Figure of modeled urchin/percent change relationship for nbinom GLMM modeling 2023 subtidal kelp count
#################################################################################
sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                     "PortOrford","PortOrford","Depoe","North","PortOrford","PortOrford","Rogue","Arago")
sites$Region <- factor(kw_sites$Region, levels = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))
sites$kelp_mean_count <- round(sites$kelp_mean * 60)

mod_urch  <- glmmTMB(kelp_mean_count ~ urchin_mean + (1|Region),
                     family = nbinom2(link = "log"), data = sites)
modeled <- predict(mod_urch, sites)
plot(sites$urchin_mean, sites$kelp_mean_count)

#Make data frames for each region 
fakedata_north <- data.frame("urchin_mean" = seq(0,26, by = 1), 
                             "Region" = rep("North", 27))
modeled_north <- predict(mod_urch, fakedata_north)
fakedata_north$logged_kelp_mean_count <- modeled_north
fakedata_north$kelp_mean_count <- 2.71828^(modeled_north)
fakedata_depoe <- data.frame("urchin_mean" = seq(0,26, by = 1), 
                             "Region" = rep("Depoe",27))
modeled_depoe <- predict(mod_urch, fakedata_depoe)
fakedata_depoe$logged_kelp_mean_count <- modeled_depoe
fakedata_depoe$kelp_mean_count <- 2.71828^(modeled_depoe)
fakedata_arago <- data.frame("urchin_mean" = seq(0,26, by = 1), 
                             "Region" = rep("Arago",27))
modeled_arago <- predict(mod_urch, fakedata_arago)
fakedata_arago$logged_kelp_mean_count <- modeled_arago
fakedata_arago$kelp_mean_count <- 2.71828^(modeled_arago)
fakedata_po <- data.frame("urchin_mean" = seq(0,26, by = 1), 
                             "Region" = rep("PortOrford",27))
modeled_po <- predict(mod_urch, fakedata_po)
fakedata_po$logged_kelp_mean_count <- modeled_po
fakedata_po$kelp_mean_count <- 2.71828^(modeled_po)
fakedata_rogue <- data.frame("urchin_mean" = seq(0,26, by = 1), 
                          "Region" = rep("Rogue",27))
modeled_rogue <- predict(mod_urch, fakedata_rogue)
fakedata_rogue$logged_kelp_mean_count <- modeled_rogue
fakedata_rogue$kelp_mean_count <- 2.71828^(modeled_rogue)
fakedata_brookings <- data.frame("urchin_mean" = seq(0,26, by = 1), 
                             "Region" = rep("Brookings",27))
modeled_brookings <- predict(mod_urch, fakedata_brookings)
fakedata_brookings$logged_kelp_mean_count <- modeled_brookings
fakedata_brookings$kelp_mean_count <- 2.71828^(modeled_brookings)

#Initiate a final dataframe that will hold all data
fakedata = data.frame("urchin_mean" = seq(0,26, by = 1),
                      "North" = fakedata_north$kelp_mean_count,
                      "Depoe" = fakedata_depoe$kelp_mean_count,
                      "Arago" = fakedata_arago$kelp_mean_count,
                      "PortOrford" = fakedata_po$kelp_mean_count,
                      "Rogue" = fakedata_rogue$kelp_mean_count,
                      "Brookings" = fakedata_brookings$kelp_mean_count)
fakedata <- pivot_longer(data = fakedata, cols = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))
fakedata$name <- factor(fakedata$name, levels = c("North","Depoe","Arago","PortOrford","Rogue","Brookings"))

#plot 
p <- ggplot(fakedata, aes(x = urchin_mean, y = value, color = name))+
  geom_line(linewidth = 1)+
  geom_point(data = sites, aes(x = urchin_mean, y = kelp_mean_count, fill = Region),shape = 21, color = "gray30", size = 3)+
  scale_fill_manual(
    name = "Regions",
    values = c("North" = "#440154", "Depoe" = "#414487", "Arago" = "#2a788e", 
               "PortOrford" = "#22a884", "Rogue" = "#7ad151", "Brookings" = "#fde725"),
    labels = c("North", "Depoe", "Arago", "Port Orford", "Rogue", "Brookings")
  )+
  scale_color_manual(
    name = "Regions",
    values = c("North" = "#440154", "Depoe" = "#414487", "Arago" = "#2a788e", 
               "PortOrford" = "#22a884", "Rogue" = "#7ad151", "Brookings" = "#fde725"),
    labels = c("North", "Depoe", "Arago", "Port Orford", "Rogue", "Brookings")
  )+
  labs(x = bquote("Mean Urchin Density "(urchin/m^2)), y = bquote('Number of Kelps per 60 m'^2))+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/KelpCountUrchinRelationship.jpeg", plot = p,
       width = 6, height = 5,dpi = 600)

################################################################################
#GLMMs for 2023 subtidal BULL KELP COUNT DATA
#GLMM with a negative binomial family on subtidal count data with "Region" as the random variable
#Now using these vars based on PCA: Mean_Kd490-Growing, P90_temp_growing, P10_NO3, P10_growing,Waves_P90
##################################################################################
#need to convert kelp density to kelp count so multiply density metric by area (60m2 transects)
sites$kelp_mean_count <- round(sites$bull_mean * 60)
#label which sites are in which regions
sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                  "PortOrford","PortOrford","Depoe","North", "PortOrford","PortOrford","Rogue","Arago")

#Ok start with variable by variable (just going to show with a single variable here and record results of 
#all below) 
mod2a <- glmmTMB(kelp_mean_count ~ urchin_mean + (1|Region) ,
                 family = nbinom2(link = "log"), data = sites)
summary(mod2a)
plot(resid(mod2a))
deviance(mod2a)
#urchin: AIC = 114.8, pval = 0.0151
#temp: AIC = 116.6, pval = 0.729
#sal: AIC = 114.7, pavl = 0.147
#NO3: AIC = 115.4, pval = 0.242
#waves: AiC = 111.4, pval = 0.0008892, estimate = 1.00
#kd: AIC = 116.7, pval = 0.853

#Waves + each var individually. . None of the models have an AIC two points lower, 
#although two of the models (temp + urchins) are significant....
mod2b <- glmmTMB(kelp_mean_count ~ P90_temp_growing + Waves_P90 + (1|Region),
                 family = nbinom2(link = "log"), data = sites)
summary(mod2b)
#Wave + urch: AIC = 110.7, ur hin + Waves are signif
#Wave + temp: AIC = 109.9, waves + temp are signif.... (temp est = -1.6)
#Waves+ sal: AIC = 112.9, salinity not signif
#Waves + NO3: AIC = 113.3, P10_NO3 is not significant
#Waves + Kd: AIC = 112.5, Kd is not signif

#All vars - AIC not better, Waves is the only significant one. 
mod2c <- glmmTMB(kelp_mean_count ~ Mean_Kd490 + P90_temp_growing + P10_NO3 + Waves_P90 + P10_growing +urchin_mean + (1|Region),
                 family = nbinom2(link = "log"), data = sites)
summary(mod2c)
################################################################################
#GLMMs for 2023 subtidal SUBCANOPY KELP DATA (NOT BULL KELP)
#GLMM with a negative binomial family on subtidal count data with "Region" as the random variable
#Now using these vars based on PCA: Mean_Kd490_Growing, P90_temp_growing, P10_NO3, P10_growing,Waves_P90
##################################################################################
#need to convert kelp density to kelp count so multiply density metric by area (60m2 transects)
sites$kelp_mean_count <- round(sites$nonbull_mean * 60)
#label which sites are in which regions
sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                  "PortOrford","PortOrford","Depoe","North", "PortOrford","PortOrford","Rogue","Arago")

#Ok start with variable by variable (just going to show with a single variable here and record results of 
#all below) 
mod2a <- glmmTMB(kelp_mean_count ~ Mean_Kd490_Growing+ (1|Region) ,
                 family = nbinom2(link = "log"), data = sites)
summary(mod2a)
plot(resid(mod2a))
deviance(mod2a)
#urchin: AIC = 118.8, pval = 0.02
#temp: AIC = 122.9, pval = 0.814
#sal: AIC = 122.9, pavl = 0.924
#NO3: AIC = 122.6, pval = 0.577
#waves: AiC = 122.7, pval = 0.679
#kd: AIC = 122.8, pval = 0.776

#Urchin + each var individually. 
mod2b <- glmmTMB(kelp_mean_count ~ P90_temp_growing + urchin_mean + (1|Region),
                 family = nbinom2(link = "log"), data = sites)
summary(mod2b)
#Urch + temp: AIC = 120.2, tmep not signif
#Urch + sal: AIC = NA (Struggling to converge), sal is not signif
#Urch + NO3: AIC = 120.1, P10_NO3 not signif
#Urch + waves: AIC = 118.3, waves not signif
#Urch + Kd: AIC = 120.8, kd not signif

#All vars - AIC not better, Waves is the only significant one. 
mod2c <- glmmTMB(kelp_mean_count ~ Mean_Kd490 + P90_temp_growing + P10_NO3 + Waves_P90 + P10_growing +urchin_mean + (1|Region),
                 family = nbinom2(link = "log"), data = sites)
summary(mod2c)


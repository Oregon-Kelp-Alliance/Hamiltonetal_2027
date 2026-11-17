library(tidyverse)
library(glmtoolbox)
library(glmmTMB)
library(MuMIn)
library(car)
library(performance)
library(tweedie)
library(knitr)
library(kableExtra)
#Note I'm seeing that a Cook's distance > 1 or greater than 4/N is a threshold
#for investigating a point (based on this https://online.stat.psu.edu/stat462/node/173/,
  # https://www.statology.org/how-to-identify-influential-data-points-using-cooks-distance/, and
  # https://scienceinsights.org/what-is-cooks-distance-definition-and-interpretation/)
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

#methods from:https://cran.r-project.org/web/packages/glmmTMB/vignettes/model_evaluation.pdf
source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))

#################################################################################
#for percent change model (Fig 8)
################################################################################

kw_sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                     "PortOrford","PortOrford","Depoe","North","PortOrford","PortOrford","Rogue","Arago")
#start with variable by variable
mod1 <- glmmTMB(Percentage_Of1013 ~ urchin_mean + (1|Region),
                family = tweedie(link = "log"), data = kw_sites)
summary(mod1)
AICc(mod1)
mod1_inf <- influence_mixed(mod1)
#interpretting cooks dist from:https://www.statology.org/how-to-identify-influential-data-points-using-cooks-distance/
#cutoff would be around 0.28 according to the post above. Nothing is above that
car::infIndexPlot(mod1_inf)
#can get Cook's distance metric alone doing this
D = cooks.distance(mod1_inf)
kw_sites$D <- D
p <- ggplot(data = kw_sites, aes(x = Site, y = D))+
  ylim(0,0.3)+
  geom_point()+
  geom_hline(yintercept = 0.29, linetype = "dashed")+
  labs(title = "Cook's D by Site for Canopy Change Model")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 80, hjust = 1))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/LeverageAnalysis_CooksD_PercChange.jpeg", plot = p,
       width = 7.5, height = 4,dpi = 600)  

################################################################################
#Now for count model (Fig 8)
################################################################################
sites$kelp_mean_count <- round(sites$kelp_mean * 60)
#label which sites are in which regions
sites$Region <- c("PortOrford","Depoe","Brookings","Arago","Arago","Brookings",
                  "PortOrford","PortOrford","Depoe","North", "PortOrford","PortOrford","Rogue","Arago")

mod2 <- glmmTMB(kelp_mean_count ~ urchin_mean + (1|Region) ,
                 family = nbinom2(link = "log"), data = sites)
summary(mod2)
AICc(mod2)
mod2_inf <- influence_mixed(mod2)
#Ok point 14 is above 0.28, potentially an outlier
car::infIndexPlot(mod2_inf)
D = cooks.distance(mod2_inf)
kw_sites$D <- D
p <- ggplot(data = kw_sites, aes(x = Site, y = D))+
  ylim(0,0.5)+
  geom_point()+
  geom_hline(yintercept = 0.29, linetype = "dashed")+
  labs(title = "Cook's D by Site for Count Model")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 80, hjust = 1))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/LeverageAnalysis_CooksD_Count.jpeg", plot = p,
       width = 7.5, height = 4,dpi = 600)  


#What happens if we remove
sites_minusout <- sites[1:13,]
mod2b <- glmmTMB(kelp_mean_count ~ urchin_mean + (1|Region) ,
                family = nbinom2(link = "log"), data = sites_minusout)
summary(mod2b)
AICc(mod2b)
#relationship is still significant (actually AICc is significantly better at 126.0, p-val smaller for urchin mean at 9.35e-06 *** and 
#estimate of urchin coefficient is -0.20, close to that of the other models)

################################################################################
#Perc change with salinity (Fig S13)
################################################################################
#start with variable by variable
mod3 <- glmmTMB(Percentage_Of1013 ~ P10_growing + (1|Region),
                family = tweedie(link = "log"), data = kw_sites)
summary(mod3)
AICc(mod3)
mod3_inf <- influence_mixed(mod3)
#interpretting cooks dist from:https://www.statology.org/how-to-identify-influential-data-points-using-cooks-distance/
#cutoff would be around 0.28 according to the post above. 
car::infIndexPlot(mod3_inf)
D = cooks.distance(mod3_inf)
kw_sites$D <- D
p <- ggplot(data = kw_sites, aes(x = Site, y = D))+
  ylim(0,1.5)+
  geom_point()+
  geom_hline(yintercept = 0.29, linetype = "dashed")+
  labs(title = "Cook's D by Site for Canopy Change Model - Salinity model")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 80, hjust = 1))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/GLMMs/LeverageAnalysis_CooksD_PercChangeSalinity.jpeg", plot = p,
       width = 7.5, height = 4,dpi = 600)  
#ok have some very strong outliers here, particularly 10 + 12
kwsites_minusout <- kw_sites[c(1,2,3,4,5,6,7,8,9,11,13,14),]
mod3b <- glmmTMB(Percentage_Of1013 ~ P10_growing + (1|Region) ,
                 family = tweedie(link = "log"), data = kwsites_minusout)
summary(mod3b)
AICc(mod3b)
#has convergence issues but salinity is still significant
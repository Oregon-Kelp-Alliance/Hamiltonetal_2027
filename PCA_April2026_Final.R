library(tidyverse)
#load in indpendent variables
temp <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithTempMetrics_20132023.csv")
sal <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithSalinityMetrics_20132018.csv")
no3 <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithNO3Metrics_20132023.csv")
wave <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithWaveMetrics_20132020.csv")
kd <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSites_WithKd490Metrics_20132023.csv")
bio <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterSite_WithBiologicalMetrics_2023.csv")
bio <- bio[,-1]
#merge it all into one dataframe
sites <- merge(temp, sal)
sites <- merge(sites, no3)
sites <- merge(sites, wave)
sites <- merge(sites, kd)
sites <- merge(sites, bio)
#Cape Lookout was surveyed in 2022, not 2023 so can't be used for this analysis
sites <- sites %>% filter(Site != "Cape Lookout")
sites$Region <- factor(c("Port Orford","Depoe","Brookings","Arago","Arago","Brookings","Port Orford",
                  "Port Orford", "Depoe","North","Port Orford", "Port Orford","Rogue", "Arago"),
                  levels = c("North","Depoe","Arago","Port Orford","Rogue","Brookings"))
#################################################################################
# PCA Analysis a la May round of revisions. 
################################################################################
library(dplyr)
library(ggfortify)
library(factoextra)
#Set up dataframe with just our variables of interest: mean kd, waves p90, P90_NO3_growing, P10_NO3_growing, p10 growing,
#p10 winter, and then p90 temp growing and p90 temp winter
sites_pca <- sites %>% select(Mean_Kd490, Mean_Kd490_Growing, 
                              Waves_P90, Winter_Waves_P90, Summer_Waves_P90, 
                              P90_NO3, P10_NO3, P90_NO3_growing, P10_NO3_growing, 
                              P10_salinity, P10_winter, P10_growing, 
                              P90_temp,P90_temp_growing, P90_temp_winter)
colnames(sites_pca) <- c("Mean Turb.", "Mean Turb. - Summer",
                         "90th Perc. Waves","90th Perc. Waves - Winter","90th Perc. Waves - Summer",
                        "90th Perc. Nitrate","10th Perc. Nitrate",  "90th Perc. Nitrate - Summer", "10th Perc. Nitrate - Summer",
                        "10th Perc. Salinity",  " 10th Perc. Salinity - Winter"," 10th Perc. Salinity - Summer",
                        "90th Perc. Temp", "90th Perc. Temp. - Summer","90th Perc. Temp - Winter")
pca <- prcomp(sites_pca, center = TRUE, scale.=TRUE)         
summary(pca)
#first pca explains 47%, two explain 74% and first three explain 85%
#first PCA - Associated with  higher p10 salinity in growing season, higher winter temps, 
#lower turbidity and lower growing season p90 temp
#second PCA - Associated with bigger waves and lower winter sal as well as with higher growing season NO3
#third PCA - Smaller waves, higher winter and growing salinity
#Ok plotting things out on first two PCs - fascinatinggggg
p1 <- autoplot(pca,
               data = sites, 
               main = "First Two  Components of Site Oceanographic Conditions",
               label = TRUE, label.label = "Site",
               label.label.repel = TRUE,
               loadings.label = TRUE,
               loadings.label.repel=TRUE)  +
  xlim(-0.5, 0.5) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

###################################################################################
#Plot series 1 - color as region
#################################################################################
#PCA plot - just loadings
p2 <- autoplot(pca,
               data = sites, 
               color = "Region",
               size = 3,
               main = "First Two Components of Site Oceanographic Conditions - Loadings",
               loadings.label.size = 3,
               loadings.label = TRUE,
               loadings.label.repel=TRUE)  +
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p2
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/PCA/PCA_Loadings_ColorRegion_May2026.jpg", 
       plot = p2, device = "jpeg", width = 6.2, height = 6, units = "in", dpi = 500)
#PCA plot - just sites names
p3 <- autoplot(pca,
               data = sites,
               color = "Region",
               size = 3,
               main = "First Two Components of Site Oceanographic Conditions - Sites",
               label = TRUE, label.label = "Site",
               label.color = "black",
               label.repel = TRUE,
               label.size = 4,
               loadings = TRUE)+
  
  scale_color_viridis_d()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p3
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/PCA/PCA_Sites_ColorRegion_May2026.jpg", 
       plot = p3, device = "jpeg", width = 6, height = 6, units = "in", dpi = 500)


###################################################################################
#Plot series 1 - color as kelp abundance
#################################################################################
#PCA plot - just loadings
p4 <- autoplot(pca,
               data = sites, 
               size = "kelp_mean",
               #color = "kelp_mean",
               main = "First Two Components of Site Oceanographic Conditions - Loadings",
               loadings.label.size = 3,
               loadings.label = TRUE,
               loadings.label.repel=TRUE)  +
  scale_size_continuous(name = "Mean kelp count\non 2023 surveys")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p4
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/PCA/PCA_Loadings_May2026.jpg", 
       plot = p4, device = "jpeg", width = 7, height = 6, units = "in", dpi = 500)
#PCA plot - just sites names
p5 <- autoplot(pca,
               data = sites,
               #color = "Region",
               size = "kelp_mean",
               main = "First Two Components of Site Oceanographic Conditions - Sites",
               label = TRUE, label.label = "Site",
               label.label.color = "black",
               label.repel = TRUE,
               label.size = 4,
               loadings = TRUE)+
  scale_size_continuous(name = "Mean kelp count\non 2023 surveys")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p5
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/PCA/PCA_Sites_May2026.jpg", 
       plot = p5, device = "jpeg", width = 7, height = 6, units = "in", dpi = 500)



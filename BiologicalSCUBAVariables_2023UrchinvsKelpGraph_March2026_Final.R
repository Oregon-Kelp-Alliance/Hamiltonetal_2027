library(tidyverse)
#Read in transect level SCUBA dataset
d23 <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterTrans_WithBiologicalMetrics_2023_updated.csv")
trans <- unique(d23[,c("TransectDateID","SurveyLatitude","SurveyLongitude","Site")])
#set up containers to put calculated density for each category for each transect
purchin_density <- NA 
rurchin_density <- NA
urchin_density <- NA
bull_density <- NA
sub_density <- NA
kelp_density <- NA
#calculate specific, summed density metrics for each transect
for (i in 1:nrow(trans)){ # 
  dsub <- d23 %>% filter(TransectDateID == trans[i,1] & SurveyLatitude == trans[i,2])
  print(dsub[1,"SurveyLatitude"])
  purchin_density[i] = as.numeric(dsub %>% filter(Species == "Strongylocentrotus purpuratus") %>% summarize(mean(Density_m2)))
  rurchin_density[i] = as.numeric(dsub %>% filter(Species == "Mesocentrotus franciscanus") %>% summarize(mean(Density_m2)))
  urchin_density[i] = as.numeric(dsub %>% filter(Taxon == "Sea Urchin") %>% 
                                   summarize(TotalDensity = (as.numeric(sum(Count)))/mean(SurveyArea_m2)))
  bull_density[i] = as.numeric(dsub %>% filter(Species == "Nereocystis luetkeana") %>% summarize(mean(Density_m2)))
  sub_density[i] = as.numeric(dsub %>% filter(Species %in% c("Laminaria setchellii","Pterygophora californica","Pleurophycus gardneri")) %>%
                                summarize(TotalDensity = (as.numeric(sum(Count)))/mean(SurveyArea_m2)))
  kelp_density[i] = as.numeric(dsub %>% filter(Taxon == "Kelp") %>% 
                                 summarize(TotalDensity = (as.numeric(sum(Count)))/mean(SurveyArea_m2)))
}
#knit it all together into a nice dataframe
trans2 = data.frame(trans,purchin_density,rurchin_density,urchin_density, bull_density,sub_density, kelp_density)

#Just urchins vs. kelps
cor(x = trans2$urchin_density, y = trans2$kelp_density, use = "pairwise.complete.obs", method = "pearson") #-0.19
p1<- ggplot(trans2, aes(x = urchin_density, y = kelp_density, color = Site))+
  geom_point(size = 2)+
  scale_color_viridis_d()+
  labs(y = bquote("Kelp Density "(kelp/m^2)), x = bquote("Urchin Density "(urchin/m^2)))+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/UrchinKelp.jpg", 
       plot = p1, device = "jpeg", width = 7.5, height = 5, units = "in", dpi = 500)
p2 <- ggplot(trans2, aes(x = urchin_density, y = kelp_density, color = Site))+
  geom_point(size = 3)+
  scale_color_viridis_d()+
  ylim(0,10)+
  xlim(0,15)+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 14))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/UrchinKelp_Zoom.jpg", 
       plot = p2, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

#Purple urchins vs. kelps
cor(x = trans2$purchin_density, y = trans2$kelp_density, use = "pairwise.complete.obs", method = "pearson") # - 0.18
p3<- ggplot(trans2, aes(x = purchin_density, y = kelp_density, color = Site))+
  geom_point(size = 2)+
  scale_color_viridis_d()+
  labs(y = bquote("Kelp Density "(kelp/m^2)), x = bquote("Purple Urchin Density "(urchin/m^2)))+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/PurpleUrchinKelp.jpg", 
       plot = p3, device = "jpeg", width = 7.5, height = 5, units = "in", dpi = 500)
p4 <- ggplot(trans2, aes(x = purchin_density, y = kelp_density, color = Site))+
  geom_point(size = 3)+
  scale_color_viridis_d()+
  ylim(0,10)+
  xlim(0,15)+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 14))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/PurpleUrchinKelp_Zoom.jpg", 
       plot = p4, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)


#Red urchins vs. kelps
cor(x = trans2$rurchin_density, y = trans2$kelp_density, use = "pairwise.complete.obs", method = "pearson") #-0.14
p5<- ggplot(trans2, aes(x = rurchin_density, y = kelp_density, color = Site))+
  geom_point(size = 2)+
  scale_color_viridis_d()+
  labs(y = bquote("Kelp Density "(kelp/m^2)), x = bquote("Red Urchin Density "(urchin/m^2)))+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/RedUrchinKelp.jpg", 
       plot = p5, device = "jpeg", width = 7.5, height = 5, units = "in", dpi = 500)
p6 <- ggplot(trans2, aes(x = rurchin_density, y = kelp_density, color = Site))+
  geom_point(size = 3)+
  scale_color_viridis_d()+
  ylim(0,10)+
  xlim(0,4)+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 14))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/RedUrchinKelp_Zoom.jpg", 
       plot = p6, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

#Urchins vs. bull kelps
cor(x = trans2$urchin_density, y = trans2$bull_density, use = "pairwise.complete.obs", method = "pearson") # - 0.12
p7<- ggplot(trans2, aes(x = urchin_density, y = bull_density, color = Site))+
  geom_point(size = 2)+
  scale_color_viridis_d()+
  labs(y = bquote("Bull Kelp Density "(kelp/m^2)), x = bquote("Urchin Density "(urchin/m^2)))+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/UrchinBullKelp.jpg", 
       plot = p7, device = "jpeg", width = 7.5, height = 5, units = "in", dpi = 500)
p8 <- ggplot(trans2, aes(x = urchin_density, y = bull_density, color = Site))+
  geom_point(size = 3)+
  scale_color_viridis_d()+
  ylim(0,5)+
  xlim(0,10)+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 14))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/UrchinBullKelp_Zoom.jpg", 
       plot = p8, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

#Urchins vs. sub canopy kelps
cor(x = trans2$urchin_density, y = trans2$sub_density, use = "pairwise.complete.obs", method = "pearson") # - 0.17
p9<- ggplot(trans2, aes(x = urchin_density, y = sub_density, color = Site))+
  geom_point(size = 2)+
  scale_color_viridis_d()+
  labs(y = bquote("Sub-canopy Kelp Density "(kelp/m^2)), x = bquote("Urchin Density "(urchin/m^2)))+
  theme_bw()
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/UrchinSubCanopyKelp.jpg", 
       plot = p9, device = "jpeg", width = 7.5, height = 5, units = "in", dpi = 500)
p10 <- ggplot(trans2, aes(x = urchin_density, y = sub_density, color = Site))+
  geom_point(size = 3)+
  scale_color_viridis_d()+
  ylim(0,10)+
  xlim(0,10)+
  labs(y = "", x = "")+
  theme_bw()+
  theme(legend.position = "none", axis.text = element_text(size = 14))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpPointGraphs/UrchinSubCanopyKelp_Zoom.jpg", 
       plot = p10, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

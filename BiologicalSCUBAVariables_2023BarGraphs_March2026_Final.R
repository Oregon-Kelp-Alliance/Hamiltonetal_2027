library(gridExtra)
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
trans2$Site <- factor(trans$Site, levels = c("Pacific City","Cape Foulweather","Otter Rock",
                                        "Gregory Point","Simpson Reef","Drake Point", 
                                        "Blanco Reef","Orford Reef","Port Orford Heads",
                                        "Redfish Rocks","Humbug","Rogue Reef",
                                        "Harris Beach","Chetco Point"))

#taxa level metrics - purple urchin
labdat <- trans2 %>%
  count(Site)%>%
  mutate(ypos= c(30,10,15,15,40,20,20,35,40,40,58,10,30,22))
p = ggplot(trans2, aes(x = Site, y = purchin_density))+
  geom_boxplot(fill = 'purple')+
  geom_text(data = labdat, aes(label = n, y = ypos),
            show.legend = TRUE)+
  theme_bw()+
  labs(title= "Purple Urchin Density by Site", y = "Purple Urchin Density (urchin/m2)")+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpBoxplots/PurpleUrchinBoxplot.jpg", 
       plot = p, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

#taxa level metrics - red urchin
labdat <- trans2 %>%
  count(Site)%>%
  mutate(ypos= c(7,6,5,4,5,5,3,11,4,7,6,4,3,4))
r = ggplot(trans2, aes(x = Site, y = rurchin_density))+
  geom_boxplot(fill = 'red')+
  geom_text(data = labdat, aes(label = n, y = ypos),
            show.legend = TRUE)+
  theme_bw()+
  labs(title= "Red Urchin Density by Site", y = "Red Urchin Density (urchin/m2)")+
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpBoxplots/RedUrchinBoxplot.jpg", 
       plot = r, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

#taxa level metrics - bull kelp
labdat <- trans2 %>%
  count(Site)%>%
  mutate(ypos= c(2,9,9,8,13,5,2,2,1,2,3,5,1,1))
b = ggplot(trans2, aes(x = Site, y = bull_density))+
  geom_boxplot(fill = 'goldenrod4')+
  geom_text(data = labdat, aes(label = n, y = ypos),
            show.legend = TRUE)+
  ylim(0,14)+
  theme_bw()+
  labs(title= "Bull Kelp Density by Site", y = "Bull Kelp Density (kelp/m2)")+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpBoxplots/BullKelpBoxplot.jpg", 
       plot = b, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

#Taxa level metrics - subcanopy kelps

labdat <- trans2 %>%
  count(Site)%>%
  mutate(ypos= c(2,3,15,8,16,16,11,4,2,2,3,8,2,2))
s = ggplot(trans2, aes(x = Site, y = sub_density))+
  geom_boxplot(fill = 'goldenrod3')+
  geom_text(data = labdat, aes(label = n, y = ypos),
            show.legend = TRUE)+
  ylim(0,17)+
  theme_bw()+
  labs(title= "Sub-canopy Kelp Density by Site", y = "Sub-canopy Kelp Density (kelp/m2)")+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpBoxplots/SubtidalKelpBoxplot.jpg", 
       plot = s, device = "jpeg", width = 6, height = 5, units = "in", dpi = 500)

merged <- grid.arrange(b,s,p,r, nrow = 4, heights = c(1,1,1,1.3))
ggsave("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/UrchinKelpBoxplots/MergedBoxplots.jpg", 
       plot = merged, device = "jpeg", width = 5, height = 13, units = "in", dpi = 500)

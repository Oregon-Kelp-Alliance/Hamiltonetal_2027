
library(tidyverse)
d23 <- read.csv("C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Data/MasterTrans_WithBiologicalMetrics_2023_updated.csv")
#Ok creating a matrix for each unique transect, even if there are multiple transects per SurveyLat/Lon
#Need to search using TransectDateID + SurveyLat/Lon to deal with some aberrant ODFW values. 
trans <- unique(d23[,c("TransectDateID","SurveyLatitude","SurveyLongitude","Site")])
trans2 <- trans %>% group_by(Site)%>%
  summarize(ntrans = n())

table <- d23 %>% group_by(Site) %>%
  summarize(Site = first(Site),
            SiteLatitude = first(SiteLatitude),
            SiteLongitude = first(SiteLongitude)
            )
table2 <- merge(table, trans2) 
table2 <- table2 %>% arrange(desc(SiteLatitude))
write.csv(table2, "C:/Users/sarah/Documents/Github/ORKA_StatusReport_FollowupPaper/StatusReport_FollowupPaper_Figures/SiteTable_Apr2026.csv")

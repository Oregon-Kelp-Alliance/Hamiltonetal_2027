# Hamiltonetal_2027
Code and data submitted alongside Hamilton et al 2027 manuscript

There are three kinds of files submitted here: 1) shapefiles, 2) data, 3) code. The zipped shapefiles, which end in the word 'poly', allow you to extract each kelp forest area canopy data when uploaded to the Kelp Watch data explorer (https://kelpwatch.org/map). The data files include .csv files and zipped .csv files that contain kelp forest data needed when running several pieces of code. Oceanographic data is not provided here as these files are very large and available on the following public repositories:
Live Ocean ROMS data can be accessed at the Data Access page of the LiveOcean website. NOAA ROMS data available freely upon request by emailing Dr. Alexander Kurapov at Alexander.Kurapov@noaa.gov. NREL Wave data is available on the NREL Marine Energy Atlas (https://maps.nlr.gov/marine-energy-atlas/).NASA KD490 data is available on the NASA Ocean Color Data L3 and L4 browser https://oceandata.sci.gsfc.nasa.gov/l3/. 

A brief guide to the code available in this resposities is described here. It could probably be consolidated but.... I'm doing my best here folks. 
Code listed below is used for taking very large .nc files provided for ROMS data and then breaking them down into more user friendly dataframes and csvs. This is needed to create data products used in the below “Oceanographic Profiles”. We needed this step of code for ROMS data but not for NREL Wave data or NASA Kd490 data because the data pulled from NREL and NASA are much smaller and can be used more directly without whittling them down first.
-	NOAA_ROMS_S&T_wrangling – prepping NOAA ROMS salinity data to be used
-	PreppingLOROMsData_Nitrate_May2026 – prepping Live Ocean Nitrate data to be used
-	PreppingLOROMsData_Temp_May2026 – prepping Live Ocean Temperature data to be used
  
Code listed below is for taking oceanographic data and creating usable metrics from it:
-	OceanographicProfiles_20132018_NOAASaltJune2025_Final
-	OceanographicProfiles_LiveOceanNitrate_20132023_Final
-	OceanographicProfiles_LiveOceanTempMay2025_Final
-	OceanographicProfiles_NASAKd490_20132023_Final
-	OceanographicProfiles_NRELWaveMay2025_Final
  
Code listed below is for taking biological data and creating usable metrics from it: 
-	KelpChangeProfile_KW_Dec2025_Final - creates MasterSites_WithKWKelpChangeMetrics_201013to202023_updated.csv
  
Code listed below is for doing specific analyses/figures presented in Hamilton et al.
-	BiologicalSCUBAVariables_2023BarGraphs_March2026_Final  - Requires MasterTrans_WithBiologicalMetrics_2023_updated.csv which is provided.
-	BiologicalSCUBAVariables_2023UrchinvsKelpGraph_March2026_Final  - Requires MasterTrans_WithBiologicalMetrics_2023_updated.csv which is provided.
-	GLMMs_April2026_Final - Requires MasterSite_WithBiologicalMetrics_2023_updated.csv which is provided, MasterSites_WithTempMetrics_20132023.csv which can be created using OceanographicProfiles_LiveOceanTempMay2025_Final, asterSites_WithSalinityMetrics_20132018.csv which can be created using OceanographicProfiles_20132018_NOAASaltJune2025_Final, MasterSites_WithNO3Metrics_20132023.csv which can be created using OceanographicProfiles_LiveOceanNitrate_20132023_Final, MasterSites_WithWaveMetrics_20132020.csv which can be created using OceanographicProfiles_NRELWaveMay2025_Final, MasterSites_WithKd490Metrics_20132023.csv which can be creating using OceanographicProfiles_NASAKd490_20132023_Final, and MasterSites_WithKWKelpChangeMetrics_201013to202023_updated.csv which can be created using KelpChangeProfile_KW_Dec2025_Final.ipynb.
-	PCA_April2026_Final -  Requires MasterSite_WithBiologicalMetrics_2023_updated.csv which is provided, MasterSites_WithTempMetrics_20132023.csv which can be created using OceanographicProfiles_LiveOceanTempMay2025_Final, asterSites_WithSalinityMetrics_20132018.csv which can be created using OceanographicProfiles_20132018_NOAASaltJune2025_Final, MasterSites_WithNO3Metrics_20132023.csv which can be created using OceanographicProfiles_LiveOceanNitrate_20132023_Final, MasterSites_WithWaveMetrics_20132020.csv which can be created using OceanographicProfiles_NRELWaveMay2025_Final, MasterSites_WithKd490Metrics_20132023.csv which can be creating using OceanographicProfiles_NASAKd490_20132023_Final, and MasterSites_WithKWKelpChangeMetrics_201013to202023_updated.csv which can be created using KelpChangeProfile_KW_Dec2025_Final.ipynb.
-	SiteLocationandNumber_Table_April2026_Final - Requires MasterTrans_WithBiologicalMetrics_2023_updated.csv which is provided.



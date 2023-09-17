## @Organization: CNAM / UCSD
## @Project: Relationship between Flooding and Vaccination in Bangladesh
## @Author: Emilie Schwarz, emilie.schwarz@edu.ehesp.fr
## @Description: This code runs the merging of the PSU-specific flood exposure and DHS individual data
## @Date: March 13, 2023

#------------------------------------------------------------------------------------
## Source configuration code 
library(here)
source(here("R/Configuration.R"))
#------------------------------------------------------------------------------------

## Loading df1
load(here("data/dhs/DHS_data.RData"))

## Reading in data for psu
df2 <- readRDS(here("data/final", "PSU_flooded"))
df6 <- readRDS(here("data/final", "nonbuffer_PSU_flooded"))

# Bangladesh admin
BGD_Adm <- gadm( country = "BGD", level=0, path= here("data/untouched/country-admin"), version="latest", resolution=1)
BGD_Adm2 <- gadm( country = "BGD", level=2, path= here("data/untouched/country-admin2"), version="latest", resolution=1)

# Flood
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))

BGD_Adm_spdf = as.data.frame(BGD_Adm)
flood_spat = rast(flood_area_percent)

flood_area_percent_mask = terra::mask(x=flood_spat, mask=BGD_Adm, inverse=FALSE, updatevalue=NA, touches=TRUE)

## Creating a new binary variable that shows flooding-prone or not
#plot(flood_area_percent_masknew>.05)
df2$flooded_prone_francois = if_else(df2$layer>0, 1, 0)
df2$flooded_prone = if_else(df2$layer>=.04755,1,0)
df2$flooded_prone02 = if_else(df2$layer>.02, 1, 0)
df2$flooded_prone10 = if_else(df2$layer>.1, 1, 0)

#-----------------------------------------------------------------------------------

# Creating a unique variable name for PSU and SurveyID in the PSU dataset 
df2$psu_id = paste0(df2$SurveyId, df2$psu)
df6$psu_id = paste0(df6$SurveyId, df6$psu)

# Creating a unique variable name for PSU and SurveyID in the DH dataset
df1$psu_id = paste0(df1$SurveyId, df1$psu)

#------------------------------------------------------------------------------------

# Merging the flooded exposure into the individual data 

#df1$flooded_prone = df2$flooded_prone[match(df2$psu_id, df1$psu_id)]

df0 = df2 %>% select(!c("SurveyId", "psu", "ID"))

df3 = df1 %>%
  full_join(df0, by = "psu_id")

df5 = df6 %>% select(!c("SurveyId", "psu", "ID"))
df4 = df1 %>% full_join(df5, by = "psu_id")

df3 = df3 %>%
  mutate(edu.level = fct_relevel(edu.level, 
                                 "no education", "primary", "secondary", 
                                 "higher"), 
         wealth = fct_relevel(wealth, 
                              "poorest", "poorer", "middle", 
                              "richer", "richest")
  )

df3$SurveyId <- recode_factor(df3$SurveyId, 
                              BD2000DHS = "2000",
                              BD2004DHS = "2004", 
                              BD2007DHS = "2007", 
                              BD2011DHS = "2011", 
                              BD2014DHS = "2014", 
                              BD2017DHS = "2017")
df3$flooded_prone <- recode_factor(df3$flooded_prone, "0" = "No", 
                                   "1" = "Yes")

df3 = apply_labels(df3,
                   age.mother = "Age of mother",
                   wealth = "Wealth category",
                   hh.size = "Household size",
                   residence = "Urbanity",
                   sex = "Sex of child",
                   allbirths = "Number of Births",
                   FIC = "Fully immunized",
                   flooded_prone = "Zone prone to flooding",
                   edu.level = "Education level", 
                   OPV.vac = "OPV Vaccine", 
                   DTP.vac = "DTP Vaccine", 
                   MCV.vac = "MCV Vaccine", 
                   BCG.vac= "BCG Vaccine", 
                   bord = "Birth Order Number")
# Saving final dataset 
saveRDS(df3,
        file = here("~/Desktop/M2 Internship/floods_vaccination/data/final", "df3"))
saveRDS(df4,
        file = here("~/Desktop/M2 Internship/floods_vaccination/data/final", "df4"))

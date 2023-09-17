## @Organization: CNAM / UCSD
## @Project: Relationship between Flooding and Vaccination in Bangladesh
## @Author: Emilie Schwarz, emilie.schwarz@edu.ehesp.fr
## @Description: This code sets up all the needed data for the rest of the analysis

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("R/Configuration.R"))

#-------------------------------------------------------------------------------

# Loading DF3
df3 <- readRDS(here("data/final", "df3"))
df4 <- readRDS(here("data/final", "df4"))
#-------------------------------------------------------------------------------

# Flood data 
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))
BGD_Adm <- gadm( country = "BGD", level=0, path= here("data/untouched/country-admin1"), version="latest", resolution=1)

BGD_Adm_spdf = as.data.frame(BGD_Adm)
flood_spat = rast(flood_area_percent)
flood_area_percent_mask = terra::mask(x=flood_spat, mask=BGD_Adm, inverse=FALSE, updatevalue=NA, touches=TRUE)
flood_area_percent_mask_2 <- 100*flood_area_percent_mask


# Pulling district and division map from Bangladesh package
country <- get_map("country")
division <- get_map("division")
district <- get_map("district")
upazila <- get_map("upazila")
union <- get_map("union")

#-------------------------------------------------------------------------------

# Changing the factors to lowercase before merging with summarized data df3

country$Country<-tolower(country$Country)
division$Division <-tolower(division$Division)
district$District <-tolower(district$District)
upazila$Upazila <-tolower(upazila$Upazila)
union$Union <-tolower(union$Union)

#-------------------------------------------------------------------------------
# Adding variables to df3 into a data frame for grouping by sub region later

df3$Non_FIC = if_else(df3$FIC==0, 1,0)
df3$Non_BCG = if_else(df3$BCG.vac==0, 1, 0)
df3$Non_DTP = if_else(df3$DTP.vac==0, 1, 0)
df3$Non_DTP1 = if_else(df3$DTP1.vac==0, 1,0)
df3$Non_DTP2 = if_else(df3$DTP2.vac==0, 1,0)
df3$Non_DTP3 = if_else(df3$DTP3.vac==0, 1,0)
df3$Non_OPV = if_else(df3$OPV.vac==0, 1,0)
df3$Non_OPV1 = if_else(df3$OPV1.vac==0, 1,0)
df3$Non_OPV2 = if_else(df3$OPV2.vac==0, 1,0)
df3$Non_OPV3 = if_else(df3$OPV3.vac==0, 1,0)
df3$Non_MCV = if_else(df3$MCV.vac==0, 1,0)

#-------------------------------------------------------------------------------

# Since the df3 right now has no data on the ADM2, have to create a new dataframe

# Loading admin 2
# BGD_Adm2 <- raster::getData("GADM",
#                             country = "BGD",
#                             level = 2,
#                             path = here("data/untouched/country-admin"))

BGD_Adm2 <- gadm( country = "BGD", level=2, path= here("data/untouched/country-admin1"), version="latest", resolution=1)


# Create sf object with DF3
df3_sf <- df3 %>%
  sf::st_as_sf(coords = c("LONGNUM", "LATNUM"), crs = 4326)
class(df3_sf) # Verifying that the data is SF data

# Create sf object with the Admin files
adm_sf <- BGD_Adm2 %>%
  sf::st_as_sf(coords = coords, crs = 4326)
class(adm_sf) # Verifying that the data is SF data 

# Merging administrative file and point file 
df3_adm <- df3_sf%>%
  # join the administrative boundary file to the linelist, based on spatial intersection
  sf::st_join(adm_sf)

# Making the file a dataframe 
df3_adm_df = as.data.frame(df3_adm)

#-------------------------------------------------------------------------------

# Group by subregion
Subregion_Sum = df3_adm_df %>% group_by(NAME_2) %>% summarize (
  FIC = stats::weighted.mean(FIC, na.rm=TRUE, w=wt)*100, 
  BCG = stats::weighted.mean(BCG.vac,na.rm=TRUE, w=wt)*100, 
  DTP = stats::weighted.mean(DTP.vac, na.rm=TRUE, w=wt)*100,
  DTP1 = stats::weighted.mean(DTP1.vac, na.rm=TRUE, w=wt)*100,
  DTP2 = stats::weighted.mean(DTP2.vac, na.rm=TRUE, w=wt)*100,
  DTP3 = stats::weighted.mean(DTP3.vac, na.rm=TRUE, w=wt)*100,
  OPV = stats::weighted.mean(OPV.vac, na.rm=TRUE, w=wt)*100, 
  OPV1 = stats::weighted.mean(OPV1.vac, na.rm=TRUE, w=wt)*100, 
  OPV2 = stats::weighted.mean(OPV2.vac, na.rm=TRUE, w=wt)*100,
  OPV3 = stats::weighted.mean(OPV3.vac, na.rm=TRUE, w=wt)*100,
  MCV = stats::weighted.mean(MCV.vac, na.rm=TRUE, w=wt)*100,
  
  
  nonFIC = stats::weighted.mean(Non_FIC, na.rm=TRUE, w=wt)*100, 
  nonBCG = stats::weighted.mean(Non_BCG,na.rm=TRUE, w=wt)*100, 
  nonDTP = stats::weighted.mean(Non_DTP, na.rm=TRUE, w=wt)*100,
  nonDTP1 = stats::weighted.mean(Non_DTP1, na.rm=TRUE, w=wt)*100,
  nonDTP2 = stats::weighted.mean(Non_DTP2, na.rm=TRUE, w=wt)*100,
  nonDTP3 = stats::weighted.mean(Non_DTP3, na.rm=TRUE, w=wt)*100,
  nonOPV = stats::weighted.mean(Non_OPV, na.rm=TRUE, w=wt)*100, 
  nonOPV1 = stats::weighted.mean(Non_OPV1, na.rm=TRUE, w=wt)*100, 
  nonOPV2 = stats::weighted.mean(Non_OPV2, na.rm=TRUE, w=wt)*100,
  nonOPV3 = stats::weighted.mean(Non_OPV3, na.rm=TRUE, w=wt)*100,
  nonMCV = stats::weighted.mean(Non_MCV, na.rm=TRUE, w=wt)*100,
  
  FIC_sum = sum(Non_FIC, na.rm = T), 
  BCG_sum = sum(Non_BCG, na.rm = T), 
  DTP_sum = sum(Non_DTP, na.rm = T),
  DTP1_sum = sum(Non_DTP1, na.rm = T),
  DTP2_sum = sum(Non_DTP2, na.rm = T),
  DTP3_sum = sum(Non_DTP3, na.rm = T),
  OPV_sum = sum(Non_OPV, na.rm = T), 
  OPV1_sum = sum(Non_OPV1, na.rm = T), 
  OPV2_sum = sum(Non_OPV2, na.rm = T),
  OPV3_sum = sum(Non_OPV3, na.rm = T),
  MCV_sum = sum(Non_MCV, na.rm = T)
)

# 6 mo 
Subregion_Sum_6mo = df3_adm_df%>% filter(age.months>=6) %>% group_by(NAME_2) %>% summarize (
  FIC = stats::weighted.mean(FIC, na.rm=TRUE, w=wt)*100, 
  BCG = stats::weighted.mean(BCG.vac, na.rm=TRUE, w=wt)*100, 
  DTP = stats::weighted.mean(DTP.vac, na.rm=TRUE, w=wt)*100,
  DTP1 = stats::weighted.mean(DTP1.vac, na.rm=TRUE, w=wt)*100,
  DTP2 = stats::weighted.mean(DTP2.vac, na.rm=TRUE, w=wt)*100,
  DTP3 = stats::weighted.mean(DTP3.vac, na.rm=TRUE, w=wt)*100,
  OPV = stats::weighted.mean(OPV.vac, na.rm=TRUE, w=wt)*100, 
  OPV1 = stats::weighted.mean(OPV1.vac, na.rm=TRUE, w=wt)*100, 
  OPV2 = stats::weighted.mean(OPV2.vac, na.rm=TRUE, w=wt)*100,
  OPV3 = stats::weighted.mean(OPV3.vac, na.rm=TRUE, w=wt)*100,
  MCV = stats::weighted.mean(MCV.vac, na.rm=TRUE, w=wt)*100,
  
  nonFIC = stats::weighted.mean(Non_FIC, na.rm=TRUE, w=wt)*100, 
  nonBCG = stats::weighted.mean(Non_BCG,na.rm=TRUE, w=wt)*100, 
  nonDTP = stats::weighted.mean(Non_DTP, na.rm=TRUE, w=wt)*100,
  nonDTP1 = stats::weighted.mean(Non_DTP1, na.rm=TRUE, w=wt)*100,
  nonDTP2 = stats::weighted.mean(Non_DTP2, na.rm=TRUE, w=wt)*100,
  nonDTP3 = stats::weighted.mean(Non_DTP3, na.rm=TRUE, w=wt)*100,
  nonOPV = stats::weighted.mean(Non_OPV, na.rm=TRUE, w=wt)*100, 
  nonOPV1 = stats::weighted.mean(Non_OPV1, na.rm=TRUE, w=wt)*100, 
  nonOPV2 = stats::weighted.mean(Non_OPV2, na.rm=TRUE, w=wt)*100,
  nonOPV3 = stats::weighted.mean(Non_OPV3, na.rm=TRUE, w=wt)*100,
  nonMCV = stats::weighted.mean(Non_MCV, na.rm=TRUE, w=wt)*100,
  
  FIC_sum = sum(Non_FIC, na.rm = T), 
  BCG_sum = sum(Non_BCG, na.rm = T), 
  DTP_sum = sum(Non_DTP, na.rm = T),
  DTP1_sum = sum(Non_DTP1, na.rm = T),
  DTP2_sum = sum(Non_DTP2, na.rm = T),
  DTP3_sum = sum(Non_DTP3, na.rm = T),
  OPV_sum = sum(Non_OPV, na.rm = T), 
  OPV1_sum = sum(Non_OPV1, na.rm = T), 
  OPV2_sum = sum(Non_OPV2, na.rm = T),
  OPV3_sum = sum(Non_OPV3, na.rm = T),
  MCV_sum = sum(Non_MCV, na.rm = T)
  
)
# 18 months  
Subregion_Sum_12mo = df3_adm_df%>% filter(age.months>=18) %>% group_by(NAME_2) %>% summarize ( # 15 months not 12 but havent changed yet for coding run ease
  FIC = stats::weighted.mean(FIC, na.rm=TRUE, w=wt)*100, 
  BCG = stats::weighted.mean(BCG.vac, na.rm=TRUE, w=wt)*100, 
  DTP = stats::weighted.mean(DTP.vac, na.rm=TRUE, w=wt)*100,
  DTP1 = stats::weighted.mean(DTP1.vac, na.rm=TRUE, w=wt)*100,
  DTP2 = stats::weighted.mean(DTP2.vac, na.rm=TRUE, w=wt)*100,
  DTP3 = stats::weighted.mean(DTP3.vac, na.rm=TRUE, w=wt)*100,
  OPV = stats::weighted.mean(OPV.vac, na.rm=TRUE, w=wt)*100, 
  OPV1 = stats::weighted.mean(OPV1.vac, na.rm=TRUE, w=wt)*100, 
  OPV2 = stats::weighted.mean(OPV2.vac, na.rm=TRUE, w=wt)*100,
  OPV3 = stats::weighted.mean(OPV3.vac, na.rm=TRUE, w=wt)*100,
  MCV = stats::weighted.mean(MCV.vac, na.rm=TRUE, w=wt)*100,
  
  nonFIC = stats::weighted.mean(Non_FIC, na.rm=TRUE, w=wt)*100, 
  nonBCG = stats::weighted.mean(Non_BCG,na.rm=TRUE, w=wt)*100, 
  nonDTP = stats::weighted.mean(Non_DTP, na.rm=TRUE, w=wt)*100,
  nonDTP1 = stats::weighted.mean(Non_DTP1, na.rm=TRUE, w=wt)*100,
  nonDTP2 = stats::weighted.mean(Non_DTP2, na.rm=TRUE, w=wt)*100,
  nonDTP3 = stats::weighted.mean(Non_DTP3, na.rm=TRUE, w=wt)*100,
  nonOPV = stats::weighted.mean(Non_OPV, na.rm=TRUE, w=wt)*100, 
  nonOPV1 = stats::weighted.mean(Non_OPV1, na.rm=TRUE, w=wt)*100, 
  nonOPV2 = stats::weighted.mean(Non_OPV2, na.rm=TRUE, w=wt)*100,
  nonOPV3 = stats::weighted.mean(Non_OPV3, na.rm=TRUE, w=wt)*100,
  nonMCV = stats::weighted.mean(Non_MCV, na.rm=TRUE, w=wt)*100,
  
  FIC_sum = sum(Non_FIC, na.rm = T), 
  BCG_sum = sum(Non_BCG, na.rm = T), 
  DTP_sum = sum(Non_DTP, na.rm = T),
  DTP1_sum = sum(Non_DTP1, na.rm = T),
  DTP2_sum = sum(Non_DTP2, na.rm = T),
  DTP3_sum = sum(Non_DTP3, na.rm = T),
  OPV_sum = sum(Non_OPV, na.rm = T), 
  OPV1_sum = sum(Non_OPV1, na.rm = T), 
  OPV2_sum = sum(Non_OPV2, na.rm = T),
  OPV3_sum = sum(Non_OPV3, na.rm = T),
  MCV_sum = sum(Non_MCV, na.rm = T)
  
)
# Making a matched variable
Subregion_Sum$District = Subregion_Sum$NAME_2
Subregion_Sum$District = as.factor(Subregion_Sum$District)
Subregion_Sum$District = tolower(Subregion_Sum$District)

# 6 mo 
Subregion_Sum_6mo$District = Subregion_Sum$NAME_2
Subregion_Sum_6mo$District = as.factor(Subregion_Sum$District)
Subregion_Sum_6mo$District = tolower(Subregion_Sum$District)
# 12 mo 
Subregion_Sum_12mo$District = Subregion_Sum$NAME_2
Subregion_Sum_12mo$District = as.factor(Subregion_Sum$District)
Subregion_Sum_12mo$District = tolower(Subregion_Sum$District)

# Subregion_sum is missing khagrachhari ***

#-------------------------------------------------------------------------------
# Making a matched variable
Subregion_Sum$District = Subregion_Sum$NAME_2
Subregion_Sum$District = as.factor(Subregion_Sum$District)
Subregion_Sum$District = tolower(Subregion_Sum$District)

# Subregion_sum is missing khagrachhari ***
#-------------------------------------------------------------------------------
# Joining the summarized vaccine data with the Bangladesh division data 
district_propvax <- district %>% 
  left_join(Subregion_Sum, by = "District") 

district_propvax6mo <- district %>% 
  left_join (Subregion_Sum_6mo, by = "District" ) 

district_propvax12mo <- district %>% 
  left_join (Subregion_Sum_12mo, by = "District" ) 

#------------------------------------------------------------------------------
# Creating survey object 
#------------------------------------------------------------------------------

# Creating a df3 with only those over 15 mo
df3_12mo = df3 %>% dplyr::filter(age.months>=15)

# Making psu_id a factor
df3$psu_id = as.factor(df3$psu_id)

# Strata, FPC, Weights, and Probs are optional arguments but can't have any missing values 
svy_dhs_floods <-
  survey::svydesign(
    id = df3$psu_id,
    weights = ~wt,
    data = df3
    #fpc = ~fpc
  )
# Only over 15 months
svy_dhs_floods_12mo <-
  survey::svydesign(
    id = df3_12mo$psu_id,
    weights = ~wt,
    data = df3_12mo
    #fpc = ~fpc
  )

#------------------------------------------------------------------------------
# Propensity score set up for R markdown
#------------------------------------------------------------------------------

# Creating the propensity scores
df3$residence = as.factor(df3$residence)
df3$sex = as.factor(df3$sex)
# Dropping the 1 observation that has NA for education 
df3 = df3 %>% filter(edu.level!="NA")

df3_match = df3 %>% dplyr::filter(age.months>=15) # For overall analysis (FIC only applies to >12mo)
df3_match_6mo = df3 %>% filter(age.months>=6) # For 6mo analysis 

# Creating our weights on our dataset that is only children over 1YO
W.out <- WeightIt::weightit(factor(flooded_prone) ~edu.level+ wealth +residence+sex+age.mother,
                            data = df3_match,
                            method = "ps",
                            estimand = "ATT",
                            s.weights = "wt")
df3_match$pscore = cobalt::get.w(W.out)

# Creating weights for our dataset that is all children 
W.out.all <- WeightIt::weightit(factor(flooded_prone) ~edu.level+ wealth +residence+sex+age.mother,
                                data = df3,
                                method = "ps",
                                estimand = "ATT",
                                s.weights = "wt")
df3$pscore = cobalt::get.w(W.out.all)

# Creating  weights for our dataset that is children over 6mo

W.out.6mo <- WeightIt::weightit(factor(flooded_prone) ~edu.level+ wealth +residence+sex+age.mother,
                                data = df3_match_6mo,
                                method = "ps",
                                estimand = "ATT",
                                s.weights = "wt")
df3_match_6mo$pscore = cobalt::get.w(W.out.6mo)

#------------------------------------------------------------------------------
# Under 12 months for descriptive tables
#------------------------------------------------------------------------------

# Creating a subset to put into a table for descriptive
Subregion_Sum_DT = Subregion_Sum_12mo %>% select(District, FIC, MCV, DTP, OPV)
Subregion_Sum_DT = Subregion_Sum_DT %>% mutate(across(where(is.numeric), round, digits=3)) 


# Cluster' s GPS
BDBR_2017_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2017-18_DHS_02212023_96_190074/BDGE7SFL",
                              "BDGE7SFL.shp"), quiet = TRUE)
BDBR_2014_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2014_DHS_02212023_97_190074/BDGE71FL",
                              "BDGE71FL.shp"), quiet = TRUE)
BDBR_2011_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2011_DHS_02212023_95_190074/BDGE61FL",
                              "BDGE61FL.shp"), quiet = TRUE)
BDBR_2007_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2007_DHS_02212023_95_190074/BDGE52FL",
                              "BDGE52FL.shp"), quiet = TRUE)
BDBR_2004_GPS <- st_read(here("data/untouched/dhs",
                              "BD_2004_DHS_02212023_95_190074/BDGE4JFL",
                              "BDGE4JFL.shp"), quiet = TRUE)
BDBR_2000_GPS <- st_read(here("data/untouched/dhs",
                              "BD_1999-00_DHS_02212023_96_190074/BDGE42FL",
                              "BDGE42FL.shp"), quiet = TRUE)

BDBR_GPS <- rbind(BDBR_2017_GPS,
                  BDBR_2014_GPS,
                  BDBR_2011_GPS,
                  BDBR_2007_GPS,
                  BDBR_2004_GPS,
                  BDBR_2000_GPS)

BDBR_GPS$Flooded <- extract(flood_area_percent_mask_2, BDBR_GPS)
BDBR_GPS_Flooded = BDBR_GPS %>% filter(Flooded$layer > 0)



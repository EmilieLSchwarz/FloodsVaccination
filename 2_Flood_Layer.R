
## @Organization: CNAM / UCSD
## @Project: Relationship between Flooding and Vaccination in Bangladesh
## @Author: Originally Francois Rerolle, re-use by Emilie Schwarz, emilie.schwarz@edu.ehesp.fr
## @Description: This code runs the creation of the exposure variable (flooding)
## @Date: March 10, 2023

#------------------------------------------------------------------------------------
## Source configuration code 
library(here)
source(here("R/Configuration.R"))

#------------------------------------------------------------------------------------

# Bangladesh admin
# BGD_Adm <- raster::getData("GADM",
#                            country = "BGD",
#                            level = 1,
#                            path = here("flood_data/untouched/country-admin"))
BGD_Adm <- gadm( country = "BGD", level=0, path= here("data/untouched/country-admin1"), version="latest", resolution=1)
BGD_Adm1 <- gadm( country = "BGD", level=1, path= here("data/untouched/country-admin1"), version="latest", resolution=1)
BGD_Adm2 <- gadm( country = "BGD", level=2, path= here("data/untouched/country-admin1"), version="latest", resolution=1)

## Uploading flood files ####
flood_jun_2002 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_1974_From_20020621_to_20020828",
                             "DFO_1974_From_20020621_to_20020828.tif"))
flood_jun_2003 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_2248_From_20030611_to_20031010",
                             "DFO_2248_From_20030611_to_20031010.tif"))
flood_jul_2004 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_2507_From_20040620_to_20041007",
                             "DFO_2507_From_20040620_to_20041007.tif"))
flood_jul_2007 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_3136_From_20070721_to_20071015",
                             "DFO_3136_From_20070721_to_20071015.tif"))
flood_jul_2010 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_3679_From_20100701_to_20100713",
                             "DFO_3679_From_20100701_to_20100713.tif"))
flood_oct_2010 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_3732_From_20101001_to_20101012",
                             "DFO_3732_From_20101001_to_20101012.tif"))
flood_jul_2016 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_4382_From_20160725_to_20160826",
                             "DFO_4382_From_20160725_to_20160826.tif"))
flood_aug_2017 <- stack(here("data/flood_data/untouched/floods",
                             "DFO_4508_From_20170810_to_20170826",
                             "DFO_4508_From_20170810_to_20170826.tif"))
#-------------------------------------------------------------------------------
# flood_area_percent_mask = terra::mask(x=flood_spat, mask=BGD_Adm, inverse=FALSE, updatevalue=NA, touches=TRUE)
flood_jun_2002_cropped <- terra::mask(x=flood_jun_2002, mask = BGD_Adm, inverse=FALSE, updatevalue=NA, touches=TRUE)
# Crop to Bangladesh #### 
flood_jun_2002_cropped <- crop(flood_jun_2002, BGD_Adm)
flood_jun_2003_cropped <- crop(flood_jun_2003, BGD_Adm)
flood_jul_2004_cropped <- crop(flood_jul_2004, BGD_Adm)
flood_jul_2007_cropped <- crop(flood_jul_2007, BGD_Adm)
flood_jul_2010_cropped <- crop(flood_jul_2010, BGD_Adm)
flood_oct_2010_cropped <- crop(flood_oct_2010, BGD_Adm)
flood_jul_2016_cropped <- crop(flood_jul_2016, BGD_Adm)
flood_aug_2017_cropped <- crop(flood_aug_2017, BGD_Adm)

# Append together to create layer of total number of days flooded
flood_area_duration <- (
  flood_jun_2002_cropped$duration +
    flood_jun_2003_cropped$duration +
    flood_jul_2004_cropped$duration +
    flood_jul_2007_cropped$duration +
    flood_jul_2010_cropped$duration +
    flood_oct_2010_cropped$duration +
    #flood_jul_2016_cropped$duration + # Missing data near confluence of bay
    flood_aug_2017_cropped$duration
)


# Calculate maximum possible number of days flooded
max_flood_duration <- (
  max(flood_jun_2002_cropped$duration[], na.rm = T) +
    max(flood_jun_2003_cropped$duration[], na.rm = T) +
    max(flood_jul_2004_cropped$duration[], na.rm = T) +
    max(flood_jul_2007_cropped$duration[], na.rm = T) +
    max(flood_jul_2010_cropped$duration[], na.rm = T) +
    max(flood_oct_2010_cropped$duration[], na.rm = T) +
    #max(flood_jul_2016_cropped$duration[], na.rm = T) +
    max(flood_aug_2017_cropped$duration[], na.rm = T)
)

# Extract percent of flooded days out of possible flooded days
flood_area_percent <- flood_area_duration / max_flood_duration

# Number of flooded event
flood_frequency <- (flood_jun_2002_cropped$flooded +
                      flood_jun_2003_cropped$flooded +
                      flood_jul_2004_cropped$flooded +
                      flood_jul_2007_cropped$flooded +
                      flood_jul_2010_cropped$flooded +
                      flood_oct_2010_cropped$flooded +
                      # flood_jul_2016_cropped$flooded + # Missing data near confluence of bay
                      flood_aug_2017_cropped$flooded
)


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(flood_area_percent,
        file = here("data/final", "flood_area_percent"))

saveRDS(flood_frequency,
        file = here("data/final", "flood_frequency"))


#-------------------------------------------------------------------------------
# Flood
flood_area_percent <- readRDS(here("data/final", "flood_area_percent"))
flood_area_percent_mask <- raster::mask(flood_area_percent, BGD_Adm)
flood_area_percent_mask_2 <- 100*flood_area_percent_mask

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


## convert the psu geographical data to spatial points data frame  
psu <- df1 %>% 
  dplyr::select(SurveyId, psu, LATNUM, LONGNUM) %>% 
  unique()

sp <- as.data.frame(psu)

coordinates(sp) <- c("LONGNUM","LATNUM")


plot (sp, add=T, col="red") 
plot (sp, add=T, lwd = 2)

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## OPTION 1: No buffer zone
## Extract the climate data for each PSU location
df6 <- raster::extract(flood_area_percent,          # raster layer (flood data in ratser format)
                       sp,           # SPDF with centroids for buffer
                       df=TRUE)      # return a dataframe 

sum(df6$layer>0, na.rm = T) # 660 flooded
sum(df6$layer==0, na.rm = T) # 1917 non flooded
 # Use these counts later to find the right amount of flooded/non flooded when using a buffer

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## OPTION 2: If you want to add buffer around the PSU locations
## NOTE: Select buffer size around the PSU; buff = 10000 (10 km)
buff = 10000

## Extract the climate data for each PSU location
df2 <- raster::extract(flood_area_percent,          # raster layer (flood data in ratser format)
                       sp,           # SPDF with centroids for buffer
                       buffer=buff,  # buffer size, units depend on CRS
                       fun=mean,     # what value to extract
                       na.rm=TRUE,
                       df=TRUE)      # return a dataframe 

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\



## Add the PSU details
df2 <- cbind(sp@data, df2)

## 
df6 <- cbind(sp@data, df6)

### Archive

## Convert to long format
# df3 <- df2 %>% 
#   dplyr::select(-c(ID))  %>% 
#   gather(key = date, value = var, -SurveyId, -psu)   %>%
#   mutate(date = as.Date(substring(date, 2), format = "%Y.%m.%d"),
#          year  = format(date, format = "%Y"),
#          month = format(date, format = "%m"),
#          day   =  format(date, format = "%d"))


saveRDS(df2,
        file = here("~/Desktop/M2 Internship/floods_vaccination/data/final", "PSU_flooded"))
saveRDS(df6,
        file = here("~/Desktop/M2 Internship/floods_vaccination/data/final", "nonbuffer_PSU_flooded"))

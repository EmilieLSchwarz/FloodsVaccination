###############################################################
###############################################################
#######                                                 ####### 
#######                   PROJECT:                      ####### 
#######      BANGLADESH - IMMUNIZATION AND FLOODING     ####### 
#######                                                 ####### 
#######         CODE: RETRIEVING THE DHS DATA           #######
###############################################################
###############################################################


## Contents:
##
##
##


## NOTE: make sure vaccination is assessed for appropriate age groups only
## DHS Guide to vaccination: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm

rm(list =ls())
library(dplyr)
library(foreign)
library(tidyverse)
library(data.table)
library(zoo)
library(psych)
library(lubridate)
library(rdhs) 
options(scipen=999)
options(digits=5)




################################################################################
## 1. Retrieve the DHS data 
################################################################################
## Set up your DHS credentials 
set_rdhs_config(email = "emilie.schwarz@edu.ehesp.fr",
                project = "Examining the relationship between flooding-prone areas and childhood vaccination in Bangladesh")


set_rdhs_config(email = "emilie.schwarz@edu.ehesp.fr",
                project = "Examining the relationship between flooding-prone areas and childhood vaccination in Bangladesh",
                config_path = "rdhs.json",
                cache_path = "dhs",
                global = FALSE)

## Make a list of surveys and download them
surveys <- dhs_datasets() %>% 
  dplyr::filter(SurveyType == "DHS") %>% 
  dplyr::filter(FileFormat == "Stata dataset (.dta)") %>% 
  dplyr::filter(FileType == "Children's Recode") %>% 
  dplyr::filter(CountryName == "Bangladesh") %>% 
  dplyr::filter(SurveyYear > 2000)



downloads <- get_datasets(surveys$FileName, reformat=TRUE, clear_cache = TRUE)
print(downloads)
#vec <- unlist(downloads)
#vec  

## Select relevant variables
vars = c(
  #maternal & household characteristics
  "caseid", "midx", "v001", "v002", "v003", "v005", "v006", "v007", "v008","v008a",
  "v012", "v016", "v017", "v021", "v023", "v024", "v025", "v040", "v104", "v106","v113",
  "v115", "v116", "v133", "v134", "v135", "v137", "v139", "v140", "v149", "v151", "v152", 
  "v155", "v157", "v158", "v159", "v160",  "v190", "v191","v201", "v438", "v445", "v467d", "v467b", 
  "v465","v502", "v632", "v704", "v705", "v716", "v717", "v208", "v501",
  #child characteristics and anthropometric measures
  "bidx", "bord", "b0", "b1", "b2" ,"b3", "b4", "b5", "b8", "b9", "b11", "b17", "b18", "b19", "b20",
  "hw70", "hw71", "hw72", "hw73", "hw1", "hw2", "hw3", "hw13", "hw16", "hw17", "hw18", "hw19", 
  #vaccines
  "h1", "h0", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h51", "h52", "h53", "h54", "h55", "h56", "h57", "h58", 
  #variables for recalculating the wealth index
  "v127", "v128", "v129", "v161", "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v153", "v136", "v745b"
)

questions <- search_variables(surveys$FileName, variables = vars,  reformat=TRUE)

## Extract the data (adding geographical covariates: add_geo = TRUE)
extract <- extract_dhs(questions, add_geo = T)

## Quick check 
head(extract[1])

df0 <- rbindlist(extract, use.names=TRUE, fill=TRUE) # Observations = 38456

## Keep only observations with valid GPS coordinates
df0 <- df0 %>% 
  filter(!is.na(LATNUM)) %>% 
  filter(!is.na(LONGNUM)) %>% 
  filter(LATNUM!=0 | LONGNUM!=0)  #LAT=0 and LONG=0 are missing coordinates  , now at 38414 (45246 with y2k)

unique(df0$SurveyId)

save.image(file='DHS_extract.RData')

rm(list =ls())

################################################################################
## 2. Harmonize the survey data 
################################################################################


load('DHS_extract.RData')

rm(list=setdiff(ls(), c("df0", "questions")))

#interview year missing for the 2007 survey 
aggregate(v007 ~ SurveyId, data=df0, function(x) {sum(is.na(x))}, na.action = NULL)


df1 <- df0 %>% 
  filter(b5 == "yes") %>% #keep observation only if child is alive
  dplyr::rename(psu           = v001, 
                hh.id         = v002,
                woman.id      = v003,
                intYr         = v007, #year of interview
                intMo         = v006, #month of interview
                intDay        = v016, #day of interview
                intCMC        = v008, #interview date in CMC format (century month code)
                birthYr       = b2,
                birthMo       = b1,
                birthCMC      = b3,
                wealth        = v190,
                age           = b8,   #age of the child measured in years 
                sex           = b4,
                birth.int     = b11,  #preceding birth interval - only for 2nd and higher order births
                children.u5   = v137, #number of children under 5 in the household
                allbirths     = v201, # all children ever born
                births        = v208, #number of births in the last 5 years
                age.mother    = v012,
                edu.level     = v106,
                edu.years     = v133,
                hh.head       = v151,
                hh.size       = v136, 
                region        = v024,
                residence     = v025,
                stratum       = v023,
                distance_facility = v467d,
                rcvd.BCG      = h2,
                rcvd.OPV0     = h0,
                rcvd.OPV1     = h4,
                rcvd.OPV2     = h6,
                rcvd.OPV3     = h8,
                rcvd.DTP1     = h3,
                rcvd.DTP2     = h5,
                rcvd.DTP3     = h7,
                rcvd.MCV      = h9,
                health.card   = h1) %>%
  #generate weights for the mother
  mutate(wt=v005/1000000) %>%   
  mutate(intMo = as.numeric(intMo)) %>% 
  mutate(intYr = as.numeric(intYr)) %>% 
  mutate(birthYr = as.numeric(birthYr)) %>% 
  mutate(birthMo = as.numeric(birthMo)) %>% 
  #interview year missing for 2007 wave
  mutate(intYr_new   = as.integer((intCMC - 1)/12)+1900) %>%
  mutate(intYr = ifelse(is.na(intYr), intYr_new, intYr)) %>% 
  #correct education level
  mutate(edu.level = ifelse(edu.level == "9", NA, edu.level)) %>% 
  #calculate the age of the child in months
  mutate(age.months = intCMC-birthCMC) %>% 
  #restrict the sample to children under 3 years of age (no info for older children for the latest survey wave for 2017)
  filter(age.months <= 60) %>% 
  #convert mother's age into 5-year age groups
  mutate(age.mother.gr = cut(age.mother, c(10, 14, 19, 24, 29, 34, 39, 44, 49, Inf), c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"), include.lowest=TRUE)) %>% 
  #household size grouped
  mutate(hh.size.gr = cut(hh.size, c(0, 3, 5, 7, Inf), c("1-3", "4-5", "6-7", "8+"), include.lowest=TRUE)) %>% 
  #usual resident at the place of interview
  mutate(resident = ifelse(v135=="visitor", 0, 1)) %>% 
  #filter to usual residents only
  filter(resident == 1) %>% 
  #marital status
  mutate(marital.status = ifelse(v501 == "not living together" | v501 == "divorced" | v501 == "no longer living together/separated", "divorced/separated", v501)) %>% 
  # Harmonize vaccination information
  mutate(BCG.vac   = ifelse(rcvd.BCG  == "vaccination date on card" | rcvd.BCG  == "reported by mother" | rcvd.BCG   == "vaccination marked on card" | rcvd.BCG == "vacc. date on card" | rcvd.BCG == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP1.vac  = ifelse(rcvd.DTP1 == "vaccination date on card" | rcvd.DTP1 == "reported by mother" | rcvd.DTP1  == "vaccination marked on card" | rcvd.DTP1 == "vacc. date on card" | rcvd.DTP1 == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP2.vac  = ifelse(rcvd.DTP2 == "vaccination date on card" | rcvd.DTP2 == "reported by mother" | rcvd.DTP2  == "vaccination marked on card" | rcvd.DTP2 == "vacc. date on card" | rcvd.DTP2 == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP3.vac  = ifelse(rcvd.DTP3 == "vaccination date on card" | rcvd.DTP3 == "reported by mother" | rcvd.DTP3  == "vaccination marked on card" | rcvd.DTP3 == "vacc. date on card" | rcvd.DTP3 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV0.vac  = ifelse(rcvd.OPV0 == "vaccination date on card" | rcvd.OPV0 == "reported by mother" | rcvd.OPV0  == "vaccination marked on card" | rcvd.OPV0 == "vacc. date on card" | rcvd.OPV0 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV1.vac  = ifelse(rcvd.OPV1 == "vaccination date on card" | rcvd.OPV1 == "reported by mother" | rcvd.OPV1  == "vaccination marked on card" | rcvd.OPV1 == "vacc. date on card" | rcvd.OPV1 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV2.vac  = ifelse(rcvd.OPV2 == "vaccination date on card" | rcvd.OPV2 == "reported by mother" | rcvd.OPV2  == "vaccination marked on card" | rcvd.OPV2 == "vacc. date on card" | rcvd.OPV2 == "vacc. marked on card", 1, 0)) %>% 
  mutate(OPV3.vac  = ifelse(rcvd.OPV3 == "vaccination date on card" | rcvd.OPV3 == "reported by mother" | rcvd.OPV3  == "vaccination marked on card" | rcvd.OPV3 == "vacc. date on card" | rcvd.OPV3 == "vacc. marked on card", 1, 0)) %>% 
  mutate(MCV.vac   = ifelse(rcvd.MCV  == "vaccination date on card" | rcvd.MCV  == "reported by mother" | rcvd.MCV   == "vaccination marked on card" | rcvd.MCV  == "vacc. date on card" | rcvd.MCV  == "vacc. marked on card", 1, 0)) %>% 
  mutate(DTP.vac = ifelse((DTP1.vac + DTP2.vac + DTP3.vac) == 3, 1, 0)) %>% 
  mutate(OPV.vac = ifelse((OPV1.vac + OPV2.vac + OPV3.vac) == 3, 1, 0)) %>% 
  # Generate a variable for fully immunized (FIC) and partially immunized (PIC) child
  mutate(FIC = ifelse((BCG.vac + DTP1.vac + DTP2.vac + DTP3.vac + OPV1.vac + OPV2.vac + OPV3.vac + MCV.vac) == 8 , 1, 0)) %>%
  mutate(FIC = ifelse(age.months<15, NA, FIC)) %>%
  mutate(nonFIC = ifelse(FIC== 1, 0, 1)) %>%
  # mutate(Not_FIC = ifelse(FIC==1,0,ifelse(FIC==0,1),FIC))%>%
  dplyr::select(SurveyId, psu, LATNUM, LONGNUM, hh.id, woman.id, wt, age.months, sex, bord, birthYr, allbirths, birthMo, birthCMC, intYr, intMo, intDay, intCMC, age.mother, age.mother.gr, edu.level, marital.status, wealth, residence, hh.size, distance_facility, hh.size.gr, region, BCG.vac, DTP1.vac, DTP2.vac, DTP3.vac, OPV0.vac, OPV1.vac, OPV2.vac, OPV3.vac, MCV.vac, DTP.vac, OPV.vac, FIC, nonFIC)



aggregate(FIC ~ SurveyId, data=df1, function(x) {mean(!is.na(x))}, na.action = NULL)



rm(list=setdiff(ls(), c("df1")))

save.image(file='DHS_data.RData')

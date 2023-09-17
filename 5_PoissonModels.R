## @Organization: CNAM / UCSD
## @Project: Relationship between Flooding and Vaccination in Bangladesh
## @Author: Emilie Schwarz
## @Description: This code runs the unajusted modified poisson regression exploring flooding-prone areas and vaccination
## @Date: March 14, 2023

#------------------------------------------------------------------------------------
## Source configuration code 
library(here)
source(here("R/Configuration.R"))
source(here("R/RMD/4_AnalysisSetUp.R"))
library(robust)
#------------------------------------------------------------------------------------

# unadjusted nonFIC
geemod_flood_nonfic = geeglm(nonFIC~flooded_prone, 
                             family = "poisson"(link="log"), 
                             id=psu_id,  # if you don't have a cluster id=1:nrow(df.wona)
                             corstr='exchangeable', 
                             data=subset(df3, age.months>=18))

tab_model(geemod_flood_nonfic,show.est = TRUE, show.se = TRUE)
summary(geemod_flood_nonfic)

# adjusted FIC
geemod_flood_adj = geeglm(nonFIC~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                          family = "poisson"(link="log"), 
                          id=psu_id,  
                          corstr='exchangeable', 
                          data=subset(df3, age.months>=18))
tab_model(geemod_flood_adj, show.intercept = FALSE)

#------------------------------------------------------------------------------------
# BCG
geemod_BCG = geeglm(Non_BCG~flooded_prone, 
                    family = "poisson"(link="log"), 
                    id=psu_id,  
                    corstr='exchangeable', 
                    data=df3)
tab_model(geemod_BCG, show.intercept = FALSE)

geemod_BCG_adj = geeglm(Non_BCG~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                        family = "poisson"(link="log"), 
                        id=psu_id,  
                        corstr='exchangeable', 
                        data=df3)
tab_model(geemod_BCG_adj, show.intercept = FALSE)


#------------------------------------------------------------------------------------
# OPV
geemod_OPV = geeglm(Non_OPV~flooded_prone, 
                    family = "poisson"(link="log"), 
                    id=psu_id,  
                    corstr='exchangeable', 
                    data=subset(df3, age.months>=6)) # OPV dose 3 given at 14 weeks
tab_model(geemod_OPV, show.intercept = FALSE)

geemod_OPV_adj = geeglm(Non_OPV~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                        family = "poisson"(link="log"), 
                        id=psu_id,  
                        corstr='exchangeable', 
                        data=subset(df3, age.months>=6)) # OPV dose 3 given at 14 weeks
tab_model(geemod_OPV_adj, show.intercept = FALSE)


geemod_OPV.1 = geeglm(Non_OPV1~flooded_prone, 
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=2)) # OPV dose 1 given at 6 weeks
tab_model(geemod_OPV.1, show.intercept = FALSE)

geemod_OPV.1_adj = geeglm(Non_OPV1~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                          family = "poisson"(link="log"), 
                          id=psu_id,  
                          corstr='exchangeable', 
                          data=subset(df3, age.months>=2)) # OPV dose 1 given at 6 weeks
tab_model(geemod_OPV.1_adj, show.intercept = FALSE)

geemod_OPV.2 = geeglm(Non_OPV2~flooded_prone, 
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=4)) # OPV dose 2 given at 10 weeks
tab_model(geemod_OPV.2, show.intercept = FALSE)

geemod_OPV.2_adj = geeglm(Non_OPV2~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                          family = "poisson"(link="log"), 
                          id=psu_id,  
                          corstr='exchangeable', 
                          data=subset(df3, age.months>=4)) # OPV dose 2 given at 10 weeks
tab_model(geemod_OPV.2_adj, show.intercept = FALSE)

geemod_OPV.3 = geeglm(Non_OPV3~flooded_prone, 
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=6)) # OPV dose 3 given at 14 weeks
tab_model(geemod_OPV.3, show.intercept = FALSE)

geemod_OPV.3adj = geeglm(Non_OPV3~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                         family = "poisson"(link="log"), 
                         id=psu_id,  
                         corstr='exchangeable', 
                         data=subset(df3, age.months>=6)) # OPV dose 3 given at 14 weeks
tab_model(geemod_OPV.3adj, show.intercept = FALSE)


#------------------------------------------------------------------------------------
# DTP
geemod_DTP = geeglm(Non_DTP~flooded_prone, 
                    family = "poisson"(link="log"), 
                    id=psu_id,  
                    corstr='exchangeable', 
                    data=subset(df3, age.months>=6)) # DTP dose 3 given at 14 weeks
tab_model(geemod_DTP, show.intercept = FALSE)

geemod_DTPadj = geeglm(Non_DTP~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                       family = "poisson"(link="log"), 
                       id=psu_id,  
                       corstr='exchangeable', 
                       data=subset(df3, age.months>=6)) # DTP dose 3 given at 14 weeks
tab_model(geemod_DTPadj, show.intercept = FALSE)

geemod_DTP.1 = geeglm(Non_DTP1~flooded_prone, 
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=2)) # DTP dose 1 given at 6 weeks
tab_model(geemod_DTP.1, show.intercept = FALSE)

geemod_DTP.1adj = geeglm(Non_DTP1~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                         family = "poisson"(link="log"), 
                         id=psu_id,  
                         corstr='exchangeable', 
                         data=subset(df3, age.months>=2)) # DTP dose 1 given at 6 weeks
tab_model(geemod_DTP.1adj, show.intercept = FALSE)

geemod_DTP.2 = geeglm(Non_DTP2~flooded_prone, 
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=4)) # DTP dose 2 given at 10 weeks
tab_model(geemod_DTP.2, show.intercept = FALSE)

geemod_DTP.2adj = geeglm(Non_DTP2~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                         family = "poisson"(link="log"), 
                         id=psu_id,  
                         corstr='exchangeable', 
                         data=subset(df3, age.months>=4)) # DTP dose 2 given at 10 weeks
tab_model(geemod_DTP.2adj, show.intercept = FALSE)

geemod_DTP.3 = geeglm(Non_DTP3~flooded_prone, 
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=6)) # DTP dose 3 given at 14 weeks
tab_model(geemod_DTP.3, show.intercept = FALSE)

geemod_DTP.3adj = geeglm(Non_DTP3~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                         family = "poisson"(link="log"), 
                         id=psu_id,  
                         corstr='exchangeable', 
                         data=subset(df3, age.months>=6)) # DTP dose 3 given at 14 weeks
tab_model(geemod_DTP.3adj, show.intercept = FALSE)



#------------------------------------------------------------------------------------
#MCV
geemod_MCV = geeglm(Non_MCV~flooded_prone, 
                    family = "poisson"(link="log"), 
                    id=psu_id,  
                    corstr='exchangeable', 
                    data=subset(df3, age.months>=18)) # MCV dose given at 15mo in bangladesh
tab_model(geemod_MCV, show.intercept = FALSE)

geemod_MCV = geeglm(Non_MCV~flooded_prone+age.mother+edu.level+wealth+residence+sex+bord, 
                    family = "poisson"(link="log"), 
                    id=psu_id,  
                    corstr='exchangeable', 
                    data=subset(df3, age.months>=18)) # MCV dose given at 15mo in bangladesh
tab_model(geemod_MCV, show.intercept = FALSE)


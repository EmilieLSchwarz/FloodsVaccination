## @Organization: CNAM / UCSD
## @Project: Relationship between Flooding and Vaccination in Bangladesh
## @Author: Emilie Schwarz, emilie.schwarz@edu.ehesp.fr
## @Description: This code runs the propensity models for the project

#------------------------------------------------------------------------------------
## Source configuration code 
library(here)
source(here("R/Configuration.R"))

#----------------------------------------------

# Models

# Running a modified poisson regression using the new data 
geemod_flood_ps = geeglm(Non_FIC~flooded_prone, 
                         family = "poisson"(link="log"), 
                         id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                         weights=pscore,
                         corstr='exchangeable', 
                         data=df3_match)

summary(geemod_flood_ps)
tab_model(geemod_flood_ps)



# stable weights 
geemod_flood_ps_stable = geeglm(Non_FIC~flooded_prone, 
                                family = "poisson"(link="log"), 
                                id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                weights=pscorestable,
                                corstr='exchangeable', 
                                data=df3_match)

summary(geemod_flood_ps_stable)
tab_model(geemod_flood_ps_stable)

#independence structure

geemod_flood_ps_ind = geeglm(Non_FIC~flooded_prone, 
                             family = "poisson"(link="log"), 
                             id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                             weights=pscore,
                             corstr='independence', 
                             data=df3_match)
tab_model(geemod_flood_ps_ind)


# BCG
BCG_geemod_flood_ps_adj = geeglm(Non_BCG~flooded_prone,
                                 family = "poisson"(link="log"), 
                                 id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                 weights=pscore,
                                 corstr='exchangeable', 
                                 data=df3) #Having to use this or it won't compute
tab_model(BCG_geemod_flood_ps_adj, show.intercept = TRUE)
summary(BCG_geemod_flood_ps_adj)

#stable 
BCG_geemod_flood_ps_adj_stable = geeglm(Non_BCG~flooded_prone,
                                        family = "poisson"(link="log"), 
                                        id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                        weights=pscorestable,
                                        corstr='exchangeable', 
                                        data=df3) #Having to use this or it won't compute
tab_model(BCG_geemod_flood_ps_adj_stable, show.intercept = TRUE)
summary(BCG_geemod_flood_ps_adj_stable)

# OPV
OPV_geemod_flood_ps_adj = geeglm(Non_OPV~flooded_prone,
                                 family = "poisson"(link="log"), 
                                 id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                 weights=pscore,
                                 corstr='exchangeable', 
                                 data=df3_match_6mo)
tab_model(OPV_geemod_flood_ps_adj, show.intercept = TRUE)
summary(OPV_geemod_flood_ps_adj)
OPV_geemod_flood_ps_adj$coefficients[2]

#stable 
OPV_geemod_flood_ps_adj_stable = geeglm(Non_OPV~flooded_prone,
                                        family = "poisson"(link="log"), 
                                        id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                        weights=pscorestable,
                                        corstr='exchangeable', 
                                        data=df3_match_6mo)
tab_model(OPV_geemod_flood_ps_adj_stable, show.intercept = TRUE)

#OPV 1 
OPV1_geemod_flood_ps_adj = geeglm(Non_OPV1~flooded_prone,
                                  family = "poisson"(link="log"), 
                                  id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                  weights=pscore,
                                  corstr='exchangeable', 
                                  data=df3_match_2mo)
tab_model(OPV1_geemod_flood_ps_adj, show.intercept = TRUE)
summary(OPV1_geemod_flood_ps_adj)

# stable 

OPV1_geemod_flood_ps_adj_stable = geeglm(Non_OPV1~flooded_prone,
                                         family = "poisson"(link="log"), 
                                         id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                         weights=pscorestable,
                                         corstr='exchangeable', 
                                         data=df3_match_2mo)
tab_model(OPV1_geemod_flood_ps_adj_stable, show.intercept = TRUE)


# OPV 2
OPV2_geemod_flood_ps_adj = geeglm(Non_OPV2~flooded_prone,
                                  family = "poisson"(link="log"), 
                                  id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                  weights=pscore,
                                  corstr='exchangeable', 
                                  data=df3_match_4mo)
tab_model(OPV2_geemod_flood_ps_adj, show.intercept = TRUE)
summary(OPV2_geemod_flood_ps_adj)

#stable 

OPV2_geemod_flood_ps_adj_stable = geeglm(Non_OPV2~flooded_prone,
                                         family = "poisson"(link="log"), 
                                         id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                         weights=pscorestable,
                                         corstr='exchangeable', 
                                         data=df3_match_4mo)
tab_model(OPV2_geemod_flood_ps_adj_stable, show.intercept = TRUE)

#DPT 
DTP_geemod_flood_ps_adj = geeglm(Non_DTP~flooded_prone,
                                 family = "poisson"(link="log"), 
                                 id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                 weights=pscore,
                                 corstr='exchangeable', 
                                 data=df3_match_6mo)
tab_model(DTP_geemod_flood_ps_adj, show.intercept = FALSE)

summary(DTP_geemod_flood_ps_adj)

#stable 

DTP_geemod_flood_ps_adj_stable = geeglm(Non_DTP~flooded_prone,
                                        family = "poisson"(link="log"), 
                                        id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                        weights=pscorestable,
                                        corstr='exchangeable', 
                                        data=df3_match_6mo)
tab_model(DTP_geemod_flood_ps_adj_stable, show.intercept = FALSE)

# DTP 1
DTP1_geemod_flood_ps_adj = geeglm(Non_DTP1~flooded_prone,
                                  family = "poisson"(link="log"), 
                                  id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                  weights=pscore,
                                  corstr='exchangeable', 
                                  data=df3_match_2mo)
tab_model(DTP1_geemod_flood_ps_adj, show.intercept = FALSE)

summary(DTP1_geemod_flood_ps_adj)

# stable 
DTP1_geemod_flood_ps_adj_stable = geeglm(Non_DTP1~flooded_prone,
                                         family = "poisson"(link="log"), 
                                         id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                         weights=pscorestable,
                                         corstr='exchangeable', 
                                         data=df3_match_2mo)
tab_model(DTP1_geemod_flood_ps_adj_stable, show.intercept = FALSE)

# DTP 2
DTP2_geemod_flood_ps_adj = geeglm(Non_DTP2~flooded_prone,
                                  family = "poisson"(link="log"), 
                                  id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                  weights=pscore,
                                  corstr='exchangeable', 
                                  data=df3_match_4mo)
tab_model(DTP2_geemod_flood_ps_adj, show.intercept = FALSE)

summary(DTP2_geemod_flood_ps_adj)

#stable 
DTP2_geemod_flood_ps_adj_stable = geeglm(Non_DTP2~flooded_prone,
                                         family = "poisson"(link="log"), 
                                         id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                         weights=pscorestable,
                                         corstr='exchangeable', 
                                         data=df3_match_4mo)
tab_model(DTP2_geemod_flood_ps_adj_stable, show.intercept = FALSE)

#MCV
MCV_geemod_flood_ps_adj = geeglm(Non_MCV~flooded_prone,
                                 family = "poisson"(link="log"), 
                                 id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                 weights=pscore,
                                 corstr='exchangeable', 
                                 data=df3_match)
tab_model(MCV_geemod_flood_ps_adj, show.intercept = FALSE)

summary(MCV_geemod_flood_ps_adj)

#stable 

MCV_geemod_flood_ps_adj_stable = geeglm(Non_MCV~flooded_prone,
                                        family = "poisson"(link="log"), 
                                        id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                                        weights=pscorestable,
                                        corstr='exchangeable', 
                                        data=df3_match)
tab_model(MCV_geemod_flood_ps_adj_stable, show.intercept = FALSE)


#----------------------------------------------

# Models with interactions

# residence
interaction1 = geeglm(nonFIC~flooded_prone*residence+edu.level+wealth+age.mother+bord+sex, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=subset(df3,age.months>=18))

summary(interaction1)
tab_model(interaction1)


# wealth
interaction2 = geeglm(nonFIC~flooded_prone*wealth+edu.level+residence+age.mother+bord+sex, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=df3)

summary(interaction2)
tab_model(interaction2)

# edu 
interaction3 = geeglm(nonFIC~flooded_prone*edu.level+wealth+residence+age.mother+bord+sex, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=df3)

summary(interaction3)
tab_model(interaction3)

# sex
interaction4 = geeglm(nonFIC~flooded_prone*sex+wealth+residence+edu.level+age.mother+bord, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=df3)

summary(interaction4)
tab_model(interaction4)

# birth order 
interaction5 = geeglm(nonFIC~flooded_prone*bord+wealth+residence+edu.level+age.mother+sex, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=df3)

summary(interaction5)
tab_model(interaction5)

# age cat
df3$age_cat = if_else((df3$age.months>=18 & df3$age.months<=24), "18-24", if_else(df3$age.months>24, ">24", NA))
interaction7 = geeglm(nonFIC~flooded_prone*age_cat+bord+wealth+residence+edu.level+age.mother+sex, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=18))

summary(interaction7)
tab_model(interaction7)

# age
interaction8 = geeglm(nonFIC~flooded_prone*age.mother+wealth+residence+edu.level+sex+bord, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=df3)

summary(interaction8)
tab_model(interaction8)

# year 
interaction9 = geeglm(nonFIC~flooded_prone*SurveyId+bord+wealth+residence+edu.level+age.mother+sex, 
                      family = "poisson"(link="log"), 
                      id=psu_id, # if you don't have a cluster id=1:nrow(df.wona)
                      weights=wt,
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=18))

summary(interaction9)
tab_model(interaction9)




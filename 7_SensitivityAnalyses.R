## @Organization: CNAM / UCSD
## @Project: Relationship between Flooding and Vaccination in Bangladesh
## @Author: Emilie Schwarz, emilie.schwarz@edu.ehesp.fr
## @Description: This code runs the sensitivity analysis 

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("R/Configuration.R"))

## Load data
df3 <- readRDS(here("data/final", "df3")) # Already has the flooded quartiles
df3 = df3 %>% filter(edu.level!="NA")
df3$psu_id = as.factor(df3$psu_id)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# FLOODING PRONE THRESHOLDS

#-------------------------------------------------------------------------------
# Looking at the threshold of .02 instead of .05 for flooding-prone vs not 

geemod_flood_adj_02 = geeglm(nonFIC~flooded_prone02+age.mother+edu.level+residence+sex+bord+wealth,
                             family = "poisson"(link="log"), 
                             id=psu_id,  
                             corstr='exchangeable', 
                             data=subset(df3, age.months>=18))
tab_model(geemod_flood_adj_02,show.est = TRUE, show.se = TRUE) 
#Using sj Plot where SE, CI and p-values are based on robust estimation

#-------------------------------------------------------------------------------
# Looking at the threshold of .1 instead of .05 for flooding-prone vs not 

geemod_flood_adj_10 = geeglm(nonFIC~flooded_prone10+age.mother+edu.level+residence+sex+bord+wealth,
                             family = "poisson"(link="log"), 
                             id=psu_id,  
                             corstr='exchangeable', 
                             data=subset(df3, age.months>=18))
tab_model(geemod_flood_adj_10,show.est = TRUE, show.se = TRUE) 
#Using sj Plot where SE, CI and p-values are based on robust estimation

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# Quartiles
df3$quantvar_all = as.factor(df3$quantvar_all)
quartile_all = geeglm(nonFIC~quantvar_all+age.mother+edu.level+residence+sex+bord+wealth,
                      family = "poisson"(link="log"), 
                      id=psu_id,  
                      corstr='exchangeable', 
                      data=subset(df3, age.months>=18))
tab_model(quartile_all)


#-------------------------------------------------------------------------------
# Vaccines and AGE
#-------------------------------------------------------------------------------

# Sensitivity analysis on age 
# Restricting to ages above 24 months of age 

model_over24mo = geeglm(nonFIC~flooded_prone+age.mother+edu.level+residence+sex+allbirths+wealth, 
                        family = "poisson"(link="log"), 
                        id=psu_id,  # if you don't have a cluster id=1:nrow(df.wona)
                        corstr='exchangeable', 
                        data=subset(df3, age.months>=24))

tab_model(model_over24mo,show.est = TRUE, show.se = TRUE)

under24 = df3 %>% filter(age.months<24)

model_under24mo = geeglm(nonFIC~flooded_prone+age.mother+edu.level+residence+sex+allbirths+wealth, 
                         family = "poisson"(link="log"), 
                         id=psu_id,  # if you don't have a cluster id=1:nrow(df.wona)
                         corstr='exchangeable', 
                         data=subset(under24, age.months>18))

tab_model(model_under24mo,show.est = TRUE, show.se = TRUE)

#-------------------------------------------------------------------------------
# MONSOON ANALYSIS 
#-------------------------------------------------------------------------------


# Sensitivity analysis on born during monsoon season
# Monsoon season according to world bank : june to october (85% of humidity for the year)

# Creating variable indicating if a child was born during the monsoon season 
df3$monsoon_birth = if_else((df3$birthMo==6|
                               df3$birthMo==7|
                               df3$birthMo==8|
                               df3$birthMo==9|
                               df3$birthMo==10), 1,0)


# Splitting the dataframe into 2 (monsoon born and not monsoon born), to see the effect of flooding prone regions in the two strata

monsoon_born = df3 %>% dplyr::filter(monsoon_birth==1)
not_monsoon_born = df3 %>% dplyr::filter(monsoon_birth==0)

# Running the FIC analysis on the two subsets 

# Monsoon born 
monsoon_model = geeglm(nonFIC~flooded_prone+age.mother+edu.level+residence+sex+bord+wealth,
                       family = "poisson"(link="log"), 
                       id=psu_id,  
                       corstr='exchangeable', 
                       data=subset(monsoon_born, age.months>=18))
tab_model(monsoon_model,show.est = TRUE, show.se = TRUE) #Using sj Plot where SE, CI and p-values are based on robust estimation


# Not Monsoon born 
non_monsoon_model = geeglm(nonFIC~flooded_prone+age.mother+edu.level+residence+sex+bord+wealth,
                           family = "poisson"(link="log"), 
                           id=psu_id,  
                           corstr='exchangeable', 
                           data=subset(not_monsoon_born, age.months>=18))
tab_model(non_monsoon_model,show.est = TRUE, show.se = TRUE) #Using sj Plot where SE, CI and p-values are based on robust estimation


# interaction between monsoon and flood prone in regular data 
monsoon_model_interaction = geeglm(nonFIC~flooded_prone*monsoon_birth+age.mother+edu.level+residence+sex+bord+wealth,
                                   family = "poisson"(link="log"), 
                                   id=psu_id,  
                                   corstr='exchangeable', 
                                   data=df3
)
tab_model(monsoon_model_interaction,show.est = TRUE, show.se = TRUE) #Using sj Plot where SE, CI and p-values are based on robust estimation



#-------------------------------------------------------------------------

family = "poisson"(link="log"), 
id=psu_id,  
corstr='exchangeable', 
data=subset(monsoon_born, age.months>=18))
tab_model(monsoon_model,show.est = TRUE, show.se = TRUE) #Using sj Plot where SE, CI and p-values are based on robust estimation


# Not Monsoon born 
non_monsoon_model = geeglm(nonFIC~flooded_prone+age.mother+edu.level+residence+sex+bord+wealth,
                           family = "poisson"(link="log"), 
                           id=psu_id,  
                           corstr='exchangeable', 
                           data=subset(not_monsoon_born, age.months>=18))
tab_model(non_monsoon_model,show.est = TRUE, show.se = TRUE) #Using sj Plot where SE, CI and p-values are based on robust estimation


# interaction between monsoon and flood prone in regular data 
monsoon_model_interaction = geeglm(nonFIC~flooded_prone*monsoon_birth+age.mother+edu.level+residence+sex+bord+wealth,
                                   family = "poisson"(link="log"), 
                                   id=psu_id,  
                                   corstr='exchangeable', 
                                   data=df3
)



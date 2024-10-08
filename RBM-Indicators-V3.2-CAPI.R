###########RMS Qv3.2 Indicator Calculations############
##########UNHCR#####CAPI#######
#########Author: Ilgi Bozdag

######################################################
#####Standard Scripts for RBM Indicators##############
######################################################

##Date: October 2024
##At this step, you should already have main and ind datasets 
###structured in a way that will allow you to calculate your indicators as you will be guided in this document
###It's not recommended to clear your work space if you have your main and ind datasets loaded

#### Load packages ( if you have not already in the first step)

##Remove past activities

rm(list = ls())

####Load libraries and data ----

# Install pacman if not already installed
if(!require(pacman)) install.packages('pacman')

# Install the unhcrthemes package from GitHub
install.packages("remotes")
remotes::install_github("unhcr/unhcrthemes")


# Load all required libraries using pacman
pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl )




###Create function that turn character values into numeric if you imported from KoBO
##This is important as it will be used frequently

labelled_chr2dbl <- function(x) {
  varlab <- var_label(x)
  vallab <- val_labels(x)
  vallab <- setNames(as.numeric(vallab),
                     names(vallab))
  x <- as.numeric(as.character(x))
  var_label(x) <- varlab
  val_labels(x) <- vallab
  x
}


###Add labels for disaggregation variables

###Age - HH07_cat

table(ind$HH07_cat) # 4 categories
table(ind$HH07_cat2) # under 18 / above 18

##Disability - disability

table(ind$disability)

###Gender - HH04 -- already labelled 

table(ind$HH04)


###Population groups

table(ind$pop_groups)
table(main$pop_groups)


#Core Impact Indicators ----
###2.2 Proportion of people residing in physically safe and secure settlements with access to basic facilities -----
##Module :	LIGHT01-LIGHT03 (9.2) + HEA01-HEA03 (health) +  DWA01-DWA04 (12.1) + SHEL01-SHEL06 and RISK01-RISK02 and DWE05 (9.1)


###All variables for this indicators are in the main dataset both for CAPI and CATI

##This indicator aims to measure the proportion of forcibly displaced and stateless people that 
##reside in safe and secure settlements with access to basic facilities such as 
##shelter, WASH, energy and security from natural hazards


###Step.1. Electricity


##Here we will create a binary variable if they use anything for lighting (LIGHT01), 
##and the light source for most of the time is electricity (LIGHT02) - 
##exclude cases if they selected LIGHT03 - 0 ( no electricity)

table(main$LIGHT01)
table(main$LIGHT02)



main <- main %>%
  mutate(electricity = ifelse(LIGHT01 == "1" & LIGHT02 == "1" & LIGHT03 != "0", 1, 0)
  ) %>%
  mutate( electricity = labelled(electricity,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "Access to electricity"))

table(main$electricity)

###Step2. Healthcare

###Access to healthcare if household has any facility available excluding 'don't know' and 'other' 
#within one hour distance (cannot be > 60) (walking or any other type of transport)



main <- main %>%
  mutate(healthcare = ifelse(HEA01 != "96" & HEA01 != "98" & HEA03 <= 60, 1, 0)
  ) %>%
  mutate( healthcare = labelled(healthcare,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Access to healthcare facility"))

table(main$healthcare)


###Step3. Drinking water

###Access to drinking water is calculated as below
##use improved sources of drinking water in their housing or within 30 minutes round trip collection time


###Convert time variable to minutes only


main <- main %>%
  mutate(time_DWA=case_when(
    DWA03a=="1"~ "1", DWA03a=="2"~"60") #convert hour into minutes
  )

main$time_DWA <- as.numeric(main$time_DWA)

table(main$time_DWA)

###Compute variable with above conditions

main <- main %>%
  mutate(time_tot=time_DWA*DWA03b
  ) %>% 
  mutate(dwa_cond1=case_when( time_tot > 30 ~ 0, 
                              TRUE ~ 1) # reachable under 30 minutes or NA
  ) %>% 
  mutate(dwa_cond2=case_when(DWA01!="7" |DWA01 !="9" |DWA01 != "13" | DWA01 != "96" |DWA01 !="98" ~ 1,
                             TRUE ~ 0) # improved source only
  ) %>%
  mutate(dwa_cond3=case_when(DWA02 == "3" ~ 0, 
                             TRUE ~ 1) # in the dwelling/yard/plot
  ) %>% 
  mutate(drinkingwater=case_when(
    ((dwa_cond1==1 | dwa_cond3==1) & dwa_cond2==1 ) ~ 1, TRUE ~ 0)
  ) %>%
  mutate(drinkingwater = labelled(drinkingwater,
                                  labels = c(
                                    "Yes" = 1,
                                    "No" = 0
                                  ),
                                  label = "Access to drinking water"))

table(main$drinkingwater)


###Step 4. Habitable housing

##Condition 1

##Classify as habitable for below conditions - if 98 selected, put into missing

##First check the variables

table(main$SHEL01)
table(main$SHEL02)
table(main$SHEL03)
table(main$SHEL04)
table(main$SHEL05)
table(main$SHEL06)


main <- main %>%
  mutate(across(starts_with("SHEL"), ~ifelse(. == "98", NA, .))) %>%
  mutate(housing = case_when(
    (SHEL01 == "1") & (SHEL02 == "1") & (SHEL05 == "1") & 
      (SHEL03 == "0" ) & (SHEL04 == "0" ) & (SHEL06 == "0" ) ~ 1,
    (SHEL01 == "0") | (SHEL02 == "0" ) | (SHEL05 == "0") |
      (SHEL03 == "1") | (SHEL04 == "1") | (SHEL06 == "1" ) ~ 0,
    TRUE ~ NA_integer_
  ))

table(main$housing)


##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus not considered as physically safe


table(main$hh_size_001)
table(main$DWE05)


main <- main %>%
  mutate(crowding=hh_size_001/DWE05
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 0)
  )


table(main$crowding)
table(main$dwe05_cat)


###Combine both conditions for habitable housing -- exclude DWE01

main <- main %>%
  mutate(shelter=case_when(
    dwe05_cat==1 & housing==1 ~ 1,
    TRUE ~ 0
  ))

table(main$shelter)



##Step 5. Safe and secure settlements are those with no risks and hazards like flooding, landslides, 
###landmines, and close proximity to military installations and hazardous zones

table(main$RISK01)
table(main$RISK02)


main <- main %>%
  mutate(secure=case_when(
    RISK01=="1" |  RISK02=="1" ~ 0,
    TRUE ~ 1
  ))

table(main$secure)


##Step 6. Combine all services

###Calculate impact indicator based on electricity, healthcare, drinkingwater, shelter and secure

##Impact 2.2 is "1" if all services above are accessible

main <-main %>%
  mutate(impact2_2=case_when(
    shelter==0 | electricity==0 | drinkingwater==0 | healthcare==0 | secure==0 ~ 0,
    shelter==1 & electricity==1 & drinkingwater==1 & healthcare==1 & secure==1 ~ 1)
  ) %>% 
  mutate(impact2_2=labelled(impact2_2,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people residing in physically safe and secure settlements with access to basic facilities"))


table(main$impact2_2)



####Rerun to have the dataset with indicators 

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )



####Standard tables 


composite_impact2_2 <- main %>%
  select(pop_groups, shelter, electricity, drinkingwater, secure, healthcare, impact2_2) %>%
  pivot_longer(cols = shelter:healthcare, names_to = "facility", values_to = "access") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(access, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 


composite_impact2_2 %>%
  ggplot(aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_unhcr_d() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(
    title = "Access to Facilities by Population Group",
    x = "Facility",
    y = "Percentage Access (%)",
    fill = "Population Groups"
  ) +
  theme_unhcr()

##Table by population groups

impact2_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "impact2_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(impact2_2, vartype = c("ci", "se"), , na.rm = TRUE),  # indicator value ( weighted) with CI and SE
  )




###Chart of impact 2_2 by pop groups

ggplot(impact2_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Impact 2.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###2.3 Proportion of people with access to health services -----
##Module :HACC01 - HACC04

table(ind$HACC01) ## Needed to see a health professional for any reason
table(ind$HACC02) ## the reason for seeking care
table(ind$HACC03) ## did receive the needed health care
table(ind$HACC04) ## if not, what are the reasons for not receiving the health care


##Calculate those who needed and accessed health services

ind <- ind %>%
  mutate(impact2_3=case_when(
    HACC01=="1" & HACC03=="1" ~ 1,
    HACC01=="0" ~ NA,
    HACC01=="1" & HACC03=="0" & (HACC04_1 == "1" | HACC04_2 == "1" | HACC04_4 == "1" | HACC04_7 == "1" | HACC04_10 == "1" | HACC04_11 == "1" | 
                                   HACC04_12 == "1" | HACC04_13 == "1") ~ 0 ,
    HACC01=="1" & HACC03=="0" & (HACC04_3 == "1" | HACC04_5 == "1" | HACC04_6 == "1" | HACC04_8 == "1" | 
                                   HACC04_9 == "1" | HACC04_96 == "1") ~ 1)
  ) %>%
  mutate(impact2_3=labelled(impact2_3,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people with access to health services"))

###Descriptives

RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


###Table by population groups

impact2_3 <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "impact2_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(impact2_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above




ggplot(impact2_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Impact 2.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity



impact2_3_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat)) %>%                     # Exclude if HH07_cat is NA
  group_by(HH07_cat, HH04, disability) %>%            # Group by Age, Gender, and Disability
  summarise(                                       # Summarise to compute values
    var_name = "impact2_3",                        # Name of the variable
    num_obs_uw = unweighted(n()),                  # Unweighted total count
    denominator = survey_total(),                  # Weighted total count
    mean_value = survey_mean(impact2_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


####Chart with the AGD variables 

ggplot(impact2_3_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_unhcr_d() +  # UNHCR color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Impact 2.3 by gender, age and disability status",
    x = "Gender",
    y = "Mean Value",
    fill = "Disability",
    caption = "Note: The disability module does not include children under 5."  #
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


#### The reasons for not being able to access to health services 

##add labels for not accessing the health services

reasons_mapping <- c(
  "HACC04_1" = "Health facility too far",
  "HACC04_2" = "Medicine or health facility too expensive",
  "HACC04_3" = "No treatment exists/Not necessary",
  "HACC04_4" = "Don't know where to go",
  "HACC04_5" = "No time",
  "HACC04_6" = "Prefer other options",
  "HACC04_7" = "Health facility does not accept new patients",
  "HACC04_8" = "Don't trust modern medicine",
  "HACC04_9" = "Don't trust doctors",
  "HACC04_10" = "Administrative/documentation issues (certificates, service cards, etc.)",
  "HACC04_11" = "Long waiting times",
  "HACC04_12" = "Lack of medical supplies",
  "HACC04_13" = "Health facility damaged/destroyed"
)

hacc_percentages <- ind %>%
  summarise(
    HACC04_1 = mean(HACC04_1 == 1, na.rm = TRUE) * 100,
    HACC04_2 = mean(HACC04_2 == 1, na.rm = TRUE) * 100,
    HACC04_3 = mean(HACC04_3 == 1, na.rm = TRUE) * 100,
    HACC04_4 = mean(HACC04_4 == 1, na.rm = TRUE) * 100,
    HACC04_5 = mean(HACC04_5 == 1, na.rm = TRUE) * 100,
    HACC04_6 = mean(HACC04_6 == 1, na.rm = TRUE) * 100,
    HACC04_7 = mean(HACC04_7 == 1, na.rm = TRUE) * 100,
    HACC04_8 = mean(HACC04_8 == 1, na.rm = TRUE) * 100,
    HACC04_9 = mean(HACC04_9 == 1, na.rm = TRUE) * 100,
    HACC04_10 = mean(HACC04_10 == 1, na.rm = TRUE) * 100,
    HACC04_11 = mean(HACC04_11 == 1, na.rm = TRUE) * 100,
    HACC04_12 = mean(HACC04_12 == 1, na.rm = TRUE) * 100,
    HACC04_13 = mean(HACC04_13 == 1, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = "Reason", 
               values_to = "Percentage") %>%
  mutate(Reason = reasons_mapping[Reason])  # Map column names to descriptive labels



##Chart creation


ggplot(hacc_percentages, aes(x = reorder(Reason, Percentage), y = Percentage, fill = Reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Reasons for Not Accessing Health Services",
    x = "Reason",
    y = "Percentage",
    caption = "Note: Percentages calculated independently for each reason."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )



###3.2a: Proportion of children and young people enrolled in primary education -----
##Module :EDU01-EDU04



ind <- ind %>% 
  mutate(
    edu_primary = case_when(
      EDU01 == "1" & EDU02 == "1" & EDU03 == "2" ~ 1,  
      EDU01 == "0" | EDU02 == "0" ~ 0,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    age_primary = case_when(
      HH07 >= "6" & HH07 <= "13" ~ 1,  ###ADJUST AGE GROUPS FOR PRIMARY LEVEL ( 6- 13 in this ex)
      TRUE ~ 0
    )
  )


###Results of the indicator table

RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


###You can check official statistics here: https://data.uis.unesco.org/index.aspx?queryid=3847


##Table 

impact3_2a <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop_groups is NA
  group_by(pop_groups) %>%                          # Group by pop_groups
  summarise(
    var_name = "impact3_2a",                        # Name of the variable
    num_obs_uw = survey_total(!is.na(age_primary), na.rm = TRUE),  # Unweighted total count
    denominator = survey_total(edu_primary, na.rm = TRUE),         # Weighted total count
    edu_primary_total = survey_total(edu_primary, na.rm = TRUE),   # Total for edu_primary
    age_primary_total = survey_total(age_primary, na.rm = TRUE)    # Total for age_primary
  ) %>%
  mutate(
    mean_value = round(edu_primary_total / age_primary_total, 4)  # Calculate the mean value
  )


###Table with gender and disability included for the chart



impact3_2a_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups) & !is.na(disability) & !is.na(HH04)) %>%                     # Exclude if pop_groups is NA
  group_by(pop_groups,disability,HH04) %>%                          # Group by pop_groups
  summarise(
    var_name = "impact3_2a",                        # Name of the variable
    num_obs_uw = survey_total(!is.na(age_primary), na.rm = TRUE),  # Unweighted total count
    denominator = survey_total(edu_primary, na.rm = TRUE),         # Weighted total count
    edu_primary_total = survey_total(edu_primary, na.rm = TRUE),   # Total for edu_primary
    age_primary_total = survey_total(age_primary, na.rm = TRUE)    # Total for age_primary
  ) %>%
  mutate(
    mean_value = round(edu_primary_total / age_primary_total, 4)  # Calculate the mean value
  )



###Chart for the above table


ggplot(impact3_2a_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each pop_group
  labs(
    title = "Enrollment in primary education by Age, Gender, and Population Groups",
    x = "Gender ",
    y = "Proportion of Education Level",
    fill = "Disability Status"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )



###3.2b: Proportion of children and young people enrolled in secondary education -----
##Module :EDU01-EDU04

#Turn character variables into vector


###This indicator comes from the individual dataset

###Include if they are attending secondary or secondary -technical and vocational

ind <- ind %>%
  mutate(
    edu_secondary = case_when(
      EDU01 == "1" & EDU02 == "1" & (EDU03 == "3" | EDU03 == "4") ~ 1,
      EDU01 == "0" | EDU02 == "0" ~ 0,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    age_secondary = case_when(
      HH07 >= "11" & HH07 <= "18" ~ 1,  # ADJUST THE SCHOOL AGE FOR SECONDARY ( 11 to 18 in this ex)
      TRUE ~ NA_real_  # NA_real_ for missing numeric values
    )
  )


###Results of the indicator table

RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

impact3_2b <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop_groups is NA
  group_by(pop_groups) %>%                          # Group by pop_groups
  summarise(
    var_name = "impact3_2b",                        # Name of the variable 
    num_obs_uw = survey_total(!is.na(age_secondary), na.rm = TRUE),  # Unweighted total count for secondary age
    denominator = survey_total(edu_secondary, na.rm = TRUE),         # Weighted total count for secondary education
    edu_secondary_total = survey_total(edu_secondary, na.rm = TRUE), # Total for secondary education
    age_secondary_total = survey_total(age_secondary, na.rm = TRUE)  # Total for secondary age
  ) %>%
  mutate(
    mean_value = round(edu_secondary_total / age_secondary_total, 4)  # Calculate the mean value
  )


###Table with gender and disability included for the chart


impact3_2b_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups) & !is.na(disability) & !is.na(HH04)) %>%  # Exclude if pop_groups, disability, or HH04 is NA
  group_by(pop_groups, disability, HH04) %>%                          # Group by pop_groups, disability, and HH04
  summarise(
    var_name = "impact3_2b",                                          # Name of the variable (changed to impact3_2b)
    num_obs_uw = survey_total(!is.na(age_secondary), na.rm = TRUE),   # Unweighted total count for secondary age
    denominator = survey_total(edu_secondary, na.rm = TRUE),          # Weighted total count for secondary education
    edu_secondary_total = survey_total(edu_secondary, na.rm = TRUE),  # Total for secondary education
    age_secondary_total = survey_total(age_secondary, na.rm = TRUE)   # Total for secondary age
  ) %>%
  mutate(
    mean_value = round(edu_secondary_total / age_secondary_total, 4)  # Calculate the mean value
  )



###Chart for the above table

ggplot(impact3_2b_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each pop_group
  labs(
    title = "Enrollment in Secondary Education by Age, Gender, and Population Groups",
    x = "Gender",
    y = "Proportion of Education Level",
    fill = "Disability Status"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


###3.3 Proportion of people that feel safe walking alone in their neighbourhood after dark ----
##Module :SAF01


##This indicator comes from main dataset based on the respondent randomly selected for individual level
###Indicator calculations

main <- main %>%
  mutate(
    impact3_3 = case_when(
      SAF01 == "1" | SAF01 == "2" ~ 1,  # Assign 1 for Yes (feels safe)
      SAF01 == "3" | SAF01 == "4" ~ 0,  # Assign 0 for No (does not feel safe)
      SAF01 == "98" | SAF01 == "99" ~ NA_real_  # Handle missing/unknown values with NA_real_
    )
  ) %>%
  mutate(
    impact3_3 = labelled(
      impact3_3,
      labels = c(
        "Yes" = 1,  # Label for Yes (1)
        "No" = 0    # Label for No (0)
      ),
      label = "Proportion of people that feel safe walking alone in their neighbourhood after dark"  # Assign the variable label
    )
  )



###Table standard 


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


impact3_3 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "impact3_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(impact3_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(impact3_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Impact 3.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

impact3_3_AGD <- RMS_XXX_202X_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "impact3_3",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(impact3_3), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(impact3_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )

###Outcome Indicators

#Core Outcome Indicators ----


###1.2 Proportion of children under 5 years of age whose births have been registered with a civil authority -----
##Module :REG03 - REG04


# ind$REG03 - birth certificate
# ind$REG04 - birth has been registered

##Calculate children who has a birth certificate

ind <- ind %>%
  mutate(birthCertificate = case_when(
    REG03 == "0" | REG03 == "98" ~ 0,
    REG03 == "1" ~ 1,
    TRUE ~ NA_real_  # Handle any missing or unknown values
  )) %>%
  mutate(birthCertificate = labelled(
    birthCertificate,
    labels = c(
      'Yes' = 1,
      'No' = 0
    ),
    label = "Children under 5 with a birth certificate"
  ))


ind <- ind %>%
  mutate(birthRegistered = case_when(
    REG04 == "0" | REG04 == "98" ~ 0,
    REG04 == "1" ~ 1,
    REG04 == "99" ~ NA_real_,  # Handle unknown/missing values
    TRUE ~ NA_real_  # Catch-all for any unexpected values
  )) %>%
  mutate(birthRegistered = labelled(
    birthRegistered,
    labels = c(
      'Yes' = 1,
      'No' = 0
    ),
    label = "Children under 5 birth registered with civil authorities"
  ))

# Mutate outcome1_2 variable 
ind <- ind %>%
  mutate(outcome1_2 = case_when(
    (birthRegistered == 1 | birthCertificate == 1) & HH07 < 5 ~ 1,  # Child has been registered or has birth certificate
    (birthRegistered == 0 & birthCertificate == 0) & HH07 < 5 ~ 0,  # Child not registered and has no birth certificate
    TRUE ~ NA_real_  # Handle cases where data is missing or unknown
  )) %>%
  mutate(outcome1_2 = labelled(
    outcome1_2,
    labels = c(
      'Yes' = 1,
      'No' = 0
    ),
    label = "Proportion of children under 5 years of age whose births have been registered with a civil authority"
  ))



##Table for srvyr

RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


###Table 


outcome1_2 <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome1_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome1_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Table with disability and gender


outcome1_2_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups) & HH07 < 5 ) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, pop_groups) %>%
  summarise(
    var_name = "outcome1_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome1_2), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome1_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


##Chart with pop groups

gender_colors <- c("Male" = "#8395B9", "Female" = "#E0E9FE")


ggplot(outcome1_2_AGD, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 1.2 by Population Groups and Gender",
    x = "Gender",
    y = "Proportion of children that registered",
    caption = "Note: Only children under 5"
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




###1.3 Proportion of people with legally recognized identity documents or credentials ----
##Module :REG01 - REG02 - REG05 - REG06

##This indicator comes from the individual dataset


###Calculate valid identity documents for under 5 with REG05 and REG06 variables


#ind$REG05a - passport
#ind$REG05b - civil/government issued ID
#ind$REG05c - residency permit
#ind$REG05d - statelessness documentation
#ind$REG05e - household card of address/family book
#ind$REG05f - social security card
#ind$REG06 - any other document establishes identity
#add birth certificate as additional document from REG03



#ind$REG01a # passport
#ind$REG01b # birth certificate
#ind$REG01c # civil/ government issued ID
#ind$REG01d # residency permit
#ind$REG01e # statelessness documentation
#ind$REG01f # household card of address/family book
#ind$REG01g # social security card
#ind$REG02 # any other document establishes identity

#Make sure to delete REG05e below from the script if you don't have any stateless 

ind <- ind %>%
  mutate(document_under5 = case_when(
    REG05a == "1" | REG05b == "1" | REG05c == "1" | REG05d == "1" | REG05e == "1" | REG05f == "1" | REG06 == "1" | REG03 == "1" ~ 1,
    REG05a != "1" & REG05b != "1" & REG05c != "1" & REG05d != "1" & REG05e != "1" & REG05f != "1" & REG06 != "1" & REG03 != "1" ~ 0,
    TRUE ~ NA_real_
  ))

# Mutate document_above5 using character values
ind <- ind %>%
  mutate(document_above5 = case_when(
    REG01a == "1" | REG01b == "1" | REG01c == "1" | REG01d == "1" | REG01e == "1" | REG01f == "1" | REG01g == "1" | REG02 == "1" ~ 1,
    REG01a != "1" & REG01b != "1" & REG01c != "1" & REG01d != "1" & REG01e != "1" & REG01f != "1" & REG01g != "1" & REG02 != "1" ~ 0,
    TRUE ~ NA_real_
  ))

# Combine both age groups
ind <- ind %>%
  mutate(outcome1_3 = case_when(
    (document_above5 == 1 | document_under5 == 1) ~ 1,  
    (document_above5 == 0 | document_under5 == 0) ~ 0
  )) %>%
  mutate(outcome1_3 = labelled(outcome1_3,
                               labels = c(
                                 'Yes' = 1,
                                 'No' = 0
                               ),
                               label = "Proportion of people with legally recognized identity documents or credentials"))






RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

###Table by population groups

outcome1_3 <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome1_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome1_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above




ggplot(outcome1_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 1.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity



outcome1_3_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat)) %>%                     # Exclude if HH07_cat is NA
  group_by(HH07_cat, HH04, disability) %>%            # Group by Age, Gender, and Disability
  summarise(                                       # Summarise to compute values
    var_name = "outcome1_3",                        # Name of the variable
    num_obs_uw = unweighted(n()),                  # Unweighted total count
    denominator = survey_total(),                  # Weighted total count
    mean_value = survey_mean(outcome1_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


####Chart with the AGD variables 

ggplot(outcome1_3_AGD, aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Grouped bar chart
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on top of bars
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  facet_wrap(~ HH07_cat) +  # Create facets for each age group
  labs(
    title = "Outcome 1.3 by Gender, Age, and Disability Status",
    x = "Gender",
    y = "Mean Value",
    fill = "Disability",
    caption = "Note: The disability module does not include children under 5."
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Adjust y-axis label size for readability
  )


###define labels 

identity_documents <- c(
  "REG01a" = "Passport",
  "REG01b" = "Birth certificate",
  "REG01c" = "Civil/ government issued ID",
  "REG01d" = "Residency permit",
  "REG01e" = "Statelessness documentation",
  "REG01f" = "Household card of address/family book",
  "REG01g" = "Social security card",
  "REG02" = "Any other document"
)

# Calculate the percentage of '1's for each identity document

identity_document_percentages <- RMS_XXX_202X_ind %>%
  summarise(across(c(REG01a, REG01b, REG01c, REG01d, REG01e, REG01f, REG01g, REG02),
                   ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Document", 
               values_to = "Percentage") %>%
  mutate(Document = identity_documents[Document])  # Map column names to descriptive labels


# Create the bar chart
ggplot(identity_document_percentages, aes(x = reorder(Document, Percentage), y = Percentage, fill = Document)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Percentage of Individuals Holding Identity Documents",
    x = "Identity Document",
    y = "Percentage",
    caption = "Note: Percentages are calculated independently for each document for individuals 5 and above."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )


###4.1 Proportion of people who know where to access available GBV service ----
##Module :GBV01

##indicator calculation

main <- main %>%
  mutate(outcome4_1 = case_when(
    GBV01a == "1" | GBV01b == "1" ~ 1,  # If GBV01a or GBV01b is "1", set to 1 (numeric)
    across(c(GBV01a, GBV01b, GBV01c, GBV01d), ~ . == "98") %>% rowSums() == 4 ~ NA_real_,  # If all selected columns are "98", set to NA_real_
    TRUE ~ 0  # For all other cases, set to 0 (numeric)
  )) %>%
  mutate(outcome4_1 = labelled(outcome4_1,
                               labels = c(
                                 'Yes' = 1,
                                 'No' = 0
                               ),
                               label = "Proportion of people who know where to access available GBV service"
  ))

######Table standard 


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


###Table 

outcome4_1 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome4_1",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(!is.na(outcome4_1), vartype = NULL),                    # Weighted total count
    mean_value = survey_mean(outcome4_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )




##Chart for the indicator above



ggplot(outcome4_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 4.1",
    x = "Population Groups",
    y = "Mean value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome4_1_AGD <- RMS_XXX_202X_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # only 18 and above 
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "outcome4_1",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome4_1), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome4_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )



####Simple bar chart of GBV01



###define labels 

gbv_services <- c(
  "GBV01a" = "Health services",
  "GBV01b" = "Psycho-social services",
  "GBV01c" = "safety and security services",
  "GBV01d" = "Legal assistance"
)

# Calculate the percentage of '1's for each identity document


gbv01_percentages <- RMS_XXX_202X_main %>%
  summarise(across(c(GBV01a, GBV01b, GBV01c, GBV01d),
                   ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Services", 
               values_to = "Percentage") %>%
  mutate(Services = gbv_services[Services])  # Map column names to descriptive labels

# Create the bar chart

ggplot(gbv01_percentages, aes(x = reorder(Services, Percentage), y = Percentage, fill = Services)) +
  geom_bar(stat = "identity", width = 0.7) +  # Use "identity" for stat
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels inside bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Knowledge on where to access available GBV services",
    x = "Gender-Based Violence Services",
    y = "Percentage",
    caption = "Note: Percentages are calculated independently for each service for individuals 18 and above."
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust y-axis text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )



##Table with education level for randomly selected adult


outcome4_1_EDU <- RMS_XXX_202X_main %>%
  filter(!is.na(EDU01_random) ) %>%  # Exclude EDU01_random
  group_by(EDU01_random) %>%
  summarise(
    var_name = "outcome4_1",                                      # Name of the variable
    num_obs_uw = survey_total(),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome4_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )





###4.2 Proportion of people who do not accept violence against women ----
##Module :VAW01


##This indicator comes from main dataset based on the respondent randomly selected for individual level

#If randomly selected adult who believes that a  husband is justified in beating his wife in various circumstances

##If yes selected for any of the circumstances
###Prefer not to respond will be put into missing

main <- main %>%
  mutate(
    outcome4_2 = case_when(
      VAW01a == "1" | VAW01b == "1" | VAW01c == "1" | VAW01d == "1" | VAW01e == "1" ~ 0,  # Any "1" results in 0
      VAW01a == "0" & VAW01b == "0" & VAW01c == "0" & VAW01d == "0" & VAW01e == "0" ~ 1,  # All "0"s result in 1
      TRUE ~ NA_real_  # Missing or other cases result in NA
    )
  ) %>%
  mutate(
    outcome4_2 = labelled(
      outcome4_2,
      labels = c(
        'Yes' = 1,  # Numeric label for "Yes"
        'No' = 0    # Numeric label for "No"
      ),
      label = "Proportion of people who do not accept violence against women"
    )
  )



######Table standard 


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


##Table 

outcome4_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome4_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome4_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(outcome4_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 4.2",
    x = "Population Groups",
    y = "Mean value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome4_2_AGD <- RMS_XXX_202X_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # only 18 and above 
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "outcome4_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome4_2), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome4_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )





## Gender-based Violence


vaw_options <- c(
  VAW01a = "If she goes out without telling him",
  VAW01b = "If she neglects the children",
  VAW01c = "If she argues with him",
  VAW01d = "If she refuses to have sex with him",
  VAW01e = "If she burns the food"
)

# Summarize the percentages for each question
vaw01_percentages <- main %>%
  summarise(across(c(VAW01a, VAW01b, VAW01c, VAW01d, VAW01e),
                   ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Question", 
               values_to = "Percentage") %>%
  mutate(Question = vaw_options[Question])  # Map column names to descriptive labels


###Chart

ggplot(vaw01_percentages, aes(x = reorder(Question, Percentage), y = Percentage, fill = Question)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Justification for Violence Against Women",
    x = "Justification",
    y = "Percentage",
    caption = "Note: Percentages represent the proportion of respondents who agree with the justification."
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )



###5.2 Proportion of children who participate in community-based child protection programmes ----
##Module :COMM01-COMM04

## Child Protection


ind$COMM01 <- labelled_chr2dbl(ind$COMM01)
ind$COMM02 <- labelled_chr2dbl(ind$COMM02)
ind$COMM03 <- labelled_chr2dbl(ind$COMM03)
ind$COMM04 <- labelled_chr2dbl(ind$COMM04)


###This indicator comes from the individual level dataset


#Children who participate in community-based programmes at least once 
##under adult supervision in a physically safe area

ind <- ind %>%
  mutate(outcome5_2 = case_when(
    (COMM01 == "1" & (COMM02 >= "1" & COMM02 != "98") & COMM03 == "1" & COMM04 == "1") ~ 1,  # All conditions are character comparisons
    (COMM01 == "0" | 
       (COMM02 == "0" | COMM02 == "98") | 
       (COMM03 == "0" | COMM03 == "98") |
       (COMM04 == "0" | COMM04 == "98")) ~ 0,  # Comparison for "0" and "98" as characters
    TRUE ~ NA_real_  # Catch-all for any missing data
  )) %>%
  mutate(outcome5_2 = labelled(outcome5_2,
                               labels = c(
                                 'Yes' = 1,  # Label for "Yes"
                                 'No' = 0    # Label for "No"
                               ),
                               label = "Proportion of children who participate in community-based child protection programmes"
  ))



###Table 


RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

##Table 

outcome5_2 <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome5_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome5_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Table with disability and gender


outcome5_2_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups)) %>%  # Missing pipe added here
  group_by(HH04, pop_groups, disability) %>%
  summarise(
    var_name = "outcome5_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome5_2), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                  # Weighted total count
    mean_value = survey_mean(outcome5_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )



##Chart with pop groups

gender_colors <- c("Male" = "#8395B9", "Female" = "#E0E9FE")


ggplot(outcome5_2_AGD, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 5.2 by Population Groups and Gender",
    x = "Gender",
    y = "Proportion of Children",
    caption = "Note: Only children between 5 to 17"
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




###8.2 Proportion of people with primary reliance on clean (cooking) fuels and technology -----
##Module :COOK01-COOK03


###indicator calculation

main$COOK01 <- labelled_chr2dbl(main$COOK01)
main$COOK02 <- labelled_chr2dbl(main$COOK02)
main$COOK03 <- labelled_chr2dbl(main$COOK03)


###Based on MICS calculation : TC4.1

main <- main %>%
  mutate(
    outcome8_2 = case_when(
      (COOK01 == "1" & (COOK02 %in% c("1", "2", "3", "4", "5")) | (COOK02 %in% c("10") & COOK03 %in% c("1"))
      ) ~ 1,  # First condition for clean cooking
      (COOK01 == "1" & (COOK02 %in% c("7", "8", "9", "10", "96")) | (COOK02 %in% c("10") & !(COOK03 %in% c("1"))
      )) ~ 0,  # Condition for unclean cooking or incomplete clean technology
      COOK01 == "0" ~ 0,  # COOK01 is "0"
      TRUE ~ NA_real_  # Handle missing or other cases
    )
  ) %>%
  mutate(
    outcome8_2 = labelled(outcome8_2,
                          labels = c(
                            "No" = 0,
                            "Yes" = 1
                          ),
                          label = "Proportion of people with primary reliance on clean (cooking) fuels and technology"
    )
  )


##Table by population groups

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

###Table 

outcome8_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome8_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome8_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )


###Chart of outcome 8_2 by pop groups

ggplot(outcome8_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 8.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###Show the bar chart for COOK02

table(main$COOK02)


# Define stove categories based on the provided list
stove_labels <- c(
  "1" = "Solar cooker (thermal energy from the sun)",
  "2" = "Electric stove",
  "3" = "Piped natural gas stove",
  "4" = "Biogas stove",
  "5" = "Liquefied petroleum gas (LPG)/cooking gas stove",
  "6" = "Manufactured solid fuel stove",
  "7" = "Traditional solid fuel stove (non-manufactured)",
  "8" = "Moveable firepan",
  "9" = "Three stone stove/open fire",
  "10" = "Liquid fuel stove",
  "96" = "Other, specify"
)

# Summarize the counts and percentages for each category
cook02_percentages <- main %>%
  filter(!is.na(COOK02)) %>%  # Exclude missing values
  count(COOK02) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(COOK02 = factor(COOK02, levels = names(stove_labels), labels = stove_labels))

# Create the chart

ggplot(cook02_percentages, aes(x = reorder(COOK02, Percentage), y = Percentage, fill = COOK02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Distribution of Stove Types (COOK02)",
    x = "Stove Type",
    y = "Percentage",
    caption = "Source: RMS XXX 202X"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

###9.1 Proportion of people living in habitable and affordable housing -----
##Module :DWE01 – SHEL01-SHEL06 – DWE05 – DWE08-DWE09


###Indicator calculation

##Module :DWE01 – SHEL01-SHEL06 – DWE05 – DWE08-DWE09


##This indicator is calculated from the main dataset

##Condition 1

##Classify as habitable for below conditions - if 98 selected, put into missing

##First check the variables

table(main$SHEL01)
table(main$SHEL02)
table(main$SHEL03)
table(main$SHEL04)
table(main$SHEL05)
table(main$SHEL06)


main <- main %>%
  mutate(across(starts_with("SHEL"), ~ifelse(. == 98, NA, .))) %>%
  mutate(habitablehousing = case_when(
    (SHEL01 == "1") & (SHEL02 == "1") & (SHEL05 == "1") & 
      (SHEL03 == "0" ) & (SHEL04 == "0" ) & (SHEL06 == "0" ) ~ 1,
    (SHEL01 == "0") | (SHEL02 == "0" ) | (SHEL05 == "0") |
      (SHEL03 == "1") | (SHEL04 == "1") | (SHEL06 == "1" ) ~ 0,
    TRUE ~ NA_integer_
  ))

table(main$habitablehousing)


##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus not considered as physically safe


table(main$hh_size_001)
table(main$DWE05)


main <- main %>%
  mutate(crowding=hh_size_001/DWE05
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 0)
  )


table(main$crowding)
table(main$dwe05_cat)

##Condition 3

main <- main %>%
  mutate(dwe09_cat = case_when( 
    # Affordable if household pays rent and without financial distress
    (DWE08 == "1" & (DWE09 == "1" | DWE09 == "2")) ~ 1,  # No financial distress
    (DWE08 == "1" & (DWE09 == "3" | DWE09 == "4")) ~ 0,  # Financial distress
    DWE08 == "0" ~ 1,  # If not paying rent, it's considered affordable (set to 1)
    TRUE ~ NA_real_  # Catch-all for missing or unexpected values
  ))


table(main$dwe09_cat)

###Combine all three conditions for habitable housing


main <- main %>%
  mutate(
    outcome9_1 = case_when(
      dwe05_cat == 1 & habitablehousing == 1 & dwe09_cat == 1  ~ 1,
      TRUE ~ 0
    ),
    outcome9_1 = labelled(outcome9_1,
                          labels = c("Yes" = 1, "No" = 0),
                          label = "Proportion of people living in habitable and affordable housing")
  )



####Standard tables 


composite_outcome9_1 <- main %>%
  select(pop_groups, dwe05_cat, habitablehousing, dwe09_cat) %>%
  pivot_longer(cols = c(dwe05_cat, habitablehousing, dwe09_cat),  # Pivot the three variables
               names_to = "facility", 
               values_to = "access") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(access, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 


ggplot(composite_outcome9_1, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "dwe05_cat" = "Not overcrowded",
    "habitablehousing" = "Habitable Housing",
    "dwe09_cat" = "Affordable"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Access to Housing Facilities by Population Group",
    x = "Facility",
    y = "Percentage Access",
    fill = "Population Groups"
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  )

##Table by population groups

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

outcome9_1 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome9_1",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome9_1, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )



###Chart of impact 9.1 by pop groups

ggplot(outcome9_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 9.1",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )





###9.2 Proportion of people that have energy to ensure lighting -----
##Module :LIGHT01-LIGHT03

###This basic service is calculated from the main dataset

### The below Calculates percentage of PoC having access to clean fuel for lighting and / or basic connectivity (9.1 Outcome Indicator)

main <- main %>%
  mutate(outcome9_2 = case_when(
    LIGHT01 == "1" & LIGHT02 %in% c("1", "2", "3", "4", "5", "6", "7") ~ 1,  # Conditions for "Yes" (1)
    TRUE ~ 0  # Default to "No" (0)
  )) %>%
  mutate(outcome9_2 = labelled(outcome9_2,
                               labels = c(
                                 "Yes" = 1,
                                 "No" = 0
                               ),
                               label = "Proportion of people that have energy to ensure lighting"
  ))

##Table by population groups


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

outcome9_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome9_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome9_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )


###Chart of outcome 9.2 by pop groups

ggplot(outcome9_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 9.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###Show the bar chart for LIGHT02

table(main$LIGHT02)


# Define stove categories based on the provided list
lighting_labels <- c(
  "1" =	"Electricity",
  "2" =	"Solar home system",
  "3"	= "Solar-powered lantern or flashlight",
  "4" = "Rechargeable flashlight, mobile, torch or lantern",
  "5" = "Battery powered flashlight, torch or lantern",
  "6" =	"Biogas lamp",
  "7" =	"LPG lamp",
  "8" = "Gasoline lamp",
  "9"	= "Kerosene or paraffin lamp",
  "10" = "Oil lamp",
  "11" = "Candle",
  "12" ="Open fire",
  "96" ="Other, specify"
)

# Summarize the counts and percentages for each category
light02_percentages <- main %>%
  filter(!is.na(LIGHT02)) %>%  # Exclude missing values
  count(LIGHT02) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(LIGHT02 = factor(LIGHT02, levels = names(lighting_labels), labels = lighting_labels))

# Create the chart

ggplot(light02_percentages, aes(x = reorder(LIGHT02, Percentage), y = Percentage, fill = LIGHT02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Distribution of Energy of Lighting (LIGHT02)",
    x = "Lighting Type",
    y = "Percentage",
    caption = "Source: RMS XXX 202X"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )




###10.1 Proportion of children aged 9 months to five years who have received measles vaccination ----
##Module :MMR01-MMR04 ##  MICS TC.1.1 UNICEF calculates on the first dose received##


ind <- ind %>%
  mutate(outcome10_1=case_when(
    MMR03=="1" ~ 1, MMR03=="0"  | MMR03=="98" ~ 0)
  ) %>%
  mutate( outcome10_1 = labelled(outcome10_1,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "Proportion of children aged 9 months to five years who have received measles vaccination*"))


###Table 

RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )



outcome10_1 <- RMS_XXX_202X_ind %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome10_1",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome10_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )



###Chart of outcome 10.1 by pop groups

ggplot(outcome10_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 10.1",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



##Table with disability and gender


outcome10_1_AGD <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups) & HH07 < 5 ) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, pop_groups) %>%
  summarise(
    var_name = "outcome10_1",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome10_1), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome10_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


##Chart with pop groups

gender_colors <- c("Male" = "#8395B9", "Female" = "#E0E9FE")


ggplot(outcome10_1_AGD, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 10.1 by Population Groups and Gender",
    x = "Gender",
    y = "Proportion of Children",
    caption = "Note: Only children under 5"
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )




###10.2 Proportion of births attended by skilled health personnel ----
##Module :BIR01-BIR04

###indicator calculation


main <- main %>%
  mutate(outcome10_2 = case_when( 
    (BIR01 == "1" | BIR02 == "1") & (BIR03 %in% c("1", "2", "3")) ~ 1,  # Skilled personnel attended
    (BIR01 == "1" | BIR02 == "1") & (BIR03 %in% c("4", "5", "6")) ~ 0,  # Unskilled personnel attended
    BIR03 %in% c("96", "98") ~ NA_real_,  # Missing or not applicable
    TRUE ~ NA_real_  # Catch-all for any other cases
  )) %>%
  mutate(outcome10_2 = labelled(outcome10_2,
                                labels = c(
                                  "Yes" = 1,  # Yes for skilled personnel
                                  "No" = 0    # No for unskilled personnel
                                ),
                                label = "Proportion of births attended by skilled health personnel"
  ))


###Table 

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )
outcome10_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome10_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome10_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )



###Chart of outcome 10.2 by pop groups

ggplot(outcome10_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 10.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###12.1 Proportion of people using at least basic drinking water services ----
##Module :DWA01-DWA03

#Turn into numeric



## Indicator calculation

main <- main %>%
  mutate(time_DWA = case_when(
    DWA03a == "1" ~ 1,  # If it's in hours, convert to 1 minute
    DWA03a == "2" ~ 60  # Convert 1 hour to 60 minutes
  )) %>%
  mutate(time_tot = time_DWA * as.numeric(DWA03b)  # Convert DWA03b to numeric if it's not already
  ) %>%
  mutate(dwa_cond1 = case_when(
    time_tot > 30 ~ 0,  # Not reachable under 30 minutes
    TRUE ~ 1  # Reachable under 30 minutes
  )) %>%
  mutate(dwa_cond2 = case_when(
    !DWA01 %in% c("7", "9", "13", "96", "98") ~ 1,  # Improved source (all except these codes)
    TRUE ~ 0
  )) %>%
  mutate(dwa_cond3 = case_when(
    DWA02 == "3" ~ 0,  # Not in the dwelling/yard/plot
    TRUE ~ 1  # In the dwelling/yard/plot
  )) %>%
  mutate(outcome12_1 = case_when(
    ((dwa_cond1 == 1 | dwa_cond3 == 1) & dwa_cond2 == 1) ~ 1,  # Basic drinking water service
    TRUE ~ 0
  )) %>%
  mutate(outcome12_1 = labelled(outcome12_1,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Proportion of people using at least basic drinking water services"
  ))


####Standard tables 


composite_outcome12_1 <- main %>%
  select(pop_groups, dwa_cond1, dwa_cond2, dwa_cond3) %>%
  pivot_longer(cols = c(dwa_cond1, dwa_cond2, dwa_cond3),  # Pivot the three variables
               names_to = "facility", 
               values_to = "conditions") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(conditions, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 


ggplot(composite_outcome12_1, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "dwa_cond1" = "Reachable under 30 minutes",
    "dwa_cond2" = "From an improved source",
    "dwa_cond3" = "In the dwelling/yard/plot"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Access to Basic Drinking Services",
    x = "At least basic drinking water services",
    y = "Percentage Access",
    fill = "Population Groups"
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  )

##Table by population groups

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

outcome12_1 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome12_1",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome12_1, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )



###Chart of outcome 12_1 by pop groups

ggplot(outcome12_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 12.1",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )




###12.2 Proportion of people with access to a safe household toilet -----
##Module :TOI01-TOI05 ##MICS calculation WS3.1/WS3.4

##This indicator measures the proportion of people with access to at 
##least basic sanitation services.

##MICS calculation WS3.1/WS3.4


###Indicator calculations

main <- main %>%
  mutate(toi_cond1 = case_when(
    TOI01 %in% c("1", "2", "3", "4", "5", "6", "7", "9") ~ 1,  # Improved sanitation facility
    TOI01 %in% c("8", "10", "11", "12", "96") ~ 0,  # Unimproved sanitation facility
    TRUE ~ NA_real_  # Missing or invalid
  )) %>%
  mutate(toi_cond2 = case_when(
    TOI02 == "1" & TOI03 %in% c("5", "96", "98") ~ 0,  # Unsafe disposal
    TOI02 == "1" & TOI03 %in% c("1", "2", "3", "4") ~ 1,  # Safe disposal
    TOI02 == "2" ~ 0,  # Unsafe if no toilet
    TOI02 == "98" ~ 0,  # Unavailable information
    TRUE ~ NA_real_  # Missing or invalid
  )) %>%
  mutate(toi_cond3 = case_when(
    TOI05 == "1" ~ 0,  # Shared toilet
    TOI05 == "0" ~ 1   # Not shared
  )) %>%
  
  ### Combine all three conditions
  mutate(outcome12_2 = case_when(
    toi_cond1 == 1 & toi_cond2 == 1 & toi_cond3 == 1 ~ 1,  # Basic sanitation service
    TRUE ~ 0  # If any condition fails
  )) %>%
  mutate(outcome12_2 = labelled(outcome12_2,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "This indicator measures the proportion of people with access to at least basic sanitation services."
  ))



####Standard tables 


composite_outcome12_2 <- main %>%
  select(pop_groups, toi_cond1, toi_cond2, toi_cond3) %>%
  pivot_longer(cols = c(toi_cond1, toi_cond2, toi_cond3),  # Pivot the three variables
               names_to = "facility", 
               values_to = "conditions") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = mean(conditions, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 


ggplot(composite_outcome12_2, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "toi_cond1" = "Improved sanitation facility",
    "toi_cond2" = "Safe disposal in situ of excreta",
    "toi_cond3" = "Toilet is not shared with other households"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Access to Basic Drinking Services",
    x = "Improved Toilet",
    y = "Percentage Access",
    fill = "Population Groups"
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  ) + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) ### limit if needed

##Table by population groups


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


outcome12_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome12_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome12_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )



###Chart of outcome 12_2 by pop groups

ggplot(outcome12_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 12.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



###13.1 Proportion of people with an account at a bank or other financial institution or with a mobile-money-service provider ----


##This indicator comes from main dataset based on the respondent randomly selected for individual level

main <- main %>%
  mutate(
    outcome13_1 = case_when(
      BANK01 == "1" | BANK02 == "1" | BANK03 == "1" | BANK05 == "1" ~ 1,  # If any is "1", set to 1
      BANK01 == "0" & BANK02 == "0" & BANK03 == "0" & BANK05 == "0" ~ 0,  # If all are "0", set to 0
      TRUE ~ 0  # Default to 0 for all other cases
    )
  ) %>%
  mutate(outcome13_1 = labelled(outcome13_1,
                                labels = c(
                                  "Yes" = 1,  # Label for Yes
                                  "No" = 0    # Label for No
                                ),
                                label = "Proportion of people with an account at a bank or other financial institution or with a mobile-money-service provider"
  ))

###Show all options separately 



composite_outcome13_1 <- main %>%
  select(pop_groups, BANK01, BANK02,  BANK04, BANK05) %>%
  pivot_longer(cols = c(BANK01, BANK02, BANK04, BANK05),  # Pivot the three variables
               names_to = "access", 
               values_to = "conditions") %>%
  group_by(pop_groups, access) %>%
  summarise(percentage = mean(conditions, na.rm = TRUE) * 100) %>%
  ungroup()



###Chart for above with all dimensions 

ggplot(composite_outcome13_1, aes(x = access, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Correct stat to "identity"
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "BANK01" = "Account in financial institution",
    "BANK02" = "Have a ATM/debit card",
    "BANK04" = "Used mobile money in the last 12 months",
    "BANK05" = "Personally used mobile money in the last 12 months"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Percentage of People Financially Included",
    x = "Financial Inclusion",
    y = "Percentage",
    fill = "Population Groups"
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  ) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0))  # Limit y-axis from 0 to 100%


###Table standard 

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


outcome13_1 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome13_1",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome13_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(outcome13_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 13.1",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome13_1_AGD <- RMS_XXX_202X_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "outcome13_1",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome13_1), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome13_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )





###13.2 Proportion of people who self-report positive changes in their income compared to previous year ----
##Module :INC01 - INC02

###To calculate the indicator value, the standard survey methodology considers 
###for the numerator only individuals who state their income increased and 
##who can also afford more goods and services, or those whose income 
##remained the same or decreased but who can still afford more goods 
##and services (to account for possible inflation).
###To calculate the indicator value, the standard survey methodology considers 
###for the numerator only individuals who state their income increased and 
##who can also afford more goods and services, or those whose income 
##remained the same or decreased but who can still afford more goods 
##and services (to account for possible inflation).

main <- main %>%
  mutate(outcome13_2 = case_when(
    INC01 == "1" & INC02 == "1" ~ 1,  # Income increased and can afford more
    (INC01 == "2" | INC01 == "3") & INC02 == "1" ~ 1,  # Income decreased/same and can afford more
    TRUE ~ 0  # Default to 0
  )) %>%
  mutate(outcome13_2 = labelled(outcome13_2,
                                labels = c(
                                  "Yes" = 1,  # Label for Yes
                                  "No" = 0    # Label for No
                                ),
                                label = "Proportion of people who self-report positive changes in their income compared to previous year"
  ))



###Check INC01 and INC02 



# Define stove categories based on the provided list
inc01_labels <- c(
  "1" = "Increased compared to previous year",
  "2" = "Been the same compared to previous year",
  "3" = "Decreased compared to previous year"  
)

# Summarize the counts and percentages for each category
INC01_percentages <- main %>%
  filter(!is.na(INC01)) %>%  # Exclude missing values
  count(INC01) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC01 = factor(INC01, levels = names(inc01_labels), labels = inc01_labels))


# Create the chart

ggplot(INC01_percentages, aes(x = reorder(INC01, Percentage), y = Percentage, fill = INC01)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Changes in income in the last 12 months (INC01)",
    x = "Changes",
    y = "Percentage",
    caption = "Source: RMS XXX 202X"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )



###INC02




# Define stove categories based on the provided list
inc02_labels <- c(
  "1" = "More",
  "2" = "The same",
  "3" = "Fewer"  
)

# Summarize the counts and percentages for each category

INC02_percentages <- main %>%
  filter(!is.na(INC02)) %>%  # Exclude missing values
  count(INC02) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC02 = factor(INC02, levels = names(inc02_labels), labels = inc02_labels))


# Create the chart

ggplot(INC02_percentages, aes(x = reorder(INC02, Percentage), y = Percentage, fill = INC02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Changes in income in the last 12 months (INC01)",
    x = "Affordability",
    y = "Percentage",
    caption = "Source: RMS SSD 2023"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )



###Table standard 


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

outcome13_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome13_2",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome13_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(outcome13_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 13.2",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome13_2_AGD <- RMS_XXX_202X_main %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, disability) %>%
  summarise(
    var_name = "outcome13_2",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome13_2), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome13_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


###13.3 Proportion of people (working age) who are unemployed -----


##Indicator calculations

### Include only the labour force ( those who are in labour force)
### International standard is 15+ 
### Eurostat calculates for 18+ as in standard RMS Q V3.2


main <- main %>%
  # Create a working_age flag for those who are 18+ years old (numeric)
  mutate(working_age = case_when(
    HH07 >= 18 ~ 1,  # If 18 or older, mark as working age (numeric)
    TRUE ~ NA_real_  # Set to NA if not 18+
  )) %>%
  
  # Determine who is employed based on various conditions (character comparisons, numeric outcome)
  mutate(employed = case_when(
    UNEM01 == "1" & working_age == 1 ~ 1,  # Paid employment - employees
    (UNEM02 == "1" & UNEM07 == "3") & working_age == 1 ~ 1,  # Self-employment
    (UNEM03 == "1" & UNEM07 == "3") & working_age == 1 ~ 1,  # Unpaid contributing family workers
    UNEM04 == "1" & working_age == 1 ~ 1,  # Absent but still employed
    (UNEM05 == "1" & UNEM06 == "3") & working_age == 1 ~ 1,  # Absent but self-employed
    (UNEM02 == "1" & UNEM07 %in% c("1", "2") & UNEM08 %in% c("1", "2")) & working_age == 1 ~ 1,  # Farming/rearing/fishing for sale
    (UNEM05 == "1" & UNEM06 %in% c("1", "2") & UNEM08 %in% c("1", "2")) & working_age == 1 ~ 1,  # Absent but farming for sale
    working_age == 1 ~ 0,  # If working age but none of the above, mark as not employed
    TRUE ~ NA_real_  # Set to NA for those not in the working age group
  )) %>%
  
  # Define unemployed: those not employed but actively looking for work
  mutate(unemployed = case_when(
    (employed == 0 & UNEM09 == "1" & UNEM10 == "1") & working_age == 1 ~ 1,  # Actively looking for work
    working_age == 1 ~ 0,  # Not unemployed if not actively looking or employed
    TRUE ~ NA_real_  # Set to NA for those not in the working age group
  )) %>%
  
  # Define labour force: anyone employed or unemployed
  mutate(labour_force = case_when(
    (employed == 1 | unemployed == 1) & working_age == 1 ~ 1,  # Employed or actively seeking work
    working_age == 1 ~ 0,  # Not in labour force but 18+
    TRUE ~ NA_real_  # Set to NA for those not in the working age group
  )) %>%
  
  # Outcome: unemployment status among those in the labor force (numeric outcome)
  mutate(outcome13_3 = case_when(
    employed == 1 & labour_force == 1 ~ 0,  # If employed, outcome is 0 (not unemployed)
    unemployed == 1 & labour_force == 1 ~ 1,  # If unemployed, outcome is 1
    TRUE ~ NA_real_  # Catch-all for cases not in the labor force or with missing data
  ))


######Table standard 

RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


outcome13_3 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Group by pop_groups
  summarise(                                         # Summarise to compute values
    var_name = "outcome13_3",                          # Name of the variable
    num_obs_uw = unweighted(n()),                    # Unweighted total count
    denominator = survey_total(),                    # Weighted total count
    mean_value = survey_mean(outcome13_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )


##Chart for the indicator above



ggplot(outcome13_3, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 13.3",
    x = "Population Groups",
    y = "Mean Value with standard errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


#### Table and chart that shows results by age/sex/diversity

outcome13_3_AGD <- RMS_XXX_202X_main %>%
  filter(!is.na(disability) & !is.na(HH07_cat) & HH07 > 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, disability,pop_groups ) %>%
  summarise(
    var_name = "outcome13_3",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome13_3), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome13_3, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )




###14.1 Proportion of returnees with legally recognized identity documents or credentials -----
##Module :REG01 - REG02 - REG03 / REG05 - REG06 
         

##DISAGGREGATE FOR REFUGEE RETURNEES ONLY 

###Calculate valid identity documents for under 5 with REG05 and REG06 variables

#ind$REG05a - passport
#ind$REG05b - civil/government issued ID
#ind$REG05c - residency permit
#ind$REG05d - statelessness documentation
#ind$REG05e - household card of address/family book
#ind$REG05f - social security card
#ind$REG06 - any other document establishes identity
#add birth certificate as additional document from REG03

#ind$REG01a # passport
#ind$REG01b # birth certificate
#ind$REG01c # civil/ government issued ID
#ind$REG01d # residency permit
#ind$REG01e # statelessness documentation
#ind$REG01f # household card of address/family book
#ind$REG01g # social security card
#ind$REG02 # any other document establishes identity

ind <- ind %>%
  # Calculate identity documents for children under 5
  mutate(document_under5 = case_when(
    REG05a == "1" | REG05b == "1" | REG05c == "1" | REG05d == "1" | REG05e == "1" | REG05f == "1" | REG06 == "1" | REG03 == "1" ~ 1,  # Document present
    REG05a == "0" & REG05b == "0" & REG05c == "0" & REG05d == "0" & REG05e == "0" & REG05f == "0" & REG06 == "0" & REG03 == "0" ~ 0,  # No document
    TRUE ~ NA_real_  # Missing or invalid data
  )) %>%
  
  # Calculate valid identity documents for people over 5
  mutate(document_above5 = case_when(
    REG01a == "1" | REG01b == "1" | REG01c == "1" | REG01d == "1" | REG01e == "1" | REG01f == "1" | REG01g == "1" | REG02 == "1" ~ 1,  # Document present
    REG01a == "0" & REG01b == "0" & REG01c == "0" & REG01d == "0" & REG01e == "0" & REG01f == "0" & REG01g == "0" & REG02 == "0" ~ 0,  # No document
    TRUE ~ NA_real_  # Missing or invalid data
  )) %>%
  
  # Combine both age groups (under 5 and above 5)
  mutate(outcome14_1 = case_when(
    (document_above5 == 1 | document_under5 == 1) ~ 1,  # If either age group has a document, mark as "Yes"
    (document_above5 == 0 & document_under5 == 0) ~ 0,  # If neither age group has a document, mark as "No"
    TRUE ~ NA_real_  # Missing or invalid data
  )) %>%
  
  # Label the outcome variable
  mutate(outcome14_1 = labelled(outcome14_1,
                                labels = c(
                                  'Yes' = 1,
                                  'No' = 0
                                ),
                                label = "Proportion of returnees with legally recognized identity documents or credentials"
  ))




#### Table for indicator


###Table by population groups

RMS_XXX_202X_ind <- ind %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


outcome14_1 <- RMS_XXX_202X_ind %>%
  filter(pop_groups == 'Refugee returnees') %>%  # Use equivalent of pop groups for refugee returnees
  group_by(pop_groups) %>%  # Group by pop_groups
  summarise(  
    var_name = "outcome14_1",  # Name of the variable
    num_obs_uw = unweighted(n()),  # Unweighted total count
    denominator = survey_total(),  # Weighted total count
    mean_value = survey_mean(outcome14_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
  )



         
###16.1 Proportion of people with secure tenure rights to housing and/or land ----
##Module :DWE06_land – DWE06a_land – DWE07_land - DWE06_housing – DWE06a_housing – DWE07_housing – DWE10
         

##Module :DWE06_land – DWE06a_land – DWE07_land - DWE06_housing – DWE06a_housing – DWE07_housing – DWE10

# likelihood of losing right for housing is unlikely
# Have the documentation both for land and housing

main <- main %>%
  mutate(outcome16_1 = case_when(
    (DWE10 %in% c("1", "2")) & (DWE06a_land == "1" & DWE06a_housing == "1") ~ 1,  # Likelihood of losing right is unlikely and has documentation
    DWE06a_land == "0" | DWE06a_housing == "0" | DWE10 %in% c("3", "4") ~ 0,  # No documentation or likely to lose the right
    TRUE ~ NA_real_  # Handle missing values
  )) %>%
  mutate(outcome16_1 = labelled(outcome16_1,
                                labels = c("No" = 0, "Yes" = 1),
                                label = "Proportion of people with secure tenure rights to housing and/or land"))


##Table by population groups


RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

outcome16_1 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome16_1",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome16_1, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )


###Chart of impact 16_1 by pop groups

ggplot(outcome16_1, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 16.1",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )




###Check on DWE10


table(main$DWE10)
# Define DWE10 categories based on the provided list
DWE10_labels <- c(
  "1" = "Very unlikely",
  "2" = "Somewhat unlikely",
  "3" = "Somewhat likely",
  "4" = "Very likely",
  "99" = "Don't know"
)

# Convert DWE10 to a factor with correct levels and labels BEFORE summarizing
main <- main %>%
  mutate(DWE10 = factor(DWE10, levels = names(DWE10_labels), labels = DWE10_labels))

# Summarize the counts and percentages for each category
DWE10_percentages <- main %>%
  filter(!is.na(DWE10)) %>%  # Exclude missing values
  count(DWE10) %>%
  mutate(Percentage = n / sum(n) * 100)  # Calculate percentage based on total valid responses



# Create the chart

ggplot(DWE10_percentages, aes(x = reorder(DWE10, Percentage), y = Percentage, fill = DWE10)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Likelihood of losing housing/land in the next 12 months",
    x = "Likelihood",
    y = "Percentage",
    caption = "Source: RMS SSD 2023"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )



         
###16.2 Proportion of people covered by national social protection systems ----
##Module :UNHCR Core Indicator Metadata	SPF01
         
#Module :UNHCR Core Indicator Metadata SPF01

main <- main %>%
  # Convert labelled/factor SPF01 columns to numeric
  mutate(across(starts_with("SPF01"), ~ as.numeric(as.character(.)))) %>%
  
  rowwise() %>%
  mutate(outcome16_2 = case_when(
    any(c_across(starts_with("SPF01")) == 1) ~ 1,  # If any SPF01 column has 1
    all(c_across(starts_with("SPF01")) == 0) ~ 0,  # If all SPF01 columns are 0
    TRUE ~ 0                                      # Default case
  )) %>%
  
  # Add labels for outcome16_2
  mutate(outcome16_2 = labelled(outcome16_2,
                                labels = c(
                                  'Yes' = 1,
                                  'No' = 0
                                ),
                                label = "Proportion of people covered by national social protection systems"))




##Table by population groups



RMS_XXX_202X_main <- main %>%
  as_survey_design(
    ids = NULL,           # Specify the column with cluster IDs
    weights = NULL, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


outcome16_2 <- RMS_XXX_202X_main %>%
  filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
  group_by(pop_groups) %>%                           # Show results disaggregated by pop groups
  summarise(                                         # put all variables here
    var_name = "outcome16_2",                          # name of the variable
    num_obs_uw = unweighted(n()),                    # unweighted total count
    denominator = survey_total(),                      # weighted total count
    mean_value = survey_mean(outcome16_2, vartype = c("ci", "se"), na.rm = TRUE) # indicator value ( weighted) with CI and SE
  )


###Chart of impact 16_1 by pop groups

ggplot(outcome16_2, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - mean_value_se, ymax = mean_value + mean_value_se),
                width = 0.2, position = position_dodge(0.7)) +
  geom_text(aes(label = round(mean_value, 2)), 
            vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Results of RBM Core Outcome 16.2",
    x = "Population Groups",
    y = "Mean Value with Standard Errors"
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
  theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )



#### social protection services

###Please delete/adjust response options accordingly for the chart

# Define the mapping for SPF01 variables (social protection services)
spf01_mapping <- c(
  "SPF01a" = "Cash / in-kind transfers",
  "SPF01b" = "School feeding",
  "SPF01c" = "Public works",
  "SPF01d" = "Subsidies / fee waivers",
  "SPF01e" = "Unemployment benefits",
  "SPF01f" = "Health insurance",
  "SPF01g" = "Old age pension",
  "SPF01h" = "Crop / livestock insurance",
  "SPF01j" = "Social work (child protection, disability, old persons, GBV)",
  "SPF01k" = "Family support",
  "SPF01l" = "Psychosocial support",
  "SPF01m" = "Vocational training",
  "SPF01n" = "Job search services",
  "SPF01o" = "Wage subsidies",
  "SPF01p" = "Improvements of land tenure security"
)

# Step 2: Calculate the percentage of individuals receiving each service
spf01_percentages <- main %>%
  group_by(pop_groups) %>%  # Group by population group (e.g., gender, age group)
  summarise(across(c(SPF01a, SPF01b, SPF01c, SPF01d, SPF01e, SPF01f, SPF01g, SPF01h, 
                     SPF01j, SPF01k, SPF01l, SPF01m, SPF01n, SPF01o, SPF01p), 
                   ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
  pivot_longer(cols = -pop_groups,  # Exclude the PopGroup column from pivoting
               names_to = "Service", 
               values_to = "Percentage") %>%
  mutate(Service = spf01_mapping[Service])  # Map column names to descriptive labels



# Step 3: Create the bar chart

ggplot(spf01_percentages, aes(x = reorder(Service, Percentage), y = Percentage, fill = Service)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +
  
  coord_flip() +  # Flip the axes for better readability
  
  # Labels and title
  labs(
    title = "Proportion of Individuals Covered by National Social Protection Systems",
    x = "Social Protection Service",
    y = "Percentage",
    caption = "Note: Each service is calculated independently."
  ) +
  
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  
  theme_unhcr() +  # Apply UNHCR theme
  
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  ) +
  
  # Separate plots for each population group (e.g., gender, age group)
  facet_wrap(~pop_groups, scales = "free_y", ncol = 1)  # Create separate plots per PopGroup


####FINAL TABLE -----



##Delete if you don't have some of the indicators
# Combine all indicators into one data frame
combined_RBM_indicators <- bind_rows(
  impact2_2,
  impact2_3,
  impact3_2a,
  impact3_2b,
  impact3_3,
  outcome1_2,
  outcome1_3,
  outcome4_1,
  outcome4_2,
  outcome5_2,
  outcome8_2,
  outcome9_1,
  outcome9_2,
  outcome10_1,
  outcome10_2,
  outcome12_1,
  outcome12_2,
  outcome13_1,
  outcome13_2,
  outcome13_3,
  outcome14_1,
  outcome16_1,
  outcome16_2
)

# Export the combined data frame to an Excel file

write_xlsx(combined_RBM_indicators, "Combined_Indicators.xlsx")

# This will export the combined data frame to a single Excel file





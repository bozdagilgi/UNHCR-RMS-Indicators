### Standard scripts for RMS
rm(list=ls()) # clear work space

# ->  install Packages & Create function that turn character values into numeric  ############
# install.packages('robotoolbox')
# install.packages("remotes")
# remotes::install_github("dickoa/robotoolbox")


library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(remotes)
library(dm)



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




# -> Data import from Kobo #####

### insert your username from kobo/UNHCR

## Or set thisup within your environement variable

#  edit directly the .Renviron file or access it by calling usethis::edit_r_environ() (assuming you have the usethis package installed)
# and entering the following two lines:
#
# KOBOTOOLBOX_URL="https://kobo.unhcr.org/"
# KOBOTOOLBOX_TOKEN=xxxxxxxxxxxxxxxxxxxxxxxxxx

# kobo_token(username = "XXXX",
#            password = "XXXX",
#            url = "https://kobo.unhcr.org")
#
#
# ### enter your token
#
# kobo_setup(url = "https://kobo.unhcr.org",
#            token = "XXXXXXXXXXXXXXX")

### access data - enter name here

# kobo_asset_list()
#
# asset_list <- kobo_asset_list()
# uid <- filter(asset_list, name == "RMS CAPI v2") |>
#   pull(uid)

## This one is the RMS Trinidad
asset <- kobo_asset("aM4SnZ43SSxXEh8HecqUzh")
#asset
df <- kobo_data(asset)
#df


glimpse(df$main)
glimpse(df$S1)
# glimpse(df$S2)
# glimpse(df$P2.S3)



main <- df$main
S1 <- df$S1 ##HH roster

##S2 <- df$S2 ##individidual
#P2 <- df$P2.S3 ##children education
#rm(list="df")

#### get dimensions of datasets above

dim(main)
dim(S1)
# dim(S2)
# dim(P2)

### merge all individual datasets
ind <- S1
# ind_merge <- merge(S1,S2, by=c("_index","_parent_index"))
# ind <- merge (ind_merge, P2, by=c("_index", "_parent_index"))
#
#
# ###Removed unused datasets
#
# rm(ind_merge)
# rm(P2)
# rm(S1)
# rm(S2)

main %>%  sjPlot::view_df()
ind %>%  sjPlot::view_df()

# -> Disaggregation variables  ############

## Calculate primary citizenship for individual dataset  ############
ind <- ind %>%

  mutate( # primary citizenship from REF01 and REF02
    citizenship_com = case_when(
      REF01 == "1" ~ "ZAF", ##here enter the country code of enumeration
      REF01 %in% c("0", "98") ~ as.character(ind$REF02),
      REF01 == "99" ~ "99"
    )
  ) %>%
  mutate(citizenship_com = labelled(citizenship_com,
                                    labels = val_labels(ind$REF02),
                                    label = var_label(ind$REF02)))

## Calculate age groups for disaggregation for ind and main dataset  ############
ind$HH07_cat <- cut(ind$HH07,
                    breaks = c(-1, 4, 17, 59, Inf),
                    labels = c("0-4", "5-17", "18-59", "60+"))

table(ind$HH07_cat, useNA = "ifany")

# ### Household head in main dataset is HH07 for age and HH04 for gender
# main$HH07_cat <- cut(main$HH07,
#                      breaks = c(-1, 4, 17, 59, Inf),
#                      labels = c("0-4", "5-17", "18-59", "60+"))
# table(main$HH07_cat, useNA = "ifany")

## Calculate Disability for disaggregation ind dataset  ############

### Calculated based on WG suggestions :
# https://www.washingtongroup-disability.com/fileadmin/uploads/wg/Documents/WG_Document__5C_-_Analytic_Guidelines_for_the_WG-SS__Stata_.pdf

##Step.1 Create variable for calculating disability
names(main)
names(ind)

## In the new telephone interview standard - disability is in main

main <-  main %>%
  mutate( # disability identifier variables according to Washington Group standards
    disaux1_234 = DIS01 %in% c("2","3","4"), # indicator variables for all 6 domains with value TRUE if SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_234 = DIS02 %in% c("2","3","4"),
    disaux3_234 = DIS03 %in% c("2","3","4"),
    disaux4_234 = DIS04 %in% c("2","3","4"),
    disaux5_234 = DIS05 %in% c("2","3","4"),
    disaux6_234 = DIS06 %in% c("2","3","4"),

    disaux1_34 = DIS01 %in% c("3","4"), # indicator variables for all 6 domains with value TRUE if A LOT OF DIFFICULTY or CANNOT DO AT ALL
    disaux2_34 = DIS02 %in% c("3","4"),
    disaux3_34 = DIS03 %in% c("3","4"),
    disaux4_34 = DIS04 %in% c("3","4"),
    disaux5_34 = DIS05 %in% c("3","4"),
    disaux6_34 = DIS06 %in% c("3","4")
  ) %>%
  mutate(
    disSum234 = rowSums(select(., disaux1_234, disaux2_234 , disaux3_234 , disaux4_234 , disaux5_234 , disaux6_234)), # count number of TRUE indicator variables over 6 domains
    disSum34 = rowSums(select(., disaux1_34, disaux2_34 , disaux3_34 , disaux4_34 , disaux5_34 , disaux6_34)) # count number of TRUE indicator variables over 6 domains

  ) %>%
  mutate(
    DISABILITY1 = case_when( # : the level of inclusion is at least one domain/question is coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum234 >= 1 ~ 1,
      disSum234 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY2 = case_when( # : the level of inclusion is at least two domains/questions are coded SOME DIFFICULTY or A LOT OF DIFFICULTY or CANNOT DO AT ALL or any 1 domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL
      disSum234 >= 2 | disSum34 >=1  ~ 1,
      disSum234 < 2 & disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY3 = case_when( # : the level of inclusion is at least one domain/question is coded A LOT OF DIFFICULTY or CANNOT DO AT ALL.
      disSum34 >= 1 ~ 1,
      disSum34 == 0 & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY4 = case_when( # : the level of inclusion is at least one domain/question is coded CANNOT DO AT ALL.
      DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4" ~ 1,
      !(DIS01=="4" | DIS02=="4" | DIS03=="4" | DIS04=="4" | DIS05=="4" | DIS06=="4") & (!(DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99"))) ~ 0,
      DIS01 %in% c("98","99") & DIS02 %in% c("98","99") & DIS03 %in% c("98","99") & DIS04 %in% c("98","99") & DIS05 %in% c("98","99") & DIS06 %in% c("98","99") ~ 98
    )
  ) %>%
  mutate(
    DISABILITY1 = labelled(DISABILITY1,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 1"),
    DISABILITY2 = labelled(DISABILITY2,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 2"),
    DISABILITY3 = labelled(DISABILITY3,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 3"),
    DISABILITY4 = labelled(DISABILITY4,
                           labels = c(
                             "Without disability" = 0,
                             "With disability" = 1,
                             "Unknown" = 98
                           ),
                           label = "Washington Group disability identifier 4"))

table(main$DISABILITY1, useNA = "ifany")
table(main$DISABILITY2, useNA = "ifany")
table(main$DISABILITY3, useNA = "ifany")
table(main$DISABILITY4, useNA = "ifany")

###Calculate having at least one disability identifier among 4 categories
main <-  main  %>%
  mutate(disab=
           case_when(DISABILITY1==1 | DISABILITY2==1 | DISABILITY3==1 | DISABILITY4==1 ~ 1,
                     DISABILITY1==0 | DISABILITY2==0 | DISABILITY3==0 | DISABILITY4==0 ~ 0,
                     TRUE ~ NA_real_)
  ) %>%
  mutate(disab = labelled(disab,
                          labels = c(
                            "Without disability" = 0,
                            "With disability" = 1)
  ))

table(main$disab, useNA = "ifany")

## Merge datasets to have disaggregation variables above in both datasets   ############

## Import individual level variables to HH dataset to analyse sampled adult question
###Create similar variable names for merging with the individual dataset

# main$HH02 <- main$name_selectedadult18
# main$HH07 <- main$name_selectedadult18_age
main$"_parent_index" <- main$"_index"



##Select indicators for merge from Ind to Main

ind_m <- ind %>%
  select("_parent_index",
         "HH07_cat",  ###Age categories
        # "disab", ###Disability
       #  "citizenship_com", ##Country of Origin
         "HH02", # name_selectedadult18
         "HH07", # name_selectedadult18_age
         "HH04") # Gender of HH head --- HH04 (filter for when HH03==1)

#main <- merge(ind_m, main, by=c("HH02", "HH07", "_parent_index"))

rm(ind_m)

## Import HH level variables from main to individual dataset
# main_m <- main %>%
#   select("_parent_index",
#          "pop_groups", ###Population group
#          "end_result") ## ADD VARIABLES FOR MERGING
#
# ind <- merge(main_m, ind, by = "_parent_index")
#
# rm(main_m)








# -> Impact Indicators ############

## 2.2 Proportion of PoCs residing in physically safe and secure settlements with access to basic facilities. ############
##Module :	HEA01-HEA03 (health) + LIGHT01-LIGHT03 (9.2) + DWA01-DWA04 (12.1) + DWE01-DWE05 & DWE08 - DWE09 (9.1)

##Step1. Calculate access to health facilities -according to Sphere standards- it should be reachable within 1 hour

####Access to health facility: If respondent knows where to reach within 1 hour distance (any type of transport)
class(main$HEA01) #character
class(main$HEA03) #integer

main$HEA01_num <- labelled_chr2dbl(main$HEA01)
class(main$HEA01_num) #double

main <- main %>%
  mutate(health_acc=
           case_when(HEA01_num!=98 & HEA03 <= 60 ~ 1,
                     TRUE ~ 0)
  ) %>%
  mutate( health_acc = labelled(health_acc,
                               labels = c(
                                 "Health facility is available" = 1,
                                 "Health facilitiy is not available" = 0
                               ),
                               label = "Access to health facilities"))

table(main$health_acc, useNA = "ifany")

#Step2. calculate percentage of PoC having access to clean fuel for lighting and / or basic connectivity (9.1)
###for LIGHT02 Only include options based on metadata (exclude diesel generator, candles, kerosene)

main$LIGHT01_num <- labelled_chr2dbl(main$LIGHT01)
main$LIGHT02_num <- labelled_chr2dbl(main$LIGHT02)
main$LIGHT03_num <- labelled_chr2dbl(main$LIGHT03)

main <- main %>%
  mutate(electricity=
           case_when(LIGHT01_num==1 & (LIGHT02_num==1 |LIGHT02_num==3 | LIGHT02_num==5 | LIGHT02_num==6
                                      | LIGHT02_num==7 | LIGHT02_num==8) &
                       (LIGHT03_num!=1 | LIGHT03_num!=96 | LIGHT03_num!=98 ) ~ 1,
                     TRUE ~ 0)
  ) %>%
  mutate( electricity = labelled(electricity,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Access to electricity"))


table(main$electricity, useNA = "ifany")

#Step3. calculate percentage of PoCs using at least basic water services (12.2)
###Improved drinking water resource that is accessible under 30 minutes in the last 30 days

main$DWA03a_num <- labelled_chr2dbl(main$DWA03a)
main$DWA02_num <- labelled_chr2dbl(main$DWA02)
main$DWA01_num <- labelled_chr2dbl(main$DWA01)
main$DWA04_num <- labelled_chr2dbl(main$DWA04)
class(main$DWA03a_num)
class(main$DWA03b)
main <- main %>%

  mutate(time_DWA=case_when(  DWA03a_num==1~1, DWA03a_num==2~60)  ) %>%

  mutate(time_tot=time_DWA*DWA03b ) %>%
  ##Accessible under 30 minutes
  mutate(dwa_cond1=case_when( time_tot > 30 ~ 0, TRUE ~ 1) ) %>%

  ## protected source
  mutate(dwa_cond2=case_when(DWA01_num!=7 |DWA01_num !=9 |DWA01_num != 13 | DWA01_num != 96 |DWA01_num !=98 ~ 1,
                             TRUE ~ 0)   ) %>%

  ## It was available in the last 30 days
  mutate(dwa_cond3=case_when(DWA04_num==1 ~ 0, TRUE ~ 1)  ) %>%

  mutate(drinkingwater=case_when( (dwa_cond1==1 & dwa_cond2==1 & dwa_cond3==1) ~ 1, TRUE ~ 0)   ) %>%
  mutate(drinkingwater = labelled(drinkingwater,
                                 labels = c( "Yes" = 1,  "No" = 0 ),
                                 label = "Access to drinking water"))

table(main$drinkingwater, useNA = "ifany")


##Step.4. indicator aims to measure the proportion of persons of concern that reside in safe
###and secure settlements with access to basic services such as shelter

###Calculate DWE01 to DWE04 for shelter

###Turn all variables into numeric

main$DWE01_num <- labelled_chr2dbl(main$DWE01)
main$DWE02_num <- labelled_chr2dbl(main$DWE02)
main$DWE03_num <- labelled_chr2dbl(main$DWE03)
main$DWE04_num <- labelled_chr2dbl(main$DWE04)
class(main$DWE05)

##classify DWE01 as 1 when it has decent quality/improved

main <- main %>%
#Only apartment and house
  mutate(dwe01_cat=case_when(  (DWE01_num==1 | DWE01_num==2) ~ 1, TRUE ~ 0 )) %>%
  #unimproved floor when earth,sand,clay,mud, dung or other
  mutate(dwe02_cat=case_when(  (DWE02_num==1 | DWE02_num==2 | DWE02_num==96) ~ 0, TRUE ~ 1 ) ) %>%
  # unimproved roof all options except metal,wood,ceramic tiles, cement, roofing shingles/sheets
  mutate(dwe03_cat=case_when(  (DWE03_num==8 |DWE03_num==9 | DWE03_num==10 | DWE03_num==11 |
                                  DWE03_num==12 | DWE03_num==13 | DWE03_num==8) ~ 1 , TRUE ~ 0)  ) %>%
  #improved wall: cement,stone,bricks,cement blocks, covered adobe, wood planks
  mutate(dwe04_cat=case_when( (DWE04_num==10| DWE04_num==11| DWE04_num==12| DWE04_num==13|
                                 DWE04_num==14| DWE04_num==15) ~ 1, TRUE ~ 0)  )



table(main$dwe04_cat, useNA = "ifany")

####Calculate crowding index
main <- main %>%
  mutate(crowding=DWE05/HH01   ) %>%
  ##if crowding < 3
  mutate(dwe05_cat=case_when( crowding < 0.333 ~ 1, TRUE ~ 0)   )

## Add DWE08 and DWE09 to calculations -
# if HH is paying rent, can they afford to pay rent without financial distress
main$DWE08_num <- labelled_chr2dbl(main$DWE08)
main$DWE09_num <- labelled_chr2dbl(main$DWE09)
main <- main %>%
  #affordable if HH pays rent and often and always without financial distress
  mutate(dwe09_cat=case_when( (DWE08_num==1 & (DWE09_num==1 | DWE09_num==2)) ~ 1,
                              (DWE08_num==1 & (DWE09_num==3 | DWE09_num==4)) ~ 0,
                              DWE08_num==0 ~ NA_real_)
  )


table(main$dwe09_cat, useNA = "ifany")

####Combine all shelter indicators

##dwe01_cat / dwe02_cat / dwe03_cat / dwe04_cat / dwe05_cat / dwe09_cat
main <- main %>%

  mutate(shelter=case_when(
    dwe01_cat==0 | dwe02_cat==0 | dwe03_cat==0 | dwe04_cat==0 | dwe05_cat==0 | dwe09_cat==0  ~ 0,
    dwe01_cat==1 & dwe02_cat==1 & dwe03_cat==1 & dwe04_cat==1 & dwe05_cat==1 & dwe09_cat==1 ~ 1)  ) %>%
  mutate( shelter = labelled(shelter,
                                 labels = c( "Yes" = 1, "No" = 0 ),
                                 label = "Habitable and affordable shelter"))


table(main$shelter, useNA = "ifany")

###Step5. Calculate impact indicator based on shelter, electricity, drinkingwater and health_acc
main <-main %>%
  mutate(impact2_2=case_when(
    shelter==0 | electricity==0 | drinkingwater==0 | health_acc==0 ~ 0,
    shelter==1 & electricity==1 & drinkingwater==1 & health_acc==1 ~ 1)
  ) %>%
  mutate(impact2_2=labelled(impact2_2,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                  label="PoCs residing in physically safe and secure settlements with access to basic facilities"))

table(main$impact2_2, useNA = "ifany")

## 2.3 Proportion of PoC with access to health services. ############
##Module :HACC01 - HACC04

####Individual level indicator asked all HH members

main$HACC01_num <- labelled_chr2dbl(main$HACC01)
main$HACC03_num <- labelled_chr2dbl(main$HACC03)
class(main$HACC04_1)

#Calculate those who were not able to access due to reasons unrelated to asked services (when HACC04 is 7/8/96)

main <- main %>%

  mutate(health_NOacc=case_when(
      HACC03_num==1 & (HACC04_7==1 | HACC04_8==1 | HACC04_96==1 ) ~ 0,
      HACC03_num==1 & (HACC04_1==1 | HACC04_2==1 | HACC04_3==1 |HACC04_4==1 |HACC04_5==1 |
                         HACC04_6==1 | HACC04_9==1 | HACC04_10==1) ~ 1, TRUE ~ 0)
  ) %>% ## Those who needed services
  mutate(HACC_need=HACC01_num + health_NOacc
  ) %>%
  mutate(impact2_3=HACC01_num/HACC_need
  ) %>%
  mutate(impact2_3=labelled(impact2_3,
                             labels =c(
                               "Yes"=1,
                               "No"=0
                             ),
                             label="PoC has access to health services in the last 30 days when needed"))
main$impact2_3[is.nan(main$impact2_3)]<-NA



#calculate overall impact2_3
class(main$impact2_3)
mean(is.na(main$impact2_3))

table(main$impact2_3, useNA = "ifany")

## 3.2a: Proportion of PoC enrolled in primary education regardless of age ############
##Module :EDU01-EDU04


#Turn character variables into vector
main$EDU01_num <- labelled_chr2dbl(main$EDU01)
main$EDU02_num <- labelled_chr2dbl(main$EDU02)
main$EDU03_num <- labelled_chr2dbl(main$EDU03)


main <- main %>%
   mutate(edu_enrollpri=case_when(
     EDU01_num==1 & EDU02_num==1 & EDU03_num==2 ~ 1, EDU01_num==0 | EDU02_num==0 ~ 0, TRUE ~ 0)
   ) %>%

   # Contextualise age group for primary school attendance
   mutate(age_primary=case_when( HH07 >= 6 & HH07 <=10 ~ 1, TRUE ~ 0) ) %>%

   mutate(impact3_2a=sum(edu_enrollpri)/sum(age_primary)) %>%
   mutate(impact3_2a=labelled(impact3_2b,
                              labels =c("Yes"=1,"No"=0  ),
                              label="PoC enrolled in primary education"))

 mean(main$impact3_2a)

 table(main$impact3_2a, useNA = "ifany")

## 3.2b: Proportion of PoC enrolled in secondary education ############
##Module :EDU01-EDU04

 #Turn character variables into vector
 main$EDU01_num <- labelled_chr2dbl(main$EDU01)
 main$EDU02_num <- labelled_chr2dbl(main$EDU02)
 main$EDU03_num <- labelled_chr2dbl(main$EDU03)

 #Include if they are attending secondary or secondary -technical and vocational
 main <- main %>%
   mutate(edu_enrollsec = case_when(
     EDU01_num==1 & EDU02_num==1 & (EDU03_num==3 | EDU03_num==4) ~ 1, EDU01_num==0 | EDU02_num==0 ~ 0,
     TRUE ~ 0)
   ) %>%
   mutate(age_secondary = case_when(
     HH07 >= 11 & HH07 <=18 ~ 1, TRUE ~ 0) #Contextualise age group for secondary school attendance
   ) %>%
   mutate(impact3_2b=sum(edu_enrollsec)/sum(age_secondary)  ) %>%
   mutate(impact3_2b=labelled(impact3_2b,
                             labels =c(
                               "Yes"=1,
                               "No"=0
                             ),
                             label="PoC enrolled in secondary education"))


 table(main$impact2_3, useNA = "ifany")

## 3.3 Proportion of PoC feeling safe walking alone in their neighborhood ############
##Module :SAF01

##This indicator comes from main dataset based on the respondent randomly selected for individual level
##questions
#Turn character variables into vector
main$SAF01_num <- labelled_chr2dbl(main$SAF01)
#if unsafe or very unsafe 0 - if don't know - 98 include, refusals will go into missing
main <- main %>%
   mutate(impact3_3=case_when(
     SAF01_num==1 | SAF01_num==2 ~ 1,
     SAF01_num==3 | SAF01_num==4 | SAF01_num==98 ~ 0, SAF01_num==99 ~ NA_real_)
   ) %>%
    mutate(impact3_3=labelled(impact3_3,
                                   labels =c(
                                     "Yes"=1,
                                     "No"=0
                                   ),
                                   label="PoC feeling safe walking alone"))


table(main$impact3_3, useNA = "ifany")

# -> Outcome Indicators ############

# 1.2 Proportion of children <5 years whose birth have been registered with a civil authority ############
##Module :REG03 - REG04

  ind$REG03_num <- labelled_chr2dbl(ind$REG03) # birth certificate
  ind$REG04_num <- labelled_chr2dbl(ind$REG04) # birth has been registered

  ##Calculate children who has a birth certificate

  ind <- ind %>%
    mutate(birthCertificate=case_when(
      REG03_num==0 | REG03_num==98 ~ 0, REG03_num==1 ~ 1)
           ) %>%
    mutate(birthCertificate=labelled(birthCertificate,
                                  labels=c(
                                    'Yes'=1,
                                    'No'=0
                                  ),
                                  label="Children under 5 with a birth certificate"))
  ##Calculate children who has been registered with civil authorities

  ind <- ind %>%
    mutate(birthRegistered=case_when(
      REG04_num==0 | REG04_num==98 ~ 0, REG04_num==1 ~ 1, REG04_num==99 ~NA_real_)
    ) %>%
    mutate(birthRegistered=labelled(birthRegistered,
                                  labels=c(
                                    'Yes'=1,
                                    'No'=0
                                  ),
                                  label="Children under 5 birth registered with civil authorities"))


  ##Calculate outcome indicator 1.2


  ind <- ind %>%
    mutate(outcome1_2=case_when(
      birthRegistered==1 & HH07 <=5 ~ 1, birthRegistered==0 & HH07 <=5 ~ 0)
    ) %>%
    mutate(outcome1_2=labelled(outcome1_2,
                                    labels=c(
                                      'Yes'=1,
                                      'No'=0
                                    ),
                                    label="Proportion of children under 5 birth registered with civil authorities"))

## 1.3 Proportion of PoC with legally recognized identity documents or credentials ############
##Module :REG01 - REG02 - REG05 - REG06

  ###Calculate having documents for children under 5

  ##Turn into numeric variables


  ind$REG05a_num <- labelled_chr2dbl(ind$REG05a) # passport
  ind$REG05b_num <- labelled_chr2dbl(ind$REG05b) # civil/government issued ID
  ind$REG05c_num <- labelled_chr2dbl(ind$REG05c) # residency permit
  ind$REG05d_num <- labelled_chr2dbl(ind$REG05d) # statelessness documentation
  ind$REG05e_num <- labelled_chr2dbl(ind$REG05e) # household card of address/family book
  ind$REG05f_num <- labelled_chr2dbl(ind$REG05f) # social security card
  ind$REG06_num <- labelled_chr2dbl(ind$REG06) # any other document establishes identity

  #add birth certificate as additional document from REG03
  ind <- ind %>%
    mutate(document_under5=case_when(
      REG05a_num==1 | REG05b_num==1 | REG05c_num==1 | REG05d_num==1 | REG05e_num==1 |
        REG05f_num==1 |REG06_num==1 | birthCertificate==1 ~ 1,
      REG05a_num==0 & REG05b_num==0 & REG05c_num==0 & REG05d_num==0 & REG05e_num==0 &
        REG05f_num==0 & REG06_num==0 & birthCertificate==0 ~ 0, TRUE ~ NA_real_
    ))

  ###Calculate having documents for above 5

  ##Turn into numeric variables


  ind$REG01a_num <- labelled_chr2dbl(ind$REG01a) # passport
  ind$REG01b_num <- labelled_chr2dbl(ind$REG01b) # birth certificate
  ind$REG01c_num <- labelled_chr2dbl(ind$REG01c) # civil/ government issued ID
  ind$REG01d_num <- labelled_chr2dbl(ind$REG01d) # residency permit
  ind$REG01e_num <- labelled_chr2dbl(ind$REG01e) # statelessness documentation
  ind$REG01f_num <- labelled_chr2dbl(ind$REG01f) # household card of address/family book
  ind$REG01g_num <- labelled_chr2dbl(ind$REG01g) # social security card
  ind$REG02_num <- labelled_chr2dbl(ind$REG02) # any other document establishes identity

  #add birth certificate as additional document from REG03
  ind <- ind %>%
    mutate(document_above5=case_when(
      REG01a_num==1 | REG01b_num==1 | REG01c_num==1 | REG01d_num==1 | REG01e_num==1 |
        REG01f_num==1 | REG01g_num==1 |REG02_num==1 ~ 1,
      REG01a_num==0 & REG01b_num==0 & REG01c_num==0 & REG01d_num==0 & REG01e_num==0 &
        REG01f_num==0 & REG01g_num==0 & REG02_num==0 ~ 0, TRUE ~ NA_real_)

  ##calculate outcome indicator by combining under 5 and above 5 responses
    ) %>%
    mutate(outcome1_3=case_when(
     (document_above5==1 | document_under5==1) ~ 1,
     (document_above5==0 | document_under5==0) ~ 0)
    ) %>%
    mutate(outcome1_3=labelled(outcome1_3,
                               labels=c(
                                 'Yes'=1,
                                 'No'=0
                               ),
                               label="PoCs with legally recognized documents"))



## 4.1 Proportion of PoC who know where to access available GBV services ############
##Module :GBV01

  ##Turn into numeric variables for services

  main$GBV01a_num <- labelled_chr2dbl(main$GBV01a) # health services
  main$GBV01b_num <- labelled_chr2dbl(main$GBV01b) # psycho-social services
  main$GBV01c_num <- labelled_chr2dbl(main$GBV01c) # safety and security services
  main$GBV01d_num <- labelled_chr2dbl(main$GBV01d) # legal assistance

  main <- main %>%
    mutate(outcome4_1=case_when(
      GBV01a_num==1 |  GBV01b_num==1 |  GBV01c_num==1 |  GBV01d_num==1 ~ 1,
      TRUE ~ 0)
    ) %>%
    mutate(outcome4_1=labelled(outcome4_1,
                               labels=c(
                                 'Yes'=1,
                                 "No"=0
                               ),
                               label="Poc who know where to access available GBV services"
                               ))



## 4.2 Proportion of PoC who do not accept violence against women ############
##Module :VAW01

  #If randomly selected adult who believes a husband is justified in beating his wife in various circumstances

  #Turn into numeric variables
  main$VAW01a_num <- labelled_chr2dbl(main$VAW01a)
  main$VAW01b_num <- labelled_chr2dbl(main$VAW01b)
  main$VAW01c_num <- labelled_chr2dbl(main$VAW01c)
  main$VAW01d_num <- labelled_chr2dbl(main$VAW01d)
  main$VAW01e_num <- labelled_chr2dbl(main$VAW01e)
  ##If yes selected for any of the circumstances
 main <- main %>%
   mutate(outcome4_2=case_when(
     VAW01a_num==1 | VAW01b_num==1 |  VAW01c_num==1 |  VAW01d_num==1 | VAW01e_num==1 ~ 0,
     VAW01a_num==0 & VAW01b_num==0 &  VAW01c_num==0 &  VAW01d_num==0 & VAW01e_num==0 ~ 1,
     TRUE ~ NA_real_)
   ) %>%
   mutate(outcome4_2=labelled(outcome4_2,
                              labels=c(
                                'Yes'=1,
                                "No"=0
                              ),
                              label="Poc who do not accept violence against women"
   ))


## 5.2 Proportion of children who participate in community-based child protection programmes ############
##Module :COMM01-COMM04

 #Turn into numeric variables
 ind$COMM01_num <- labelled_chr2dbl(ind$COMM01)
 ind$COMM02_num <- labelled_chr2dbl(ind$COMM02)
 ind$COMM03_num <- labelled_chr2dbl(ind$COMM03)
 ind$COMM04_num <- labelled_chr2dbl(ind$COMM04)

 #Children who attended at least 2 times, under adult supervision in a physically safe area

 ind <- ind %>%
   mutate(outcome5_2=case_when(
     (COMM01_num==1 & COMM02_num >=2 & COMM03_num==1 & COMM04_num==1) ~ 1,
     COMM01_num==0 | COMM02_num < 2 | COMM03_num==0 | COMM03_num==98 |
       COMM03_num==0 | COMM03_num==98 ~ 0, TRUE ~ NA_real_)
   ) %>%
   mutate(outcome5_2=labelled(outcome5_2,
                              labels=c(
                                'Yes'=1,
                                "No"=0
                              ),
                              label="Poc of children who participate in community-based
                              child protection programmes"
   ))


## 8.2 Proportion of PoC with primary reliance on clean (cooking) fuels and technology ############
##Module :COOK01-COOK03

 ###Based on MICS calculation : TC4.1

 main$COOK01_num <- labelled_chr2dbl(main$COOK01)
 main$COOK02_num <- labelled_chr2dbl(main$COOK02)
 main$COOK03_num <- labelled_chr2dbl(main$COOK03)

main <- main %>%
   mutate(
     outcome8_2 = case_when(
       COOK01_num == 1 & (COOK02 %in% c("1", "2", "3", "4", "5") | (COOK02 %in% c("6", "96") & COOK03 %in% c("1", "2", "3", "22"))) ~ 1, # see https://mics.unicef.org/files?job=W1siZiIsIjIwMTcvMDIvMDMvMTYvMjcvMjUvNTk5L1BpY3RvcmlhbHNfV0hPX0hvdXNlaG9sZF9FbmVyZ3lfVXNlX0NhdGFsb2d1ZV9TZXB0ZW1iZXJfMjAxNl8ucGRmIl1d&sha=57b4a452fcc0ac88
       COOK01_num == 1 & (COOK02 %in% c("7", "8", "9", "10") | (COOK02 %in% c("6", "96") & !(COOK03 %in% c("1", "2", "3", "22")))) ~ 0,
       COOK01_num == 0 ~ NA_real_
     )
   ) %>%
   mutate(
     outcome8_2 = labelled(outcome8_2,
                            labels = c(
                              "No" = 0,
                              "Yes" = 1
                            ),
                            label = "PoC with primary reliance on clean (cooking) fuels and technology"
     )

   )


table(main$outcome8_2, useNA = "ifany")

## 9.1 Proportion of PoC living in habitable and affordable housing ############
##Module :DWE01-DWE05 & DWE08-DWE09

###Calculate DWE01 to DWE04 for shelter

###Turn all variables into numeric

main$DWE01_num <- labelled_chr2dbl(main$DWE01)
main$DWE02_num <- labelled_chr2dbl(main$DWE02)
main$DWE03_num <- labelled_chr2dbl(main$DWE03)
main$DWE04_num <- labelled_chr2dbl(main$DWE04)
class(main$DWE05)

##classify DWE01 as 1 when it has decent quality/improved

main <- main %>%

  mutate(dwe01_cat=case_when( #Only apartment and house
    (DWE01_num==1 | DWE01_num==2) ~ 1, TRUE ~ 0 )
  ) %>%
  mutate(dwe02_cat=case_when( #unimproved floor when earth,sand,clay,mud, dung or other
    (DWE02_num==1 | DWE02_num==2 | DWE02_num==96) ~ 0, TRUE ~ 1 )
  ) %>%
  mutate(dwe03_cat=case_when( #unimproved roof all options except metal,wood,ceramic tiles, cement, roofing shingles/sheets
    (DWE03_num==8 |DWE03_num==9 | DWE03_num==10 | DWE03_num==11 |
       DWE03_num==12 | DWE03_num==13 | DWE03_num==8) ~ 1 , TRUE ~ 0)
  ) %>%
  mutate(dwe04_cat=case_when( #improved wall: cement,stone,bricks,cement blocks, covered adobe, wood planks
    (DWE04_num==10| DWE04_num==11| DWE04_num==12| DWE04_num==13| DWE04_num==14| DWE04_num==15) ~ 1,
    TRUE ~ 0)
  )

table(main$dwe04_cat, useNA = "ifany")

####Calculate crowding index

main <- main %>%

  mutate(crowding=DWE05/HH01
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding < 3
    crowding < 0.333 ~ 1, TRUE ~ 0)
  )

## Add DWE08 and DWE09 to calculations - if HH is paying rent, can they afford to pay rent without financial distress
main$DWE08_num <- labelled_chr2dbl(main$DWE08)
main$DWE09_num <- labelled_chr2dbl(main$DWE09)

main <- main %>%
#affordable if HH pays rent and often and always without financial distress
  mutate(dwe09_cat=case_when(
    (DWE08_num==1 & (DWE09_num==1 | DWE09_num==2)) ~ 1,
    (DWE08_num==1 & (DWE09_num==3 | DWE09_num==4)) ~ 0,  DWE08_num==0 ~ NA_real_)
  )

####Combine all shelter indicators

##dwe01_cat / dwe02_cat / dwe03_cat / dwe04_cat / dwe05_cat / dwe09_cat


main <- main %>%

  mutate(outcome9_1=case_when(
    dwe01_cat==0 | dwe02_cat==0 | dwe03_cat==0 | dwe04_cat==0 | dwe05_cat==0 | dwe09_cat==0  ~ 0,
    dwe01_cat==1 & dwe02_cat==1 & dwe03_cat==1 & dwe04_cat==1 & dwe05_cat==1 & dwe09_cat==1 ~ 1)
  ) %>%
  mutate( outcome9_1 = labelled(outcome9_1,
                             labels = c(
                               "Yes" = 1,
                               "No" = 0
                             ),
                             label = "PoC living in habitable and affordable housing"))


table(main$outcome9_1, useNA = "ifany")

## 9.2 Proportion of PoC that have energy to ensure lighting ############

##Module :LIGHT01-LIGHT03

main$LIGHT01_num <- labelled_chr2dbl(main$LIGHT01)
main$LIGHT02_num <- labelled_chr2dbl(main$LIGHT02)
main$LIGHT03_num <- labelled_chr2dbl(main$LIGHT03)

main <- main %>%
  mutate(outcome9_2=
           case_when(LIGHT01_num==1 & (LIGHT02_num==1 |LIGHT02_num==3 | LIGHT02_num==5 | LIGHT02_num==6
                                       | LIGHT02_num==7 | LIGHT02_num==8) &
                       (LIGHT03_num!=1 | LIGHT03_num!=96 | LIGHT03_num!=98 ) ~ 1,
                     TRUE ~ 0)
  ) %>%
  mutate( outcome9_2 = labelled(outcome9_2,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "PoC that have energy to ensure lighting"))


table(main$outcome9_2, useNA = "ifany")

## 10.1 Proportion of children 9mo-5years who have received measles vaccination ############
##Module :MMR01-MMR04 ##  MICS TC.1.1 UNICEF calculates on the first dose received##

#Calculate into numeric
ind$MMR03_num <- labelled_chr2dbl(ind$MMR03)

##Children who had et least one dose of measles vaccination

ind <- ind %>%
  mutate(outcome10_1=case_when(
    MMR03_num==1 ~ 1, MMR03_num==0  | MMR03_num==98 ~ 0)
  ) %>%
  mutate( outcome10_1 = labelled(outcome10_1,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Children 9mo-5years who have received measles vaccination"))



## 10.2 Proportion of births attended by skilled health personnel ############
##Module :BIR01-BIR04

##If there are live births in the last 2 years

main$BIR03_num <- labelled_chr2dbl(main$BIR03)
main$BIR01_num <- labelled_chr2dbl(main$BIR01)
main$BIR02_num <- labelled_chr2dbl(main$BIR02)

main <- main %>%
  mutate(outcome10_2=case_when(
    (BIR01_num==1 | BIR02_num==1) & (BIR03_num==1 |BIR03_num==2 | BIR03_num==3 ) ~ 1,
    (BIR01_num==1 | BIR02_num==1) & (BIR03_num==4 |BIR03_num==5 | BIR03_num==6
                                     | BIR03_num==96| BIR03_num==98) ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate( outcome10_2 = labelled(outcome10_2,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "Births attended by skilled health personnel"))



## 12.1 Proportion of PoC using at least basic drinking water services ############
##Module :DWA01-DWA04

###Improved drinking water resource that is accessible under 30 minutes in the last 30 days

main$DWA03a_num <- labelled_chr2dbl(main$DWA03a)
main$DWA02_num <- labelled_chr2dbl(main$DWA02)
main$DWA01_num <- labelled_chr2dbl(main$DWA01)
main$DWA04_num <- labelled_chr2dbl(main$DWA04)
class(main$DWA03a_num)
class(main$DWA03b)

main <- main %>%

  mutate(time_DWA=case_when(    DWA03a_num==1~1, DWA03a_num==2~60)) %>%

  mutate(time_tot=time_DWA*DWA03b) %>%
  ## accessible under 30 minutes
  mutate(dwa_cond1=case_when( time_tot > 30 ~ 0, TRUE ~ 1)  ) %>%

  ## protected source
  mutate(dwa_cond2=case_when(DWA01_num!=7 |DWA01_num !=9 |DWA01_num != 13 |
                               DWA01_num != 96 |DWA01_num !=98 ~ 1,  TRUE ~ 0) ) %>%

  ## drinking water was available in the last 30 days
  mutate(dwa_cond3=case_when(DWA04_num==1 ~ 0, TRUE ~ 1)  ) %>%

  mutate(outcome12_1=case_when( (dwa_cond1==1 & dwa_cond2==1 & dwa_cond3==1) ~ 1, TRUE ~ 0) ) %>%
  mutate(outcome12_1 = labelled(outcome12_1,
                                  labels = c( "Yes" = 1,"No" = 0
                                  ),
                                  label = "PoC using at least basic drinking water services"))

table(main$outcome12_1, useNA = "ifany")

## 12.2 Proportion of PoC with access to a safe household toilet ############
##Module :TOI01-TOI05 ##MICS calculation WS3.1/WS3.4

main$TOI01_num <- labelled_chr2dbl(main$TOI01)
main$TOI02_num <- labelled_chr2dbl(main$TOI02)
main$TOI03_num <- labelled_chr2dbl(main$TOI03)
main$TOI04_num <- labelled_chr2dbl(main$TOI04)
main$TOI05_num <- labelled_chr2dbl(main$TOI05)

main <- main %>%
  mutate(toi_cond1=case_when(TOI01_num==1 |TOI01_num==2 | TOI01_num==3 | TOI01_num==4 |TOI01_num==5 |
                               TOI01_num==6 | TOI01_num==7 | TOI01_num==9 ~ 1,
                             TOI01_num==8 | TOI01_num==10 | TOI01_num==11 | TOI01_num==12 | TOI01_num==96 ~ 0,
                             TRUE ~ NA_real_)
  ) %>%
  mutate(toi_cond2=case_when(
    TOI02_num==1 & (TOI03_num==5 |TOI03_num==96) ~ 0, #Unsafe disposal
    TOI02_num==1 & (TOI03_num==1 |TOI03_num==2 |TOI03_num==3 |TOI03_num==4| TOI03_num==98 ) ~ 1, #safe
    TOI02_num==2 ~ 0, TOI02_num==98 ~ 1, TRUE ~ NA_real_)
  ) %>%
  mutate(toi_cond3=case_when(
    TOI05_num==1 ~ 0, TOI05_num==0 ~ 1) # HH sharing toilet with other HHs
  ) %>%


### improved sanitation facility / safely disposed in situ / not shared with other HHs

  mutate(outcome12_2=case_when(
    toi_cond1==0 | toi_cond2==0 |toi_cond3==0 ~ 0,
    TRUE ~ 1)
  ) %>%
  mutate(outcome12_2 = labelled(outcome12_2,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "PoC with access to a safe household toilet"))



## 13.1 Proportion of PoC with an account at a bank or other financial institution or with a mobile-money service provider ############

##Module :BANK01-BANK05

main$BANK01_num <- labelled_chr2dbl(main$BANK01)
main$BANK02_num <- labelled_chr2dbl(main$BANK02)
main$BANK03_num <- labelled_chr2dbl(main$BANK03)
main$BANK04_num <- labelled_chr2dbl(main$BANK04)
main$BANK05_num <- labelled_chr2dbl(main$BANK05)

##include if the respondent has an account on their own/with someone else, ATM card, personal bank card
##or personally used phone to send money (proxy)

main <- main %>%
  mutate(
    outcome13_1 = case_when(
      BANK01_num ==1 | BANK02_num== 1 | BANK03_num==1 |BANK05_num==1 ~ 1,
      BANK01_num==0 & BANK02_num==0 & BANK03_num==0 & BANK04_num==0 ~ 0,
      TRUE ~ 0)
  ) %>%
  mutate(outcome13_1 = labelled(outcome13_1,
                            labels = c(  "Yes" = 1, "No" = 0 ),
                            label = "PoC with an account at a bank or other financial institution or
                            with a mobile-money service provider")
  )

table(main$outcome13_1, useNA = "ifany")

## 13.2 Proportion of PoC who self-report positive changes in their income compared to previous year ############

##Module :INC01

### Only calculate as positive if they responded 'more'

main$INC01_num <- labelled_chr2dbl(main$INC01)

main <- main %>%
  mutate(outcome13_2=case_when(
    INC01_num==1 ~ 1,
    INC01_num==2 |INC01_num==3 |INC01_num==98 ~ 0 )  ) %>%
  mutate(outcome13_2 = labelled(outcome13_2,
                                labels = c( "Yes" = 1, "No" = 0  ),
                                label = "PoC who self-report positive changes in their income compared
                                to previous year")  )


table(main$outcome13_2, useNA = "ifany")

## 13.3 Proportion of PoC (working age) who are unemployed ############

####Numerator: Those of working age who were not in employment, looked for employment in the past 30 days and were available to take up employment
####Denominator: Those of working age in labour force

###Turn into numeric variables
main$UNEM01_num <- labelled_chr2dbl(main$UNEM01)
main$UNEM02_num <- labelled_chr2dbl(main$UNEM02)
main$UNEM03_num <- labelled_chr2dbl(main$UNEM03)
main$UNEM04_num <- labelled_chr2dbl(main$UNEM04)
main$UNEM05_num <- labelled_chr2dbl(main$UNEM05)
main$UNEM06_num <- labelled_chr2dbl(main$UNEM06)
main$UNEM07_num <- labelled_chr2dbl(main$UNEM07)
main$UNEM08_num <- labelled_chr2dbl(main$UNEM08)
main$UNEM09_num <- labelled_chr2dbl(main$UNEM09)
main$UNEM10_num <- labelled_chr2dbl(main$UNEM10)


main <- main %>%
  mutate(employed=case_when(
    UNEM01_num==1 ~ 1,
    UNEM02_num==1 & UNEM07_num==3 ~ 1,
    UNEM04_num==1 ~ 1,
    UNEM02_num==1 & UNEM07_num==1 & (UNEM08_num==1 | UNEM08_num==2) ~ 1,
    UNEM05_num==1 & UNEM06_num==3 ~ 1,
    UNEM05_num==1 & (UNEM06_num==1 | UNEM06_num==2) & (UNEM08_num==1 | UNEM08_num==2) ~ 1)   ) %>%

  mutate(unemployed=case_when(     employed==0 & UNEM09_num==1 & UNEM10_num==1 ~ 1,  TRUE ~ 0)) %>%

  mutate(labour_force=case_when( employed==1 | unemployed==1 ~ 1) ) %>%

  mutate(outcome13_3=unemployed/labour_force)

table(main$outcome13_3, useNA = "ifany")

## 14.1 Proportion of returnees with legally recognized identity documents or credentials to support the return ############

##Module :REG01-REG06


###DISAGGREGATE FOR RETURNEES ONLY

###Calculate having documents for children under 5

##Turn into numeric variables


ind$REG05a_num <- labelled_chr2dbl(ind$REG05a) # passport
ind$REG05b_num <- labelled_chr2dbl(ind$REG05b) # civil/government issued ID
ind$REG05c_num <- labelled_chr2dbl(ind$REG05c) # residency permit
ind$REG05d_num <- labelled_chr2dbl(ind$REG05d) # statelessness documentation
ind$REG05e_num <- labelled_chr2dbl(ind$REG05e) # household card of address/family book
ind$REG05f_num <- labelled_chr2dbl(ind$REG05f) # social security card
ind$REG06_num <- labelled_chr2dbl(ind$REG06) # any other document establishes identity

#add birth certificate as additional document from REG03
ind <- ind %>%
  mutate(document_under5=case_when(
    REG05a_num==1 | REG05b_num==1 | REG05c_num==1 | REG05d_num==1 | REG05e_num==1 |
      REG05f_num==1 |REG06_num==1 | birthCertificate==1 ~ 1,
    REG05a_num==0 & REG05b_num==0 & REG05c_num==0 & REG05d_num==0 & REG05e_num==0 &
      REG05f_num==0 & REG06_num==0 & birthCertificate==0 ~ 0, TRUE ~ NA_real_
  ))

###Calculate having documents for above 5

##Turn into numeric variables


ind$REG01a_num <- labelled_chr2dbl(ind$REG01a) # passport
ind$REG01b_num <- labelled_chr2dbl(ind$REG01b) # birth certificate
ind$REG01c_num <- labelled_chr2dbl(ind$REG01c) # civil/ government issued ID
ind$REG01d_num <- labelled_chr2dbl(ind$REG01d) # residency permit
ind$REG01e_num <- labelled_chr2dbl(ind$REG01e) # statelessness documentation
ind$REG01f_num <- labelled_chr2dbl(ind$REG01f) # household card of address/family book
ind$REG01g_num <- labelled_chr2dbl(ind$REG01g) # social security card
ind$REG02_num <- labelled_chr2dbl(ind$REG02) # any other document establishes identity

#add birth certificate as additional document from REG03
ind <- ind %>%
   ##calculate outcome indicator by combining under 5 and above 5 responses
  mutate(document_above5=case_when(
    REG01a_num==1 | REG01b_num==1 | REG01c_num==1 | REG01d_num==1 | REG01e_num==1 |
      REG01f_num==1 | REG01g_num==1 |REG02_num==1 ~ 1,
    REG01a_num==0 & REG01b_num==0 & REG01c_num==0 & REG01d_num==0 & REG01e_num==0 &
      REG01f_num==0 & REG01g_num==0 & REG02_num==0 ~ 0, TRUE ~ NA_real_)  ) %>%

  mutate(outcome14_1=case_when(
    (document_above5==1 | document_under5==1) ~ 1,
    (document_above5==0 | document_under5==0) ~ 0)  ) %>%
  mutate(outcome14_1=labelled(outcome14_1,
                             labels=c( 'Yes'=1,  'No'=0 ),
                             label="PoCs with legally recognized documents for returnees"))


table(main$outcome14_1, useNA = "ifany")

## 16.1 Proportion of PoC with secure tenure rights and/or property rights to housing and/or land ############

##Module :DWE06-DWE07 & DWE10-DWE11

main$DWE06_num <- labelled_chr2dbl(main$DWE06)
main$DWE07_num <- labelled_chr2dbl(main$DWE07)
main$DWE10_num <- labelled_chr2dbl(main$DWE10)
main$DWE11_num <- labelled_chr2dbl(main$DWE11)

main <- main %>%

  # likelihood of losing right for housing is unlikely
  mutate(housing_cond1=case_when(  (DWE11_num==1 | DWE11_num==2 ) ~ 1,  TRUE ~ 0)  ) %>%

  mutate(housing_cond2=case_when(
    # have official documents to proof residency
    DWE10_num==96~ 0,
    DWE10_num==1 | DWE10_num==2 |  DWE10_num==3 | DWE10_num==4 | DWE10_num==5 | DWE10_num==6 ~ 1) ) %>%

  mutate(housing_cond3=case_when(
    # not un-owned or not squatting
    DWE06_num==9 | DWE07_num==9 ~ 0,  TRUE ~ 1)  ) %>%

  mutate(outcome16_1=case_when(
    housing_cond1==1 & housing_cond2==1 & housing_cond3==1 ~ 1,
    housing_cond1==0 | housing_cond2==0 | housing_cond3==0 ~ 0) ) %>%
  mutate(outcome16_1=labelled(outcome16_1,
                             labels=c( 'Yes'=1, 'No'=0 ),
                             label="PoC with secure tenure rights and/or property rights
                             to housing and/or land"))

table(main$outcome16_1, useNA = "ifany")

## 16.2 Proportion of PoC covered by social protection floors/systems ############

##Module :UNHCR Core Indicator Metadata	SPF01
##Turn into numeric variables
main$SPF01a_num <- labelled_chr2dbl(main$SPF01a)
main$SPF01b_num <- labelled_chr2dbl(main$SPF01b)
main$SPF01c_num <- labelled_chr2dbl(main$SPF01c)
main$SPF01d_num <- labelled_chr2dbl(main$SPF01d)
main$SPF01e_num <- labelled_chr2dbl(main$SPF01e)
main$SPF01f_num <- labelled_chr2dbl(main$SPF01f)
main$SPF01g_num <- labelled_chr2dbl(main$SPF01g)
main$SPF01h_num <- labelled_chr2dbl(main$SPF01h)
# If PoC has covered by at least one of the social protection floors/systems
main <- main %>%
  mutate(outcome16_2=case_when(
    SPF01a_num==1 |SPF01b_num==1 |SPF01c_num==1 |SPF01d_num==1 | SPF01e_num==1 | SPF01f_num==1
     | SPF01g_num==1 | SPF01h_num==1 ~ 1,  TRUE ~ 0)  ) %>%
  mutate(outcome16_2=labelled(outcome16_2,
                              labels=c(  'Yes'=1, 'No'=0),
                              label="PoC covered by social protection floors/systems"))

table(main$outcome16_2, useNA = "ifany")

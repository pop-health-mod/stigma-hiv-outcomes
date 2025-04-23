rm(list=ls()) #clears workspace
gc()
library(data.table)
library(dplyr)
library(sjmisc)
library(haven)
library(survey)
library(openxlsx)
library(countrycode)

path_data <- here::here("data")
path_out <- here::here("outputs")

'%ni%' <- Negate('%in%')

setwd("~/Dropbox (Personal)/McGill Research Projects/Stigma")
root  <- "~/Dropbox (Personal)/McGill Research Projects/Stigma"
surveys_dir <- "~/Library/CloudStorage/OneDrive-McGillUniversity/Projects/2024-9 Stigma/HIV-Stigma-Africa/Survey files/SABSSM"
path_data <- here::here("data")
path_out <- here::here("outputs")

'%ni%'  <- Negate('%in%')
set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }

za17 <- read.csv(paste0(surveys_dir, '/SABSSM2017_Combined.csv') )
za12 <- read.csv(paste0(surveys_dir, '/SABSSM2012_Combined_V02.csv') )
za08 <- read.csv(paste0(surveys_dir, '/SABSSM2008_Adult.csv') ) 


za17 <- subset(za17, age_q >= 15 & !is.na(idnum)) ## some variables have all missing data (including idnum) except for age and a few vars 
za12 <- subset(za12, q1_1 >= 15 & !is.na(uniqueid)) ## some variables have all missing data (including idnum) except for age and a few vars 
za08 <- subset(za08, q1_1 >= 15) ## everyone is over 25 in this dataset however. Need to use this since combined dataset has not HIV testing and stigma data 

# 2017 Survey ----

dat <- za17 %>% dplyr::select(psu = sal,
                                   stratum = province, ## also admin 1
                                   indweight = ibreal12_combined, # individual weight 
                                   hivweight = ibreal1_combined, # specimen data weight
                                   sex = sex_q, # sex
                                   geotype = geotype, # residence type
                                   age = age_q, # age
                                   edu = q1_15c, ## educational level 
                                   eversex = q5_1, # ever had sex
                                   marr =  q1_4a, # marriage (current)
                                   con = q6_20_1, # condom use @ last sex with most recent partner 
                                   life_partners = q5_4, # lifetime partners
                                   ## HIV test
                                   hivtest_ever = q9_2, # ever had an HIV test
                                   test_time = q9_3, # how long ago was last HIV test
                                   received_results = q9_4, # received results of most recent test
                                   preg_24 = q7_1, ## have you been pregnant in
                                   anc_attend = q7_5, ## have you attended ANC 
                                   anc_offered = q7_5, ## have you been offered an HIV test
                                   anc_test = q7_6, # have you been tested for HIV during any of your antenatal care clinic visits?
                                   ## Stigma 
                                   fresh_veg = q2_5b, ## would you buy fresh vegetables 
                                   teacher_allowed = q2_5d, ## should a teacher be allowed?
                                   children_allowed = q2_5j, ## should children be allowed?
                                   secret = q2_5f, # would you keep family members HIV status a secret
                                   ## Knowledge (mosquito and witchcraft not collected )
                                   share_food = q2_2f, ## sharing food with someone who is infected
                                   healthy = q2_2c, ## can a healthy looking person have HIV?
                                   one_part = q2_2e, ## reduce risk one uninfected partner who has no other partners
                                   always_cond = q2_2g, ## risk reduced always condom 
                                     
                                   arv_bio = arv, # exposed to antiretroviral therapy
                                   hivstatus = hivstat, # HIV status (viral load based)
                                   fdhvlf00, fdhvlf01, # HIV viral load ??
                                   vl_suppresion01, # viral load suppression
                                   
                                   hiv_self =  q9_10a, #self reported HIV status 
                                   arv_self = q9_11a, # self-reported ARV use 
                                   ever_miss = q9_13, ## have you ever missed 
                                   rec_miss = q9_14, ## did you miss arts in the past 30 days 
                                   q9_16_days, ## how long were you not taking ARTs
                                   q9_16_months ## how long were you not taking ARTs
                                  ) 


dat$country <- 'South Africa'
dat$surveyid <- as.factor('ZA2017SABSSM')
dat$survyear <- '2017'
dat$psu <- as.numeric(as.character(dat$psu))
dat$psu_u <- as.factor(as.character(paste0(dat$psu,sep= "_",dat$surveyid)))
dat$age <- as.numeric(dat$age)

dat$stratum <- as.numeric(dat$stratum)
dat$hivweight <- as.numeric(dat$hivweight)
dat$indweight <- as.numeric(dat$indweight)

dat$iso <- countrycode(dat$country, "country.name", "iso3c")

agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat$sex <- as.numeric(dat$sex)
dat$sex <- factor(dat$sex, levels=c(1,2),labels=c("male", "female"))


dat$ad1 <- as.factor(as.character(paste0(dat$stratum,sep= "_",dat$surveyid))) ## (province)

dat$restype <- factor(set_na(dat$geotype, 9) > 1, c(FALSE, TRUE), c("Urban", "Rural")) #for when value is >1 code as TRUErural, otherwise urban

dat$hivstatus <- as.numeric(set_na(dat$hivstatus) < 2)  ## 2 = negative 1= post

dat$hiv_self<-as.numeric(set_na(dat$hiv_self, c(3)) < 2 ) ## 2 = negative 1= post (only asked to people who report ever testing)

dat$indweight_std <- (dat$indweight/sum(dat$indweight, na.rm = T) ) * nrow(dat[!is.na(dat$indweight),])## total sample size 
dat$hivweight_std <- (dat$hivweight/sum(dat$hivweight, na.rm = T) ) * sum(!is.na(dat$hivstatus)) ## sample size in the HIV analysis 


dat$edu <- as.numeric(set_na(dat$edu, 98)) 
dat$edu_new <- cut(dat$edu,
    breaks=c(-1, 0.5, 6, 13, 15),
    labels=c(0, 1, 2, 3))

dat$edu<- factor(dat$edu_new, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))

dat$eversex <- as.numeric(set_na(dat$eversex,3) < 2)  ## 2 = no 1=yes

dat$part <- as.numeric(set_na(dat$life_partners))

## If never had sex, no partners in the past year or in their lifetime 
dat$part[dat$eversex==0] <- 0

dat$currMarried <- as.numeric(set_na(dat$marr) < 2)  ## 1- married; 2 - never unmarried; 3 - divorced; 4 - widown 

dat$evermarried <- as.numeric (set_na(dat$marr )!= 2)


## Stigma questions
dat$veg <- as.numeric(set_na(dat$fresh_veg, 3) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat$teach <- as.numeric(set_na(dat$teacher_allowed, 3) <2 ) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?
dat$learn <- as.numeric(set_na(dat$children_allowed, 3) < 2) ## In your opinion, if a children should be able to attend school

## use the children question to match the DHS indicator 
dat$stig <- apply(dat[ , c("veg","learn")], 1, function(m) { ifelse(any(m %in% 0, na.rm = TRUE), 1,
                                                                    ifelse (all (m %in% 1), 0, NA))})

dat$social_judge <- as.numeric(set_na(dat$secret, 3) < 2)
dat$social_judge2 <- NA

## Knowledge of HIV 

dat$healthy <- as.numeric(set_na(dat$healthy,3) < 2 ) ## healthy looking person can get AIDS (Yes/no)
dat$sharing <- as.numeric(set_na(dat$share_food, 3) <2 ) ## sharing food with people 
dat$always_condom <- as.numeric(set_na(dat$always_cond, 3) <2 ) ## always condoms to reduce the change of getting HIV 
dat$one_part <- as.numeric(set_na(dat$one_part, 3) <2 ) ## one partner to reduce the chance 

dat$comp_know <- ifelse (dat$healthy==1 & dat$sharing ==0 & dat$always_condom ==1 & dat$one_part==1 , 1 , 0)
setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat) [(is.na(healthy) | is.na(sharing) | is.na(always_condom) | is.na(one_part)),comp_know :=  NA, by = RowID]

## HIV testing ----

dat$evertest <- as.numeric(set_na(dat$hivtest_ever,3) < 2 )
dat$rec_res <- as.numeric(set_na(dat$received_results,3) < 2 )
dat$evertest[dat$rec_res ==0 ] <- 0 ## If did not receive results, code as 0 

## HIV testing in the past year 

dat$test12m <- as.numeric(set_na(dat$test_time) < 5 )
dat$test12m[dat$evertest==0] <- 0 ## If never tested - code as not tested in past 12 months 
## ANC testing 
dat$anc_att <- as.numeric(set_na(dat$anc_attend) < 2 )
dat$anc_off <- as.numeric(set_na(dat$anc_offered) < 2 )
dat$anc_test <- as.numeric(set_na(dat$anc_test) < 2 )

dat$anc_labor_test <- dat$anc_test 
dat$anc_labor_test[dat$anc_att== 0 | dat$anc_off ==0 ] <- 0 ## if not offered and not attend, not testing at anc

dat$evertest[dat$anc_labor_test==1 ] <- 1 ## if tested during labor or ANC, evertested

dat$plhiv_oldtesting <- ifelse(dat$evertest==1 & dat$test12m ==0 &  dat$hivstatus==1, 1, 
                               ifelse (dat$evertest==1 & dat$test12m ==1 &  dat$hivstatus==1, 0, 
                                       ifelse (dat$evertest==0 & dat$hivstatus==1, 0, 
                                               NA)))



#View(dat[,c('anc_att', 'anc_off', 'anc_test', 'anc_labor_test', 'evertest')])

## Self-reported ART uptake 
dat$arv_self <- as.numeric(set_na(dat$arv_self) < 2 )

## ART adherence -- question is asked about how long they were not taking the pills-ever (in months and days). Not just in the past 30 months 


## Biomarker-based ART uptake
dat$arv_bio <- as.numeric(set_na(dat$arv_bio))

dat$arv=apply(dat[, c("arv_self","arv_bio")],1, function(y) { ifelse(any(y== 1, na.rm = TRUE), 1,  # if either or both are 1, code as 1 
                                                                     ifelse (all(is.na(y)), NA, 0 ))})    
##' If someone reported that they never tested but they have ARTs in blood, they must be living with HIV and have taken ART before. Thus they must have known their status.  In this case the ART variable is based on biomarkers only because non-tested people are not asked about their HIV or ART uptake status 
##' If someone said they never tested and do not have ART is blood, they must only JUST (during the survey) tested for HIV and ART. Thus, its possible they were not on ART before. Again, people not tested for HIV are not asked about self-reported ART/ HIV status so the 'arv' variable is based on biomarkers
##' If someone said they tested they can be on ART or not, not coded as non-disclosing 

dat$nondisc <- ifelse(dat$evertest ==0 & dat$arv_bio==1, 1, 
                      ifelse(dat$evertest ==0 & dat$arv_bio==0, 0, 
                             ifelse(dat$evertest ==1 & (dat$arv_bio==1 | dat$arv_bio==0), 0, NA)))

## If (in MZ only) someone reported being HIV negative (s706 = 0) but they have ART in blood, code as nondisclosure
## here 'arv' variable is only based on biomrker test because people who self-report HIV negativity are not asked self reported ART test 
dat$nondisc[dat$hiv_self== 0 & dat$arv_bio==1] <- 1


## viral suppression 
dat$vlsup <- as.numeric(set_na(dat$vl_suppresion01))

dat$med_stig <- NA
dat$stig_anticip <- NA
dat$stig_perc <- NA
dat$self_test <- NA
dat$adh_bin <- NA 
dat$wealth  <- NA
dat$low2q <- NA
dat$cd4_cat <- NA
dat$cd4_bin <- NA
dat$incidence <- NA 
dat$everheard <- NA


dat <- dat[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'indweight', 'hivweight', 'indweight_std', 'hivweight_std',  "ad1", "sex",
              "age", "agegr", "edu",  "wealth",  'low2q', "restype", 'hivstatus', 'hiv_self', 'everheard',
              "comp_know", "stig", 'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
              "evertest", "test12m",
              'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting' )]

k <- subset(dat, hivstatus ==1)
apply(dat, MARGIN = 2, function (x) round(sum(is.na(x))/nrow(dat),2))

# 2012 survey ----

max (za12$education, na.rm = T)
dat12 <- za12 %>% dplyr::select(psu = ea,
                                stratum = province, ## also admin 1
                                indweight = ibreal12, # individual weight 
                                hivweight = ibreal1, # specimen data weight
                                sex = q1_2, # sex
                                geotype = geotype, # residence type
                                age = q1_1, # age
                                edu = education, ## educational level 
                                eversex = q5_1a, # ever had sex
                                marr =  q1_7, # marriage (current)
                                age_marr = q1_8, # age at first marriage n
                                con = q6_19_1, # condom use @ last sex with most recent partner 
                                life_partners = q5_4, # lifetime partners
                                ## HIV test
                                hivtest_ever = q11_2, # ever had an HIV test
                                test_time = q11_3, # how long ago was last HIV test
                                received_results = q11_4, # received results of most recent test
                                ## Antenatal care questions are not collected 

                                ## Stigma 
                                fresh_veg = q3_2a, ## would you buy fresh vegetables 
                                teacher_allowed = q3_2c, ## should a teacher be allowed?
                                secret = q3_2e, # would you keep family members' HIV status a secret
                                ## Knowledge (mosquito and witchcraft not collected )
                                share_food = q3_1f, ## sharing food with someone who is infected
                                healthy = q3_1c, ## can a healthy looking person have HIV?
                                one_part = q3_1e, ## reduce risk one uninfected partner who has no other partners
                                always_cond = q3_1g, ## risk reduced always condom 
                                
                                arv_bio = arv, # exposed to antiretroviral therapy
                                hivstatus = hivstat, # HIV status (viral load based)
) 

dat12$country <- 'South Africa'
dat12$surveyid <- as.factor('ZA2012SABSSM')
dat12$survyear <- '2012'
dat12$psu <- as.numeric(as.character(dat12$psu))
dat12$psu_u <- as.factor(as.character(paste0(dat12$psu,sep= "_",dat12$surveyid)))
dat12$age <- as.numeric(dat12$age)


dat12$stratum <- as.numeric(dat12$stratum)
dat12$hivweight <- as.numeric(dat12$hivweight)
dat12$indweight <- as.numeric(dat12$indweight)


dat12$iso <- countrycode(dat12$country, "country.name", "iso3c")

agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat12$agegr <- cut(dat12$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat12$sex <- as.numeric(dat12$sex)
dat12$sex <- factor(dat12$sex, levels=c(1,2),labels=c("male", "female"))


dat12$ad1 <- as.factor(as.character(paste0(dat12$stratum,sep= "_",dat12$surveyid))) ## (province)

dat12$restype <- factor(set_na(dat12$geotype, 9) > 2, c(FALSE, TRUE), c("Urban", "Rural")) #for when value is >1 code as TRUErural, otherwise urban

dat12$hivstatus <- as.numeric(set_na(dat12$hivstat))  ## 0 = negative 1= post

dat12$indweight_std <- (dat12$indweight/sum(dat12$indweight, na.rm = T) ) * nrow(dat12[!is.na(dat12$indweight),])## total sample size 
dat12$hivweight_std <- (dat12$hivweight/sum(dat12$hivweight, na.rm = T) ) * sum(!is.na(dat12$hivstatus)) ## sample size in the HIV analysis 


dat12$edu <- as.numeric(set_na(dat12$edu, 98)) 
dat12$edu_new <- ifelse(dat12$edu ==99, 0, 
                        ifelse (dat12$edu %in% c(1:7), 1, 
                        ifelse (dat12$edu %in% c(8:13), 2, 
                                ifelse (dat12$edu %in% c(14:15), 3, NA))))

dat12$edu<- factor(dat12$edu_new, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))

dat12$eversex <- as.numeric(set_na(dat12$eversex,3) < 2)  ## 2 = no 1=yes

dat12$part <- as.numeric(set_na(dat12$life_partners))

## If never had sex, no partners in the past year or in their lifetime 
dat12$part[dat12$eversex==0] <- 0

## Married or living togther as if marriedn
dat12$currMarried<- ifelse(dat12$marr  %in% c(1:3), 1, ## 1 - married + living togheter; ##2 - married but not living together; ##3 - living together but not married 
                        ifelse (dat12$marr %in% c(4:9), 0, NA))
## there is no question on ever marriage 

## Stigma questions
dat12$veg <- as.numeric(set_na(dat12$fresh_veg,3) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat12$teach <- as.numeric(set_na(dat12$teacher_allowed, 3) <2 ) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?

dat12$stig <- apply(dat12[ , c("veg","teach")], 1, function(m) { ifelse(any(m %in% 0, na.rm = TRUE), 1,
                                                                    ifelse (all (m %in% 1), 0, NA))})

dat12$social_judge <- as.numeric(set_na(dat12$secret, 3) < 2)
dat12$social_judge2 <- NA

## Knowledge of HIV 

dat12$healthy <- as.numeric(set_na(dat12$healthy,3) < 2 ) ## healthy looking person can get AIDS (Yes/no)
dat12$sharing <- as.numeric(set_na(dat12$share_food, 3) <2 ) ## sharing food with people 
dat12$always_condom <- as.numeric(set_na(dat12$always_cond, 3) <2 ) ## always condoms to reduce the change of getting HIV 
dat12$one_part <- as.numeric(set_na(dat12$one_part, 3) <2 ) ## one partner to reduce the chance 

dat12$comp_know <- ifelse (dat12$healthy==1 & dat12$sharing ==0 & dat12$always_condom ==1 & dat12$one_part==1 , 1 , 0)
setDT(dat12)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat12) [(is.na(healthy) | is.na(sharing) | is.na(always_condom) | is.na(one_part)),comp_know :=  NA, by = RowID]

## HIV testing 

dat12$evertest <- as.numeric(set_na(dat12$hivtest_ever,3) < 2 )
dat12$rec_res <- as.numeric(set_na(dat12$received_results,3) < 2 )
dat12$evertest[dat12$rec_res ==0 ] <- 0 ## If did not receive results, code as 0 

## HIV testing in the past year 

dat12$test12m <- as.numeric(set_na(dat12$test_time) < 5 )
dat12$test12m[dat12$evertest==0] <- 0 ## If never tested - code as not tested in past 12 months 
## No ANC testing 

dat12$plhiv_oldtesting <- ifelse(dat12$evertest==1 & dat12$test12m ==0 &  dat12$hivstatus==1, 1, 
                               ifelse (dat12$evertest==1 & dat12$test12m ==1 &  dat12$hivstatus==1, 0, 
                                       ifelse (dat12$evertest==0 & dat12$hivstatus==1, 0, 
                                               NA)))


## Biomarker-based ART uptake
dat12$arv_bio <- as.numeric(set_na(dat12$arv_bio))

dat12$arv <- dat12$arv_bio 
  
  
##' If someone reported that they never tested but they have ARTs in blood, they must be living with HIV and have taken ART before. Thus they must have known their status.  In this case the ART variable is based on biomarkers only because non-tested people are not asked about their HIV or ART uptake status 
##' If someone said they never tested and do not have ART is blood, they must only JUST (during the survey) tested for HIV and ART. Thus, its possible they were not on ART before. Again, people not tested for HIV are not asked about self-reported ART/ HIV status so the 'arv' variable is based on biomarkers
##' If someone said they tested they can be on ART or not, not coded as non-disclosing 

dat12$nondisc <- ifelse(dat12$evertest ==0 & dat12$arv_bio==1, 1, 
                      ifelse(dat12$evertest ==0 & dat12$arv_bio==0, 0, 
                             ifelse(dat12$evertest ==1 & (dat12$arv_bio==1 | dat12$arv_bio==0), 0, NA)))

dat12$med_stig <- NA
dat12$stig_anticip <- NA
dat12$stig_perc <- NA
dat12$self_test <- NA
dat12$hiv_self<- NA
dat12$adh_bin <- NA 
dat12$wealth  <- NA
dat12$low2q <- NA
dat12$cd4_cat <- NA
dat12$cd4_bin <- NA
dat12$incidence <- NA 
dat12$vlsup <- NA
dat12$evermarried <- NA
dat12$anc_labor_test <- NA 
dat12$everheard <- NA


dat12 <- dat12[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'hivweight', 'indweight', 'hivweight_std', 'indweight_std', "ad1", "sex",
              "age", "agegr", "edu",  "wealth",  'low2q', "restype", 'hivstatus', 'hiv_self', 'everheard',
              "comp_know", "stig", 'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
              "evertest", "test12m",
              'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting' )]

#sum(is.na(za12$q11_2))/nrow (za12)

# 2008 survey ----

sum(is.na(za08$hivstat))/nrow(za08)
dat08 <- za08 %>% dplyr::select(psu = ea,
                              stratum = prov, ## also admin 1
                              indweight = bcreal12, ## individual, 5 year age group 
                              hivweight = bcreal1, ## specimen, 5 year age gorup 
                              sex = q1_2, # sex
                              geotype = geotype, # residence type
                              age = q1_1, # age
                              edu = q1_7, ## educational level 
                              eversex = q5_1, # ever had sex
                              marr =  q1_12, # marriage (current)
                              con = q7_3, # condom use @ last sex 
                              eversex = q5_1, 
                              male_partners = q5_4, # male partners 
                              female_partners = q5_5, # male partners 
                              ## HIV test
                              hivtest_ever = q9_2, # ever had an HIV test
                              test_time = q9_3, # how long ago was last HIV test
                              received_results = q9_4, # received results of most recent test
                              ## Stigma 
                              fresh_veg = q3_4a, ## would you buy fresh vegetables 
                              teacher_allowed = q3_4c, ## should a teacher be allowed?
                              secret = q3_4e, # would you keep family member's HIV status a secret
                              
                              hivstatus = hivstat # HIV status (viral load based)
) 


dat08$country <- 'South Africa'
dat08$surveyid <- as.factor('ZA2008SABSSM')
dat08$survyear <- '2008'
dat08$psu <- as.numeric(as.character(dat08$psu))
dat08$psu_u <- as.factor(as.character(paste0(dat08$psu,sep= "_",dat08$surveyid)))
dat08$age <- as.numeric(dat08$age)


dat08$stratum <- as.numeric(dat08$stratum)
dat08$hivweight <- as.numeric(dat08$hivweight)
dat08$indweight <- as.numeric(dat08$indweight)


dat08$iso <- countrycode(dat08$country, "country.name", "iso3c")

agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat08$agegr <- cut(dat08$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat08$sex <- as.numeric(dat08$sex)
dat08$sex <- factor(dat08$sex, levels=c(1,2),labels=c("male", "female"))

dat08$currMarried <- as.numeric(set_na(dat08$marr,11) %ni%  c(6,7,9))  ## 11 - other?? 10- civil unions??? 6 - single 7 - divorced 9 - widown


dat08$ad1 <- as.factor(as.character(paste0(dat08$stratum,sep= "_",dat08$surveyid))) ## (province)

dat08$restype <- factor(set_na(dat08$geotype, 9) > 2, c(FALSE, TRUE), c("Urban", "Rural")) #for when value is >1 code as TRUErural, otherwise urban

dat08$hivstatus <- as.numeric(set_na(dat08$hivstat))  ## 0 = negative 1= post

dat08 <- dat08 %>%
  group_by(ad1) %>%
  mutate(hiv_avg = (sum(hivstatus, na.rm = T) - hivstatus) / (sum(!is.na(hivstatus)) - 1)) %>% ## average all values except for the actual person 
  group_by(ad1) %>%
  mutate(hiv_avg = if(all(is.na(hivstatus))) NA_real_ else hiv_avg )

dat08$indweight_std <- (dat08$indweight/sum(dat08$indweight, na.rm = T) ) * nrow(dat08[!is.na(dat08$indweight),])## total sample size 
dat08$hivweight_std <- (dat08$hivweight/sum(dat08$hivweight, na.rm = T) ) * sum(!is.na(dat08$hivstatus)) ## sample size in the HIV analysis 



dat08$edu_new <- ifelse(dat08$edu ==1, 0, 
                        ifelse (dat08$edu %in% c(2:5), 1, 
                                ifelse (dat08$edu %in% c(6:9), 2, 
                                        ifelse (dat08$edu %in% c(10:11), 3, NA))))

dat08$edu<- factor(dat08$edu_new, levels=c(0,1,2,3),labels=c("None","Primary", "Secondary", "Higher"))

## Stigma questions
dat08$veg <- as.numeric(set_na(dat08$fresh_veg,3) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat08$teach <- as.numeric(set_na(dat08$teacher_allowed, 3) <2 ) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?

dat08$stig <- apply(dat08[ , c("veg","teach")], 1, function(m) { ifelse(any(m %in% 0, na.rm = TRUE), 1,
                                                                        ifelse (all (m %in% 1), 0, NA))})

dat08$social_judge <- as.numeric(set_na(dat08$secret, 3) < 2)
dat08$social_judge2 <- NA
dat08$social_judge_avg2 <- NA

## HIV testing 

dat08$evertest <- as.numeric(set_na(dat08$hivtest_ever,3) < 2 )
dat08$rec_res <- as.numeric(set_na(dat08$received_results,3) < 2 )
dat08$evertest[dat08$rec_res ==0 ] <- 0 ## If did not receive results, code as 0 

## HIV testing in the past year 

dat08$test12m <- as.numeric(set_na(dat08$test_time, 0) ==1 )

dat08$test12m[dat08$evertest==0] <- 0 ## If never tested - code as not tested in past 12 months 
## No ANC testing 

dat08$plhiv_oldtesting <- ifelse(dat08$evertest==1 & dat08$test12m ==0 &  dat08$hivstatus==1, 1, 
                                 ifelse (dat08$evertest==1 & dat08$test12m ==1 &  dat08$hivstatus==1, 0, 
                                         ifelse (dat08$evertest==0 & dat08$hivstatus==1, 0, 
                                                 NA)))

dat08$med_stig <- NA
dat08$stig_anticip <- NA
dat08$stig_perc <- NA
dat08$self_test <- NA
dat08$hiv_self<- NA
dat08$adh_bin <- NA 
dat08$wealth  <- NA
dat08$low2q <- NA
dat08$cd4_cat <- NA
dat08$cd4_bin <- NA
dat08$incidence <- NA 
dat08$vlsup <- NA
dat08$evermarried <- NA
dat08$anc_labor_test <- NA 
dat08$arv_bio <- NA
dat08$arv <- NA 
dat08$nondisc <- NA
dat08$comp_know <- NA 
dat08$comp_know_avg <- NA
dat08$everheard <- NA


dat08 <- dat08[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'hivweight', 'indweight', 'hivweight_std', 'indweight_std',  "ad1", "sex",
                  "age", "agegr", "edu",  "wealth",  'low2q', "restype", 'hivstatus', 'hiv_self', 'everheard',
                  "comp_know", "stig", 'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
                  "evertest", "test12m",
                  'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting' )]

apply(dat08, MARGIN = 2, function (x) round(sum(is.na(x))/nrow(dat08),2))

all_sabssm <- rbind (dat08, dat12, dat)

saveRDS(all_sabssm, paste0(path_data, '/SABSSM_stg_0403.rds'))

surveys = unique(all_sabssm $surveyid)
nas<-function (data, survs ) {
  
  lst <- list()
  for (i in seq_along(survs)) {
    
    df<-data[data$surveyid==survs[i], ]
    
    sumna <- apply(df, MARGIN = 2, function (x) round(sum(is.na(x))/nrow(df),2))
    
    lst[i] = list(sumna)
    dtlst = as.data.frame(do.call("cbind", lst))
    
    
  }
  
  return(dtlst)
}


nas_dt<- nas ( data = all_sabssm ,  survs = surveys )


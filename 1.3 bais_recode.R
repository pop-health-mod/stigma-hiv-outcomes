rm(list=ls()) #clears workspace
gc()
library(data.table)
library(dplyr)
library(sjmisc)
library(sjlabelled)
library(haven)
library(survey)
library(openxlsx)
library(countrycode)
library(anytime)
library(lubridate)

path_data <- here::here("data")
path_out <- here::here("outputs")

'%ni%'  <- Negate('%in%')
set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }
surveys_dir = "~/Library/CloudStorage/OneDrive-McGillUniversity/Projects/2024-9 Stigma/HIV-Stigma-Africa/Survey files/BAIS"

## BAIS 2013 ------
bai13 <- read_spss(paste0 (surveys_dir, "/bais-iv-2013-20150319-v1.sav2.sav"))

## most of the missing people(without respondent ID) are children. They also have sex and other data missing 
## also remove people who have no sex reported (these people have other data all missing as well)
#View(bai13[c('ID_RESPONDENT_LINE_NO', 'Q101', 'CONSENT_hhold', 'HINT_RESULTS_CODE', 'HINT_RESULTS_CODE2', 'HINT_RESULTS_CODE3'  )])

bai13 <- subset(bai13, !is.na (ID_RESPONDENT_LINE_NO) & P04_AGE_YEARS >= 15 & !is.na(Q101))


dat <- bai13%>% select(
               sex = Q101, 
               region = District_Code , # admin 1 is province  ## 1-Nairobi; 2 - Centra; 3 - Coast; 4 - Eastern; 5 - North eastern ; 6- Nyanza; 7 - Rift Valley ; 8 - Western 
               memberid = ID_RESPONDENT_LINE_NO,
               stratum = Stratum_No, # strata (all adults datasets)
               psu = EA_Code,  # PSU cluster 
               geotype = UrbanRural, # Rural urban
               age  =P04_AGE_YEARS, # age at last birthday
               Weight1,
               eversex =   Q301, #  age at first sex 
               edu = Q103,
               currmar = Q106, 
               
               ever_heard = Q501, # heard of HIV
               ever_test = q607a, # ever tested for HIV
               last_test = q608a, # In the past 12 months how many times have you been tested 
               rec_results = q610a, #  were you told the result of the HIV test 
               
               attend_anc = q705b, # did you attend ANC  during preg. 
               test_anc = q706a, # did you test for HIV @ ANC  (Q on being offered a test is AFTER this question in the survey so not relevenat )
               rec_anc= q706b, # did you receive the results of that test at the ANC? 

          
               mosquito = Q507, # mosquito bites
               witchcraft = Q510, # get aids from witchraft 
               always_condom = Q506, #always using condom 
               sharing_food  = Q509, # sharing utensils 
               healthy_looking = Q505,
               one_part = Q508, ## one partner 
               
               buy_veg = q605, 
               teach_allowed = q604, 
               secret = q606,
               
               arv_sel= q611, # currently taking ARVs dail
               hiv_self_anc = q706b, # willing to share the results of your last HIV test? (anc)
               hiv_self = q610b, # did the test show that you have an HIV virus 
               HIV_LAB_Result,
               INCIDENCE_RESULTS) ## HIV status


dat$country <- 'Botswana'
dat$surveyid <- as.factor('BW2013BAIS')
dat$survyear <- '2013'
dat$psu <- as.numeric(as.character(dat$psu))
dat$psu_u <- as.factor(as.character(paste0(dat$psu,sep= "_",dat$surveyid)))
dat$age <- as_numeric(set_na(dat$age))

dat$stratum <- as.numeric(dat$stratum)
dat$hivweight <- as.numeric(dat$Weight1)
dat$indweight <- as.numeric(dat$Weight1) ## there seems to be no specific variable for HIV analysis 

dat$iso <- countrycode(dat$country, "country.name", "iso3c")

agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat$sex <- as_numeric(dat$sex)
dat$sex <- factor(dat$sex, levels=c(1,2),labels=c("male", "female"))

dat$admin1 <- ifelse(dat$region  %in% c(1, 3, 20), 'South East District', 
                     ifelse(dat$region  %in% c(2, 60), 'North East District', 
                            ifelse(dat$region  %in% c(4,5, 7, 50, 51, 52, 53, 54), 'Central District', 
                                   ifelse(dat$region  %in% c(6, 10, 11, 12), 'Southern District', 
                                          ifelse(dat$region  %in% c(30,31), 'Kweneng District', 
                                                 ifelse(dat$region  %in% c(70,71), 'Ngamiland District', 
                                                        ifelse(dat$region  %in% c(40), 'Kgatleng District', 
                                                               ifelse(dat$region  %in% c(72), 'Chobe District', 
                                                                      ifelse(dat$region  %in% c(80), 'Ghanzi District', 
                                                                             ifelse(dat$region  %in% c(90, 91), 'Kgalagadi District', NA ))))))))))

dat$ad1 <- as.factor(as.character(paste0(dat$admin1, sep= "_",dat$surveyid))) ## (province)


dat$geotype <- as_numeric(dat$geotype) ## 0- cities, 1 - towns; 2 - urban villages ; 3 - rural 
dat$restype <- factor(set_na(dat$geotype, 9) > 2, c(FALSE, TRUE), c("Urban", "Rural")) 


dat$HIV <- as_numeric(dat$HIV_LAB_Result) 
dat$hivstatus <- as.numeric(set_na(dat$HIV, 2)) ## 0 - negative; 1 = positive ; 2 - indeterm

dat$indweight_std <- (dat$indweight/sum(dat$indweight, na.rm = T) ) * nrow(dat[!is.na(dat$indweight),])## total sample size 
dat$hivweight_std <- (dat$hivweight/sum(dat$hivweight, na.rm = T) ) * sum(!is.na(dat$hivstatus)) ## sample size in the HIV analysis 


dat$wealth <- NA
dat$low2q <- NA


dat$edu<-factor(set_na(dat$edu)) #codes as is +NA
dat$edu<- ifelse(dat$edu %in% c(1,2), 'None', 
                    ifelse(dat$edu %in% c(3), 'Primary', 
                           ifelse (dat$edu  %in% c(4,5), 'Secondary', 
                                   ifelse (dat$edu %in% c(6), 'Higher', NA))))
dat$edu <- factor(dat$edu, levels = c("None", "Primary", "Secondary", "Higher"))

dat$eversex <- as_numeric(dat$eversex) 
dat$eversex <- as.numeric(set_na(dat$eversex) < 2 )  # 1 - yes; 2 - no 
dat$part <- NA 


dat$currmar <- as_numeric(dat$currmar)
dat$currMarried <- ifelse(dat$currmar %in% c(1,3), 1, 
                          ifelse(dat$currmar %in% c(2,4,5,6), 0, NA))
dat$evermarried <- as.numeric(set_na(dat$currmar) != 2)  ## 2 = never married 

dat$currMarried[dat$evermarried == 0] <- 0 # if never married not currently married 


dat$buy_veg <- as_numeric(dat$buy_veg)
dat$teach_allowed <- as_numeric(dat$teach_allowed)
dat$veg <- as.numeric(set_na(dat$buy_veg) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat$teach <- as.numeric(set_na(dat$teach_allowed) < 2) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?

dat$ever_heard <- as_numeric(dat$ever_heard)
dat$everheard <- as.numeric(set_na(dat$ever_heard, 9) < 2) 

dat$stig <- apply(dat[ , c("veg","teach")], 1, function(m) { ifelse(any(m %in% 0, na.rm = TRUE), 1,
                                                                    ifelse (all (m %in% 1), 0, NA))})
dat <- dat %>%
  group_by(sex) %>%
  mutate(
    stig = if (all(is.na(stig))) NA_real_ 
    else if (all(is.na(everheard))) stig 
    else ifelse(everheard == 0, 0, stig)
  ) %>%
  ungroup()

dat$secret = as.numeric(dat$secret)
dat$social_judge <- as.numeric(set_na(dat$secret, 9) < 2)

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    social_judge = if (all(is.na(social_judge))) NA_real_ 
    else if (all(is.na(everheard))) social_judge 
    else ifelse(everheard == 0, 0, social_judge)
  ) %>%
  ungroup()

dat$social_judge2 <- NA

## HIV testing 

## Overall testing 

dat$ever_test <- as_numeric(dat$ever_test)
dat$rec_results <- as_numeric(dat$rec_results)


dat$evertest <-as.numeric(set_na(dat$ever_test, 9) < 2) ## HIV testing, not in the ANC 
dat$rec_res <- as.numeric(set_na(dat$rec_results, 9) < 2) ## receive results 

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    evertest = if (all(is.na(evertest))) NA_real_ 
    else if (all(is.na(everheard))) evertest 
    else ifelse(everheard == 0, 0, evertest)
  ) %>%
  ungroup()

dat$evertest[dat$rec_res ==0 ] <- 0 ## If never heard or did not receive results, code as 0 

## some data from ever-heard is missing, but i am not sure what skip pattern is responsible for this. 


## Anc testing 
dat$attend_anc <- as_numeric(dat$attend_anc) #Did see anyone for antenatal care during pregnancy
dat$anc_test <- as_numeric(dat$test_anc) #Did you test for HIV at anc 
dat$rec_anc <- as_numeric(dat$rec_anc) #Did you receive the results 

dat$anc_att <- as.numeric(set_na(dat$attend_anc) < 2) 
dat$anc_test<- as.numeric(set_na(dat$anc_test) < 2) 
dat$anc_rec <- as.numeric(set_na(dat$rec_anc, 5)  != 4)  # 4 - did not receive test resuls 

dat$anc_test[dat$attend_anc ==0|dat$anc_rec ==0 ] <- 0 # if did not attend ANC or did not receive results 0 


## Past-year testing 
dat$last_test <- as_numeric(dat$last_test) # overall 
dat$test12m <- as.numeric(set_na(dat$last_test) > 0) 

dat$test12m[dat$evertest ==0 ] <- 0 ## if never tested not tested in the past 12 months 

#View(dat[c('test12m', 'last_test', 'evertest', 'ever_test', 'everheard' )])
dat$evertest[dat$test12m==1 ] <- 1 ## if tested in the past 12 months, tested in the pat 12 months

dat$plhiv_oldtesting <- ifelse(dat$evertest==1 & dat$test12m ==0 &  dat$hivstatus==1, 1, 
                               ifelse (dat$evertest==1 & dat$test12m ==1 &  dat$hivstatus==1, 0, 
                                       ifelse (dat$evertest==0 & dat$hivstatus==1, 0, 
                                               NA)))

## HIV knowledge 
dat$witchcraft <- as_numeric(dat$witchcraft)
dat$witch<- as.numeric(set_na(dat$witchcraft, 9) < 2 ) ## One can get aids from whitchcraft (Yes/no)

dat$one_part <- as_numeric(dat$one_part)
dat$one_part<- as.numeric(set_na(dat$one_part, 9) < 2 ) 

dat$mosquito <- as_numeric(dat$mosquito)
dat$mosquito <- as.numeric(set_na(dat$mosquito, 9) < 2 ) 

dat$always_condom <- as_numeric(dat$always_condom)
dat$always_condom  <- as.numeric(set_na(dat$always_condom , 9) < 2 ) 

dat$sharing_food <- as_numeric(dat$sharing_food)
dat$sharing <- as.numeric(set_na(dat$sharing_food, 9) < 2 ) 

dat$healthy_looking <- as_numeric(dat$healthy_looking)
dat$healthy <- as.numeric(set_na(dat$healthy_looking, 9) < 2 ) 



dat$comp_know_old <- ifelse (dat$witch==0 & dat$healthy==1 & dat$sharing ==0 & dat$mosquito==0 & dat$always_condom ==1 & dat$one_part==1 , 1 , 0)
dat$comp_know <- dat$comp_know_old
setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat) [(is.na(witch) | is.na(healthy) | is.na(sharing) |is.na(mosquito) | is.na(always_condom)  | is.na(one_part)),comp_know :=  NA, by = RowID]

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    comp_know = if (all(is.na(comp_know))) NA_real_ 
    else if (all(is.na(everheard))) comp_know 
    else ifelse(everheard == 0, 0, comp_know)
  ) %>%
  ungroup()


# self-reported ART 
dat$arv_self <- as_numeric(dat$arv_sel)
dat$arv_self <- as.numeric(set_na(dat$arv_self) < 2 ) 


## self-reported HIV status 
dat$hiv_self <- as_numeric(dat$hiv_self)
dat$hiv_self_gen <- as.numeric(set_na(dat$hiv_self, c(3,9) )< 2 ) # 3- don't want to tell 

dat$hiv_self_anc <- as_numeric(dat$hiv_self_anc)
dat$hiv_self_anc <- as.numeric(set_na(dat$hiv_self_anc, c(3,4, 9)) < 2 ) # 3- don't want to tell ; 4- not received results 5 - dont know 

## if both or one of them (even if the other is NA) is 1, then its HIV positive 
## if both or one of them (even if the other is NA) is 0, its HIV negative 

dat$hiv_self <- ifelse (dat$hiv_self_gen  %in% 1 & dat$hiv_self_anc %in%1 , 1, 
                        ifelse (dat$hiv_self_gen %in% 1 & dat$hiv_self_anc %in%0 , 1, 
                                ifelse (dat$hiv_self_gen  %in% 0 & dat$hiv_self_anc%in%1 , 1,       
                                        ifelse (dat$hiv_self_gen  %in% 1 & is.na(dat$hiv_self_anc) , 1, 
                                                ifelse (is.na(dat$hiv_self_gen ) & dat$hiv_self_anc %in% 1 , 1,  
                                                        ifelse (is.na(dat$hiv_self_gen ) & dat$hiv_self_anc%in% 0 , 0,  
                                                                ifelse (dat$hiv_self_gen  %in% 0 & is.na(dat$hiv_self_anc) , 0,  
                                                                        ifelse (dat$hiv_self_gen  %in% 0 & dat$hiv_self_anc %in% 0 , 0,  NA ))))))))

dat$HIV_LAB_Result <- as_numeric(dat$HIV_LAB_Result)
dat$hivstatus <- as.numeric(set_na(dat$HIV_LAB_Result, 2)) # 1 - Positive; 0 - negiatve

dat$INCIDENCE_RESULTS <- as_numeric(dat$INCIDENCE_RESULTS)
dat$recHIV <-as.numeric(set_na(dat$INCIDENCE_RESULTS, 3) ==2 ) ## 3 - sample finished ( incident missing specimen per https://microdata.statsbots.org.bw/index.php/catalog/13/data-dictionary/F5?file_name=bais-iii-2008-hp-v1 )


dat$incidence <-ifelse (dat$hivstatus %in% c(1) & dat$recHIV %in% (0), 0, 
                        ifelse (dat$hivstatus %in% c(1) & dat$recHIV %in% (1), 1, 
                                ifelse (dat$hivstatus %in% c(0), 0, NA))) 


dat$med_stig <- NA 
dat$stig_perc <- NA
dat$stig_anticip <- NA 
dat$self_test <- NA 
dat$nondisc <- NA # ARV bio data not collected 
dat$arv_bio <- NA 
dat$arv <- NA ## no biomarker for ARV, only self reported 
dat$vlsup <- NA 
dat$adh_bin <- NA 
dat$anc_labor_test <- dat$anc_test
dat$cd4_cat <- NA
dat$cd4_bin <- NA


dat1 <- dat[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'hivweight', 'indweight',   'hivweight_std', 'indweight_std', "ad1", "sex",
              "age", "agegr", "edu",  "wealth",  'low2q', "restype", 'hivstatus', 'hiv_self', 'everheard',
              "comp_know", "stig",'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
              "evertest", "test12m",'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting' )]

## BAIS 2008 ------
bai08_ind <- read_spss(paste0 (surveys_dir, "/bais-iii-2008-ind.sav"))
bai08_hp <- read_spss(paste0 (surveys_dir, "/bais-iii-2008-hp.sav"))

bai08 <-left_join(bai08_ind, bai08_hp)
bai08$Q102A_AGE <- as_numeric(bai08$Q102A_AGE)


## if ID_P00_SR_NO is missing its because they were unavailable or somehow did not participate in the surveys 
## remove people withought individual line number since they have no other data 
bai08 <- subset(bai08, !is.na (ID_P00_SR_NO) & Q102A_AGE >= 15)

dat <- bai08%>% select(
  sex = Q101_SEX, 
  region = ID_DISTRICT , # admin 1 is province  ## 1-Nairobi; 2 - Centra; 3 - Coast; 4 - Eastern; 5 - North eastern ; 6- Nyanza; 7 - Rift Valley ; 8 - Western 
  memberid = ID_P00_SR_NO,
  stratum = ID_STRATUM_ID, # stratum number within districts (check this)
  psu = ID_EA_NO,  # PSU cluster 
  geotype = ID_URRURAL, # Rural urban
  age  =Q102A_AGE, # age at last birthday
  indweight = INT_EA_WGHT, 
  hivweight = PEA_WGHT, 
  sex_age =   Q302_AGE, #  age at first sex (weird skip pattern)
  ever_sex = Q301B_EVER,
  ever_child = Q301A_CHLD, ## have you ever had a biological child
  edu = EDUCATION_ATTAINMENT,
  currmar = Q112_MARITA, ## marital status 

  
  ever_heard = Q501_HEARD, # heard of HIV
  ever_test = Q606A_TESTED, # ever tested for HIV
  last_test = Q607A_TESTED, # In the past 12 months how many times have you been tested 
  rec_results = Q608A_TOLD, #  were you told the result of the HIV test 
  
  mosquito = Q510_MOSQUITO, # mosquito bites
  witchcraft = Q513_WITCH, # get aids from witchraft 
  always_condom = Q509_REDUCE, #always using condom 
  sharing_food  = Q512_MEAL, # sharing utensils 
  healthy_looking =Q508_POSS,
  one_part = Q511_ONLY, ## one partner 
  
  buy_veg = Q604_SHOP, 
  teach_allowed = Q603_TEACH, 
  secret = Q605_SECRET,
  
  HDBS_RSLU, ## HIV status 
  HDBS_STATUT) ##  incidence data 


dat$country <- 'Botswana'
dat$surveyid <- as.factor('BW2008BAIS')
dat$survyear <- '2008'
dat$psu <- as.numeric(as.character(dat$psu))
dat$psu_u <- as.factor(as.character(paste0(dat$psu,sep= "_",dat$surveyid)))
dat$age <- as_numeric(set_na(dat$age))

dat$stratum <- as.numeric(dat$stratum)
dat$hivweight <- as.numeric(dat$hivweight)
dat$indweight <- as.numeric(dat$indweight)


dat$iso <- countrycode(dat$country, "country.name", "iso3c")

agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat$sex <- as_numeric(dat$sex)
dat$sex <- factor(dat$sex, levels=c(1,2),labels=c("male", "female"))

dat$admin1 <- ifelse(dat$region  %in% c(1, 3, 20), 'South East District', 
                     ifelse(dat$region  %in% c(2, 60), 'North East District', 
                            ifelse(dat$region  %in% c(4,5, 7, 50, 51, 52, 53, 54), 'Central District', 
                                   ifelse(dat$region  %in% c(6, 10, 11, 12), 'Southern District', 
                                          ifelse(dat$region  %in% c(30,31), 'Kweneng District', 
                                                 ifelse(dat$region  %in% c(70,71), 'Ngamiland District', 
                                                        ifelse(dat$region  %in% c(40), 'Kgatleng District', 
                                                               ifelse(dat$region  %in% c(72), 'Chobe District', 
                                                                      ifelse(dat$region  %in% c(80), 'Ghanzi District', 
                                                                             ifelse(dat$region  %in% c(90, 91), 'Kgalagadi District', NA ))))))))))

dat$ad1 <- as.factor(as.character(paste0(dat$admin1, sep= "_",dat$surveyid))) ## (province)



dat$geotype <- as_numeric(dat$geotype) ## 1- cities/ towns; 2 - urban villages ; 3 - rural 
dat$restype <- factor(set_na(dat$geotype, 9) > 2, c(FALSE, TRUE), c("Urban", "Rural")) 


dat$HIV <- as_numeric(dat$HDBS_RSLU) 
dat$hivstatus <- as.numeric(set_na(dat$HIV, 2)) ## 0 - negative; 1 = positive ; 2 - indeterm

dat$indweight_std <- (dat$indweight/sum(dat$indweight, na.rm = T) ) * nrow(dat[!is.na(dat$indweight),])## total sample size 
dat$hivweight_std <- (dat$hivweight/sum(dat$hivweight, na.rm = T) ) * sum(!is.na(dat$hivstatus)) ## sample size in the HIV analysis 

dat$wealth <- NA
dat$low2q <- NA


dat$edu<-factor(set_na(dat$edu)) #codes as is +NA
dat$edu<- ifelse(dat$edu %in% c(1,2), 'None', 
                    ifelse(dat$edu %in% c(3), 'Primary', 
                           ifelse (dat$edu  %in% c(4,5), 'Secondary', 
                                   ifelse (dat$edu %in% c(6), 'Higher', NA))))
dat$edu <- factor(dat$edu, levels = c("None", "Primary", "Secondary", "Higher"))

dat$ever_child <- as_numeric(dat$ever_child) ## have you had a kid
dat$ever_sex <- as_numeric(dat$ever_sex) ## have you ever had sex

dat$ever_child <- as.numeric(set_na(dat$ever_child) < 2) 
dat$ever_sex <- as.numeric(set_na(dat$ever_sex) < 2) 
dat$eversex <- dat$ever_sex
dat$eversex[dat$ever_child ==1 ] <- 1 # if had a child, had sex 

#View(dat[c('ever_child', 'ever_sex', 'eversex')])
dat$part <- NA 


dat$currmar <- as_numeric(dat$currmar)
dat$currMarried <- ifelse(dat$currmar %in% c(2,3), 1, 
                          ifelse(dat$currmar %in% c(1,4,5,6), 0, NA))
dat$evermarried <- as.numeric(set_na(dat$currmar) != 1)  ## 1 = never married 

dat$currMarried[dat$evermarried == 0] <- 0 # if never married not currently married 

## stigma 
dat$buy_veg <- as_numeric(dat$buy_veg)
dat$teach_allowed <- as_numeric(dat$teach_allowed)
dat$veg <- as.numeric(set_na(dat$buy_veg, 9) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat$teach <- as.numeric(set_na(dat$teach_allowed, 9) < 2) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?

dat$ever_heard <- as_numeric(dat$ever_heard)
dat$everheard <- as.numeric(set_na(dat$ever_heard, 9) < 2) 

dat$stig <- apply(dat[ , c("veg","teach")], 1, function(m) { ifelse(any(m %in% 0, na.rm = TRUE), 1,
                                                                    ifelse (all (m %in% 1), 0, NA))})
dat <- dat %>%
  group_by(sex) %>%
  mutate(
    stig = if (all(is.na(stig))) NA_real_ 
    else if (all(is.na(everheard))) stig 
    else ifelse(everheard == 0, 0, stig)
  ) %>%
  ungroup()

dat$secret = as.numeric(dat$secret)
dat$social_judge <- as.numeric(set_na(dat$secret, 9) < 2)

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    social_judge = if (all(is.na(social_judge))) NA_real_ 
    else if (all(is.na(everheard))) social_judge 
    else ifelse(everheard == 0, 0, social_judge)
  ) %>%
  ungroup()

dat$social_judge2 <- NA

## Overall testing 

dat$ever_test <- as_numeric(dat$ever_test)
dat$rec_results <- as_numeric(dat$rec_results)

dat$evertest <- as.numeric(set_na(dat$ever_test, 9) < 2) ## HIV testing, not in the ANC 
dat$rec_res <- as.numeric(set_na(dat$rec_results, 9) < 2) ## receive results 

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    evertest = if (all(is.na(evertest))) NA_real_ 
    else if (all(is.na(everheard))) evertest 
    else ifelse(everheard == 0, 0, evertest)
  ) %>%
  ungroup()

dat$evertest[dat$rec_res == 0 ] <- 0 ## If never heard or did not receive results, code as 0 

## some data from ever-heard is missing, but i am not sure what skip pattern is responsible for this. 


## Anc testing 
dat$anc_test <- NA

## Past-year testing 
dat$last_test <- as_numeric(dat$last_test) # overall #In the past 12 months have you been tested for HIV?
dat$test12m <- as.numeric(set_na(dat$last_test) < 2 ) 

dat$test12m[dat$evertest ==0 ] <- 0 ## if never tested not tested in the past 12 months 


dat$evertest[dat$test12m==1 ] <- 1 ## if tested in the past 12 months, tested in the pat 12 months

dat$plhiv_oldtesting <- ifelse(dat$evertest==1 & dat$test12m ==0 &  dat$hivstatus==1, 1, 
                               ifelse (dat$evertest==1 & dat$test12m ==1 &  dat$hivstatus==1, 0, 
                                       ifelse (dat$evertest==0 & dat$hivstatus==1, 0, 
                                               NA)))

## HIV knowledge 
dat$witchcraft <- as_numeric(dat$witchcraft)
dat$witch<- as.numeric(set_na(dat$witchcraft, 9) < 2 ) ## One can get aids from whitchcraft (Yes/no)

dat$one_part <- as_numeric(dat$one_part)
dat$one_part<- as.numeric(set_na(dat$one_part, 9) < 2 ) 

dat$mosquito <- as_numeric(dat$mosquito)
dat$mosquito <- as.numeric(set_na(dat$mosquito, 9) < 2 ) 

dat$always_condom <- as_numeric(dat$always_condom)
dat$always_condom  <- as.numeric(set_na(dat$always_condom , 9) < 2 ) 

dat$sharing_food <- as_numeric(dat$sharing_food)
dat$sharing <- as.numeric(set_na(dat$sharing_food, 9) < 2 ) 

dat$healthy_looking <- as_numeric(dat$healthy_looking)
dat$healthy <- as.numeric(set_na(dat$healthy_looking, 9) < 2 ) 

dat$comp_know_old <- ifelse (dat$witch==0 & dat$healthy==1 & dat$sharing ==0 & dat$mosquito==0 & dat$always_condom ==1 & dat$one_part==1 , 1 , 0)
dat$comp_know <- dat$comp_know_old
setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat) [(is.na(witch) | is.na(healthy) | is.na(sharing) |is.na(mosquito) | is.na(always_condom)  | is.na(one_part)),comp_know :=  NA, by = RowID]

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    comp_know = if (all(is.na(comp_know))) NA_real_ 
    else if (all(is.na(everheard))) comp_know 
    else ifelse(everheard == 0, 0, comp_know)
  ) %>%
  ungroup()


dat$HDBS_STATUT <- as_numeric(dat$HDBS_STATUT)
dat$recHIV <-as.numeric(set_na(dat$HDBS_STATUT, c(3,4,5,6)) ==2 )


dat$incidence <- dat$recHIV


dat$med_stig <- NA 
dat$stig_perc <- NA
dat$stig_anticip <- NA 
dat$self_test <- NA 
dat$nondisc <- NA # ARV bio data not collected 
dat$arv_bio <- NA 
dat$arv <- NA ## no biomarker for ARV, only self reported 
dat$vlsup <- NA 
dat$adh_bin <- NA 
dat$anc_labor_test <- dat$anc_test
dat$cd4_cat <- NA
dat$cd4_bin <- NA
dat$hiv_self <- NA

dat2 <- dat[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'hivweight', 'indweight',   'hivweight_std', 'indweight_std', "ad1", "sex",
               "age", "agegr", "edu",  "wealth",  'low2q', "restype", 'hivstatus', 'hiv_self', 'everheard',
               "comp_know", "stig", 'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
                "evertest", "test12m",'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting' )]

apply(dat2, MARGIN = 2, function (x) round(sum(is.na(x))/nrow(dat2),2))

## BAIS 2001 ------
 # bai01_all <- read_spss(paste0 (surveys_dir, "/bais-2001-individual-all.sav")) %>% rename_with(tolower)
 # bai01_hh1 <- read_spss(paste0 (surveys_dir, "/bais-2001-hh1.sav")) %>% rename_with(tolower)
 # bai01_hh2 <- read_spss(paste0 (surveys_dir, "/bais-2001-hh2.sav")) %>% rename_with(tolower) # household (n=2136_)
 bai01 <- read_sav(paste0 (surveys_dir, "/bais-2001-all-missingwgt-removed.sav"))


 # bai01 <-left_join(bai08_ind, bai08_hp)
bai01$q102 <- as_numeric(bai01$q102)


## if ID_P00_SR_NO is missing its because they were unavailable or somehow did not participate in the surveys 
## remove people withought individual line number since they have no other data 
bai01 <- subset(bai01, q102 >= 15 & !is.na(sex))

dat <- bai01 %>% select(
  sex = sex, 
  region = q01 , 
  memberid = q08,
  stratum = q03, # stratum number within districts (check this)
  psu = q06,  # PSU cluster 
  geotype = rururb, # Rural urban
  age  = q102, # age at last birthday
  indweight = wgt, 
  hivweight = wgt, # there seems to be no specific HIV weight
  sex_age =   q302, #  age at first sex (weird skip pattern)
  ever_sex = q301,
  ever_child = q701, ## have you ever given birth
  edu = q105, # highest level of education
  currmar = q202, ## marital status 
  
  
  ever_heard = q501, # heard of HIV
  ever_test = q607, # ever tested for HIV
  last_test = q608, # In the past 12 months how many times have you been tested 
  rec_results = q609, #  were you told the result of the HIV test 
  
  mosquito = mosquito, # mosquito bites
  witchcraft = q514, # get aids from witchraft 
  always_condom = q510, #always using condom 
  sharing_food  = q513, # sharing utensils 
  healthy_looking = q509,
  one_part = q512, ## one partner 
  
  buy_veg = shopkeep, 
  teach_allowed = teacher, 
  secret = secret) ##  no HIV biomarker information


dat$country <- 'Botswana'
dat$surveyid <- as.factor('BW2001BAIS')
dat$survyear <- '2001'
dat$psu <- as.numeric(as.character(dat$psu))
dat$psu_u <- as.factor(as.character(paste0(dat$psu,sep= "_",dat$surveyid)))
dat$age <- as_numeric(set_na(dat$age))

dat$stratum <- as.numeric(dat$stratum)
dat$hivweight <- as.numeric(dat$hivweight)
dat$indweight <- as.numeric(dat$indweight)


dat$iso <- countrycode(dat$country, "country.name", "iso3c")

agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat$sex <- as_numeric(dat$sex)
dat$sex <- factor(dat$sex, levels=c(1,2),labels=c("male", "female"))

dat$admin1 <- ifelse(dat$region  %in% c(1, 3, 20), 'South East District', 
                     ifelse(dat$region  %in% c(2, 60), 'North East District', 
                            ifelse(dat$region  %in% c(4,5, 7, 50, 51, 52, 53, 54), 'Central District', 
                                   ifelse(dat$region  %in% c(6, 10, 11, 12), 'Southern District', 
                                          ifelse(dat$region  %in% c(30,31), 'Kweneng District', 
                                                 ifelse(dat$region  %in% c(70,71), 'Ngamiland District', 
                                                        ifelse(dat$region  %in% c(40), 'Kgatleng District', 
                                                               ifelse(dat$region  %in% c(72), 'Chobe District', 
                                                                      ifelse(dat$region  %in% c(80), 'Ghanzi District', 
                                                                             ifelse(dat$region  %in% c(90, 91), 'Kgalagadi District', NA ))))))))))

dat$ad1 <- as.factor(as.character(paste0(dat$admin1, sep= "_",dat$surveyid))) ## (province)



dat$geotype <- as_numeric(dat$geotype) ## 1- cities/ towns; 2 - urban villages ; 3 - rural 
dat$restype <- factor(set_na(dat$geotype, 9) > 2, c(FALSE, TRUE), c("Urban", "Rural")) 


dat$hivstatus <- NA 
dat$incidence <- NA
dat$hiv_avg <- NA


dat$indweight_std <- (dat$indweight/sum(dat$indweight, na.rm = T) ) * nrow(dat[!is.na(dat$indweight),])## total sample size 
dat$hivweight_std <- (dat$hivweight/sum(dat$hivweight, na.rm = T) ) * sum(!is.na(dat$hivstatus)) ## sample size in the HIV analysis 

dat$wealth <- NA
dat$low2q <- NA


dat$edu<-factor(set_na(dat$edu)) #codes as is +NA
dat$edu<- ifelse(dat$edu %in% c(1,2), 'None', 
                    ifelse(dat$edu %in% c(3), 'Primary', 
                           ifelse (dat$edu  %in% c(4,5), 'Secondary', 
                                   ifelse (dat$edu %in% c(6), 'Higher', NA))))
dat$edu <- factor(dat$edu, levels = c("None", "Primary", "Secondary", "Higher"))

dat$ever_child <- as_numeric(dat$ever_child) ## have you had a kid
dat$ever_sex <- as_numeric(dat$ever_sex) ## have you ever had sex

dat$ever_child <- as.numeric(set_na(dat$ever_child) < 2) 
dat$ever_sex <- as.numeric(set_na(dat$ever_sex) < 2) 
dat$eversex <- dat$ever_sex
dat$eversex[dat$ever_child ==1 ] <- 1 # if had a child, had sex 

#View(dat[c('ever_child', 'ever_sex', 'eversex')])
dat$part <- NA 


dat$currmar <- as_numeric(dat$currmar)
dat$currMarried <- ifelse(dat$currmar %in% c(2,3), 1, 
                          ifelse(dat$currmar %in% c(1,4,5,6), 0, NA))
dat$evermarried <- as.numeric(set_na(dat$currmar) != 1)  ## 1 = never married 

dat$currMarried[dat$evermarried == 0] <- 0 # if never married not currently married 

## stigma 
dat$buy_veg <- as_numeric(dat$buy_veg)
dat$teach_allowed <- as_numeric(dat$teach_allowed)
dat$veg <- as.numeric(set_na(dat$buy_veg, 9) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat$teach <- as.numeric(set_na(dat$teach_allowed, 9) < 2) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?

dat$ever_heard <- as_numeric(dat$ever_heard)
dat$everheard <- as.numeric(set_na(dat$ever_heard, 9) < 2) 

dat$stig <- apply(dat[ , c("veg","teach")], 1, function(m) { ifelse(any(m %in% 0, na.rm = TRUE), 1,
                                                                    ifelse (all (m %in% 1), 0, NA))})
dat <- dat %>%
  group_by(sex) %>%
  mutate(
    stig = if (all(is.na(stig))) NA_real_ 
    else if (all(is.na(everheard))) stig 
    else ifelse(everheard == 0, 0, stig)
  ) %>%
  ungroup()

dat$secret = as.numeric(dat$secret)
dat$social_judge <- as.numeric(set_na(dat$secret, 9) < 2)

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    social_judge = if (all(is.na(social_judge))) NA_real_ 
    else if (all(is.na(everheard))) social_judge 
    else ifelse(everheard == 0, 0, social_judge)
  ) %>%
  ungroup()

dat$social_judge2 <- NA
dat$social_judge_avg2 <- NA

## Overall testing 

dat$ever_test <- as_numeric(dat$ever_test)
dat$rec_results <- as_numeric(dat$rec_results)

dat$evertest <- as.numeric(set_na(dat$ever_test, 9) < 2) ## HIV testing, not in the ANC 
dat$rec_res <- as.numeric(set_na(dat$rec_results, 9) < 2) ## receive results 

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    evertest = if (all(is.na(evertest))) NA_real_ 
    else if (all(is.na(everheard))) evertest 
    else ifelse(everheard == 0, 0, evertest)
  ) %>%
  ungroup()

dat$evertest[dat$rec_res ==0 ] <- 0 ## If never heard or did not receive results, code as 0 

## some data from ever-heard is missing, but i am not sure what skip pattern is responsible for this. 

## Anc testing 
dat$anc_test <- NA

## Past-year testing 
dat$last_test <- as_numeric(dat$last_test) # overall #In the past 12 months have you been tested for HIV?
dat$test12m <- as.numeric(set_na(dat$last_test) < 2 ) 

dat$test12m[dat$evertest ==0 ] <- 0 ## if never tested not tested in the past 12 months 

dat$evertest[dat$test12m==1 ] <- 1 ## if tested in the past 12 months, tested in the pat 12 months

dat$plhiv_oldtesting <- ifelse(dat$evertest==1 & dat$test12m ==0 &  dat$hivstatus==1, 1, 
                               ifelse (dat$evertest==1 & dat$test12m ==1 &  dat$hivstatus==1, 0, 
                                       ifelse (dat$evertest==0 & dat$hivstatus==1, 0, 
                                               NA)))

## HIV knowledge 
dat$witchcraft <- as_numeric(dat$witchcraft)
dat$witch<- as.numeric(set_na(dat$witchcraft, 9) < 2 ) ## One can get aids from whitchcraft (Yes/no)

dat$one_part <- as_numeric(dat$one_part)
dat$one_part<- as.numeric(set_na(dat$one_part, 9) < 2 ) 

dat$mosquito <- as_numeric(dat$mosquito)
dat$mosquito <- as.numeric(set_na(dat$mosquito, 9) < 2 ) 

dat$always_condom <- as_numeric(dat$always_condom)
dat$always_condom  <- as.numeric(set_na(dat$always_condom , 9) < 2 ) 

dat$sharing_food <- as_numeric(dat$sharing_food)
dat$sharing <- as.numeric(set_na(dat$sharing_food, 9) < 2 ) 

dat$healthy_looking <- as_numeric(dat$healthy_looking)
dat$healthy <- as.numeric(set_na(dat$healthy_looking, 9) < 2 ) 

dat$comp_know_old <- ifelse (dat$witch==0 & dat$healthy==1 & dat$sharing ==0 & dat$mosquito==0 & dat$always_condom ==1 & dat$one_part==1 , 1 , 0)
dat$comp_know <- dat$comp_know_old
setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat) [(is.na(witch) | is.na(healthy) | is.na(sharing) |is.na(mosquito) | is.na(always_condom)  | is.na(one_part)),comp_know :=  NA, by = RowID]

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    comp_know = if (all(is.na(comp_know))) NA_real_ 
    else if (all(is.na(everheard))) comp_know 
    else ifelse(everheard == 0, 0, comp_know)
  ) %>%
  ungroup()

#View(dat[,c("witch", 'healthy', 'sharing', 'mosquito', 'always_condom', 'one_part', 'comp_know', 'everheard')])

dat$recHIV <- NA
dat$med_stig <- NA 
dat$stig_perc <- NA
dat$stig_anticip <- NA 
dat$self_test <- NA 
dat$nondisc <- NA # ARV bio data not collected 
dat$arv_bio <- NA 
dat$arv <- NA ## no biomarker for ARV, only self reported 
dat$vlsup <- NA 
dat$adh_bin <- NA 
dat$anc_labor_test <- dat$anc_test
dat$cd4_cat <- NA
dat$cd4_bin <- NA
dat$hiv_self <- NA

dat3 <- dat[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'hivweight', 'indweight',   'hivweight_std', 'indweight_std', "ad1", "sex",
                 "age", "agegr", "edu",  "wealth",  'low2q', "restype", 'hivstatus', 'hiv_self', 'everheard',
                 "comp_know", "stig", 'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
                 "evertest", "test12m",'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting' )]

apply(dat3, MARGIN = 2, function (x) round(sum(is.na(x))/nrow(dat3),2))

all_bais <- rbind(dat1, dat2, dat3)
saveRDS(all_bais, paste0(path_data, '/BAIS_stg_0403.rds'))




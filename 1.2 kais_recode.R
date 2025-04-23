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

setwd("~/Dropbox (Personal)/McGill Research Projects/Stigma")
path_data <- here::here("data")

'%ni%' <- Negate('%in%')
set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }

## as.numeric codes missing values e.g. 8 or 9 as '3'. so use as_numeric 
ke07 <- read_spss(paste0 (surveys_dir, "/Individual Questionnire.sav"))

dat <- ke07%>% select(sex, 
                      region = qprov , # admin 1 is province  ## 1-Nairobi; 2 - Centra; 3 - Coast; 4 - Eastern; 5 - North eastern ; 6- Nyanza; 7 - Rift Valley ; 8 - Western 
                          memberid = qh01,
                          stratum = strata1, # strata (all adults datasets)
                          psu = qclust,  # PSU cluster 
                          ind_weight,
                          bl_weight,
                          windex5 = wlthind5, # wealth index
                          geotype = qresid, # Rural urban
                          age  =q103, # age at last birthday
                          agefsex =   q321, #  age at first sex 
                          edu = q105,
                          evermar = q302, 
                          currmar = q301, 
                          lifetime_part = q359, 
                      
                          ever_heard = q501, # heard of HIV
                          ever_test = q612, # ever tested for HIV
                          last_test = q613, # Last HIV test done
                          rec_results = q616, #  receive results
                          attend_anc = q213b, # did you attend ANC  during preg. 
                          test_anc = q605, # did you test for HIV @ ANC  (Q on being offered a test is AFTER this question in the survey so not relevenat )
                          rec_anc= q609, # did you receive the results of that test at the ANC? 
                          rec_anc= q609, # did you receive the results of that test at the ANC? 
                          last_test_anc = q607, # when was the last time that you tested for ANC 
                      
                          mosquito = q503, # mosquito bites
                          witchcraft = q507, # get aids from witchraft 
                          always_condom = q504, #always using condom 
                          sharing_food  = q505, # sharing utensils 
                          healthy_looking = q510,
                          one_part = q502, ## one partner 
                      
                          buy_veg = q524, 
                          teach_allowed = q527, 
                          secret = q525,
                          denied_services = q528, # do you personally know someone who had been denied health services in the past year due to their HIV status? 
                        
                          arv_sel= q707, # currently taking ARVs dail
                          share_res = q704, # willing to share the results of your last HIV test? 
                          hiv_self = q705, # did the test show that you have an HIV virus (general)
                          hiv_self_anc = q609b,  # did the test show that you have an HIV virus (ANc)
                          HIV, ## HIV status
                          CD4) # pres

dat$country <- 'Kenya'
dat$surveyid <- as.factor('KE2007KAIS')
dat$survyear <- '2007'
dat$stratum <- as.numeric(dat$stratum)
dat$psu <- as.numeric(as.character(dat$psu))
dat$psu_u <- as.factor(as.character(paste0(dat$psu,sep= "_",dat$surveyid)))
dat$age <- as_numeric(set_na(dat$age, c(97, 98)))
dat$hivweight <- as.numeric(dat$bl_weight)
dat$indweight <- as.numeric(dat$ind_weight)
dat$iso <- countrycode(dat$country, "country.name", "iso3c")

agegr <- c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), 
                 labels = agegr, TRUE, FALSE)

dat$sex <- as_numeric(dat$sex)
dat$sex <- factor(dat$sex, levels = c(1, 2), labels = c("male", "female"))

dat$ad1 <- as.factor(as.character(paste0(dat$region, sep = "_", dat$surveyid))) ## (province)

dat$geotype <- as_numeric(dat$geotype) ## 1 - rural; 2 - Urban (reversed from DHS and PHIA )
dat$restype <- factor(set_na(dat$geotype, 9) > 1, c(FALSE, TRUE), c("Rural", "Urban")) 

# Individual-level HIV status
dat$HIV <- as_numeric(dat$HIV) 
dat$hivstatus <- as.numeric(set_na(dat$HIV, 3) < 2) ## i think 2 - no 1 - yes

##sum(dat$hivstatus==1, na.rm = NA)/15893  hiv prevalenc eis ~ 7 % so that make sense 

dat$indweight_std <- (dat$indweight / sum(dat$indweight, na.rm = T) ) * nrow(dat[!is.na(dat$indweight), ])## total sample size 
dat$hivweight_std <- (dat$hivweight / sum(dat$hivweight, na.rm = T) ) * sum(!is.na(dat$hivstatus)) ## sample size in the HIV analysis 


dat$wealth <- as_numeric(set_na(dat$windex5)) #code as quintiles +NA
dat$low2q <- cut(dat$wealth, breaks = c(0, 2, 5), 
                 labels = c("1-2", "3-5"))
#dat$wealth <- as.factor(set_na(dat$wealth)) #code as quintiles +NA

dat$edu <- factor(set_na(dat$edu)) #codes as is +NA
dat$edu <- ifelse(dat$edu == 0, 'None', 
                    ifelse(dat$edu %in% c(1, 2), 'Primary', 
                    ifelse(dat$edu == 3, 'Secondary', 
                    ifelse(dat$edu %in% c(4, 5), 'Higher', NA))))
dat$edu <- factor(dat$edu, levels = c("None", "Primary", "Secondary", "Higher"))
#View(dat[c('edu', 'school')])

dat$eversex <- as.numeric(set_na(dat$agefsex) != 0) #if ==0m - never had sex, otherwise 1 
dat$part <- as.numeric(set_na(dat$lifetime_part, 98)) 

#View(dat[c('agefsex', 'eversex', 'part', 'lifetime_part')])
## If never had sex, no partners in the past year or in their lifetime 
dat$part[dat$eversex == 0] <- 0

dat$evermar <- as_numeric(dat$evermar)
dat$currmar <- as_numeric(dat$currmar)
dat$evermarried <- as.numeric(set_na(dat$evermar) < 3)  # 3 - no' 1 - yes ever married 2 - yes lived with a man 
dat$currMarried <- as.numeric(set_na(dat$currmar) < 3)
dat$currMarried[dat$evermarried == 0] <- 0 # if never married not currently married 

#View(dat[c('evermarried', 'currMarried')])

dat$ever_heard <- as_numeric(dat$ever_heard)
dat$everheard <- as.numeric(set_na(dat$ever_heard) < 2) 

## HIV knowledge 
dat$witchcraft <- as_numeric(dat$witchcraft)
dat$witch <- as.numeric(set_na(dat$witchcraft, 8) < 2) ## One can get aids from whitchcraft (Yes/no)

dat$one_part <- as_numeric(dat$one_part)
dat$one_part <- as.numeric(set_na(dat$one_part, 8) < 2) 

dat$mosquito <- as_numeric(dat$mosquito)
dat$mosquito <- as.numeric(set_na(dat$mosquito, 8) < 2) 

dat$always_condom <- as_numeric(dat$always_condom)
dat$always_condom <- as.numeric(set_na(dat$always_condom, 8) < 2) 

dat$sharing_food <- as_numeric(dat$sharing_food)
dat$sharing <- as.numeric(set_na(dat$sharing_food, 8) < 2) 

dat$healthy_looking <- as_numeric(dat$healthy_looking)
dat$healthy <- as.numeric(set_na(dat$healthy_looking, 8) < 2) 

dat$comp_know <- ifelse(dat$witch == 0 & dat$healthy == 1 & dat$sharing == 0 & 
                          dat$mosquito == 0 & dat$always_condom == 1 & 
                          dat$one_part == 1 , 1 , 0)
setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat) [(is.na(witch) | is.na(healthy) | is.na(sharing) |is.na(mosquito) | 
               is.na(always_condom)  | is.na(one_part)),comp_know :=  NA, by = RowID]

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    comp_know = if (all(is.na(comp_know))) NA_real_ 
    else if (all(is.na(everheard))) comp_know 
    else ifelse(everheard == 0, 0, comp_know)
  ) %>%
  ungroup()

# HIV stigma variables

dat$buy_veg <- as_numeric(dat$buy_veg)
dat$teach_allowed <- as_numeric(dat$teach_allowed)
dat$veg <- as.numeric(set_na(dat$buy_veg, 8) < 2) ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
dat$teach <- as.numeric(set_na(dat$teach_allowed, 8) < 2) ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?

dat$stig <- apply(dat[ , c("veg","teach")], 1, 
                  function(m) { 
                    ifelse(any(m %in% 0, na.rm = TRUE), 1,
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
dat$social_judge <- as.numeric(set_na(dat$secret, 8) < 2)

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


dat$evertest_gen <- as.numeric(set_na(dat$ever_test) < 2) ## HIV testing, not in the ANC 
dat$rec_res <- as.numeric(set_na(dat$rec_results) < 2) ## receive results 

dat <- dat %>%
  group_by(sex) %>%
  mutate(
    evertest_gen = if (all(is.na(evertest_gen))) NA_real_ 
    else if (all(is.na(everheard))) evertest_gen 
    else ifelse(everheard == 0, 0, evertest_gen)
  ) %>%
  ungroup()
dat$evertest_gen[dat$rec_res == 0] <- 0 

## Anc testing 
dat$attend_anc <- as_numeric(dat$attend_anc) #Did see anyone for antenatal care during pregnancy
dat$anc_test <- as_numeric(dat$test_anc) #Did you test for HIV at anc 
dat$rec_anc <- as_numeric(dat$rec_anc) #Did you receive the results 

dat$anc_att<- as.numeric(set_na(dat$attend_anc) < 2) 
dat$anc_test<- as.numeric(set_na(dat$anc_test) < 2) 
dat$anc_rec <- as.numeric(set_na(dat$rec_anc) < 2) 

dat$anc_test[dat$attend_anc == 0 | dat$anc_rec == 0] <- 0 # if did not attend ANC or did not receive results 0 

## combine general testing and ANC testing 
## if tested outside OR inside anc, code as 1 even if one of the values is NA 
## if one, or both of the values is 0, code as 0, even if one of the values is NA 

dat$evertest <- ifelse(dat$evertest_gen %in% 1 & dat$anc_test %in% 1, 1, 
                ifelse(dat$evertest_gen %in% 1 & dat$anc_test %in% 0, 1, 
                ifelse(dat$evertest_gen %in% 0 & dat$anc_test %in% 1, 1,       
                ifelse(dat$evertest_gen %in% 1 & is.na(dat$anc_test), 1, 
                ifelse(is.na(dat$evertest_gen) & dat$anc_test %in% 1, 1,  
                ifelse(is.na(dat$evertest_gen) & dat$anc_test %in% 0, 0,  
                ifelse(dat$evertest_gen %in% 0 & is.na(dat$anc_test), 0,  
                ifelse(dat$evertest_gen %in% 0 & dat$anc_test %in% 0, 0, NA))))))))

## Past-year HIV testing at ANC and overall 
dat$last_test <- as_numeric(dat$last_test) # overall 
dat$last_test_anc <- as_numeric(dat$last_test_anc) #anc

dat$last_test <- as.numeric(set_na(dat$last_test) == 1) 
dat$last_test_anc <- as.numeric(set_na(dat$last_test_anc) == 1) 
#View(dat[c('last_test', 'evertest')])

dat$last_test[dat$evertest == 0] <- 0 # if never tested (overall), not tested in past 12 months
dat$last_test_anc[dat$anc_test == 0] <- 0 # if never tested (anc), not tested in past 12 months

## create a combined past year testing variable as above 
dat$test12m <- ifelse(dat$last_test %in% 1 & dat$last_test_anc %in% 1, 1, 
               ifelse(dat$last_test %in% 1 & dat$last_test_anc%in% 0, 1, 
               ifelse(dat$last_test %in% 0 & dat$last_test_anc %in% 1, 1,       
               ifelse(dat$last_test %in% 1 & is.na(dat$last_test_anc), 1, 
               ifelse(is.na(dat$last_test) & dat$last_test_anc %in% 1, 1,  
               ifelse(is.na(dat$last_test) & dat$last_test_anc %in% 0, 0,  
               ifelse(dat$last_test %in% 0 & is.na(dat$last_test_anc), 0,  
               ifelse(dat$last_test %in% 0 & dat$last_test_anc %in% 0, 0, NA))))))))

## there are 8 people who tested in the past 12 months but are coded as never tested. these 8 people are 
## those who tested at the ANC but did not receive the results, thus were REcoded as nevertest. 
## if tested in he past 12 months, make sure they are evertest 
dat$evertest[dat$test12m == 1] <- 1
#View(dat[c('test12m', 'evertest_new', 'evertest', 'anc_test', 'attend_anc', 'anc_rec',  'test_anc')])

dat$plhiv_oldtesting <- ifelse(dat$evertest == 1 & dat$test12m == 0 & dat$hivstatus == 1, 1, 
                        ifelse(dat$evertest == 1 & dat$test12m == 1 & dat$hivstatus == 1, 0, 
                        ifelse(dat$evertest == 0 & dat$hivstatus == 1, 0, NA)))

# self-reported ART 
dat$arv_self <- as_numeric(dat$arv_sel)
dat$arv_self <- as.numeric(set_na(dat$arv_self, 8) < 2) 

## self-reported HIV status 
dat$hiv_self_gen <- as_numeric(dat$hiv_self)
dat$hiv_self_gen <- as.numeric(set_na(dat$hiv_self_gen) < 2) 

dat$hiv_self_anc <- as_numeric(dat$hiv_self_anc)
dat$hiv_self_anc <- as.numeric(set_na(dat$hiv_self_anc) < 2) 

## if both or one of them (even if the other is NA) is 1, then its HIV positive 
## if both or one of them (even if the other is NA) is 0, its HIV negative 

dat$hiv_self <- ifelse(dat$hiv_self_gen %in% 1 & dat$hiv_self_anc %in% 1, 1, 
                ifelse(dat$hiv_self_gen %in% 1 & dat$hiv_self_anc %in% 0, 1, 
                ifelse(dat$hiv_self_gen %in% 0 & dat$hiv_self_anc%in% 1, 1,       
                ifelse(dat$hiv_self_gen %in% 1 & is.na(dat$hiv_self_anc), 1, 
                ifelse(is.na(dat$hiv_self_gen) & dat$hiv_self_anc %in% 1, 1,  
                ifelse(is.na(dat$hiv_self_gen) & dat$hiv_self_anc%in% 0, 0,  
                ifelse(dat$hiv_self_gen %in% 0 & is.na(dat$hiv_self_anc), 0,  
                ifelse(dat$hiv_self_gen %in% 0 & dat$hiv_self_anc %in% 0, 0, NA))))))))

#View(dat[c('hiv_self', 'hiv_self_gen', 'hiv_self_anc')])

## CD4 count 
dat$cd4cat <- as.numeric(dat$CD4)
dat$cd4_cat <- ifelse(dat$cd4cat %in% c(0:99), '0-99', 
               ifelse(dat$cd4cat %in% c(100:199), '100-199', 
               ifelse(dat$cd4cat %in% c(200:349), '200-349', 
               ifelse(dat$cd4cat %in% c(350:499), '350-499', 
               ifelse(dat$cd4cat %in% c(500:max(dat$cd4cat, na.rm = T)), '500+', NA)))))
dat$cd4_bin <- ifelse(dat$cd4cat %in% c(0:199),  0, ## less than 200- 0 ; more than 200 - 1 
               ifelse(dat$cd4cat %in% c(200:max(dat$cd4cat, na.rm = T)), 1, NA))

dat$med_stig <- NA 
dat$med_stig_avg <- NA 
dat$stig_anticip <- NA 
dat$stig_perc <- NA 
dat$self_test <- NA 
dat$nondisc <- NA # ARV bio data not collected 
dat$arv_bio <- NA 
dat$arv <- NA ## no biomarker for ARV, only self reported 
dat$vlsup <- NA 
dat$incidence <- NA 
dat$adh_bin <- NA 
dat$anc_labor_test <- dat$anc_test

dat <- dat[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 
              'indweight', 'hivweight',  'indweight_std', 'hivweight_std',  "ad1", 
              "sex", "age", "agegr", "edu", "wealth",  'low2q', "restype", 
              'hivstatus', 'hiv_self', "comp_know", "stig", 'everheard',
              'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', "social_judge2", 'cd4_cat', 
              'cd4_bin', "anc_labor_test", 'self_test', 
              "evertest", "test12m",  'nondisc', "arv", 'arv_bio', 'adh_bin', 
              "vlsup", "incidence", 'plhiv_oldtesting' )]


k <- subset(dat, hivstatus == 1)
apply(k, MARGIN = 2, function (x) round(sum(is.na(x)) / nrow(k), 2))
apply(dat, MARGIN = 2, function (x) round(sum(is.na(x)) / nrow(dat), 2))

saveRDS(dat, paste0(path_data, '/KAIS_stg_0403.rds'))

rm(list=ls()) #clears workspace
library(data.table)
library(dplyr)
library(sjmisc)
library(haven)
library(demogsurv)
library(survey)
library(openxlsx)
library(meta)

#install.packages("meta")


setwd("~/Dropbox (Personal)/McGill Research Projects/Stigma")
root  <- "~/Dropbox (Personal)/McGill Research Projects/Stigma"
surveys_dir <- "~/Library/CloudStorage/OneDrive-McGillUniversity/Projects/2024-9 Stigma/HIV-Stigma-Africa/Survey files/NAIIS"
path_data <- here::here("data")
path_out <- here::here("outputs")


set_na <- function(x, na_codes = 99){ x[x %in% na_codes] <- NA; x }

nai_ind <- read.csv(paste0 (surveys_dir, '/NAIIS2018adultind.csv') ) 
nai_bio <- read.csv(paste0 (surveys_dir, '/NAIIS2018adultbio.csv') ) 

nai <- setDT(nai_ind)[setDT(nai_bio), on = "personid"] ## left join


vars <- c('gender', 'country', 'varunit', 'indstatus', 'personid', 'intwt0', 'btwt0', 'varstrat',  'surveystyear', 
          'age', 'urban', 'wealthquintile','evermar', 'curmar',  'hivstatdeniedcare', 'hivstathide',
          'everhadsex_ng', 'condomlastsex12months',  'hivpostell_a',
          'cd4cat',
          'hivtstever', 'hivtstrslt', 'hivstatusfinal',  'arvstatus',  'vls', 'art', 'recentlagvlarv', 
          'receivedresult_ng', 'receivedresult12months_ng',
          'hivtsbp', 'hivtprg', 'hivrptg', 'hivttlb', 'hivrslr', 'hivpsbp',
          'state_ng', ## state is the admin 1 level 
          'arvstakenev', 'arvscurrent',  'arvsmissdays',
          'education')

dat <- setDT(nai)[,..vars]

dat$country <- "Nigeria"
dat$surveyid <- "NG2018NAIIS"
dat$region <- "Western Africa"
dat$survyear <- c(2018)



dat$stratum <- as.numeric(dat$varstrat)
dat$varunit <- as.numeric(dat$varunit)
dat$psu <- as.factor(as.character(paste0(dat$stratum,sep= "_", dat$varunit )))
dat$psu_u <- as.factor(as.character(paste0(dat$stratum, dat$varunit, sep= "_",dat$surveyid))) ## psu is the combination of varstrat and varunit in PHIA and probably here as well. 


dat$indweight <- as.numeric(dat$intwt0)
dat$hivweight <- as.numeric(dat$btwt0)
dat$surveyid <-as.factor(dat$surveyid)

dat$admin <- as.numeric(dat$state_ng)
dat$ad1 <- as.factor(as.character(paste0(dat$admin,sep= "_",dat$surveyid)))

dat$iso <- c("NGA")

dat$wealth <- as.numeric(dat$wealthquintile)

dat$low2q <- cut(dat$wealth,breaks=c(0,2,5), labels = c("1-2", "3-5"))
#dat$wealth <- as.factor(dat$wealth )

dat$age <- as.numeric(dat$age)
agegr = c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), labels = agegr, TRUE, FALSE)

dat$urban <- as.numeric(dat$urban)
dat$restype <- factor(set_na(dat$urban, 99) > 1, c(TRUE, FALSE), c("Rural", "Urban"))


dat$sex <- as.numeric(dat$gender)
dat$sex <- factor(set_na(dat$sex, 99))
dat$sex <- factor(dat$sex, levels=c(1,2),labels=c("male", "female"))

## Education 
dat$education <- as.numeric(dat$education)
dat$edu <- set_na(dat$education, 99)
dat$edu <- ifelse (dat$edu == 1, "None", 
        ifelse (dat$edu %in% c(2,5), "Primary", 
                ifelse (dat$edu == 3, "Secondary", 
                        ifelse (dat$edu == 4, "Higher", NA)))) ## 5 - Quranic or adult literacy school 
dat$edu <- factor(dat$edu, levels = c("None", "Primary", "Secondary", "Higher"))


## Ever had sex 
dat$eversex<- as.numeric(set_na(dat$everhadsex_ng, c(-8,-9)) < 2) # if = 1, Yes had sex, if 2, no sex 

## Ever married 

dat$evermar <- as.numeric(dat$evermar)
dat$evermarried<- as.numeric(set_na(dat$evermar, c(-8,-9)) < 2) # if = 1, Yes, married, if 2, not married 

## currently married

dat$curmar <- as.numeric(dat$curmar)
dat$currMarried <- as.numeric(set_na(dat$curmar, c(-8,-9)) < 3) # if 1 (currently married) or 2 (living together), Yes, otherwise not currenltly married  

dat$currMarried[dat$evermarried == 0] <- 0 # if never married not currently married 

dat$comp_know <- NA

dat$stig <- NA

## Medical stigma and anticip(only asked to people who self-reported living with HIV)

dat$hivstatdeniedcare <- as.numeric (dat$hivstatdeniedcare) ##medical stigma 
dat$hivstathide <- as.numeric(dat$hivstathide) ## anticip

dat$med_stig <- as.numeric(set_na(dat$hivstatdeniedcare, c(-8,-9, 3)) < 2) # 3=  did not seek care in the past 12 months 
dat$stig_anticip <- as.numeric(set_na(dat$hivstathide, c(-8,-9, 3)) < 2) # 3= nobody knows my stataus 

'%ni%' <- Negate('%in%')

#View(dat[,c('hivstatdeniedcare', 'hivstathide', 'med_stig', 'stig_anticip' )])

dat$stig_perc <- NA 
## HIv status 

dat$hivstatusfinal <- as.numeric(dat$hivstatusfinal)
dat$hivstatus <- as.numeric(set_na(dat$hivstatusfinal, 99) <2 )

#sum(is.na(dat$indweight)) ==0 so the sample size is all people in the surv
dt <- dat%>% ## sample size by survey 
  group_by(surveyid) %>%
  summarise(n = sum(!is.na(indweight)), 
            n_hiv  = sum(!is.na(hivstatus)))


dat <- left_join(dat, dt, by = 'surveyid') %>%
  mutate (indweight_std = (indweight/sum(indweight, na.rm = T) ) * n )  %>%
  mutate (hivweight_std= (hivweight/sum(hivweight, na.rm = T) ) * n_hiv ) 

## lifetime testing and receiving the rsults (so awareness)

dat$testedreceiveddetail <- as.numeric(dat$receivedresult_ng ) 
dat$evertest  <- as.numeric(set_na(dat$testedreceiveddetail , c(99)) <2 ) #


## ANC testing

dat$hivtprg <- as.numeric(dat$hivtprg)  ## Tested for HIV as part of the ANC visit 
dat$anc <- as.numeric(set_na(dat$hivtprg, c(-8,-9)) < 2 ) # 2 = no


dat$hivrptg<- as.numeric(dat$hivrptg)  ## What was the result of your last HIV test during your pregnancy with xxx
dat$hivrptg <- as.numeric(set_na(dat$hivrptg, c(-8,-9))) 
dat$anc_result <-  ifelse(dat$hivrptg %in% c(1,2,3), 1, ## positive negative indeterminate 
                          ifelse (dat$hivrptg %in% c(4), 0, NA )) ## 4 - did not receive result

dat$ancTest<- dat$anc
dat$ancTest[dat$anc == 0 | dat$anc_result== 0] <- 0


#View(dat[,c('surveyid', 'hivtprg', 'anc', 'hivrtpg', 'anc_result', 'ancTest')])

dat$hivttlb <- as.numeric(dat$hivttlb)  ## Tested for HIV during labor
dat$labor <- as.numeric(set_na(dat$hivttlb, c(-8,-9)) < 2 ) 


dat$hivrslr <- as.numeric(dat$hivrslr)  ## Received results during labor 
dat$labor_result <-  ifelse(dat$hivrslr %in% c(1,2,3), 1, ## positive negative indeterminate 
                            ifelse (dat$hivrslr%in% c(4), 0, NA )) ## 4 - did not receive result

dat$laborTest <- dat$labor
dat$laborTest[dat$labor == 0 | dat$labor_result== 0] <- 0

## tested at the ANC or during labor. we onlyl care about those who did test (to remove them from denom) so how we code 0 does not really matter
dat$anc_labor_test <- apply(dat[ , c('ancTest', 'laborTest')], 1, function(m) { ifelse(any(m %in% 1, na.rm = TRUE), 1,
                                                                                       ifelse (all (m %in% 0), 0, NA))})

## HIV testing in past 12 m and receiving 
dat$receivedresult12months_ng <- as.numeric(dat$receivedresult12months_ng)
dat$test12m <- as.numeric(set_na(dat$receivedresult12months_ng, c(99)) < 2 ) ## tested in the past 12 months, received results 



## people who who ever tested for HIV prior to past year (so ever-tested but not tested in the past year and who are living with HIV)
dat$plhiv_oldtesting <- ifelse(dat$evertest==1 & dat$test12m ==0 &  dat$hivstatus==1, 1, 
                               ifelse (dat$evertest==1 & dat$test12m ==1 &  dat$hivstatus==1, 0, 
                                       ifelse (dat$evertest==0 & dat$hivstatus==1, 0, 
                                               NA)))

#View(dat[,c('evertest', 'receivedresult_ng', 'test12m', 'receivedresult12months_ng',  'surveyid')])


## self-reported result of the HIV test 

## self-reported result of the HIV test 
dat$hivtstrslt <- as.numeric(dat$hivtstrslt)
dat$hivtstrslt <- as.numeric(set_na(dat$hivtstrslt, c(-8, -9, 3, 4)) == 1 ) ## 2 = my result was hiv NEGATIVE; 1 = it was positive ; 3 - indeterminate ; 4 - did not receive results 
dat$hivpsbp <- as.numeric(dat$hivpsbp)
dat$hivpsbp <- as.numeric(set_na(dat$hivpsbp, c(-8, -9, 3, 4)) == 1 ) ## 2 = my result was hiv NEGATIVE; 1 = it was positive ; 3 - indeterminate ; 4 - did not receive results 
dat$hivrslr <- as.numeric(dat$hivrslr == 1 )  ## Received results during labor 
dat$hivrptg <- as.numeric(set_na(dat$hivrptg, c(4)) == 1 ) 

dat$hiv_self <- ifelse(
  rowSums(is.na(dat[, c("hivtstrslt", "hivpsbp", "hivrslr", "hivrptg")])) == 4, 
  NA, 
  pmax.int(dat$hivtstrslt, dat$hivpsbp, dat$hivrslr, dat$hivrptg, na.rm = TRUE)
)
## 3/4 - did not know or refused to say the result of last HIV test 

## we are mostly interested in people who say that they are positive since that's the denom for medical stig

#View(dat[,c("hiv_self", "hivtstrslt")])
dat$self_test <-NA 


## ART - biomarker

dat$art <- as.numeric(dat$art) 
dat$arv_bio <-as.numeric(set_na(dat$art, c(99)) < 2 ) ## 1 detectable 



## ART - self-reported
dat$arv_self <- ifelse(dat$arvscurrent == "1 ", 1,
                       ifelse((dat$arvscurrent == "2 " | dat$arvstakenev == "2 "), 0,
                              ifelse(dat$arvstakenev == ". ", NA, NA)))
dat$arv_self <- as.numeric(dat$arv_self)

## ARV combined 
dat$arv=apply(dat[, c("arv_self","arv_bio")],1, function(y) { ifelse(any(y== 1, na.rm = TRUE), 1,  # if either or both are 1, code as 1 
                                                                     ifelse (all(is.na(y)), NA, 0 ))})     # if all are NA, code as NA. otherwise "No" (so those with one of them as "No" and other as NA will be coded as "No")


## ART adherence 

dat$arvsmissdays <- as.numeric(dat$arvsmissdays)
dat$adh_bin <-  ifelse(dat$arvsmissdays <= 2,  1,
                       ifelse (dat$arvsmissdays > 2, 0, NA ))




dat$nondisc <- ifelse(dat$evertest ==0 & dat$arv_bio==1, 1, 
                      ifelse(dat$evertest ==0 & dat$arv_bio==0, 0, 
                             ifelse(dat$evertest ==1 & (dat$arv_bio==1 | dat$arv_bio==0), 0, NA)))

## if they self-report as being HIV negative but are on ARV, code as non-disclosing 
dat$nondisc[dat$hiv_self== 0 & dat$arv_bio==1] <- 1


#View(dat[,c("hiv_self", 'arv_bio', 'evertest', 'testedreceiveddetail', 'nondisc', 'hivstatus' )])
#View(dat[c('evertest', 'arv', 'nondisc', 'hiv_self')]  )  

# Viral load suppression

dat$vls <- as.numeric(dat$vls)
dat$vlsup <-as.numeric(set_na(dat$vls, c(99)) < 2 ) ## 1 suppressed


## Recency 

dat$recHIV <- as.numeric(dat$recentlagvlarv)
dat$recHIV <- as.numeric(set_na(dat$recHIV, 99) < 2) # if 1, recent HIV, if 2 long -term infection (0)

## cd4 
dat$cd4cat <- as.numeric(dat$cd4cat)
dat$cd4cat <- as.numeric(set_na(dat$cd4cat, 99)) # 

dat$cd4_cat <- ifelse (dat$cd4cat %in% c(1),  '0-99', 
                       ifelse (dat$cd4cat %in% c(2),  '100-199', 
                               ifelse (dat$cd4cat %in% c(3),  '200-349', 
                                       ifelse (dat$cd4cat %in% c(4),  '350-499', 
                                               ifelse (dat$cd4cat %in% c(5),  '500+', NA)))))

dat$cd4_bin <- ifelse (dat$cd4cat %in% c(1,2),  0, ## less than 200- 0 ; more than 200 - 1 
                       ifelse (dat$cd4cat %in% c(3, 4,5),  1,NA))



dat$incidence <-ifelse (dat$hivstatus %in% c(1) & dat$recHIV %in% (0), 0, 
                        ifelse (dat$hivstatus %in% c(1) & dat$recHIV %in% (1), 1, 
                                ifelse (dat$hivstatus %in% c(0), 0, NA))) 


dat$social_judge <- NA 
dat$social_judge2 <- NA 
dat$everheard <- NA

## why so many missing in HIV self reported?
## check  evertest and test 12m 


dat1 <- setDT(dat)[,. (country, iso, surveyid, survyear, psu, psu_u, stratum, indweight, hivweight, indweight_std, hivweight_std, ad1, 
                       age, agegr, sex, edu,  wealth, low2q,  restype, hivstatus,  hiv_self, comp_know, everheard,
                       stig,  med_stig, stig_perc, stig_anticip, social_judge, social_judge2, cd4_cat, cd4_bin,self_test,
                       plhiv_oldtesting, evertest, test12m, anc_labor_test, nondisc, arv, arv_bio, adh_bin, vlsup, incidence)]

saveRDS(dat1, paste0(path_data, "/NAIS_stg_0403.rds"))



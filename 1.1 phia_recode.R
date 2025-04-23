# Libraries and data ----

rm(list=ls()) # clears workspace
gc()

# packages
library(data.table)
library(dplyr)
library(sjmisc)
library(haven)
library(survey)
library(openxlsx)
library(meta)
library(here)

# set data and output directories
path_data <- here("data")
path_out <- here("outputs")
surveys_dir <- paste0(path_data, "/phia-surveys/")

# Useful functions ----
set_na <- function(x, na_codes = 99) { x[x %in% na_codes] <- NA; x }

select_cols <- function(dt, cols) {
  cols <- intersect(names(dt), cols)
  dt[, ..cols]
}

# Recode datasets by country ----

# Define variables to collect in each survey
vars <- c('gender', 'country', 'varunit', 'centroidid', 'indstatus', 'personid', 'intwt0', 'btwt0', 'varstrat',  'surveystyear', 
          'age', 'urban', 'wealthquintile','evermar', 'curmar', 'sexever',  'hivstatdeniedcare', 'hivstathide',
          'condomlastsex12months', 'onepartnr', 'mosquito', 'condoms', 'sharefood', 'healthyinf', 'hivpostell_a',
          'buyfood', 'kidsschool', 'feartest', 'talkbad', 'respect', 'famshame', 'cd4cat',
          'hivtstever', 'hivtstrslt', 'testedreceiveddetail', 'hivstatusfinal',  'arvstatus',  'vls', 'art', 'recentlagvlarv', 
          'hivk_eligible', 'hivk_status',
          'hivselftst',
          
          'hivtsbp', 'hivtprg', 'hivpsbp', 'hivrtpg', 'hivttlb', 'hivrslr', 
          'district', 'county', 'zone', 'region', 'province',
          'arvstakenev', 'arvscurrent',  'arvsmissdays',
          'education','educationkenya', 'educationlesotho', 'educationtanzania', 'educationcameroon', 'educationeswatini', 
          'educationnamibia', 'educationuganda', 'educationrwanda')

## Cameroon ----

# Load datasets
cm.ind <- fread(file = paste0(surveys_dir, 'CMR/', "camphia2017adultind.csv"))
cm.bio <- fread(file = paste0(surveys_dir, 'CMR/', "camphia2017adultbio.csv")) 

# Perform left-join using data table
cm1 <- setDT(cm.ind)[setDT(cm.bio), on = "personid"]

cm_new <- select_cols(cm1, vars)
rm( cm.bio, cm.ind, cm1 )

setnames(cm_new, c('region'), c( 'admin'))
setnames(cm_new, c('educationcameroon'), c('education'))

cm_new$education <- ifelse(cm_new$education == "1 ", 0, cm_new$education)
cm_new$education <- ifelse(cm_new$education == "2 ", 1, cm_new$education)
cm_new$education <- ifelse(cm_new$education  == "3 ",  2, cm_new$education) # secondary
cm_new$education <- ifelse(cm_new$education %in%  c("4 ", "5 "), 3, cm_new$education) ## 4 - high school; 5 - tertiary 

# cm_test <- subset(cm_new, ! (is.na(hivstathide) ))

## Eswatini ----
sw.ind <- fread(paste0(surveys_dir, 'SWZ/', "shims22016adultind.csv")) ; 
sw.bio <- fread(paste0(surveys_dir, 'SWZ/', "shims22016adultbio.csv")) ; 
sw1 <- setDT(sw.ind)[setDT(sw.bio), on = "personid"] ## left join
sw_new <- select_cols(sw1, vars)

setnames(sw_new, c('region'), c( 'admin'))
setnames(sw_new, c('educationeswatini'), c('education'))
rm(sw.bio,sw.ind, sw1 )

sw_new$education <- ifelse(sw_new$education == "1 ", 0, sw_new$education)
sw_new$education <- ifelse(sw_new$education == "2 ", 1, sw_new$education)
sw_new$education <- ifelse(sw_new$education  == "3 ",  2, sw_new$education) # secondary
sw_new$education <- ifelse(sw_new$education %in%  c("4 ", "5 "), 3, sw_new$education) ## 4 - high school; 5 - tertiary 

## Namibia ----
nm.ind <- fread(paste0(surveys_dir, 'NAM/', "namphia2017adultind.csv")) 
nm.bio <- fread(paste0(surveys_dir, 'NAM/', "namphia2017adultbio.csv")) 
nm1 <- nm.ind[nm.bio, on = "personid"]
nm_new <- select_cols(nm1, vars)
rm(nm.bio,nm.ind, nm1 )

setnames(nm_new, c('region'), c( 'admin'))
setnames(nm_new, c('educationnamibia'), c('education'))

nm_new$education <- ifelse(nm_new$education == "1 ", 0, nm_new$education) # None
nm_new$education <- ifelse(nm_new$education == "2 ", 1, nm_new$education) # Primary
nm_new$education <- ifelse(nm_new$education == "3 ",  2, nm_new$education) # Secondary
nm_new$education <- ifelse(nm_new$education == "4 ", 3, nm_new$education) # Higher 

## Uganda  ----
ug.ind <- fread(paste0(surveys_dir, 'UGA/', "uphia2016adultind.csv")) 
ug.bio <- fread(paste0(surveys_dir, 'UGA/', "uphia2016adultbio.csv")) 

ug1 <- ug.ind [ug.bio, on = "personid"]
ug_new <- select_cols(ug1 , vars)
rm(ug.bio,ug.ind, ug1)

setnames(ug_new, c('educationuganda', 'region'),c( 'education', 'admin'))
ug_new$education <- ifelse(ug_new$education == "1 ", 0, ug_new$education) ## None
ug_new$education <- ifelse(ug_new$education %in% c("2 ","3 "),  1, ug_new$education) #primary
ug_new$education <- ifelse(ug_new$education %in% c("4 ", "5 ") , 2, ug_new$education) # secondary

## Ethiopia  ----
eth.ind <- fread(paste0(surveys_dir, 'ETH/', "ephia2017adultind.csv")) 
eth.bio <- fread(paste0(surveys_dir, 'ETH/', "ephia2017adultbio.csv")) 

eth1 <- setDT(eth.ind)[setDT(eth.bio), on = "personid"]
eth_new <- select_cols(eth1 , vars)
## everyone in Ethiopia is from urban
rm(eth.bio,eth.ind, eth1)

setnames(eth_new, c('region'),c( 'admin'))
eth_new$education <- ifelse(eth_new$education == "1 ", 0, eth_new$education) ## None
eth_new$education <- ifelse(eth_new$education %in% c("2 "),  1, eth_new$education) #primary
eth_new$education <- ifelse(eth_new$education %in% c("3 ") , 2, eth_new$education) # secondary
eth_new$education <- ifelse(eth_new$education %in% c("4 ") , 3, eth_new$education) # higher

## Rwanda  ----
rw.ind <- fread(paste0(surveys_dir, 'RWA/', "rphia2018adultind.csv")) 
rw.bio <- fread(paste0(surveys_dir, 'RWA/', "rphia2018adultbio.csv")) 

rw1 <- rw.ind [rw.bio, on = "personid"]
rw_new <- select_cols(rw1 , vars)
rm(rw.bio,rw.ind, rw1)
#View(rw_new[,c('hivstathide')])
setnames(rw_new, c('educationrwanda', 'province'), c('education', 'admin'))

rw_new$education <- ifelse(rw_new$education == "1 ", 0, rw_new$education)
rw_new$education <- ifelse(rw_new$education == "2 ", 1,rw_new$education)
rw_new$education <- ifelse(rw_new$education =="3 ",  2, rw_new$education) # secondary
rw_new$education <- ifelse(rw_new$education == "4 ", 3, rw_new$education) ## Higher

## Côte d'Ivoire ----
ci.ind <- fread(paste0(surveys_dir, 'CIV/', "ciphia2017adultind.csv")) ; 
ci.bio <- fread(paste0(surveys_dir, 'CIV/', "ciphia2017adultbio.csv")) 
ci1 <- ci.ind[ci.bio, on = "personid"] ## left join

ci_new <- select_cols(ci1, vars)
rm(ci.bio,ci.ind, ci1 )

ci_new$education <- ifelse(ci_new$education == "1 ", 0, ci_new$education) # none
ci_new$education <- ifelse(ci_new$education == "2 ", 1, ci_new$education) # primary
ci_new$education <- ifelse(ci_new$education == "3 ", 2, ci_new$education) # secondary 
ci_new$education <- ifelse(ci_new$education == "4 ", 3, ci_new$education) # higher 
setnames(ci_new, c('district'), c( 'admin'))

## Kenya  ----
ke.ind <- fread(paste0(surveys_dir, 'KEN/', "kenphia2018adultind.csv")) 
ke.bio <- fread(paste0(surveys_dir, 'KEN/', "kenphia2018adultbio.csv")) 
 
ke1 <- ke.ind[ke.bio, on = "personid"]
ke_new <- select_cols(ke1, vars)
rm(ke.bio,ke.ind,ke1)
setnames(ke_new, c('educationkenya'), c('education'))

## Kenya has lots of NAs with education 
ke_new$education <- ifelse(ke_new$education == "1", 0, ke_new$education) # None
ke_new$education <- ifelse(ke_new$education %in% c("2", "3"), 1, ke_new$education) # Primary 
ke_new$education <- ifelse(ke_new$education == c("4"),  2, ke_new$education) # Secondary
## No higher than secondary education in Kenya 
setnames(ke_new, c('county'), c( 'admin'))

## Malawi  ----
mw.ind <- fread(paste0(surveys_dir, 'MWI/', "mphia2015adultind.csv")) 
mw.bio <- fread(paste0(surveys_dir, 'MWI/', "mphia2015adultbio.csv")) 

mw1 <- mw.ind[mw.bio, on = "personid"]
mw_new <- select_cols(mw1, vars)
rm(mw.bio, mw.ind, mw1 )

mw_new$education <- ifelse(mw_new$education == "1 ", 0, mw_new$education) # none
mw_new$education <- ifelse(mw_new$education == "2 ", 1, mw_new$education) # primary 
mw_new$education <- ifelse(mw_new$education == "3 ", 2, mw_new$education) # secondary
mw_new$education <- ifelse(mw_new$education == "4 ", 3, mw_new$education) # higher
setnames(mw_new, c('zone'), c( 'admin'))

## Lesotho  ----
le.ind <- fread(paste0(surveys_dir, 'LSO/', "lephia2016adultind.csv")) 
le.bio <- fread(paste0(surveys_dir, 'LSO/', "lephia2016adultbio.csv")) 

le1 <- le.ind[le.bio, on = "personid"]
le_new <- select_cols(le1, vars)
rm(le.bio,le.ind, le1 )
setnames(le_new, c('educationlesotho'), c('education'))
#View(le_new[, c('econ_cap', 'admin')])

le_new$education <- ifelse(le_new$education == "1 ", 0, le_new$education) ## none
le_new$education <- ifelse(le_new$education == "2 ", 1, le_new$education) ## Primary 
le_new$education <- ifelse(le_new$education %in% c("3 "),  2, le_new$education) # Secondary
le_new$education <- ifelse(le_new$education %in% c("4 ", "5 "), 3, le_new$education) ## Higher 
setnames(le_new, c('district'), c( 'admin'))

## Tanzania 2016 ----
tz.ind <- fread(paste0(surveys_dir, 'TZA/', "this2016adultind.csv")) 
tz.bio <- fread(paste0(surveys_dir, 'TZA/', "this2016adultbio.csv")) 
tz1 <- tz.ind [tz.bio, on = "personid"]
tz_new <- select_cols(tz1, vars)

rm(tz.bio,tz.ind, tz1)

setnames(tz_new, c('educationtanzania'),  c('education'))
tz_new$education <- ifelse(tz_new$education == "1 ", 0, tz_new$education) ## None
tz_new$education <- ifelse(tz_new$education %in% c("2 ","3 "),  1, tz_new$education) #primary
tz_new$education <- ifelse(tz_new$education %in% c("4 ", "5 ", "6 ", "7 ") , 2, tz_new$education) # secondary
tz_new$education <- ifelse(tz_new$education %in% c("8 ", "9 ") , 3, tz_new$education) # Higher 
setnames(tz_new, c('region'), c( 'admin'))

## Tanzania 2022 -----
# has info on community stigma
tz2.ind <- fread(paste0(surveys_dir, 'TZA/', "this2022adultind.csv")) %>% 
  rename(buyfood = buyfood_tz,
         kidsschool = kidsschool_tz,
         famshame = famshame_tz)
tz2.bio <- fread(paste0(surveys_dir, 'TZA/', "this2022adultbio.csv")) 
tz2 <- tz2.ind [tz2.bio, on = "personid"]
tz2_new <- select_cols(tz2, vars)

rm(tz2.bio, tz2.ind, tz2)

tz2_new$education <- ifelse(tz2_new$education == "1 ", 0, tz2_new$education) ## None
tz2_new$education <- ifelse(tz2_new$education %in% c("2 ","3 "),  1, tz2_new$education) #primary
tz2_new$education <- ifelse(tz2_new$education %in% c("4 ", "5 ", "6 ", "7 ") , 2, tz2_new$education) # secondary
tz2_new$education <- ifelse(tz2_new$education %in% c("8 ", "9 ") , 3, tz2_new$education) # Higher 
setnames(tz2_new, c('region'), c( 'admin'))

## Zambia 2016  ----
zm.ind <- fread(paste0(surveys_dir, 'ZAM/', "zamphia2016adultind.csv")) 
zm.bio <- fread(paste0(surveys_dir, 'ZAM/', "zamphia2016adultbio.csv")) 

zm1 <- zm.ind [zm.bio, on = "personid"]
zm_new <- select_cols(zm1, vars)
rm(zm.bio,zm.ind, zm1 )

zm_new$education <- ifelse(zm_new$education == "1 ", 0, zm_new$education)
zm_new$education <- ifelse(zm_new$education == "2 ", 1, zm_new$education) #primary
zm_new$education <- ifelse(zm_new$education == "3 ", 2, zm_new$education) #Secondary
zm_new$education <- ifelse(zm_new$education == "4 ", 3, zm_new$education) #Higher

setnames(zm_new , c('province'), c( 'admin'))

## Zimbabwe 2015  ----
zw.ind <- fread(paste0(surveys_dir, 'ZWE/', "zimphia2015adultind.csv")) 
zw.bio <- fread(paste0(surveys_dir, 'ZWE/', "zimphia2015adultbio.csv")) 

zw1 <- zw.ind [zw.bio, on = "personid"]
zw_new <- select_cols(zw1, vars)

zw_new$education <- ifelse(zw_new$education == "1 ", 0, zw_new$education)
zw_new$education <- ifelse(zw_new$education == "2 ", 1, zw_new$education) #Primary
zw_new$education <- ifelse(zw_new$education %in% c("3 "), 2, zw_new$education) # secondary
zw_new$education <- ifelse(zw_new$education == "4 ", 3, zw_new$education) ## Higher
setnames(zw_new , c('province'), c( 'admin'))
rm( zw.bio, zw.ind, zw1 )

# ---- Merging PHIAS ----
phias_merged <- rbindlist(list(ci_new, cm_new, ke_new, mw_new, le_new, sw_new, eth_new, nm_new, rw_new, ug_new,
                             tz_new, tz2_new, zm_new, zw_new), fill = T) %>%
  .[gender %in% c("1", "1 ", "2", "2 ", 1, 2)] %>%
  .[,country := ifelse(grepl("CI", personid), "Cote d'Ivoire",
                        ifelse(grepl("MW", personid), "Malawi", 
                        ifelse(grepl("CM", personid), "Cameroon",
                        ifelse(grepl("ZM", personid), "Zambia", 
                        ifelse(grepl("ZW", personid), "Zimbabwe", 
                        ifelse(grepl("KE", personid), "Kenya", 
                        ifelse(grepl("NA", personid), "Namibia", 
                        ifelse(grepl("ET", personid), "Ethiopia",
                        ifelse(grepl("SW", personid), "Eswatini", 
                        ifelse(grepl("LS", personid), "Lesotho",
                        ifelse(grepl("TZ", personid), "Tanzania",
                        ifelse(grepl("RW", personid), "Rwanda", 
                        ifelse(grepl("BW", personid), "Botswana", 
                        ifelse(grepl("UG", personid), "Uganda", NA))))))))))))))] %>%
  .[, surveyid := ifelse(grepl("CI", personid), "CI2017PHIA",
                        ifelse(grepl("CM", personid), "CM2017PHIA",
                        ifelse(grepl("MW", personid), "MW2015PHIA", 
                        ifelse(grepl("ZM", personid), "ZM2016PHIA", 
                        ifelse(grepl("^ZW0", personid), "ZW2015PHIA", 
                        ifelse(grepl("^ZW2", personid), "ZW2020PHIA", 
                        ifelse(grepl("KE", personid), "KE2018PHIA", 
                        ifelse(grepl("NA", personid), "NA2017PHIA", 
                        ifelse(grepl("ET", personid), "ETH2017PHIA",
                        ifelse(grepl("SW", personid), "SW2016PHIA", 
                        ifelse(grepl("LS", personid), "LS2016PHIA",
                        ifelse(grepl("TZ2", personid), "TZ2022PHIA",
                        ifelse(grepl("TZ0", personid), "TZ2016PHIA",
                        ifelse(grepl("RW", personid), "RW2017PHIA", 
                        ifelse(grepl("BW", personid), "BW2021BAIS", 
                        ifelse(grepl("UG", personid), "UG2016PHIA", NA ))))))))))))))))] %>%
  .[,region := ifelse (grepl("CI|CM", personid), "Western Africa",
                       ifelse(grepl("MW | UG | LS | ZM | ZW | ET | TZ | RW | KE", personid), "Eastern Africa", 
                       ifelse(grepl("SW | NA | BW", personid), "Southern Africa", NA)))] %>%
  .[, survyear := ifelse(grepl("UG|LS|NA|ET|ZM|SW|TZ0", personid), 2016,
                         ifelse(grepl("CI|NA|CM|RW", personid), 2017, 
                                ifelse(grepl("^ZW0|MW", personid), 2015, 
                                       ifelse(grepl("KE", personid), 2018, 
                                              ifelse(grepl("^ZW2", personid), 2020,
                                                     ifelse(grepl("BW", personid), 2021,
                                                            ifelse(grepl("TZ2", personid), 2022, NA)))))))]

#sb <- subset (phia_bound, is.na(hivstathide) & is.na(hivstatdeniedcare)) 

#View(phia_bound[,c('hivstatdeniedcare', 'hivstathide', 'surveyid', 'hivtstrslt')])
#View(subset (phia_bound, is.na(hivstathide) & is.na(hivstatdeniedcare)) )
#dat <- subset (phia_bound, ! (is.na(hivstathide) & is.na(hivstatdeniedcare))) 

saveRDS(phias_merged, paste0(path_data, "/all_phias_0129.rds")) ##no need to subset 

dat <- readRDS(paste0(path_data, "/all_phias_0129.rds"))

#View(dat[,c('hivtstrslt', 'hivstatdeniedcare', 'hivstathide', 'surveyid' )])
rm(ci_new, ke_new, mw_new, le_new, tz_new, tz2_new, zm_new, zw_new, 
   cm_new, eth_new, rw_new, ug_new, sw_new, nm_new)
## Remove people who were not eligible for the HIV knowledge/stigma module . 
## These people have neither of these two variables included in the study. this cna be shown as empty space or period 

## Recoding ----
dat <- dat %>%
  mutate(
    stratum = as.numeric(varstrat),
    varunit = as.numeric(varunit),
    psu = as.character(centroidid),
    psu_u = as.factor(paste0(psu, "_", surveyid)),
    indweight = as.numeric(intwt0),
    hivweight = as.numeric(btwt0),
    surveyid = as.factor(surveyid),
    admin = as.numeric(admin),
    ad1 = as.factor(paste0(admin, "_", surveyid))
  )


dat <- dat %>%
  mutate(iso = recode(
    country,
    "Cameroon" = "CMR",
    "Cote d'Ivoire" = "CIV",
    "Ethiopia" = "ETH",
    "Eswatini" = "SWZ",
    "Kenya" = "KEN",
    "Lesotho" = "LSO",
    "Malawi" = "MWI",
    "Mozambique" = "MOZ",
    "Namibia" = "NAM",
    "Rwanda" = "RWA",
    "Tanzania" = "TZA",
    "Uganda" = "UGA",
    "Zambia" = "ZMB",
    "Zimbabwe" = "ZWE",
    .default = NA_character_
  ))

### Individual-level variables ----

# Wealth
dat$wealth <- as.numeric(dat$wealthquintile)
dat$wealth <- set_na(dat$wealth, na_codes = 99)

dat$low2q <- cut(dat$wealth,breaks=c(0, 2, 5), 
                 labels = c("1-2", "3-5"))

dat$age <- as.numeric(dat$age)
agegr <- c('15-24', '25-34', '35-44', '45-54', '55-64',"65+")
dat$agegr <- cut(dat$age, breaks = c(15, 25, 35, 45, 55, 65,  Inf), 
                 labels = agegr, TRUE, FALSE)

dat$urban <- as.numeric(dat$urban)
dat$urban <- ifelse(dat$surveyid == "LS2016PHIA", 
             ifelse(dat$urban %in% c(2, 3), 2, 1), dat$urban)
dat$restype <- factor(set_na(dat$urban, 99) > 1, c(TRUE, FALSE), c("Rural", "Urban"))

dat$sex <- as.numeric(dat$gender)
dat$sex <- factor(set_na(dat$sex, 99))
dat$sex <- factor(dat$sex, levels = c(1, 2), labels = c("male", "female"))

## Education 
dat$education <- as.numeric(dat$education)
dat$edu_ <- factor(set_na(dat$education, 99))
dat$edu <- factor(dat$edu_, levels = c(0, 1, 2, 3), labels = c("None", "Primary", "Secondary", "Higher"))

# Ever heard of HIV
dat$everheard <- NA

## Ever had sex 
dat$eversex <- as.numeric(set_na(dat$sexever, 99) < 2) # if = 1, Yes had sex, if 2, no sex 

## Ever married 
dat$evermar <- as.numeric(dat$evermar)
dat$evermarried <- as.numeric(set_na(dat$evermar, c(-8, -9)) < 2) # if = 1, Yes, married, if 2, not married 

## Currently married
dat$curmar <- as.numeric(dat$curmar)
dat$currMarried <- as.numeric(set_na(dat$curmar, c(-8, -9)) < 3) # if 1 (currently married) or 2 (living together), Yes, otherwise not currently married  
dat$currMarried[dat$evermarried == 0] <- 0 # if never married not currently married 

## HIV knowledge 
#dat$witch <- as.integer(set_na(dat$v823, 8:9)) ## One can get aids from whitchcraft (Yes/no)
dat$healthyinf <- as.numeric(dat$healthyinf)
dat$mosquito <- as.numeric (dat$mosquito)
dat$condoms <- as.numeric(dat$condoms)
dat$onepartnr <- as.numeric(dat$onepartnr)
dat$sharefood <- as.numeric (dat$sharefood)

dat$healthy <- as.numeric(set_na(dat$healthyinf, c(-8, -9, 3)) < 2 ) ## healthy looking person can get AIDS (Yes/no)
dat$sharing <- as.numeric(set_na(dat$sharefood, c(-8, -9, 3)) < 2 ) ## sharing food with people 
dat$mosquito <- as.numeric(set_na(dat$mosquito, c(-8, -9, 3)) < 2) ## mosquito bites
dat$always_condom <- as.numeric(set_na(dat$condoms,c(-8, -9, 3)) < 2) ## always condoms to reduce the change of getting HIV dat$one_part <- as.integer(set_na(dat$onepartnr, c(-8,-9, 3)) < 2) ## one partner to reduce the chance 
dat$one_part <- as.numeric(set_na(dat$onepartnr, c(-8, -9, 3)) < 2)

dat$comp_know <- ifelse ( dat$healthy == 1 & dat$sharing == 0 & dat$mosquito == 0 & dat$always_condom == 1 & dat$one_part == 1, 1, 0)
setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
setDT(dat)[(is.na(healthy) | is.na(sharing) | is.na(mosquito)  | is.na(always_condom) | is.na(one_part)),comp_know :=  NA, by = RowID]

### Stigma variables ----

dat$feartest <- as.numeric(dat$feartest)
dat$talkbad <- as.numeric(dat$talkbad)
dat$respect <- as.numeric(dat$respect)
dat$famshame <- as.numeric(dat$famshame)

#### Discriminatory attitudes ----

dat <- dat %>% mutate(
  across(c(buyfood, kidsschool), as.numeric),
  veg = as.numeric(set_na(buyfood, c(-8, -9)) < 2), ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
  attend_sch = as.numeric(set_na(kidsschool, c(-8, -9)) < 2) ## In your opinion, if a student teacher has HIV but is not sick, should she be allowed to study w other kids 
)

dat <- dat %>%
  mutate(
    stig = case_when(
      veg == 0 | attend_sch == 0 ~ 1,
      veg == 1 & attend_sch == 1 ~ 0,
      .default = NA
    )
  )

#### Shame ----

# Collect individual-level variable
dat$social_judge <- as.numeric(set_na(dat$famshame, c(-8, -9)) < 2)  ## social judgement 
dat$social_judge2 <- dat$social_judge # social judgement (excluding secrets question)

#dat$stig_anticip <- as.integer(set_na(dat$feartest, c(-8,-9)) < 2) ## anticipated stigma 

#### Perceived stigma ----
dat$talkbad1 <- as.numeric(set_na(dat$talkbad, c(-8, -9)) < 2) 
dat$respect1 <- as.numeric(set_na(dat$respect, c(-8, -9)) < 2) 

dat$stig_perc <- apply(dat[ , c("talkbad1","respect1")], 1, 
                       function(m) { 
                         ifelse(any(m %in% 1, na.rm = TRUE), 1,
                                ifelse (all (m %in% 0), 0, NA))})

# Individual-level stigma in healthcare (only asked to people who self-reported living with HIV)

# Anticipated stigma ----
dat$hivstathide <- as.numeric(dat$hivstathide)  
dat$stig_anticip <- as.numeric(set_na(dat$hivstathide, c(-8, -9, 3)) < 2) # 3= did not attend health facility in the last 3 months 

# Experienced stigma ----
dat$hivstatdeniedcare <- as.numeric (dat$hivstatdeniedcare)
dat$med_stig <- as.numeric(set_na(dat$hivstatdeniedcare, c(-8, -9, 3, 4)) < 2) # 3= nobody knows my status, so code as NA ; 4= did not seek care in the past 12 months 

'%ni%' <- Negate('%in%')

#View(dat[,c("med_stig", "hivstatdeniedcare", 'surveyid')])
## if all data are missing in the PSU, replace all with NA

## HIV status 
dat$hivstatusfinal <- as.numeric(dat$hivstatusfinal)
dat$hivstatus <- as.numeric(set_na(dat$hivstatusfinal, 99) < 2)

#sum(is.na(dat$indweight)) ==0 so the sample size is all people in the surv
dt <- dat %>% ## sample size by survey 
  group_by(surveyid) %>%
  summarise(n = sum(!is.na(indweight)), 
            n_hiv = sum(!is.na(hivstatus)))
 
dat <- left_join(dat, dt, by = 'surveyid') %>%
      mutate (indweight_std = (indweight / sum(indweight, na.rm = T) ) * n )  %>%
      mutate (hivweight_std = (hivweight / sum(hivweight, na.rm = T) ) * n_hiv ) 
#View(dat[c('hivweight', 'indweight','hivweight_std', 'indweight_std', 'surveyid', 'n', 'n_hiv', 'sex', 'agegr', 'stig', 'curmar' )])

## lifetime testing and receiving the rsults (so awareness)
dat$testedreceiveddetail <- as.numeric(dat$testedreceiveddetail) 
dat$testedreceiveddetail <- as.numeric(set_na(dat$testedreceiveddetail, c(99))) #
dat$evertest <- ifelse(dat$testedreceiveddetail %in% c(1, 2, 3), 1, 
                ifelse(dat$testedreceiveddetail %in% c(4, 5, 6, 7), 0, NA )) 

## ANC testing
dat$hivtprg <- as.numeric(dat$hivtprg)  ## Tested for HIV as part of the ANC visit 
dat$anc <- as.numeric(set_na(dat$hivtprg, c(-8, -9)) < 2 ) # 2 = no


dat$hivrtpg <- as.numeric(dat$hivrtpg)  ## What was the result of your last HIV test during your pregnancy with xxx
dat$hivrtpg <- as.numeric(set_na(dat$hivrtpg, c(-8, -9))) 
dat$anc_result <- ifelse(dat$hivrtpg  %in% c(1, 2, 3), 1, ## positive negative indeterminate 
                  ifelse (dat$hivrtpg %in% c(4), 0, NA)) ## 4 - did not receive result

dat$ancTest <- dat$anc
dat$ancTest[dat$anc == 0 | dat$anc_result== 0] <- 0

#View(dat[,c('surveyid', 'hivtprg', 'anc', 'hivrtpg', 'anc_result', 'ancTest')])

dat$hivttlb <- as.numeric(dat$hivttlb)  ## Tested for HIV during labor
dat$labor <- as.numeric(set_na(dat$hivttlb, c(-8, -9)) < 2 ) 

dat$hivrslr <- as.numeric(dat$hivrslr)
dat$hivrslr <- as.numeric(set_na(dat$hivrslr, -8))  ## Received results during labor 
dat$labor_result <- ifelse(dat$hivrslr %in% c(1, 2, 3), 1, ## positive negative indeterminate 
                    ifelse (dat$hivrslr%in% c(4), 0, NA )) ## 4 - did not receive result

dat$laborTest <- dat$labor
dat$laborTest[dat$labor == 0 | dat$labor_result== 0] <- 0

## tested at the ANC or during labor. we only care about those who did test (to remove them from denom) so how we code 0 does not really matter
dat$anc_labor_test <- apply(dat[ , c('ancTest', 'laborTest')], 1, 
                            function(m) { 
                              ifelse(any(m %in% 1, na.rm = TRUE), 1,
                              ifelse(all (m %in% 0), 0, NA))})

#View(dat[,c('ancTest', 'laborTest', 'anc_labor_test')])
## HIV testing in past 12 m and receiving 
dat$testedreceiveddetail <- as.numeric(dat$testedreceiveddetail)
dat$test12m <- as.numeric(set_na(dat$testedreceiveddetail, c(99)) == 1) ## tested in the past 12 months, received results 

## people who who ever tested for HIV prior to past year (so ever-tested but not tested in the past year and who are living with HIV)
dat$plhiv_oldtesting <- ifelse(dat$evertest == 1 & dat$test12m == 0 &  dat$hivstatus == 1, 1, 
                        ifelse(dat$evertest == 1 & dat$test12m == 1 &  dat$hivstatus == 1, 0, 
                        ifelse(dat$evertest == 0 & dat$hivstatus == 1, 0, NA)))
#View(dat[,c('evertest', 'test12m', 'hivstatus', 'plhiv_oldtesting', 'surveyid')])

## self-reported result of the HIV test 
dat$hivtstrslt <- as.numeric(dat$hivtstrslt)
dat$hivtstrslt <- as.numeric(set_na(dat$hivtstrslt, c(-8, -9, 3, 4)) == 1 ) ## 2 = my result was hiv NEGATIVE; 1 = it was positive ; 3 - indeterminate ; 4 - did not receive results 
dat$hivpsbp <- as.numeric(dat$hivpsbp)
dat$hivpsbp <- as.numeric(set_na(dat$hivpsbp, c(-8, -9, 3, 4)) == 1 ) ## 2 = my result was hiv NEGATIVE; 1 = it was positive ; 3 - indeterminate ; 4 - did not receive results 
dat$hivrslr <- as.numeric(dat$hivrslr == 1 )  ## Received results during labor 
dat$hivrtpg <- as.numeric(set_na(dat$hivrtpg, c(4)) == 1 ) 

dat$hiv_self <- ifelse(
  rowSums(is.na(dat[, c("hivtstrslt", "hivpsbp", "hivrslr", "hivrtpg")])) == 4, 
  NA, 
  pmax.int(dat$hivtstrslt, dat$hivpsbp, dat$hivrslr, dat$hivrtpg, na.rm = TRUE)
)

## we are mostly interested in people who say that they are positive since that's the denom for medical stig

## HIV self-testing 
dat$hivselftst <- as.numeric(dat$hivselftst)
dat$self_test <- as.numeric(set_na(dat$hivselftst, c(-8, -9)) < 2) ## 1 yes , 2- no

## ART - biomarker
dat$art <- as.numeric(dat$art) 
dat$arv_bio <-as.numeric(set_na(dat$art, c(99)) < 2 ) ## 1 detectable 

## ART - self-reported
dat$arv_self <- ifelse(dat$arvscurrent == "1 ", 1,
                ifelse((dat$arvscurrent == "2 " | dat$arvstakenev == "2 "), 0,
                ifelse(dat$arvstakenev == ". ", NA, NA)))
dat$arv_self <- as.numeric(dat$arv_self)

## ARV combined 
dat$arv=apply(dat[, c("arv_self","arv_bio")], 1, 
              function(y) { 
                ifelse(any(y == 1, na.rm = TRUE), 1,  # if either or both are 1, code as 1 
                ifelse(all(is.na(y)), NA, 0 ))})     # if all are NA, code as NA. otherwise "No" (so those with one of them as "No" and other as NA will be coded as "No")

## ART adherence 
dat$arvsmissdays <- as.numeric(dat$arvsmissdays)
dat$adh_bin <- ifelse(dat$arvsmissdays <= 2,  1,
               ifelse (dat$arvsmissdays > 2, 0, NA))
#View(dat[c('adh_bin','arvsmissdays', 'surveyid')])

##' If someone reported that they never tested but they have ARTs in blood, they must be living with HIV and have taken ART before. Thus they must have known their status.  In this case the ART variable is based on biomarkers only because non-tested people are not asked about their HIV or ART uptake status 
##' If someone said they never tested and do not have ART is blood, they must only JUST (during the survey) tested for HIV and ART. Thus, its possible they were not on ART before. Again, people not tested for HIV are not asked about self-reported ART/ HIV status so the 'arv' variable is based on biomarkers
##' If someone said they tested they can be on ART or not, not coded as non-disclosing 
dat$nondisc <- ifelse(dat$evertest == 0 & dat$arv_bio == 1, 1, 
               ifelse(dat$evertest == 0 & dat$arv_bio == 0, 0, 
               ifelse(dat$evertest == 1 & (dat$arv_bio == 1 | dat$arv_bio == 0), 0, NA)))

## if they self-report as being HIV negative but are on ARV, code as non-disclosing 
dat$nondisc[dat$hiv_self == 0 & dat$arv_bio == 1] <- 1

#View(dat[, c("hiv_self", 'arv_bio', 'evertest', 'testedreceiveddetail', 'nondisc', 'hivstatus' )])
#View(dat[c('evertest', 'arv', 'nondisc', 'hiv_self')]  )  

# Viral load suppression
dat$vls <- as.numeric(dat$vls)
dat$vlsup <- as.numeric(set_na(dat$vls, c(99)) < 2 ) ## 1 suppressed

## Recent
dat$recHIV <- as.numeric(dat$recentlagvlarv)
dat$recHIV <- as.numeric(set_na(dat$recHIV, 99) < 2) # if 1, recent HIV, if 2 long -term infection (0)

## CD4 
dat$cd4cat <- as.numeric(dat$cd4cat)
dat$cd4cat <- as.numeric(set_na(dat$cd4cat, 99)) # 

dat$cd4_cat <- ifelse(dat$cd4cat %in% c(1), '0-99', 
               ifelse(dat$cd4cat %in% c(2), '100-199', 
               ifelse(dat$cd4cat %in% c(3), '200-349', 
               ifelse(dat$cd4cat %in% c(4), '350-499', 
               ifelse(dat$cd4cat %in% c(5), '500+', NA)))))

dat$cd4_bin <- ifelse(dat$cd4cat %in% c(1, 2), 0, ## less than 200- 0 ; more than 200 - 1 
               ifelse (dat$cd4cat %in% c(3, 4, 5), 1, NA))

dat$incidence <- ifelse(dat$hivstatus %in% c(1) & dat$recHIV %in% (0), 0, 
                 ifelse(dat$hivstatus %in% c(1) & dat$recHIV %in% (1), 1, 
                 ifelse(dat$hivstatus %in% c(0), 0, NA))) 


dat_phia <- setDT(dat)[, .(country, iso, surveyid, survyear, psu, psu_u, stratum,  
                    indweight,hivweight, indweight_std,hivweight_std, ad1, 
                    age, agegr, sex, edu, wealth, low2q,  restype, hivstatus,  
                    hiv_self, comp_know, everheard,
                    stig,  med_stig, stig_perc, stig_anticip, 
                    social_judge, social_judge2, cd4_cat, cd4_bin,self_test, 
                    plhiv_oldtesting,
                    evertest, test12m, anc_labor_test, nondisc, arv, arv_bio, adh_bin, 
                    vlsup, incidence)]

saveRDS(dat_phia, paste0(path_data, "/PHIA_stg_0403.rds"))

surveys <- unique(dat1$surveyid)
nas <-function (data, survs ) {
  lst <- list()
  for (i in seq_along(survs)) {
    df <- data[data$surveyid == survs[i], ]
    sumna <- apply(df, MARGIN = 2, function (x) round(sum(is.na(x)) / nrow(df), 2))
    lst[i] <- list(sumna)
    dtlst <- as.data.frame(do.call("cbind", lst))
  }
  return(dtlst)
}

nas_dt <- nas(data = subset(dat1, hivstatus == 1), survs = surveys )
colnames(nas_dt) <- surveys
View(nas_dt)


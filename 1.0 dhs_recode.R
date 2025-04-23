# Libraries and data ----

rm(list=ls()) # clear workspace
gc() # Free unused R memory

# packages
library(here)
library(rdhs)
library(haven)
library(survey)
library(data.table)
library(gt)
library(sf)
library(labelled) 
library(tidyverse)
library(gtsummary)
library(dplyr)
library(countrycode)
library(openxlsx)

# set data and output directories
path_data <- here("data")
path_out <- here("outputs")

# Useful functions ----

# if you encounter 9 as a variable, encode to NA otherwise keep as is. 
set_na <- function(x, na_codes = 9){ x[x %in% na_codes] <- NA; x }

# Download surveys ----

# log in to DHS system
# choose 1 (Yes) then enter password
rdhs::set_rdhs_config(data_frame = "data.table::as.data.table",
                      email = toString("james.stannah@mail.mcgill.ca"),
                      project = "The effects of HIV stigma on the HIV prevention and treatment cascade",
                      config_path = "rdhs.json",
                      global = FALSE)

# get country codes for desired countries
countries <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde",
              "Central African Republic", "Chad", "Comoros", "Congo", "Congo Democratic Republic", 
               "Cote d'Ivoire", "Equatorial Guinea", "Eritrea", "Ethiopia", 'Eswatini', "Gabon", "Gambia", "Ghana", 
               "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", 
               "Namibia", "Nigeria", 'Niger', "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone",
               "South Africa", "Sudan", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe")

cc <- dhs_countries()[CountryName %in% countries]$DHS_CountryCode

# Identify survey characteristics related to HIV
survchar <- dhs_survey_characteristics()
survchar[grepl("HIV", SurveyCharacteristicName)][order(SurveyCharacteristicID)]
survchar[grepl("perceive", SurveyCharacteristicName)][order(SurveyCharacteristicID)]

# Identify surveys with HIV testing from desired countries
# TODO check why used characteristic ID 24 (HIV behaviour) instead of 23 (HIV testing)
surveys <- dhs_surveys(surveyCharacteristicIds = 24, 
                       surveyYearStart = 2000, 
                       countryIds = cc,
                       surveyType = c("DHS", "AIS", "MIS"))

dat <- dhs_data(indicatorIds = c("HA_CPHT_M_T1R", # Men receiving an HIV test and receiving test results in the last 12 months
                                 "HA_CPHT_W_T1R", # Women receiving an HIV test and receiving test results in the last 12 months
                                 "HA_DATT_W_DIS", # Women discriminatory attitudes towards PLHIV
                                 "HA_DATT_M_DIS", # Men discriminatory attitudes towards PLHIV
                                 "HA_CPHT_W_ETR", # Women ever test and receive results
                                 "HA_CPHT_M_ETR" # Men ever test and receive results
                                  ), 
                countryIds = cc, 
                surveyYearStart = 2000) 

setdiff(surveys$SurveyId, dat$SurveyId)

survids <- union(dat$SurveyId, surveys$SurveyId)


# Identify datasets for these surveys
mrd <- dhs_datasets(fileType = "MR", fileFormat = "FL")[SurveyId %in% survids] # mens survey 
ird <- dhs_datasets(fileType = "IR", fileFormat = "FL")[SurveyId %in% survids] # womens survey
ard <- dhs_datasets(fileType = "AR", fileFormat = "FL")[SurveyId %in% survids] # HIV Test Results Recode

# Get local path to dataset (download if needed)
mrd$path <- unlist(get_datasets(mrd)) # 
ird$path <- unlist(get_datasets(ird)) # 
ard$path <- unlist(get_datasets(ard)) # 

ird <- ird[ird$SurveyId %in% survids] # 
mrd <- mrd[mrd$SurveyId %in% survids] # 
ard <- ard[ard$SurveyId %in% survids] # 

# Manually add Rwanda 2019 HIV biomarker file to ard
#path <- "C:/Users/James/AppData/Local/James/rdhs/Cache/datasets/RWAR81FL.rds"
path = "~/Library/Caches/rdhs/datasets/RWAR81FL.rds"
ard <- rbind(ard, data.frame(FileFormat = "Flat ASCII data (.dat)", 
                             FileSize = NA,
                             DatasetType = "HIV Datasets",
                             SurveyNum = NA,
                             SurveyId = "RW2019DHS",
                             FileType = "HIV Test Results Recode",
                             FileDateLastModified = NA,
                             SurveyType = "DHS",
                             SurveyYearLabel = "2019",
                             SurveyYear = "2019",
                             DHS_CountryCode = "RW",
                             FileName = "RWAR81FL.ZIP",
                             CountryName = "Rwanda", path = path))

ir <- readRDS(tail(ird$path, 1))

irvars <- c("v000",  # Alphabetic country code to identify the survey.
            "v001",  # Cluster number.
            "v002",  # Household number.
            "v003",  # Respondent's line number in the household schedule.
            "v005",  # Individual sample weight (to use divide by 1e6).
            "v008a", # date of the interview
            "v012",  # Current age of respondent.
            "v022",  # Sample strata for SE.
            "v023",  # Sample strata for SE.
            "v101",  # Region of residence (same as v024). #
            "v102",  # Rural/urban (same of v025). #
            "v190",  # Wealth 
            "v502",  # Currently/formerly/never in union
            
            "v823",  # Get AIDS from: Witchcraft or supernatural means
            "v756",  # Thinks a healthy-looking person can have AIDS
            "v754wp", # Get AIDS from: Sharing food with person with AIDS
            "v754jp", # Get AIDS from: Mosquito bites
            "v754cp", # Reduce risk of getting HIV: always use condoms during sex
            'v754dp', #  Reduce risk of getting HIV: have 1 sex partner only, who has no other partners 
            
            "v525",  # Ever had sex. #
            "v532",  # Flag variable for inconsistencies in v525. #
            "v751",  # Ever heard about HIV/AIDS
            "v781",  # Ever tested for HIV.
            "v826a", # Months ago most recent HIV test
            'v856',  # HIV self-testing
            
            'v825',  # Buy vegetables
            'v779',  # Female teacher 
            'v857a', # Do you think children living with HIV should be allowed to attend school with children who do not have HIV
            
            'v857b', # Hesitate to take an HIV test because of fear of how others will react if test is positive?
            'v857c', # Do you think people talk badly about PLHIV 
            'v857d', # Do you think PLHIV lose respect of others?
            'v777a', # I would be ashamed if someone in my family had HIV 
            
            ## Only DHS 8 
            'v861', # Self-reported HIV test 
            'v865', # Disclosed HIV status to anyone 
            'v866', # Feels ashamed due to their HIV status 
            'v867a', # People talk badly because of my HIV status
            'v867b', # Someone else disclosed my HIV status
            'v867c', # Verbally insulted/harassed/threatened 
            'v867d', # Healthcare workers talked badly
            'v867e', # Healthcare workers verbally abused 
            
            "v840",  # Tested for HIV as part of ANC visit
            "v840a", # Tested for HIV between the time went for delivery and before baby was born
            "v841",  # Got results of HIV test as part of ANC visit (general)
            "v841a", # Got results of HIV test when tested before baby was born ( mz2015AIS)
            "v843",  # Tested for HIV since antenatal visit test
            
            "s816e", # Tested for HIV as part of ANC visit (CM2004DHS)
            "s816g", # Got results of HIV test as part of ANC visit (CM2004DHS)
            's816h', # HIV tested since pregnancy (CM2004DHS)
            
            "s819",  # Test for ANC as part of ANC visit (RW2005DHS; GN2005DHS; SN2005DHS)
            "s820", # Got results of HIV test as part of ANC visit (RW2005)DHS; SN2005DHS
            's822', # HIV tested since pregnancy (CRW2005DHS; GN2005DHS; SN2005DHS)
            
            's910j', # Test for ANC as part of ANC visit (ET2005DHS)
            's910k', # Got results of HIV test as part of ANC visit  (ET2005DHS)
            's910m', # HIV testing After ANC (ET2005DHS)
            
            's816b3', # Test for ANC as part of ANC visit (GH2003DHS)
            's816b5', # Got results of HIV test as part of ANC visit(GH2003DHS)
            
            's412ab', # Test for ANC as part of ANC visit ( MW2004DHS)
            's412ac', # Got results of HIV test as part of ANC visit (MW2004DHS)
            
            "s1312",  # same as v826 but for Malawi
            
            "v826",  # When was the last time they tested for HIV
            "v828",  # Receving HIV test result
            "v836",  # Lifetime number of sex parnters 
            "v501",  # Current marital or union status
            "v531",  # Age at first sex 
            "v761",  # Condom use at last sex ( last 12 months)
            "v106",  # Highest level of school attended *
            "s706",  # Report tested + (mozambique)
            "s718",  # Have ever taken ARV (mozambique)
            "s719",  # Are currently taking ARV (mozambique)) [ self-reported 
            "aidsex",
            "sstruct")

arvars <- c("hivclust", "hivnumb", "hivline", "hiv03", "hiv05","shiv50", "sviral", "sbioarv", "slagrecn", 'srecent', "hivstruct") #03-blood T result; 05- HIV weight
allvars <- c("SurveyId", "CountryName", "SurveyYear", irvars, "hiv03", "hiv05", "shiv50", "sviral", "sbioarv", "slagrecn", 'srecent')

options(warn = 1)

# Load and merge datasets ----

datlst <- list()

# survid<- "GH2022DHS"
for(survid in ird$SurveyId) {
  
  print(survid)
  ir <- readRDS(ird[SurveyId == survid]$path)
  ir <- ir[, intersect(irvars, names(ir))]
  
  if("aidsex" %in% names(ir)) {
    # Case: Both men and women are in `ird`
    print(paste("Splitting aidsex for", survid))
    ir$sex <- ifelse(ir$aidsex == 2, "female", "male")  # Assign sex
    
    # Split into separate datasets
    mr <- ir[ir$aidsex == 1, ]  # Men's data
    ir <- ir[ir$aidsex == 2, ]  # Women's data
    
  } else {
    # Case: Separate `mrd` dataset (as originally intended)
    ir$aidsex <- haven::labelled(2, c("men" = 1, "women" = 2))
    attr(ir$aidsex, "label") <- "Sex"
    ir$sex <- 'female'
    
    if(nrow(mrd[SurveyId == survid]))  {
      mr <- readRDS(mrd[SurveyId == survid]$path)
      names(mr) <- sub("^mv", "v", names(mr))
      
      if (survid == "MZ2015AIS") {
        oldnames = c('sm506', 'sm518', 'sm519') 
        newnames = c("s706","s718", 's719')
        mr <- mr %>% rename_at(vars(oldnames), ~ newnames)
      }
      
      mr <- mr[, intersect(irvars, names(mr))]
      mr$aidsex <- haven::labelled(1, c("men" = 1, "women" = 2))
      attr(mr$aidsex, "label") <- "Sex"
      mr$sex <- 'male'
    }
  }
  
  # Ensure missing variables are filled with NA
  ir[setdiff(irvars, names(ir))] <- NA
  mr[setdiff(irvars, names(mr))] <- NA
  
  # Add labels
  if (!all(is.na(mr$v779))) { val_label(mr$v779, 8) <- "don't know" }
  if (!all(is.na(ir$v779))) { val_label(ir$v779, 8) <- "don't know" }
  
  val_label(mr$v502, 1) <- "currently in union/living with smb"
  val_label(mr$v502, 2) <- "currently in union/living with smb"
  val_label(ir$v502, 1) <- "currently in union/living with smb"
  val_label(ir$v502, 2) <- "currently in union/living with smb"
  
  # Merge male and female datasets
  dat <- rbind_labelled(ir, mr)
  
  # HIV dataset merging
  if(nrow(ard[SurveyId == survid])) { 
    ar <- readRDS(ard[SurveyId == survid]$path) 
    ar <- ar[, intersect(arvars, names(ar))]
    setDT(ar)[, c(setdiff(arvars, names(ar))) := NA]
    ar[, c("hivclust", "hivnumb", "hivline")] <- lapply(ar[, c("hivclust", "hivnumb", "hivline")], as.integer)
    if( survid == "CI2005AIS") {
      dat <- left_join(dat, ar, by = c("v001" = "hivclust", "v002" = "hivnumb", "v003" = "hivline", "sstruct" = "hivstruct"))
    } else {
      dat <- left_join(dat, ar, by = c("v001" = "hivclust", "v002" = "hivnumb", "v003" = "hivline"))
    }
  }
  
  # Add survey metadata
  dat$SurveyId <- survid
  dat$CountryName <- ird[SurveyId == survid]$CountryName
  dat$SurveyYear <- ird[SurveyId == survid]$SurveyYear
  dat$RegionId <- ird[SurveyId == survid]$RegionId
  dat[setdiff(allvars, names(dat))] <- NA
  datlst[[survid]] <- dat[c(allvars, 'sex')]
}


## check which surveys still may not have HIV testing info 
fun_t <- function(x) {
  name <- x$SurveyId[1]
  print(paste("Survey:", name))
  
  # Check if all values are missing in the three variables
  all_missing <- all(is.na(x$v826)) & all(is.na(x$v826a)) & all(is.na(x$s1312))
  print(paste("All missing:", all_missing))
  
  # Check variable labels and print them for debugging
  labels <- sapply(c("v826", "v826a", "s1312"), function(var) {
    lbl <- labelled::var_label(x[[var]])
    print(paste("Label for", var, ":", lbl))  # Print each label
    lbl
  }, simplify = TRUE)
  
  # Check if labels contain "NA" (as text) or are NULL
  label_issue <- all(sapply(labels, function(lbl) {
    is.null(lbl) || grepl("NA", lbl, ignore.case = TRUE)
  }))
  print(paste("Label issue:", label_issue))
  
  # Determine final classification
  if (all_missing) { 
    print("no testing") 
  } else if (label_issue) { 
    print("no testing") 
  } else { 
    print("all good") 
  }
}

# Apply function to list of datasets
tot_hiv <- lapply(datlst, fun_t)

## check which surveys still may not have stigma info 
fun_s <- function (x) {
  name <- x$SurveyId[1]
  print(name)
  if ( all(is.na(x$v825))  & all(is.na(x$v779)) ) { print ("no stigma") }
}
tot_stig <- lapply(datlst, fun_s)


exl <- createWorkbook()
addWorksheet(exl, "dhs")
writeData(exl, sheet = "dhs", x = as.data.frame(names(datlst)) )
saveWorkbook(exl, 
             paste0(path_out, "/tables/dhs_survs.xlsx"), 
             overwrite = TRUE)


## Total of 105 surveys have HIV testing data ( +18 PHIA)
## TODO check number. I count 107...

# Recode variables and create new ----
recode_dhs <- function(dat) {
 # dat <- datlst$BF2003DHS
  
  name <- dat$SurveyId[1]
  print(name)
  
  # Survey characteristics
  dat <- dat %>% 
    mutate(
      country = CountryName,
      iso = countrycode(country, "country.name", "iso3c"),
      region = factor(v101),
      ad1 = factor(paste0(v101, "_", SurveyId)),
      surveyid = factor(SurveyId),
      survyear = SurveyYear,
      psu = as.numeric(as.character(v001)),
      psu_u = factor(paste0(psu, "_", surveyid)),
      stratum = as.numeric( as.character( v023 ) )
    )
  
  if( all( is.na( dat$v023 ) ) | all( dat$v023 == 0 ) ) {
    dat <- dat %>% 
      mutate( stratum = as.numeric( interaction( v101, v102 ) ) )
  }
  
  # Individual-level variables
  agegr <- c('15-24', '25-34', '35-44', '45-54', '55-64', '65+')
  dat <- dat %>%
    mutate(
      pid = paste(v001, v002, v003), # make a person ID variable as concatenation of cluster, household, and line numbers
      age = v012,
      agegr = cut(age, breaks = c(15, 25, 35, 45, 55, 65, Inf), labels = agegr, include.lowest = TRUE, right = FALSE),
      sex = factor(sex),
      restype = factor(set_na(v102, 9) > 1, c(FALSE, TRUE), c("Urban", "Rural")),
      wealth = as.numeric(set_na(v190, 9)),
      low2q = cut(wealth, breaks = c(0, 2, 5), labels = c("1-2", "3-5")),
      edu = factor(set_na(v106, 9), levels = c(0, 1, 2, 3), labels = c("None", "Primary", "Secondary", "Higher")),
      eversex = as.numeric(set_na(v531, 97:99) > 0),
      part = replace(set_na(v836, 96:99), eversex == 0, 0),
      evermarried = as.numeric(set_na(v502, 9) > 0),
      currMarried = ifelse(evermarried == 0, 0, as.numeric(set_na(v502, 9) == 1)),
      everheard = as.numeric(set_na(dat$v751, 9) > 0), # ever heard of HIV
      # HIV status
      hivstatus = as.numeric(set_na(hiv03, 4:9) > 0)
    )
  
  # Sampling and HIV weights, unstandardised and standardised
  dat <- dat %>% mutate(
    indweight = as.numeric(v005),
    hivweight = as.numeric(hiv05)
  )
  
  dat$indweight_std <- (dat$indweight / sum(dat$indweight, na.rm = T)) * nrow(dat[!is.na(dat$indweight), ]) ## total sample size 
  dat$hivweight_std <- (dat$hivweight / sum(dat$hivweight, na.rm = T)) * sum(!is.na(dat$hivstatus)) ## sample size in the HIV analysis 
  
  
  # HIV knowledge
  dat <- dat %>% mutate(
    # Collect individual-level HIV knowledge variables
    witch = as.numeric(set_na(dat$v823, 8:9)), ## One can AIDS aids from witchcraft (Yes/no)
    healthy = as.numeric(set_na(dat$v756, 8:9)), ## healthy looking person can get AIDS (Yes/no)
    sharing = as.numeric(set_na(dat$v754wp, 8:9)), ## Can get HIV from sharing food with people
    mosquito = as.numeric(set_na(dat$v754jp, 8:9)), ## Can get HIV from mosquito bites
    always_condom = as.numeric(set_na(dat$v754cp, 8:9)), ## always condoms to reduce the change of getting HIV 
    one_part = as.numeric(set_na(dat$v754dp, 8:9)) ## one partner to reduce the chance 
  )
    
  if (all(is.na(dat$witch)) ) {
    dat$comp_know_old <- ifelse (dat$healthy==1 & dat$sharing == 0 & dat$mosquito == 0 & dat$always_condom == 1 & dat$one_part == 1 , 1 , 0)
    dat$comp_know <- dat$comp_know_old
    setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
    setDT(dat) [(is.na(healthy) | is.na(sharing) |is.na(mosquito) | is.na(always_condom)  | is.na(one_part)),comp_know :=  NA, by = RowID]
  } else {
    dat$comp_know_old <- ifelse (dat$witch == 0 & dat$healthy == 1 & dat$sharing == 0 & dat$mosquito == 0 & dat$always_condom == 1 & dat$one_part == 1 , 1 , 0)
    dat$comp_know <- dat$comp_know_old
    setDT(dat)[, RowID := .I] ## if any of the variables contain an NA, replace with NA 
    setDT(dat)[(is.na(witch) | is.na(healthy) | is.na(sharing) |is.na(mosquito) | is.na(always_condom)  | is.na(one_part)), comp_know := NA, by = RowID]
  }
  
  dat <- dat %>%
    group_by(sex) %>%
    mutate(
      comp_know = if (all(is.na(comp_know))) NA_real_ 
      else if (all(is.na(everheard))) comp_know 
      else ifelse(everheard == 0, 0, comp_know)
    ) %>%
    ungroup()
  
  
  # Stigma variables ----
  
  #### Discriminatory attitudes ----
 
  # surveys collected information on should a child with HIV be allowed to attend school, and should teachers with HIV be allowed to teach.
  # to align with the PHIA definitions we use only the variable asking about a child with HIV, where possible
  # Collect individual-level variables
  dat <- dat %>% mutate(
    veg = as.numeric(set_na(v825, 8:9)), ## Would you buy fresh vegetables from a shopkeeper or vendor if you..
    teach = as.numeric(set_na(v779, 8:9)), ## In your opinion, if a female teacher has HIV but is not sick, should she be allowed to continue teaching in the school?
    learn = as.numeric(set_na(v857a, 8:9)) ## In your opinion, if a children should be able to attend school
  )
  
  #### Create community-level variable by merging the individual-level variables
  #
  # if responding 'No' to either question -- 'Yes' for stigma (even if one of them is NA)
  # if responding 'Yes' to both questions -- 'No' for stigma 
  # if both NA -- 'NA for stigma
  
  dat <- dat %>% mutate(
    stig = case_when(
      veg == 0 | teach == 0 | learn == 0 ~ 1,
      veg == 1 & teach == 1 & learn == 1 ~ 0,
      is.na(veg) & is.na(teach) & is.na(learn) ~ NA,
      .default = 0
    )
  )

  dat <- dat %>%
    group_by(sex) %>%
    mutate(
      stig = if (all(is.na(stig))) NA_real_ 
      else if (all(is.na(everheard))) stig 
      else ifelse(everheard == 0, 0, stig)
    ) %>%
    ungroup()
    

 #### Shame ----
  
 # Collect individual-level variable
 dat$social_judge <- as.numeric(set_na(dat$v777a, 8:9))
 
  dat <- dat %>%
    group_by(sex) %>%
    mutate(
      social_judge = if (all(is.na(social_judge))) NA_real_ 
      else if (all(is.na(everheard))) social_judge 
      else ifelse(everheard == 0, 0, social_judge)
    ) %>%
    ungroup()
  
  dat$social_judge2 <- dat$social_judge

  dat <- dat %>%
    mutate(
      talkbad = as.numeric(set_na(v857c, 8:9)),
      respect = as.numeric(set_na(v857d, 8:9)),
      stig_perc = apply(cbind(talkbad, respect), 1, function(m) {
        ifelse(any(m == 1, na.rm = TRUE), 1, ifelse(all(m == 0), 0, NA))
      })
    )
  
  ## code those who have not heard of HIV as not stigmatizing 
  dat <- dat %>%
    group_by(sex) %>%
    mutate(
      stig_perc = if (all(is.na(stig_perc))) NA_real_ 
      else if (all(is.na(everheard))) stig_perc 
      else ifelse(everheard == 0, 0, stig_perc)
    ) %>%
    ungroup()
  
 # HIV testing ---- 
 
 ### Ever ----
 
 # Ever HIV testing
 
 # Need to make specific considerations for Malawi 2010 which uses different variable names
 if (all(dat$SurveyId %in% c("MW2010DHS"))) { dat$v826 <- dat$s1312 } #in Malawi v826 does not exist and is coded as s1312. To minimize code length just replace 
  
  ## Testing 
  if (all(dat$SurveyId %in% c("MW2010DHS"))) { 
    dat$v826 <- dat$s1312 } #in Malawi v826 does not exist and is coded as s1312. To minimize code length just replace 
  
  dat$evertest  <- as.numeric(set_na(dat$v781, 9) > 0) 
  
  dat$rec_res   <- as.numeric(set_na(dat$v828, 9) > 0) 
  
  dat <- dat %>%
    group_by(sex) %>%
    mutate(
      evertest = if (all(is.na(evertest))) NA_real_ 
      else if (all(is.na(everheard))) evertest 
      else ifelse(everheard == 0, 0, evertest)
    ) %>%
    ungroup()
  
  dat <- dat %>% ## If never heard or did not receive results, code as 0 
    group_by(sex) %>%
    mutate(
      rec_res = if (all(is.na(evertest))) NA_real_ 
      else if (all(is.na(rec_res))) evertest 
      else ifelse(rec_res == 0, 0, evertest)
    ) %>%
    ungroup()
  
  if(!all(is.na(dat$v826)) & all(is.na(dat$v826a))) { 
    dat$test12m <- as.numeric(set_na(dat$v826, 9) == 1)
  } else { 
    dat$test12m <- ifelse(cut(set_na(as.numeric(dat$v826a),98:99), c(0, 12, Inf), 
                              labels = FALSE, include.lowest = TRUE, right = FALSE) == 2, 0,
                          cut(set_na(as.numeric(dat$v826a),98:99), c(0, 12, Inf), 
                              labels = FALSE, include.lowest = TRUE, right = FALSE))
  }

  dat <- dat %>% ## If never tested - code as not tested in past 12 months 
    group_by(sex) %>%
    mutate(
      test12m = if (all(is.na(test12m))) NA_real_ 
      else if (all(is.na(evertest))) test12m 
      else ifelse(evertest == 0, 0, test12m)
    ) %>%
    ungroup()
  
  ##' Testing during ANC or during delivery 
  ##' Since we looking at 'ever-knowledge of status' its okay to include testing during labot and after delivery 
  if (all(dat$SurveyId %in% c("CM2004DHS"))) {
    dat$anc <-as.numeric(set_na(dat$s816e, 8:9)) ## Tested for HIV as part of the ANC visit 
    dat$anc_result <- as.numeric(set_na(dat$s816g, 9))    ## Received results as part of the ANC visit 
    dat$after_anc <- as.numeric(set_na(dat$s816h, 8:9))  ## tested since ANC 
  } else if (all(dat$SurveyId %in% c("RW2005DHS", 'GN2005DHS', 'SN2005DHS'))) {
    dat$anc <-as.numeric(set_na(dat$s819, 8:9)) ## Tested for HIV as part of the ANC visit 
    dat$anc_result <- as.numeric(set_na(dat$s820, 9))    ## Received results as part of the ANC visit 
    dat$after_anc <- as.numeric(set_na(dat$s822, 8:9))  ## tested since ANC 
  } else if (all(dat$SurveyId %in% c("ET2005DHS"))) {
    dat$anc <-as.numeric(set_na(dat$s910j, 8:9)) ## Tested for HIV as part of the ANC visit 
    dat$anc_result <- as.numeric(set_na(dat$s910k, 9))    ## Received results as part of the ANC visit 
    dat$after_anc <- as.numeric(set_na(dat$s910m, 8:9))  ## tested since ANC 
  } else if (all(dat$SurveyId %in% c('GH2003DHS'))) {
    dat$anc <-as.numeric(set_na(dat$s816b3, 8:9)) ## Tested for HIV as part of the ANC visit 
    dat$anc_result <- as.numeric(set_na(dat$s816b5, 9))    ## Received results as part of the ANC visit 
  } else {
    dat$anc <-as.numeric(set_na(dat$v840, 8:9)) ## Tested for HIV as part of the ANC visit 
    dat$anc_result <- as.numeric(set_na(dat$v841, 9))    ## Received results as part of the ANC visit 
    dat$after_anc <- as.numeric(set_na(dat$v843, 8:9))  ## tested since ANC 
  }
  
  if (all (is.na(dat$v840))) { print("no anc data")}
  
  dat$labor<-as.numeric(set_na(dat$v840a, 8:9)) ## Tested for HIV during labor
  dat$labor_result<-as.numeric(set_na(dat$v841a, 8:9))   ## Received results as part of labor 
  
  dat$ancTest<- dat$anc
  dat$ancTest[dat$anc == 0 | dat$anc_result == 0] <- 0
  dat$laborTest<- dat$labor
  dat$laborTest[dat$labor == 0 | dat$labor_result == 0] <- 0
  
  
  if (all(is.na(dat$v840a))) { ## if testing during labor not collected directly use anc test
    dat$anc_labor_test <- dat$ancTest
  } else {
    ## tested at the ANC or during labor. we onlyl care about those who did test (to remove them from denom) so how we code 0 does not really matter
    dat$anc_labor_test <- apply(dat[ , c('ancTest', 'laborTest')], 1, 
                                function(m) { 
                                  ifelse(any(m %in% 1, na.rm = TRUE), 1,
                                         ifelse (all (m %in% 0), 0, NA))})
  }
  
  # Final evertest 
  ## if evertest, or tested during labor (and received) or during anc (and received), or afterward, evertest 
  dat$evertest[dat$evertest == 1 | dat$laborTest == 1 | dat$ancTest == 1 | dat$after_anc == 1  ] <- 1
  
  dat$plhiv_oldtesting <- ifelse(dat$evertest == 1 & dat$test12m == 0 &  dat$hivstatus == 1, 1, 
                                 ifelse (dat$evertest == 1 & dat$test12m == 1 &  dat$hivstatus == 1, 0, 
                                         ifelse (dat$evertest == 0 & dat$hivstatus == 1, 0, NA)))
  
  ## Self-testing
  if (is.labelled(dat$v856)) {
    if (grepl( "NA", labelled:: var_label (dat$v856) ) )  { dat$self_test <- NA
    } else {
      dat$self_test <- as.numeric(set_na(dat$v856, c(3:9)) == 1) ;
      dat$self_test[dat$everheard == 0] <- 0  ## if never heard never tested  
    }
  }
  
  if (!is.labelled(dat$v856)) {
    if (all(is.na(dat$v856))) { dat$self_test <- NA }  
  }
  

  # ART uptake ----
  
  ## Self-reported ART uptake (LSO - Lesotho does not collect)
  
  if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
    dat$ever<- as.numeric(set_na(dat$s718, 8:9)) ## ever taken ARVs?
    dat$curr<- as.numeric(set_na(dat$s719, 9))  ## currently taking ARVs
  }
  if(all(dat$SurveyId %in% c("MZ2015AIS"))) { ## self reported HIV and subsequently ART status is only collected in MZ 
    dat$hiv_self<-as.numeric(set_na(dat$s706, c(3, 4,9))) 
   } else if (!all(is.na(dat$v861))) {   
    dat$hiv_self<-as.numeric(set_na(dat$v861, c(3, 4, 5, 9)) < 2 ) 
   ## DHS 8 2 - negative; 3 - indeterminate ; 4 - declined to answer; 5 - did not receive results; 9 - missing 
   } else { 
     dat$hiv_self <- NA 
  }
 
  ## biomarker based ART uptake (LSO does not collect)
  if(all(dat$SurveyId %in% c("MZ2015AIS"))) {
    dat$arv_bio <- as.numeric(set_na(dat$sbioarv, 9)) 
  } else { dat$arv_bio  <- NA}

  
  ## create self-reported ARV variable 
  if (all(dat$SurveyId %in% c("MZ2015AIS"))) { 
    dat$arv_self <- apply(dat[, c("curr","ever")], 1, 
                          function(y) { 
                            ifelse(any(y == 0, na.rm = TRUE), 0, # Dummy based on self-report: if either EVER or current use = 0,   code 0  (if never that meansn they are not taking it now)
                            ifelse (all(is.na(y)), NA, 1 ))})     # if both ever and current use = 1, code 1, otherwise NA
    dat$arv <- apply(dat[, c("arv_self","arv_bio")], 1, 
                     function(y) { 
                            ifelse(any(y == 1, na.rm = TRUE), 1,  # if either or both are 1, code as 1 
                            ifelse (all(is.na(y)), NA, 0 ))})     # if all are NA, code as NA. otherwise "No" (so those with one of them as "No" and other as NA will be coded as "No")
  } else { 
    dat$arv <- NA }
  ##' Non-disclosure of status: people who are unaware of their status but are actually living with HIV.
  ##' Unaware of status are people who have ever-tested but did not receive results OR people who have never tested 
  ##' Are actually living with HIV are people who have ARTs in blood. 

  ##' If someone reported that they never tested but they have ARTs in blood, they must be living with HIV and have taken ART before. Thus they must have known their status.  In this case the ART variable is based on biomarkers only because non-tested people are not asked about their HIV or ART uptake status 
  ##' If someone said they never tested and do not have ART is blood, they must only JUST (during the survey) tested for HIV and ART. Thus, its possible they were not on ART before. Again, people not tested for HIV are not asked about self-reported ART/ HIV status so the 'arv' variable is based on biomarkers
  ##' If someone said they tested they can be on ART or not, not coded as non-disclosing 
  
  dat$nondisc <- ifelse(dat$evertest == 0 & dat$arv_bio == 1, 1, 
                        ifelse(dat$evertest == 0 & dat$arv_bio == 0, 0, 
                        ifelse(dat$evertest == 1 & (dat$arv_bio == 1 | dat$arv_bio == 0), 0, NA)))
  
  ## If (in MZ only) someone reported being HIV negative (s706 = 0) but they have ART in blood, code as nondisclosure
  ## here 'arv' variable is only based on biomrker test because people who self-report HIV negativity are not asked self reported ART test 
  dat$nondisc[dat$hiv_self == 0 & dat$arv_bio == 1] <- 1
  
  # Viral suppression ----

  if (all(dat$SurveyId %in% c("MZ2015AIS", "LS2014DHS")))  {
    dat$sviral <- as.numeric(set_na(dat$sviral, c(999999, 999994, 999993, 9999999))) 
    dat$vlsup <- ifelse(dat$sviral <= 999 | dat$sviral == 999992 |dat$sviral == 999991, 1, 0)
  } else { 
    dat$vlsup <- NA }

  
 # HIV Incidence ---- 
  if (all(dat$SurveyId %in% c("MZ2015AIS"))) {
    dat$recHIV <- as.numeric(set_na(dat$slagrecn, 9) < 2) 
  } else { 
    dat$recHIV <- as.numeric(set_na(dat$srecent, 7:9) < 2)
  }
  
  
  if (all(dat$SurveyId %in% c("MZ2015AIS"))) {
    dat$incidence <-ifelse (dat$hivstatus %in% c(1) & dat$recHIV %in% (0), 0, 
                            ifelse (dat$hivstatus %in% c(1) & dat$recHIV %in% (1), 1, 
                            ifelse (dat$hivstatus %in% c(0), 0, NA))) 
  } else { 
    dat$incidence <- dat$recHIV ## LSO incorporates HIV negative people already in their incidence measure 
  }
  
  dat$med_stig <- NA
  dat$adh_bin <- NA
  dat$stig_anticip <- NA

  if (all(dat$SurveyId %in% c("MZ2015AIS"))) {
    cd4cat <- c('0-99', '100-199', '200-349', '350-499', '500+')
    dat$cd4_cat <- cut(dat$shiv50, breaks = c(0, 100, 200, 350, 500, Inf), 
                       labels = cd4cat, TRUE, FALSE)
    ## If more then 350 cd4 count, 1 otherwise 0. So stigma should be negative associated with cd4 ==1
    dat$cd4_bin <- ifelse (dat$shiv50 < 200, 0, 
                  ifelse (dat$shiv50 >= 200, 1, NA))
  } else { 
    dat$cd4_cat <- NA
    dat$cd4_bin <- NA
  }
 
  #' subset data to correct denominators. Some surveys did not ask recent testing question to those women who received a recent HIV test at ANC. 
  #' Therefore, we remove those from the denominator recent testing. 
  #' This means that they are removed from ever-testing denominator as well, but this is fine since the denoms should be the same
  if (all(dat$SurveyId %in% c("CM2004DHS"))) {
    dat <- dat[!(dat$s816e %in% 1), ] # CM2004 not ask recent testing question to those women who received a recent HIV test at ANC (s816 = 1)
  } else if (all(dat$SurveyId %in% c("ZW2005DHS"))) { 
    dat <- dat[!(dat$v843 %in% 0), ] # ZW2005 did not ask recent testing question to women who did NOT test for HIV since their last HIV test as part of ANC test (v843 = 0 )
  } else if (all(dat$SurveyId %in% c("MW2010DHS"))) {
    dat <- dat[!(dat$v843 %in% 0), ]  # MW2010 did not ask recent recent testing question to women who did NOT test for HIV since their last HIV test as part of ANC test (v843 = 0 )
  } else {
    dat <- dat
  }
  
  dat <- dat[, c( "country", 'iso', "surveyid", "survyear", "psu", "psu_u", "stratum", 'indweight' ,'hivweight',  'indweight_std' ,'hivweight_std',  "ad1", "sex",
                "age", "agegr", "edu",  "wealth", 'low2q', "restype", "hiv_self", 'hivstatus', 'everheard',
                "comp_know", "stig",  'med_stig', 'stig_perc', 'stig_anticip', 'social_judge', 'social_judge2', 'cd4_cat', 'cd4_bin', "anc_labor_test", 'self_test',
                "evertest", "test12m", 'nondisc', "arv", 'arv_bio', 'adh_bin', "vlsup", "incidence", 'plhiv_oldtesting')]
  
  dat <- as.data.frame(dat)
  return(dat)
}

# Apply recode function to list of surveys and merge output to create one data frame
datr_pars <- do.call(rbind, lapply(datlst, recode_dhs))

# Save dataframe of surveys to file ----
saveRDS(datr_pars, paste0(path_data, "/DHS_stg_0416.rds"))

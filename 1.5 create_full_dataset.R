
#### ---- Set up ----

# load libraries
library(dplyr)
library(countrycode)

# set filepaths
path_data <- here::here( "data" )
path_out <- here::here( "outputs" )

# read in and join all data 
dhs_dt  <- as.data.frame( readRDS( paste0(path_data, "/DHS_stg_0416.rds" ) ) )
phia_dt <- as.data.frame( readRDS( paste0(path_data, "/PHIA_stg_0403.rds" ) ) ) 
kais_dt <- as.data.frame( readRDS( paste0(path_data, "/KAIS_stg_0403.rds" ) ) )
sabssm_dt <- as.data.frame( readRDS( paste0(path_data, "/SABSSM_stg_0403.rds" ) ) )
bais_dt <- as.data.frame( readRDS( paste0(path_data, "/BAIS_stg_0403.rds" ) ) )
nais_dt <- as.data.frame( readRDS( paste0(path_data, "/NAIS_stg_0403.rds" ) ) )
dat_ <- rbind( dhs_dt, phia_dt, kais_dt, sabssm_dt, bais_dt, nais_dt )

popl <- read.csv( paste0( getwd(), "/population.csv" ) )[ ,-1 ]
#View(subset (dat, is.na(stratum)))

dat <- left_join( dat_, popl, by = c( "iso" = 'iso3' ) )

#### ---- Data maniuplation ----

dat <- dat %>%
  mutate( across( everything(), ~ replace( ., is.nan( . ), NA ) ) )

# order the data by psu
dat <- dat[ order( dat$psu_u ), ]  

# calculate weights
dat <- dat %>%
  group_by( iso ) %>% 
  mutate( wtind = indweight / sum( indweight, na.rm = T ) ) %>%
  mutate( wthiv = hivweight / sum( hivweight, na.rm = T ) ) %>% 
  ungroup() %>% 
  mutate( std_indweight = wtind * pop_2023 ) %>%
  mutate( std_hivweight = wthiv * pop_2023 )

# set the country codes
dat$reg <- countrycode( dat$country, "country.name", "region23" )

dat$reg[ dat$reg == 'Middle Africa' ] <- "Central Africa"
rm( dhs_dt, phia_dt, kais_dt, sabssm_dt, bais_dt )

#### Create additional variables ----

# dichotomize region
dat$reg_alt <- ifelse( dat$reg %in% c( "Southern Africa", "Eastern Africa" ), "ES", 
                       ifelse( dat$reg %in% c( "Central Africa",  "Western Africa" ), "CW", NA ) )

##### Community-level stigma measures ----

# Discriminatory attitudes
dat <- dat %>%
  group_by( psu_u ) %>%
  mutate(
    # Average all values except for the index person 
    stig_avg = case_when( all( is.na( stig ) ) ~ NA_real_,
                          is.na( stig ) ~ sum( stig, na.rm = T ) / sum( ! is.na( stig ) ),
                          ! is.na( stig ) ~ ( sum( stig, na.rm = T ) - stig ) / ( sum( ! is.na( stig ) ) - 1 ) ),
    # Identify which sex was asked the question
    stig_asked_male = any( ! is.na( stig[ sex == "male" ] ) ),
    stig_asked_female = any( ! is.na( stig[ sex == "female" ] ) ),
    stig_asked_both = stig_asked_male & stig_asked_female,
    # If not asked to both, set the sex that wasn't asked to NA
    # stig_avg = ifelse( ! stig_asked_female & sex == "female", NA_real_, stig_avg ),
    # stig_avg = ifelse( ! stig_asked_male & sex == "male", NA_real_, stig_avg ) 
    stig_avg = ifelse( ! stig_asked_both, NA_real_, stig_avg ) ) %>% 
  ungroup()

# Shame of association (including secrets)
dat <- dat %>%
  group_by( psu_u ) %>%
  mutate(
    # Average all values except for the index person 
    social_judge_avg = case_when( all( is.na( social_judge ) ) ~ NA_real_,
                                  is.na( social_judge ) ~ sum( social_judge, na.rm = T ) / sum( ! is.na( social_judge ) ),
                                  ! is.na( social_judge ) ~ ( sum( social_judge, na.rm = T ) - social_judge ) / ( sum( ! is.na( social_judge ) ) - 1 ) ),
    # Identify which sex was asked the question
    social_judge_asked_male = any( ! is.na( social_judge[ sex == "male" ] ) ),
    social_judge_asked_female = any( ! is.na( social_judge[ sex == "female" ] ) ),
    social_judge_asked_both = social_judge_asked_male & social_judge_asked_female,
    # If not asked to both, set the sex that wasn't asked to NA
    # social_judge_avg = ifelse( ! social_judge_asked_female & sex == "female", NA_real_, social_judge_avg ),
    # social_judge_avg = ifelse( ! social_judge_asked_male & sex == "male", NA_real_, social_judge_avg )
    social_judge_avg = ifelse( ! social_judge_asked_both, NA_real_, social_judge_avg ) ) %>% 
  ungroup()

# Shame of association (no secrets)
dat <- dat %>%
  group_by( psu_u ) %>%
  mutate(
    # Average all values except for the index person 
    social_judge_avg2 = case_when( all( is.na( social_judge2 ) ) ~ NA_real_,
                                   is.na( social_judge2 ) ~ sum( social_judge2, na.rm = T ) / sum( ! is.na( social_judge2 ) ),
                                   ! is.na( social_judge2 ) ~ ( sum( social_judge2, na.rm = T ) - social_judge2 ) / ( sum( ! is.na( social_judge2 ) ) - 1 ) ),
    # Identify which sex was asked the question
    social_judge2_asked_male = any( ! is.na( social_judge2[ sex == "male" ] ) ),
    social_judge2_asked_female = any( ! is.na( social_judge2[ sex == "female" ] ) ),
    social_judge2_asked_both = social_judge2_asked_male & social_judge2_asked_female,
    # If not asked to both, set the sex that wasn't asked to NA
    # social_judge_avg2 = ifelse( ! social_judge2_asked_female & sex == "female", NA_real_, social_judge_avg2 ),
    # social_judge_avg2 = ifelse( ! social_judge2_asked_male & sex == "male", NA_real_, social_judge_avg2  ) 
    social_judge_avg2 = ifelse( ! social_judge2_asked_both, NA_real_, social_judge_avg2  ) ) %>% 
  ungroup()

# Perceived HIV stigma
dat <- dat %>%
  group_by( psu_u ) %>%
  mutate(
    # Average all values except for the index person 
    stig_perc_avg = case_when( all( is.na( stig_perc ) ) ~ NA_real_,
                               is.na( stig_perc ) ~ sum( stig_perc, na.rm = T ) / sum( ! is.na( stig_perc ) ),
                               ! is.na( stig_perc ) ~ ( sum( stig_perc, na.rm = T ) - stig_perc ) / ( sum( ! is.na( stig_perc ) ) - 1 ) ),
    # Identify which sex was asked the question
    stig_perc_asked_male = any( ! is.na( stig_perc[ sex == "male" ] ) ),
    stig_perc_asked_female = any( ! is.na( stig_perc[ sex == "female" ] ) ),
    stig_perc_asked_both = stig_perc_asked_male & stig_perc_asked_female,
    # If not asked to both, set the sex that wasn't asked to NA
    # stig_perc_avg = ifelse( ! stig_perc_asked_female & sex == "female", NA_real_, stig_perc_avg ),
    # stig_perc_avg = ifelse( ! stig_perc_asked_male & sex == "male", NA_real_, stig_perc_avg )
    stig_perc_avg = ifelse( ! stig_perc_asked_both, NA_real_, stig_perc_avg ) ) %>% 
  ungroup()

##### Community-level confounders ----
# for these we group at the level of psu and remove each individual's response from the average

# HIV prevalence (Admin1)
dat <- dat %>%
  group_by( surveyid, ad1 ) %>%
  mutate( hiv_avg = (sum(hivstatus, na.rm = T) - hivstatus)/(sum(!is.na(hivstatus)) - 1), ## average all values except for the actual person 
          hiv_avg = if(all(is.na(hivstatus))) NA_real_ else hiv_avg ) %>% 
  ungroup()

# Comprehensive HIV knowledge
dat <- dat %>%
  group_by(psu_u) %>%
  mutate(comp_know_avg = (sum(comp_know, na.rm = T) - comp_know) / (sum(!is.na(comp_know)) - 1), ## average all values except for the actual person
         comp_know_avg = if(all(is.na(comp_know))) NA_real_ else comp_know_avg,
         comp_know_avg  = ifelse(is.na(comp_know), (sum(comp_know, na.rm = T)/ sum(!is.na(comp_know))), comp_know_avg)) %>% 
  ungroup()

# Average age
dat <- dat %>%
  group_by( psu_u ) %>%
  mutate( age_avg = ( sum(age, na.rm = TRUE) - as.numeric( age ) ) / ( sum( !is.na( age) ) - 1 ), ## average all values except for the actual person
          age_avg = if( all (is.na( age ) ) ) NA_real_ else age_avg,
          age_avg = ifelse( is.na( age ), ( sum(age, na.rm = TRUE ) / sum( ! is.na( age ) ) ), age_avg ) ) %>% 
  ungroup() %>% 
  mutate( age_dichot = ifelse( is.na( age ) , NA, ifelse( age >= 25, "25+", "<25" ) ) )

# Wealth score
dat <- dat %>%
  group_by( psu_u ) %>%
  mutate( wealth_avg = (sum(wealth >= 3, na.rm = TRUE) - as.numeric(wealth >= 3)) / (sum(!is.na(wealth)) - 1),
          wealth_avg = if(all(is.na(wealth))) NA_real_ else wealth_avg,
          wealth_avg = ifelse(is.na(wealth), (sum(wealth >= 3, na.rm = TRUE) / sum(!is.na(wealth))), wealth_avg) ) %>% 
  ungroup() %>% 
  mutate( wealth_dichot = ifelse( is.na( wealth ), NA, ifelse( wealth >= 3, "3+", "<3" ) ),
          wealth = as.factor( wealth ) )

# Educational attainment
dat <- dat %>%
  group_by(psu_u) %>%
  mutate(edu_avg = (sum(edu %in% c("Secondary", "Higher"), na.rm = TRUE) - as.numeric(edu %in% c("Secondary", "Higher"))) / (sum(!is.na(edu)) - 1),
         edu_avg = if(all(is.na(edu))) NA_real_ else edu_avg,
         edu_avg = ifelse(is.na(edu), ( sum( edu %in% c("Secondary", "Higher"), na.rm = TRUE) / sum(!is.na(edu))), edu_avg)) %>% 
  ungroup() %>% 
  mutate( edu_dichot = ifelse( is.na( edu ) , NA, ifelse( edu %in% c( "Secondary", "Higher" ), "Secondary+", "<Secondary" ) ) )

##### Combined anticipated and experienced stigma measure ----

dat <- dat %>%
  mutate(
    ant_exp_stig = case_when(
      med_stig == 1 | stig_anticip == 1 ~ 1,
      med_stig == 0 & stig_anticip == 0 ~ 0,
      is.na( med_stig ) & is.na( stig_anticip ) ~ NA_real_,
      TRUE ~ 0
    )
  )

# Exclude the 5 surveys that have anticipated but not experienced stigma
# (when looking at combined anticipated and experienced stigma, we want surveys that reported both)
dat$ant_exp_stig[ dat$surveyid %in% c( "CI2017PHIA", "CM2017PHIA", "ETH2017PHIA", 
                                       "KE2018PHIA", "RW2017PHIA" ) ] <- NA 

#### ---- Exclusions ----

vars_stigma <- c( "stig_avg", "social_judge_avg", "stig_perc_avg", "stig_anticip", "med_stig" )

nSurveys_start <- dat$surveyid %>% 
  unique() %>% 
  length()

# Restriction 1: ensure keeping surveys with stigma data
dat_anyStigma <- dat %>%
  group_by( surveyid ) %>%
  filter( any( !is.na( across( all_of( vars_stigma ) ) ) ) ) %>%
  ungroup()
  
nSurveys_anyStigma <- dat_anyStigma %>%
  distinct( surveyid ) %>%
  nrow()

# type of surveys
# dat_anyStigma %>% filter( grepl( "DHS", surveyid ) ) %>% distinct(surveyid) %>% nrow()
# dat_anyStigma %>% filter( grepl( "KAIS", surveyid ) ) %>% distinct(surveyid) %>% nrow()
# dat_anyStigma %>% filter( grepl( "BAIS", surveyid ) ) %>% distinct(surveyid) %>% nrow()
# dat_anyStigma %>% filter( grepl( "AIS", surveyid ) ) %>% distinct(surveyid) %>% nrow()
# dat_anyStigma %>% filter( grepl( "PHIA", surveyid ) ) %>% distinct(surveyid) %>% nrow()
# dat_anyStigma %>% filter( grepl( "NAIIS", surveyid ) ) %>% distinct(surveyid) %>% nrow()
# dat_anyStigma %>% filter( grepl( "SABSSM", surveyid ) ) %>% distinct(surveyid) %>% nrow()

# check which surveys were pulled but are missing stigma data
# dat %>%
#   filter( ! surveyid %in% dat_anyStigma$surveyid ) %>%
#   distinct( surveyid )

# Restriction 2: 
## Q, do we restrict to surveys that only ask stigma questions to both sexes?
# check by sex (e.g. "BF2003DHS" )

# dat_anyStigma_bothSexes <- 
  # dat_anyStigma %>%
  # group_by( surveyid, sex ) %>%
  # filter( any( is.na( across( all_of( vars_stigma ) ) ) ) ) %>%
  # ungroup() %>%
  #   distinct( surveyid )
  # 
  # test <- dat_anyStigma %>%
  #   group_by( surveyid ) %>%
  #   summarize( across( all_of( vars_stigma ), 
  #                    ~ case_when(
  #                      all( !is.na( .x ) & sex == "male" ) ~ "male only",
  #                      all( !is.na( .x ) & sex == "female" ) ~ "female only",
  #                      TRUE ~ "both"
  #                    ), .names = "asked_{.col}" ) ) %>%
  #   ungroup()
  #   
  # test %>% 
  #   filter( if_any( starts_with( "asked_") , ~ .x != "both" ) ) %>%
  #   distinct( surveyid ) 
  # 
  # table( dat_anyStigma$sex[ dat_anyStigma$surveyid == "BF2003DHS" ], 
  #        dat_anyStigma$stig[ dat_anyStigma$surveyid == "BF2003DHS" ], 
  #        useNA = 'ifany' )

#### ---- Save the dataset ----

# saveRDS( dat_anyStigma, paste0( path_data, "/all_surveys_", Sys.Date(), ".rds" ) )

#### Flow chart ----

# number of surveys with all stigma measures
nSurveys_allStigma <- dat_anyStigma %>%
  group_by( surveyid ) %>%
  filter( all( across( all_of( vars_stigma ), ~ any( !is.na( . ) ) ) ) ) %>%
  ungroup() %>%
  distinct( surveyid ) %>%
  nrow()

# number of surveys with only one stigma measure
nSurveys_oneStigma <- dat_anyStigma %>%
  group_by( surveyid ) %>%
  filter( sum( across( all_of( vars_stigma ), ~ any( ! is.na( . ) ) ) ) == 1 ) %>%
  ungroup() %>%
  distinct( surveyid ) %>%
  nrow()

# number of surveys with biomarkers
nSurveys_anyStigmaAndBiomarkers <- dat_anyStigma %>%
  group_by( surveyid ) %>%
  filter( any( ! is.na( hivstatus ) ) ) %>%
  ungroup() %>%
  distinct( surveyid ) %>%
  nrow()

# stigma and testing
dat_anyStigmaAndTesting <- dat_anyStigma %>% 
  group_by( surveyid ) %>%
  filter( any( ! is.na( test12m ) ) ) %>%
  ungroup() 

# dat_anyStigma %>%
#   filter( ! surveyid %in% dat_anyStigmaAndTesting$surveyid ) %>%
#   distinct( surveyid )

nSurveys_anyStigmaAndTesting <- dat_anyStigmaAndTesting %>%
  distinct( surveyid ) %>%
  nrow()

# stigma and ART
dat_anyStigmaAndART <- dat_anyStigma %>% 
  group_by( surveyid ) %>%
  filter( any( !is.na( arv ) ) ) %>%
  ungroup() 

# dat_anyStigma %>%
#   filter( ! surveyid %in% dat_anyStigmaAndART$surveyid ) %>%
#   distinct( surveyid )

# any in ART that aren't in testing?
# dat_anyStigmaAndART %>%
#   filter( ! surveyid %in% dat_anyStigmaAndTesting$surveyid ) %>%
#   distinct( surveyid )

nSurveys_anyStigmaAndART <- dat_anyStigmaAndART %>%
  distinct( surveyid ) %>%
  nrow()

# stigma and VLS
dat_anyStigmaAndVLS <- dat_anyStigma %>% 
  group_by( surveyid ) %>%
  filter( any( !is.na( vlsup ) ) ) %>%
  ungroup() 

# dat_anyStigma %>%
#   filter( ! surveyid %in% dat_anyStigmaAndVLS$surveyid ) %>%
#   distinct( surveyid )

nSurveys_anyStigmaAndVLS <- dat_anyStigmaAndVLS %>%
  distinct( surveyid ) %>%
  nrow()

# overall number of surveys and participants
nSurveys_anyStigmaAndOneOutcome <- 
  c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) %>%
   unique( ) %>%
   length()

nParticipants_anyStigmaAndOneOutcome <- 
  dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  nrow()

# surveys with stigma but no outcomes
dat_anyStigma %>%
  filter( ! surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid, dat_anyStigmaAndVLS$surveyid ) ) %>%
  distinct( surveyid )

# Note: one survey has exposure measures but doesn't have PSU data, so data was not extracted and included here
nSurveysFlowChart <- 
  rbind( 
    data.frame( type = "Start", n = nSurveys_start ),
    data.frame( type = "Restriction: Surveys w any stigma measure", n = nSurveys_anyStigma ),
    data.frame( type = "Note: Surveys w all stigma measures", n = nSurveys_allStigma ),
    data.frame( type = "Note: Surveys w only one stigma measure", n = nSurveys_oneStigma ),
    data.frame( type = "Note: Surveys w any stigma measure and biomarkers", n = nSurveys_anyStigmaAndBiomarkers ),
    data.frame( type = "Surveys w any stigma measure and testing", n = nSurveys_anyStigmaAndTesting ),
    data.frame( type = "Surveys w any stigma measure and ART", n = nSurveys_anyStigmaAndART ),
    data.frame( type = "Surveys w any stigma measure and VLS", n = nSurveys_anyStigmaAndVLS ),
    data.frame( type = "Surveys w any stigma measure and at least one outcome", n = nSurveys_anyStigmaAndOneOutcome ),
    data.frame( type = "Participants in surveys w any stigma measure and at least one outcome", n = nParticipants_anyStigmaAndOneOutcome ) )

countries_anyStigmaAndOneOutcome <- dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  select( country, reg_alt, surveyid ) %>%
  distinct( country, reg_alt, surveyid ) %>%
  group_by( reg_alt ) %>%
  summarise( n_countries = n_distinct( country ),
             n_surveys = n_distinct( surveyid ) )

nSurveys_stigAvg <- dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  group_by( surveyid ) %>%
  filter( any( ! is.na( stig_avg ) ) ) %>%
  ungroup( ) %>%
  summarise( n_surveys = n_distinct( surveyid ),
             n_participants = n( ) )

nSurveys_socialJudgeAvg <- dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  group_by( surveyid ) %>%
  filter( any( ! is.na( social_judge_avg ) ) ) %>%
  ungroup( ) %>%
  summarise( n_surveys = n_distinct( surveyid ),
             n_participants = n( ) )

nSurveys_stigPercAvg <- dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  group_by( surveyid ) %>%
  filter( any( ! is.na( stig_perc_avg ) ) ) %>%
  ungroup( ) %>%
  summarise( n_surveys = n_distinct( surveyid ),
             n_participants = n( ) )

nSurveys_stigAnticip <- dat_anyStigma %>% 
  filter( surveyid %in% dat_anyStigmaAndART$surveyid ) %>%
  group_by( surveyid ) %>%
  filter( any( ! is.na( stig_anticip ) ) ) %>%
  ungroup( ) %>%
  filter( hivstatus == 1 ) %>%
  summarise( n_surveys = n_distinct( surveyid ),
             n_participants = n( ) )

nSurveys_medStig <- dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  group_by( surveyid ) %>%
  filter( any( ! is.na( med_stig ) ) ) %>%
  ungroup( ) %>%
  filter( hivstatus == 1 ) %>%
  summarise( n_surveys = n_distinct( surveyid ),
             n_participants = n( ) )

nSurveys_stigAnticipAndMedAvg <- dat_anyStigma %>%
  group_by( surveyid ) %>%
  filter( all( across( all_of( c( "stig_anticip", "med_stig" ) ), ~ any( !is.na( . ) ) ) ) ) %>%
  ungroup() %>%
  filter( hivstatus == 1 ) %>%
  summarise( n_surveys = n_distinct( surveyid ),
             n_participants = n( ) )

# summary of survey years
dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  select( surveyid, survyear ) %>% 
  unique() %>% 
  mutate( survyear = as.numeric( survyear ) ) %>% 
  arrange( survyear ) %>%
  summarize( median = median( survyear ),
             q25 = quantile( survyear, probs = 0.25 ),
             q75 = quantile( survyear, probs = 0.75 ),
             min = min( survyear ),
             max = max( survyear ) )

# summary of participants per psu
dat_anyStigma %>% 
  filter( surveyid %in% c( dat_anyStigmaAndTesting$surveyid, dat_anyStigmaAndART$surveyid ) ) %>%
  group_by( psu_u ) %>% 
  summarize( n = n() ) %>% 
  summarize( nPartPerPSU_median = median( n ),
             nPartPerPSU_q25 = quantile( n, probs = 0.25 ),
             nPartPerPSU_q75 = quantile( n, probs = 0.75 ),
             nPartPerPSU_min = min( n ), 
             nPartPerPSU_max = max( n ), 
             nPSUs = n() )

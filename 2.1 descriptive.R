rm(list = ls()) #clears
gc()

#### ---- Set up ----

# load libraries
library( tidyr )
library( dplyr )
library( rlang )
library( survey )
library( openxlsx )
library( ggplot2 )
library( stringr )

# set filepaths
path_data <- here::here( "data" )
path_out <- here::here( "outputs/tables" )

# source funtions
source( "0.0-functions.R" )

# read in the data 
dat <- as.data.frame(readRDS(paste0(path_data, "/all_surveys_2025-04-16.rds")))

# specify the exposure and covariate variable names
vars <- list()
vars$stig_ind <- c( "stig", "stig_perc", "social_judge", "med_stig", "stig_anticip", "ant_exp_stig" )
vars$stig_comm <- c( "stig_avg", "stig_perc_avg", "social_judge_avg" )
vars$demo  <- c( "sex", "agegr", "edu",  "restype", "wealth", "comp_know", "hivstatus" )
vars$out <- c( "test12m", "arv", "vlsup")

resLists <- list( )

#### ---- Summarize the data ----

##### * Number of countries with data (per exposure) ----
unique_country_counts <- sapply( c( vars$stig_ind, vars$stig_comm ), function( x ) {
  dat %>%
    filter( !is.na( .data[[ x ]] ) ) %>%
    summarise( unique_countries = n_distinct( surveyid ) ) %>%
    pull( unique_countries ) 
  } )

dat <- dat %>% 
  group_by( surveyid ) %>% 
  filter( any( !is.na( arv ) ) | any( !is.na( test12m ) ) | any( !is.na( vlsup ) ) ) %>%
  ungroup()
  

##### * Demographics (per exposure) ----
resLists$demographics <- fn_demographics( df = dat, varlist = vars$demo, explist = vars$stig_ind )

##### * Prevalence of each individual-level exposure ----
resLists$expPrevalence <- fn_expPrevalence( df = dat, explist = vars$stig_ind )

##### * Summary of each community-level exposure ----
resLists$commExpSummary <- fn_commExpSummary( df = dat, explist = vars$stig_comm )

##### * Prevalence of each outcome by exposure ----
resLists$outPrevbyExp <- fn_outPrev( df = dat, outlist = vars$out )

##### * Summary of each community-level exposure by outcome ----
resLists$outSummarybycommExp <- fn_outSummary( df = dat, outlist = vars$out )

#### ---- Save results files ----

# save_resLists_to_excel( resLists = resLists, save_file_path = path_out )



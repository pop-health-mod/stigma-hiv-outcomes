
# ---- Set up ----

# load libraries
library( dplyr )
library( rlang )
library( geepack )
library( stringr )
library( tidyr )
library( MuMIn )

# set seed
set.seed( 555 )

# set filepaths
path_data <- here::here( "data" )
path_out <- here::here( "outputs/R-regression-results/" )

# source funtions
source( "0.0-functions.R" )

# read in the data 
dat <- as.data.frame( readRDS( paste0( path_data, "/all_surveys_2025-04-16.rds" ) ) )

# ---- Define variable sets ----
vars <- list()

# community- and individual-level exposures
vars$exp$main$stig_comm <- c( "stig_avg", "stig_perc_avg", "social_judge_avg" )
vars$exp$main$stig_ind <- c( "ant_exp_stig" )

# main confounder sets per exposure
vars$covars$main$stig_avg <- c( "age_avg", "edu_avg", "wealth_avg", "restype", "hiv_avg", "surveyid" ) 
vars$covars$main$stig_perc_avg <- c( "age_avg", "wealth_avg", "stig_avg", "surveyid" ) 
vars$covars$main$social_judge_avg <- c( "age_avg", "wealth_avg", "stig_avg", "surveyid" )
vars$covars$main$ant_exp_stig <- c( "sex", "age", "edu", "wealth", "stig_avg", "surveyid" )

vars$out <- c( "test12m", "arv", "vlsup" ) # "cd4_bin", "adh_bin"

# ---- Run regression models ----- 

## * Main analysis ----
# No effect modifiers modelled
res_main <- runGEE( df = dat, 
                    explist = vars$exp$main, 
                    outlist = vars$out, 
                    covarlist = vars$covars$main,
                    predictions = TRUE,
                    nSamples = 1000,
                    nCores = 6,
                    save = TRUE, 
                    save_file_path = paste0( path_out, "MAIN_" ) )

## * EMM: region ----
# For community-level stigma measures and HIV testing only
vars$exp$emmRegion$stig_comm <- vars$exp$main$stig_comm
vars$exp$emmRegion$stig_ind <- NULL

res_emm_reg <- runGEE( df = dat,
                       explist = vars$exp$emmRegion,
                       outlist = "test12m",
                       covarlist = vars$covars$main,
                       intervar = "reg_alt",
                       intervar_covar = "reg_alt",
                       predictions = TRUE,
                       intervarLevel1 = "ES",
                       intervarLevel0 = "CW",
                       nSamples = 1000,
                       nCores = 6,
                       save = TRUE,
                       save_file_path = paste0( path_out, "EMM_" ) )

## * EMM: sex ----
res_emm_sex <- runGEE( df = dat, 
                       explist = vars$exp$main, 
                       outlist = vars$out, 
                       covarlist = vars$covars$main, 
                       intervar = "sex",
                       intervar_covar = "sex",
                       predictions = TRUE,
                       intervarLevel1 = "male",
                       intervarLevel0 = "female",
                       nSamples = 1000,
                       nCores = 4,
                       save = TRUE, 
                       save_file_path = paste0( path_out, "EMM_" ) )

## * EMM: age ----
res_emm_age <- runGEE( df = dat,
                       explist = vars$exp$main,
                       outlist = vars$out,
                       covarlist = vars$covars$main,
                       intervar = "age_dichot",
                       intervar_covar = "age",
                       predictions = TRUE,
                       intervarLevel1 = "25+",
                       intervarLevel0 = "<25",
                       nSamples = 1000,
                       nCores = 6,
                       save = TRUE, 
                       save_file_path = paste0( path_out, "EMM_" ) )

## * EMM: urban/rural ----
res_emm_restype <- runGEE( df = dat, 
                           explist = vars$exp$main, 
                           outlist = vars$out, 
                           covarlist = vars$covars$main, 
                           intervar = "restype", 
                           intervar_covar = "restype",
                           predictions = TRUE,
                           intervarLevel1 = "Urban",
                           intervarLevel0 = "Rural",
                           nSamples = 1000,
                           nCores = 6,
                           save = TRUE, 
                           save_file_path = paste0( path_out, "EMM_" ) )

## * EMM: wealth ----
res_emm_wealth <- runGEE( df = dat, 
                          explist = vars$exp$main, 
                          outlist = vars$out, 
                          covarlist = vars$covars$main,
                          intervar = "wealth_dichot",
                          intervar_covar = "wealth",
                          predictions = TRUE,
                          intervarLevel1 = "3+",
                          intervarLevel0 = "<3",
                          nSamples = 1000,
                          nCores = 6,
                          save = TRUE, 
                          save_file_path = paste0( path_out, "EMM_" ) )

## * EMM: education ----
res_emm_education <- runGEE( df = dat,
                             explist = vars$exp$main,
                             outlist = vars$out, 
                             covarlist = vars$covars$main,
                             intervar = "edu_dichot",
                             intervar_covar = "edu",
                             predictions = TRUE,
                             intervarLevel1 = "Secondary+",
                             intervarLevel0 = "<Secondary",
                             nSamples = 1000,
                             nCores = 6,
                             save = TRUE,
                             save_file_path = paste0( path_out, "EMM_" ) )

## * SA: Removing PLHIV from testing analysis ---- 

# For community-level stigma measures and HIV testing only
vars$exp$sa_noPLHIV$stig_comm <- vars$exp$main$stig_comm
vars$exp$sa_noPLHIV$stig_ind <- NULL

# No effect modifiers modelled
res_sa_noPLHIV <- runGEE( df = dat[ dat$hivstatus == 0 | ( dat$hivstatus == 1 & dat$evertest == 0 ), ],
                          explist = vars$exp$sa_noPLHIV, 
                          outlist = "test12m",
                          covarlist = vars$covars$main,
                          predictions = TRUE,
                          nSamples = 1000,
                          nCores = 4,
                          save = TRUE, 
                          save_file_path = paste0( path_out, "SA_noPLHIV_" ) )

## * SA: Removing PLHIV not on ART from viral suppression analysis ---- 

# No effect modifiers modelled
res_sa_onART <- runGEE( df = dat[ dat$hivstatus == 1 & dat$arv == 1, ],
                        explist = vars$exp$main,
                        outlist = "vlsup",
                        covarlist = vars$covars$main,
                        predictions = FALSE,
                        save = TRUE, 
                        save_file_path = paste0( path_out, "SA_onART_" ) )


## * SA: Shame of association w/o secret questions ---- 
vars$exp$sa$noSecret$stig_comm <- "social_judge_avg2"
vars$covars$sa$noSecret$social_judge_avg2 <- vars$covars$main$social_judge_avg

res_sa_noSecret <- runGEE( df = dat,
                           explist = vars$exp$sa$noSecret,
                           outlist = vars$out,
                           covarlist = vars$covars$sa$noSecret,
                           save = TRUE, 
                           save_file_path = paste0( path_out, "SA_noSecret_" ) )

# These are the 7 surveys that will be removed: 
# CI2017PHIA KE2018PHIA LS2016PHIA MW2015PHIA TZ2016PHIA ZM2016PHIA ZW2015PHIA


## * SA: Adding perceived stigma as a confounder ---- 
vars$exp$sa$addConfPercStig$stig_comm <- "social_judge_avg"
vars$exp$sa$addConfPercStig$stig_ind <- "ant_exp_stig"

vars$covars$sa$addConfPercStig$social_judge_avg <- c( vars$covars$main$social_judge_avg, "stig_perc_avg" )
vars$covars$sa$addConfPercStig$ant_exp_stig <- c( vars$covars$main$ant_exp_stig, "stig_perc" )

res_sa_addConfPercStig <- runGEE( df = dat,
                                  explist = vars$exp$sa$addConfPercStig,
                                  outlist = vars$out,
                                  covarlist = vars$covars$sa$addConfPercStig,
                                  save = TRUE, 
                                  save_file_path = paste0( path_out, "SA_addConfPercStig_" ) )


## * SA: Adding HIV knowledge as a confounder ---- 

vars$covars$sa$addConfHIVKnow <- vars$covars$main
vars$covars$sa$addConfHIVKnow$stig_avg <- c( vars$covars$sa$addConfHIVKnow$stig_avg, "comp_know_avg" )
vars$covars$sa$addConfHIVKnow$stig_perc_avg <- c( vars$covars$sa$addConfHIVKnow$stig_perc_avg, "comp_know_avg" )
vars$covars$sa$addConfHIVKnow$social_judge_avg <- c( vars$covars$sa$addConfHIVKnow$social_judge_avg, "comp_know_avg" )
vars$covars$sa$addConfHIVKnow$ant_exp_stig <- c( vars$covars$sa$addConfHIVKnow$ant_exp_stig, "comp_know" )

res_sa_addConfHIVKnow <- runGEE( df = dat,
                                 explist = vars$exp$main,
                                 outlist = vars$out,
                                 covarlist = vars$covars$sa$addConfHIVKnow,
                                 save = TRUE, 
                                 save_file_path = paste0( path_out, "SA_addConfHIVKnow_" ) )

## * SA: individual anticipated and experienced stigma ---- 
vars$exp$sa$indAntAndExp$stig_ind <- c( "stig_anticip", "med_stig" )

vars$covars$sa$indAntAndExp$med_stig <- c( "sex", "age", "edu", "wealth", "stig_avg", "stig_anticip", "surveyid" )
vars$covars$sa$indAntAndExp$stig_anticip <- c( "sex", "age", "edu", "wealth", "stig_avg", "med_stig", "surveyid" )

res_sa_indAntAndExp <- runGEE( df = dat, 
                               explist = vars$exp$sa$indAntAndExp, 
                               outlist = vars$out, 
                               covarlist = vars$covars$sa$indAntAndExp, 
                               save = TRUE, 
                               save_file_path = paste0( path_out, "SA_indAntAndExp_" ) )

## * SA: individual perceived stigma ---- 
vars$exp$sa$indPerceived$stig_ind <- c( "stig_perc" )

vars$covars$sa$indPerceived$stig_perc <- c( "sex", "edu", "age", "wealth", "stig_avg", "surveyid" )

res_sa_indAntAndExp <- runGEE( df = dat, 
                               explist = vars$exp$sa$indPerceived, 
                               outlist = vars$out, 
                               covarlist = vars$covars$sa$indPerceived, 
                               save = TRUE, 
                               save_file_path = paste0( path_out, "SA_indPerceived_" ) )

## * SA: Heterogeneity ---- 
vars$covars$hetero <- lapply( vars$covars$main, function( x ){ setdiff( x, "surveyid" ) } )

surveys_hetero <- dat %>% select( surveyid ) %>% distinct( ) %>% unlist() %>% unname() %>% as.character()
noHIVPrev <- NULL # to record which surveys with discriminatory attitudes don't have this

res_sa_hetero <- lapply( surveys_hetero, function( x ) {
  
  print( as.character( x ) )
  dat_survey <- dat[ dat$surveyid == x, ]
  
  # get the exposures and outcomes that can be assessed in the survey
  exp_survey <- list( )
  exp_survey$comm <- vars$exp$main$stig_comm[ sapply( vars$exp$main$stig_comm, function( expVar ) any( ! is.na( dat_survey[[ expVar ]] ) ) ) ]
  exp_survey$ind <- vars$exp$main$stig_ind[ sapply( vars$exp$main$stig_ind, function( expVar ) any( ! is.na( dat_survey[[ expVar ]] ) ) ) ]
  if( length( exp_survey$comm ) == 0 ) { exp_survey$comm <- NULL }
  if( length( exp_survey$ind ) == 0 ) { exp_survey$ind <- NULL }
  
  out_survey <- vars$out[ sapply( vars$out, function( outVar ) any( ! is.na( dat_survey[[ outVar ]] ) ) ) ]
  
  # adapt confounder sets to remove those completely missing
  covars_survey <- vars$covars$hetero
  # if( x == "MW2010DHS" ) { covars_survey <- lapply( covars_survey, function( y ) setdiff( y, "sex" ) ) } # "MW2010DHS" only has testing among the outcomes, and only for females
  if( "stig_avg" %in% exp_survey$comm & all( is.na( dat_survey[[ "hiv_avg" ]] ) ) ) { covars_survey$stig_avg <- setdiff( covars_survey$stig_avg, "hiv_avg" ); noHIVPrev <<- c( noHIVPrev, x ) }
  if( x %in% c( "NA2017PHIA", "SW2016PHIA", "UG2016PHIA", "NG2018NAIIS" ) ) { covars_survey <- lapply( covars_survey, function( y ) setdiff( y, "stig_avg" ) ) }
  if( x == "ZA2012SABSSM" ) { covars_survey <- lapply( covars_survey, function( y ) setdiff( y, c( "wealth_avg", "wealth", "edu_avg", "edu" ) ) ) } # everyone has primary school?
  if( x %in% c( "ZA2008SABSSM", "ZA2017SABSSM", "BW2013BAIS", "BW2008BAIS", "BW2001BAIS" ) ) { covars_survey <- lapply( covars_survey, function( y ) setdiff( y, c( "wealth_avg", "wealth" ) ) ) }
  
  res_survey <- runGEE( df = dat_survey,
                        explist = exp_survey,
                        outlist = out_survey,
                        covarlist = covars_survey,
                        segReg = FALSE,
                        predictions = FALSE,
                        save = FALSE )
  
  return( res_survey )
  } )

names( res_sa_hetero ) <- surveys_hetero

# saveRDS( res_sa_hetero, file = paste0( path_out, "SA_heterogeneity_regression-model-results_",  Sys.Date(), ".rds" ) )

## * SA: HIV incidence ---- 

# vars$exp$incidence$stig_comm <- vars$exp$main$stig_comm
# vars$exp$incidence$stig_ind <- NULL
# 
# res_incidence <- runGEE( df = dat, 
#                          explist = vars$exp$incidence, 
#                          outlist = "incidence", 
#                          covarlist = vars$covars$main, 
#                          save = TRUE,
#                          save_file_path = paste0( path_out, "SA_incidence_" ) )
# 
# res_incidence_noSegReg <- runGEE( df = dat, 
#                          explist = vars$exp$incidence, 
#                          outlist = "incidence", 
#                          covarlist = vars$covars$main, 
#                          save = TRUE,
#                          save_file_path = paste0( path_out, "SA_incidence_noSegReg" ) )
# 
# res_incidence_noSegReg_linear <- runGEE( df = dat, 
#                                   explist = vars$exp$incidence, 
#                                   outlist = "incidence", 
#                                   covarlist = vars$covars$main, 
#                                   save = TRUE,
#                                   save_file_path = paste0( path_out, "SA_incidence_noSegReg_linear" ) )
# 
# table( dat$incidence, useNA = 'ifany' )
# 
# dat %>%
#   select( stig_avg, incidence ) %>%
#   na.omit( ) %>% 
#   mutate( segment = ifelse( stig_avg < 0.17, "Below 0.17", "Above 0.17" ) ) %>%
#   group_by( segment, incidence ) %>% 
#   summarise( n = n() )
# 
# dat %>%
#   select( stig_perc_avg, incidence ) %>%
#   na.omit( ) %>% 
#   mutate( segment = ifelse( stig_perc_avg < 0.69, "Below 0.69", "Above 0.69" ) ) %>%
#   group_by( segment, incidence ) %>% 
#   summarise( n = n() )
# 
# dat %>%
#   select( social_judge_avg, incidence ) %>%
#   na.omit( ) %>% 
#   mutate( segment = ifelse( social_judge_avg < 0.15, "Below 0.15", "Above 0.15" ) ) %>%
#   group_by( segment, incidence ) %>% 
#   summarise( n = n() )
# 
# library(ggplot2)
# 
# dat %>%
#   select( stig_avg, incidence ) %>%
#   na.omit( ) %>%
#   ggplot( aes(x = stig_avg, color = as.factor(incidence))) +
#   geom_density() +
#   labs(title = "Density of Stig Avg by Incidence", color = "Incidence")
# 
# dat %>%
#   select( stig_perc_avg, incidence ) %>%
#   na.omit( ) %>%
#   ggplot( aes(x = stig_perc_avg, color = as.factor(incidence))) +
#   geom_density() +
#   labs(title = "Density of Stig Perc Avg by Incidence", color = "Incidence")
# 
# dat %>%
#   select( social_judge_avg, incidence ) %>%
#   na.omit( ) %>%
#   ggplot( aes(x = social_judge_avg, color = as.factor(incidence))) +
#   geom_density() +
#   labs(title = "Density of Social Judge Avg by Incidence", color = "Incidence")



